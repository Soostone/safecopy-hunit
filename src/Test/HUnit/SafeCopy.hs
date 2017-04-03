{-# LANGUAGE ScopedTypeVariables #-}
module Test.HUnit.SafeCopy
    ( testSafeCopy
    , MissingFilesPolicy(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Monad          (unless)
import qualified Data.ByteString        as BS
import           Data.List              (intercalate, stripPrefix, (\\))
import qualified Data.SafeCopy          as SC
import qualified Data.SafeCopy.Internal as SCI
import qualified Data.Serialize         as S
import qualified Path                   as P
import qualified Path.Internal          as PI
import qualified Path.IO                as PIO
import qualified System.IO              as SIO
import qualified Test.HUnit             as HU
import           Text.Read              (readMaybe)
-------------------------------------------------------------------------------


-- | Safecopy provides a list of all supported versions. If we are
-- unable to find a corresponding file for a version, what should we
-- do? A missing file typically means you forgot to run tests when you
-- were at one of the versions of @a@.
data MissingFilesPolicy = IgnoreMissingFiles
                        | WarnMissingFiles
                        | FailMissingFiles
                        deriving (Show, Eq)


-------------------------------------------------------------------------------
testSafeCopy
    :: forall a b. (SC.SafeCopy a, Eq a, Show a)
    => MissingFilesPolicy
    -> P.Path b P.File
    -- ^ Base filename, e.g. @test/data/MyType.golden@. Will be
    -- postfixed with each version, e.g. @MyType.safecopy.1@,
    -- @MyType.safecopy.2@, etc. If its a primitive value, versioning
    -- is not supported and thus no extension will be added.
    -> a
    -- ^ The current value that all past versions of this file must upgrade to.
    -> HU.Assertion
testSafeCopy missingFilesPolicy baseFile a = do
  case SC.objectProfile :: SC.Profile a of
    SC.PrimitiveProfile                         -> do
      -- dump file, test
      assertLatest baseFile
    SC.InvalidProfile e                         -> HU.assertFailure e
    SC.Profile currentVersion supportedVersions -> do
      let versions = (SCI.Version <$> supportedVersions) :: [SC.Version a]
      let currentFile = mkVersionPath (SCI.Version currentVersion) baseFile
      dumpVersionUnlessExists currentFile a
      files <- discoverSafeCopyFiles baseFile
      let missingVersions = versions \\ (scfVersion <$> files)
      --TODO: make this a warning or omit based on option
      unless (null missingVersions) $ do
        let msg = ("Missing files for the following versions: " ++ intercalate "," (show . SCI.unVersion <$> missingVersions))
        case missingFilesPolicy of
          IgnoreMissingFiles -> return ()
          WarnMissingFiles   -> SIO.hPutStrLn SIO.stderr msg
          FailMissingFiles   -> HU.assertFailure msg
      -- TODO: check versions
      mapM_ (\f -> assertFile (scfPath f) a) files
  where
    assertLatest f = do
      dumpVersionUnlessExists f a
      assertFile f a


-------------------------------------------------------------------------------
data SafeCopyFile rel a = SafeCopyFile
  { scfPath    :: P.Path rel P.File
  , scfVersion :: SC.Version a
  } deriving (Show)


-------------------------------------------------------------------------------
mkVersionPath :: SC.Version a -> P.Path rel P.File -> P.Path rel P.File
mkVersionPath (SCI.Version v) (PI.Path fp) = PI.Path (fp ++ "." ++ show v)


-------------------------------------------------------------------------------
discoverSafeCopyFiles
    :: P.Path rel P.File
    -> IO [SafeCopyFile P.Abs a]
discoverSafeCopyFiles baseFile = do
  dir <- P.parent <$> PIO.makeAbsolute baseFile
  (_, files) <- PIO.listDir dir
  return [ SafeCopyFile f v | Just (f, v) <- check <$> files]
  where
    fname = P.toFilePath . P.filename
    check f = do
      ext <- stripPrefix (fname baseFile) (fname f)
      v <- case ext of
        '.':rawVersion -> SCI.Version <$> readMaybe rawVersion
        _              -> Nothing
      return (f, v)


-------------------------------------------------------------------------------
assertFile
    :: ( SC.SafeCopy a
       , Eq a
       , Show a
       )
    => P.Path b P.File
    -> a
    -> HU.Assertion
assertFile f expected = do
  raw <- BS.readFile rawFile
  case S.runGet SC.safeGet raw of
    Left e       -> HU.assertFailure ("SafeCopy error in " ++ rawFile ++ ": " ++ e)
    Right actual -> HU.assertEqual "" expected actual
  where
    rawFile = P.toFilePath f


-------------------------------------------------------------------------------
dumpVersionUnlessExists
    :: (SC.SafeCopy a)
    => P.Path rel P.File
    -> a
    -> IO ()
dumpVersionUnlessExists f a = do
 fabs <- PIO.makeAbsolute f
 PIO.ensureDir (P.parent fabs)
 exists <- PIO.doesFileExist f
 unless exists $
   BS.writeFile (P.toFilePath f) (S.runPut (SC.safePut a))
