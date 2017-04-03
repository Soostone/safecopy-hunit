{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           Data.Foldable       (forM_)
import           Data.List           (isPrefixOf)
import           Data.SafeCopy
import           Path
import           Path.IO
import qualified Test.HUnit.Lang     as HU
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Test.HUnit.SafeCopy
-------------------------------------------------------------------------------


data TrivialRecord_v0 = TrivialRecord_v0
                      deriving (Show, Eq)


data TrivialRecord_v1 = TrivialRecord_v1 Int
                      deriving (Show, Eq)


data TrivialRecord = TrivialRecord Int String
                   deriving (Show, Eq)

testFilesDir :: Path Rel Dir
testFilesDir = $(mkRelDir "test/files")

trivialRecordBase :: Path Rel File
trivialRecordBase = testFilesDir </> $(mkRelFile "TrivialRecord")


$(deriveSafeCopy 0 'base ''TrivialRecord_v0)
$(deriveSafeCopy 1 'extension ''TrivialRecord_v1)
$(deriveSafeCopy 2 'extension ''TrivialRecord)


instance Migrate TrivialRecord_v1 where
  type MigrateFrom TrivialRecord_v1 = TrivialRecord_v0
  migrate TrivialRecord_v0 = TrivialRecord_v1 42


instance Migrate TrivialRecord where
  type MigrateFrom TrivialRecord = TrivialRecord_v1
  migrate (TrivialRecord_v1 n) = TrivialRecord n "example"


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "safecopy-hunit"
  [ testCase "tests multiple versions" $
      testSafeCopy FailMissingFiles trivialRecordBase trivialRecord
  , testCase "can ignore missing versions" $ withVersionGaps $ \tmpDir ->
      testSafeCopy IgnoreMissingFiles (tmpDir </> $(mkRelFile "TrivialRecord")) trivialRecord
  , testCase "can fail on missing versions" $ withVersionGaps $ \tmpDir -> do
      res <- try (testSafeCopy FailMissingFiles (tmpDir </> $(mkRelFile "TrivialRecord")) trivialRecord)
      let expectedMsg = "Missing files for the following versions: 1"
      case fmapL getReason res of
        Left (Just actualMsg) -> assertEqual "" expectedMsg actualMsg
        _ -> assertFailure ("Expected failure reason of \"" ++ expectedMsg ++ "\" but got " ++ show res)
  , testCase "can fail on safecopy errors" $ withTempFiles [
        $(mkRelFile "TrivialRecord.0")
      , $(mkRelFile "TrivialRecord.1")
      ] $ \tmpDir -> do
      let badFile = (tmpDir </> $(mkRelFile "TrivialRecord.2"))
      writeFile (toFilePath badFile) "corrupted"
      res <- try (testSafeCopy FailMissingFiles (tmpDir </> $(mkRelFile "TrivialRecord")) trivialRecord)
      let pfx = "SafeCopy error in " ++ toFilePath badFile ++ ":"
      case fmapL getReason res of
        Left (Just actualMsg)
          | pfx `isPrefixOf` actualMsg -> return ()
        _ -> assertFailure ("Expected failure starting with \"" ++ pfx ++ "\" but got " ++ show res)
  ]
  where
    trivialRecord = TrivialRecord 42 "example"
    withTempFiles files callback = withSystemTempDir "safecopy-hunit-tests" $ \tmpDir -> do
      forM_ files $ \f ->
        copyFile (testFilesDir </> f) (tmpDir </> f)
      callback tmpDir
    withVersionGaps = withTempFiles
      [ $(mkRelFile "TrivialRecord.0")
      , $(mkRelFile "TrivialRecord.2")
      ]


getReason :: HU.HUnitFailure -> Maybe String
#if MIN_VERSION_HUnit(1,5,0)
getReason (HU.HUnitFailure _ r) = case r of
  HU.Reason s -> Just s
  _        -> Nothing
#else
getReason (HU.HUnitFailure _ s) = Just s
#endif

fmapL :: (a -> c) -> Either a b -> Either c b
fmapL f (Left x) = Left (f x)
fmapL _ (Right x) = Right x
