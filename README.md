# safecopy-hunit
[![Build Status](https://travis-ci.org/Soostone/safecopy-hunit.svg?branch=master)](https://travis-ci.org/Soostone/safecopy-hunit)

This is a simple library for ensuring testing that as you move to
newer versions of datatypes and create SafeCopy migrations, that
ByteStrings based on old versions can successfully be migrated to your
newest data structure.

The idea behind safecopy is that you can continue to upgrade types in
your Haskell applications as needed and can be sure that old
serialized values, say in a storage layer like S3 or even your
database will be safely migrated to new values. You can defer
upgrading your long term storage and when the time is right, all it
takes is a read-and-save to upgrade the values.

This library helps you during development to ensure that examples of
old data will still parse correctly and will be migrated to the
expected new values.

## How It Works
`Test.HUnit.SafeCopy` exports one function, `testSafeCopy`. This
function takes a base filename and the current value of any `SafeCopy`
type as well as a policy on what to do if there are missing test
files. From there:

1. It gets the full history of supported versions of the given record.
2. It generates filenames based on your base file. So if you have
   versions 0, 1, 2 and your base filename is `test/data/MyType`, it
   will generate filenames `test/data/MyType.1`, `test/data/MyType.2`,
   and `test/data/MyType.3`.
3. Depending on the policy you chose, if files corresponding to past
   versions don't exist, it will either ignore the issue, warn, or
   fail the test.
4. It walks through each file and asserts that when read and migrated
   with safecopy, it produces the value you gave it.
5. Finally, if the file for the current version is missing, it
   generates it. Thus, as you create new versions of your type, as
   long as you run the test, it will generate a history of safecopy
   files for that value. *You should be committing these files to
   source control*.

You can see a full example of this in the test suite in `test/Main.hs`.

## Testing Multiple Examples

Say there are multiple significant forms that your type takes and
having a single sequence of tests on 1 value is not sufficient. You
can easily deal with this by providing different base files:

```haskell
import Path
import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit.SafeCopy

data MyType = A | B deriving (Show, Eq)

-- .. safecopy instances, etc

tests = testGroup "MyType SafeCopy" [
    testCase "A" $
      testSafeCopy FailMissingFiles $(mkRelFile "test/data/MyType.A") A
  , testCase "B" $
      testSafeCopy FailMissingFiles $(mkRelFile "test/data/MyType.B") B
  ]

```

This will start with `test/data/MyType.A.1` and `test/data/MyType.B.1`
and as you add new versions, you can keep upgrading the test value to
assert how older formats will be migrated.
