{-|
Module      : Test.QuickCheck.Common
Description : Auxiliary QuickCheck functions
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Test.QuickCheck.Common (
    quickCheckN, executePropertyTest
) where

import           System.Exit
import           System.IO
import           Test.QuickCheck

-- quickCheck with parametrizable tests number
quickCheckN :: Testable prop => Int -> prop -> IO Result
quickCheckN n = quickCheckWithResult (Args 
    Nothing -- ^ Should we replay a previous test? No. 
    n       -- ^ Maximum number of successful tests before succeeding set to N. 
    1       -- ^ Maximum number of discarded tests per successful test before giving up - gave up after first failure. 
    n       -- ^ Size to use for the biggest test cases.
    True    -- ^ Whether to print anything? yes. 
    0)      -- ^ Maximum number of shrinks to before giving up. Turn shrinking off.

-- | Execute property test and check result:
-- | exit test suite with successs code if no errors occured
-- | exit test suite with failure code if any error occured
executePropertyTest :: (
    Testable prop 
    ) => String -- ^ Tested property name
      -> Int    -- ^ Number of tests to do
      -> prop   -- ^ Property to test
      -> IO ()
executePropertyTest propName n f = do
    putStr $ "  Checking " ++ propName ++ " "
    r <- quickCheckN n f
    case r of
        Success _ _ _  -> hFlush stdout
        _ -> exitFailure
