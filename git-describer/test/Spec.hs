import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Parser
import Processing

import Data.List as List
import Data.Ord
import        			     Data.Char as Char

--test1 = TestCase (assertEqual "doubleElems [3,5]" [6,10] (doubleElems [3,5]))
--test2 = TestCase (assertEqual "repl 3 7" [7, 7, 7] (repl 3 7))

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


main = defaultMain tests

--properties = testGroup "(checked by QuickCheck)"
	--[testProperty "removeneg => all >= 0" $ \a -> all (\x -> 0 <= x) (removeNeg a::[Int])]
    
properties = testGroup "(checked by QuickCheck)"
	[testProperty "test1" $ delete_extra_symbols "   git push  " == "git push"
    , testProperty "test2" $ delete_extra_symbols ".,.-=git push  " == "git push"
    , testProperty "test3" $ delete_extra_symbols ".,.-=git push  .,." == "git push"
    , testProperty "test4" $ delete_extra_symbols "-=git push  " == "git push"
    , testProperty "test5" $ delete_extra_symbols ".   ,.-=git push  " == "git push"
    , testProperty "test6" $ delete_extra_symbols ".,.-=git pull  " == "git pull"
    , testProperty "test7" $ delete_extra_symbols ".,.-=git commit  " == "git commit"
    , testProperty "test8" $ delete_extra_symbols "git fetch  " == "git fetch"
    , testProperty "test9" $ delete_extra_symbols ".,.-=git clone  " == "git clone"
    , testProperty "test10" $ delete_extra_symbols "git push , , , , ," == "git push"
    , testProperty "test11" $ delete_extra_symbols "git remote  " == "git remote"
    , testProperty "test12" $ delete_extra_symbols ".0.0.0.git push  " == "git push"
    , testProperty "test13" $ delete_extra_symbols "....git pull  " == "git pull"
    , testProperty "test14" $ delete_extra_symbols ". 1git clone  " == "git clone"
    , testProperty "test15" $ delete_extra_symbols "2453 git remote 1231234 " == "git remote"]

unitTests = testGroup "(Unit tests)"
	[testCase "test16" $ "git remote" `compare` (delete_extra_symbols "git remote  ") @?= EQ
    , testCase "test17" $ "git remote" `compare` (delete_extra_symbols "0.git remote  ") @?= EQ
    , testCase "test18" $ "git pull" `compare` (delete_extra_symbols "-=-=--=@git pull@@@@  ") @?= EQ
    , testCase "test19" $ "git pull" `compare` (delete_extra_symbols "   git pull  ") @?= EQ
    , testCase "test20" $ "git fetch" `compare` (delete_extra_symbols "git fetch") @?= EQ
    , testCase "test21" $ "git fetch" `compare` (delete_extra_symbols "222git fetch  ") @?= EQ
    , testCase "test22" $ "git clone" `compare` (delete_extra_symbols " - - - git clone  ") @?= EQ
    , testCase "test23" $ "git clone" `compare` (delete_extra_symbols "git clone  ") @?= EQ
    , testCase "test24" $ "git push" `compare` (delete_extra_symbols "12///37git push7501....  ") @?= EQ
    , testCase "test25" $ "git push" `compare` (delete_extra_symbols "12---37git push - - -  ") @?= EQ
    , testCase "test26" $ "git pull" `compare` (delete_extra_symbols "1234git pull - - = ") @?= EQ
    , testCase "test27" $ "git clone" `compare` (delete_extra_symbols "1234537git clone55373  ") @?= EQ
    , testCase "test28" $ "git pull" `compare` (delete_extra_symbols "137git pull  ") @?= EQ
    , testCase "test29" $ "git push" `compare` (delete_extra_symbols "1git push  ") @?= EQ
    , testCase "test30" $ "git push" `compare` (delete_extra_symbols "git push  ") @?= EQ]