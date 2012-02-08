import Test.HUnit
import TreeDiff

sameTree = TestCase $ assertEqual "Compare tree with itself" (Cost 0 [Same 1 1,Same 2 2,Same 3 3]) $ treeDiff
    (Tree 0 [Tree 1 [], Tree 2 []])
    (Tree 0 [Tree 1 [], Tree 2 []])

simpleTree = TestCase $ assertEqual "Very simple diff" (Cost 2 [Same 1 1,Same 2 2,Delete 3,Insert 3]) $ treeDiff
    (Tree 0 [Tree 1 [Tree 2 []]])
    (Tree 0 [Tree 1 [], Tree 2 []])

chawathe = TestCase $ assertEqual "Example from Chawathe paper" (Cost 10
    [Same 1 1,Update 2 2,Same 3 3,Same 4 4,Delete 5,Update 6 5,Delete 7,Delete 8,Delete 9,Same 10 6,Update 11 7,Same 12 8,Insert 9,Insert 10,Insert 11])
    $ treeDiff
    (Tree "a" [Tree "b" [Tree "a" [Tree "b" [], Tree "a" [], Tree "c" []], Tree "c" [Tree "d" [], Tree "a" []]], Tree "d" [Tree "a" [], Tree "b" []]])
    (Tree "a" [Tree "c" [Tree "a" [Tree "b" [], Tree "b" []]], Tree "d" [Tree "b" [], Tree "b" []], Tree "a" [Tree "b" [], Tree "c" []]])
                
stress node = TestCase $ case result of
        Cost _ _ -> return ()
        _        -> assertString "Result not returned"
    where result = treeDiff
                (Tree "a" [Tree "b" [Tree "a" [Tree "b" [], Tree "a" [], Tree "c" []], Tree "c" [Tree "d" [], Tree "a" []]], Tree "d" [Tree "a" [], Tree "b" []]])
                (Tree "a" [Tree "b" [Tree "a" [Tree "b" [], Tree "a" [], Tree "c" []], Tree "c" [Tree "d" [], Tree "a" []]], Tree "d" [Tree "a" [], Tree "b" []], node])

stressTest = TestList $ map (stress . (\a -> Tree [a] [])) "abcdefghijklmnopqrstuvwxyz"

main = runTestTT $ TestList [ sameTree, simpleTree, chawathe, stressTest ]

-- Before optimisation:
--
-- Cases: 29  Tried: 29  Errors: 0  Failures: 0
-- 
-- real	0m7.098s
-- user	0m5.114s
-- sys	0m1.034s

-- Memoized (17x quicker):
--
-- Cases: 29  Tried: 29  Errors: 0  Failures: 0
-- 
-- real	0m0.422s
-- user	0m0.030s
-- sys	0m0.119s
