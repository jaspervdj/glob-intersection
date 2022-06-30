module Data.GlobIntersection.Tests where

import           Data.GlobIntersection
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.HUnit      as Tasty

nonEmptyIntersections :: [(String, [String])]
nonEmptyIntersections =
    [ ("abcd",        ["abcd", "....", "[a-d]*"])
    , ("pqrs",        [".qrs", "p.rs", "pq.s", "pqr."])
    , (".*",          ["asdklfj", "jasdfh", "asdhfajfh", "asdflkasdfjl"])
    , ("d*",          ["[abcd][abcd]", "d[a-z]+", ".....", "[d]*"])
    , ("[a-p]+",      ["[p-z]+", "apapapaapapapap", ".*", "abcdefgh*"])
    , ("abcd[a-c]z+", ["abcd[b-d][yz]*", "abcdazzzz", "abcdbzzz", "abcdcz"])
    , (".*\\\\",      [".*", "asdfasdf\\\\"]) -- Escaped \ character.
    , (".a.a",        ["b.b.", "c.c.", "d.d.", "e.e."])
    , (".*.*.*.*.*.*.*.*.*.*.*.*.*.*.*", [".*.*.*.*.*.*.*.*.*.*.*"])
    , ("foo.*bar",                       ["foobar", "fooalkdsjfbar"])
    ]

emptyIntersections :: [(String, [String])]
emptyIntersections =
    [ ("abcd",      ["lsdfhda", "abcdla", "asdlfk", "ksdfj"])
    , ("[a-d]+",    ["xyz", "p+", "[e-f]+"])
    , ("[0-9]*",    ["[a-z]", ".\\*"])
    , ("mamama.*",  ["dadada.*", "nanana.*"])
    , (".*mamama",  [".*dadada", ".*nanana"])
    , (".xyz.",     ["paaap", ".*pqr.*"])
    , ("ab+",       ["a", "b", "abc"])
    , (".*.*.*.*f", [".*.*.*.*g"])
    -- , (".*",        [""])
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Data.GlobIntersection.Tests" $
    [test l r True  | (l, rs) <- nonEmptyIntersections, r <- rs] ++
    [test l r False | (l, rs) <- emptyIntersections, r <- rs]
  where
    test lstr rstr expected = Tasty.testCase description $ do
        lp <- either fail pure $ parse lstr
        rp <- either fail pure $ parse rstr
        intersects lp rp Tasty.@=? expected
      where
        description =
            (show lstr) ++ " and " ++ (show rstr) ++
            if expected then " intersect" else " do not intersect"
