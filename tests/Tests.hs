import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified NormalizationTests as Norm
import qualified SegmentationTests as Segm

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
         testGroup "norm" [
                        testCase "g1" (Norm.g1_check @?= True)
                       ],
         testGroup "segm" [
            testGroup "graph" [
               -- FIXME testProperty "g1" Segm.g1_prop,
               testCase "g2" (Segm.g2_check @?= True)
            ],
            testGroup "word" [
               testCase "w1" (Segm.w1_check @?= True)
            ]
        ]
       ]


