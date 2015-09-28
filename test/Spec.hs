
import Data.Functor
import Data.Turing
import Test.Hspec
import Test.QuickCheck

data Sigma = Zero | One deriving (Eq, Show)

instance Arbitrary Sigma where
    arbitrary = elements [Zero, One]

data State = Q1 | Q2 | Q3 deriving (Eq, Show)

main :: IO ()
main = hspec $ do
    describe "run" $ do
        it "accepts with a simple function" $
            let f (Q1, _) = Just (Q2, Just Zero, MoveRight)
                f (Q2, _) = Nothing
            in run (mkTuringMachine f Q1 [Q2]) (replicate 5 One) `shouldBe` Accept

        it "can deal with empty inputs" $
            let f (Q1, _) = Just (Q2, Just Zero, MoveRight)
                f (Q2, _) = Nothing
            in run (mkTuringMachine f Q1 [Q2]) [] `shouldBe` Accept

        it "executes a simple DTM correctly" $
            let f (Q1, Just Zero) = Just (Q2, Just Zero, DontMove)
                f (Q1, Just One) = Just (Q3, Just Zero, DontMove)
                f _ = Nothing
                t = mkTuringMachine f Q1 [Q2]
            in [run t [input] | input <- [Zero, One]] `shouldBe` [Accept, Reject]

        it "executes a more complex DTM correctly" $ property $
            \input -> let f (q, Just Zero) = Just (q, Just Zero, MoveRight)
                          f (Q1, Just One) = Just (Q2, Just Zero, MoveRight)
                          f (Q2, Just One) = Just (Q1, Just Zero, MoveRight)
                          f _ = Nothing
                          t = mkTuringMachine f Q1 [Q1]
                          result = run t input
                      in result === if (even . sum . map (\x -> if x == One then 1 else 0)) input
                                    then Accept else Reject

    describe "runFor" $ do
        it "terminates even for machines that would run forever" $
            let f (Q1, _) = Just (Q1, Just Zero, DontMove)
            in runFor 10 (mkTuringMachine f Q1 [Q2]) [] `shouldBe` Nothing

        it "can handle negative step counts" $
            let f (Q1, _) = Just (Q1, Just Zero, DontMove)
            in runFor (-5) (mkTuringMachine f Q1 [Q2]) [] `shouldBe` Nothing

        it "can handle a step count of zero" $
            let f (Q1, _) = Just (Q1, Just Zero, DontMove)
            in runFor 0 (mkTuringMachine f Q1 [Q2]) [] `shouldBe` Nothing

        it "counts execution steps correctly" $
            let f (Q1, Just Zero) = Just (Q2, Just Zero, DontMove)
                f (Q1, Just One) = Just (Q3, Just Zero, DontMove)
                f _ = Nothing
                t = mkTuringMachine f Q1 [Q2]
            in [runFor 1 t [input] | input <- [Zero, One]] `shouldBe` [Just Accept, Just Reject]
