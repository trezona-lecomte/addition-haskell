import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x :: Int)
  describe "Multiplication" $ do
    it "1 + 1 is greater than 1" $
      (1 + 1) > 1 `shouldBe` True

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- λ> sample genTuple
-- ((),())
-- ((),())

-- λ> sample (genTuple :: Gen (Int, Float))
-- (0,0.0)
-- (1,-4.670173)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
