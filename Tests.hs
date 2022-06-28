module Tests where

import Data.Char
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x,  t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

multiplicationAssociative x y z = x * (y * z) == (x * y) * z

multiplicationCommutative x y = x * y == y * x

main :: IO ()
main = hspec $ do
  it "halfIdentity" $ do
    property $ \x -> x == halfIdentity (x :: Double)
  it "listOrdered" $ do
    property $ \xs -> listOrdered (sort xs :: [Int]) == True
  it "plusAssociative" $ do
    property $ \x y z ->
      plusAssociative (x :: Int) (y :: Int) (z :: Int) == True
  it "plusCommutative" $ do
    property $ \x y ->
      plusCommutative (x :: Int) (y :: Int) == True
  it "multiplicationAssociative" $ do
    property $ \x y z ->
      multiplicationAssociative (x :: Int) (y :: Int) (z :: Int) == True
  it "multiplicationCommutative" $ do
    property $ \x y ->
      multiplicationCommutative (x :: Int) (y :: Int) == True
  it "quot rem" $ do
    property $ \x y ->
      (quot (x :: Int) (y :: Int)) * y + (rem x y) == x
  it "div mod" $ do
    property $ \x y ->
      (div (x :: Int) (y :: Int)) * y + (mod x y) == x
  it "Is ^ operation associative?" $ do
    property $ \x y z ->
      (x :: Int) ^ ((y :: Int) ^ (z :: Int)) == (x ^ y) ^ z
  it "Is ^ operation commutative?" $ do
    property $ \x y ->
      (x :: Int) ^ (y :: Int) == y ^ x
  it "reversing a list twice is\
     \ the same as the identity\
     \ of the original list" $ do
    property $ \xs -> (xs :: [Int]) == (reverse . reverse) xs

prop_dollar :: Fun String Integer -> String -> Bool
prop_dollar (Fun _ f) x = f x == (f $ x)

prop_compose :: Fun Int Int -> Fun Int Int -> Int -> Bool
prop_compose (Fun _ f) (Fun _ g) x = (f . g) x == f (g x)

prop_list :: [Int] -> [Int] -> Bool
prop_list xs ys = foldr (:) xs ys == xs ++ ys

prop_concat :: [[Int]] -> Bool
prop_concat xxs = foldr (++) [] xxs == concat xxs

f :: Int -> [Int] -> Bool
f n xs = length (take n xs) == n

g :: Int -> Bool
g x = (read (show x)) == x

square x = x * x

squareIdentity = square . sqrt

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = squareIdentity x == x

twice f = f . f

fourTimes = twice . twice

h :: String -> Bool
h x =    (capitalizeWord x == twice capitalizeWord x)
      && (capitalizeWord x == fourTimes capitalizeWord x)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

h' :: [Int] -> Bool
h' x =    (sort x == twice sort x)
       && (sort x == fourTimes sort x)

runQc :: IO ()
runQc = do
  quickCheck prop_dollar
  quickCheck prop_compose
  quickCheck prop_list
  quickCheck prop_concat
  quickCheck f
  quickCheck g
  quickCheck prop_squareIdentity
  quickCheck h
  quickCheck h'
