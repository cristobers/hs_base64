-- This only works for ASCII characters
-- https://en.wikipedia.org/wiki/Base64

module Main where

import Data.Maybe
import Data.Foldable
import Data.Char
import System.Environment

type Table = String
type Binary = [Int]
type BinaryString = String

table :: Table
table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

dictionary :: [(BinaryString, Char)]
dictionary = makeDictionary table

charToIndex :: Char -> Maybe Int
charToIndex c   | c `elem` table = Just $ head [ n | (n, c') <- indexes, c' == c ]
                | otherwise      = Nothing 
    where
        indexes = zip [0..] table

-- getQuotient 3.5  ->  3
getQuotient :: (RealFrac b, Integral a) => b -> a
getQuotient s = fst $ properFraction s 

hasRemainder :: (Eq b, RealFrac b) => b -> Int 
hasRemainder n  | snd (properFraction n) == 0.0     = 0
                | otherwise                         = 1

getBits :: (RealFrac a) => a -> Binary
getBits 0       = []
getBits n = do
    let div = n / 2
    let q = fromIntegral (getQuotient div) :: Float
    let r = hasRemainder div
    r : getBits q

groupsOfSix :: Binary -> [Binary]
groupsOfSix []  = []
groupsOfSix x   = take 6 x : groupsOfSix (drop 6 x)

intoGroupsOfSix :: Binary -> [String]
intoGroupsOfSix xs  = map (addPadding . foldMap show) groups
    where
        groups = groupsOfSix $ foldMap intoEight xs
        intoEight x = makeEightBits (fromIntegral x :: Float)

indexToBits :: Int -> Binary 
indexToBits n   = replicate rest 0 ++ reverse initial
    where
        blorp   = fromIntegral n :: Float
        initial = getBits blorp 
        rest    = 6 - length initial 

-- Make sure that our binary number ends up being 8 bits long.
makeEightBits :: (RealFrac a) => a -> Binary
makeEightBits n     = foo ++ reverse initial
    where
        initial = getBits n
        foo = replicate (8 - length initial) 0

-- Create a dictionary of (key, value) pairs, where key is a sextet,
-- and value is a Char.
makeDictionary :: Table -> [(String, Char)]
makeDictionary t    = map indexesToBinary indexes
    where
        indexes = zip [0..] t
        indexesToBinary x = ( foldMap show (indexToBits (fst x)), snd x)

-- Given a key and a dictionary to search, find that key within the dictionary.
findKey :: String -> [(String, Char)] -> Char
findKey key dict   = head [ v | (k', v) <- dict, k' == key ]

-- Map all of the associated character keys.
intoBase64 :: [String] -> String
intoBase64    = map (`findKey` dictionary)

-- Add the padding to the end of a sextet.
addPadding :: String -> String
addPadding s    | length s < 6      = s ++ replicate (6 - length s) '0'
                | otherwise         = s

-- See how much padding is needed at the end of the base64 string.
howMuchPadding :: String -> Int
howMuchPadding s    | amount == 2       = 1
                    | amount == 1       = 2
                    | otherwise         = amount
    where
        amount = length s `mod` 3

main :: IO ()
main = do
    args <- getArgs
    let input = head args
    let ascii = map ord input 

    -- turn these back into characters, using the base64 table.
    let sixBits = intoGroupsOfSix ascii

    if length input `mod` 3 /= 0 then
        do
            let padding = howMuchPadding input
            putStrLn $ intoBase64 sixBits ++ replicate padding '='
    else
        putStrLn $ intoBase64 sixBits
