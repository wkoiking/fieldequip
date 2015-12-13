module Core.Replace where

-- parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- base
import Control.Monad
import Data.Char

-- (<>) = liftM2 (++)
main = do
    c <- readFile "sampleKm.txt"
    case (parse replaceMain "" c) of
        Left err -> print err
        Right ss -> putStrLn ss

replaceMain :: Parser String
replaceMain = liftM concat $ many $ try replace <|> oneChar

-- abcdef --> abcdefghi
replace :: Parser String
replace = do
 d <- kmP
 return $ show d


oneChar :: Parser String
oneChar = liftM return anyChar

kmP :: Parser Double
kmP = do
 sign <- option id (char '-' >> return negate)
 ks <- many1 (liftM digitToInt digit)
 char 'K' <|> char 'k'
 ds <- replicateM 3 (liftM digitToInt digit)
 d <- option 0 decimalPlaces
 return $ sign $ sumup ks 1000 + sumup ds 1 + d

sumup ls x = sum $ zipWith (*) (reverse $ map fromIntegral ls) (iterate (*10) x)

decimalPlaces :: Parser Double
decimalPlaces = do
 char 'M'
 ds <- many1 (liftM digitToInt digit)
 let ls = iterate (*10) 1
 return $ sumup ds 1 / fromIntegral (10 ^ (length ds))

showKm :: Double -> String
showKm d = "K" ++ show k ++ "+" ++ pad0 (show $ abs $ {-f + fromIntegral -} r)
 where (n,f) = properFraction d
       (k, r) = n `quotRem` 1000

pad0 str = replicate (3 - length str) '0' ++ str