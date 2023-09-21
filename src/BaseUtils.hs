{-# LANGUAGE TupleSections #-}
module BaseUtils (module BaseUtils, module ErrOr) where

import ErrOr

import Numeric.Natural (Natural)
import Data.Char (toUpper, toLower)
import Text.Pretty.Simple (pShowLightBg)
import Data.Text.Lazy (unpack)

toInt :: Natural -> Int
toInt = fromInteger . toInteger

fromInt :: Int -> Natural
fromInt = fromInteger . toInteger

intercalate' :: String -> [String] -> String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

inc :: Natural -> Natural
inc = (+2)

pfxStr :: Natural -> String
pfxStr n = replicate (fromInteger (toInteger n)) ' '

prefix :: Natural -> String -> String
prefix n s = pfxStr n ++ s

parens' :: Char -> Char -> String -> String
parens' l r str = l : str ++ [r]

parens :: String -> String
parens = parens' '(' ')'

brackets :: String -> String
brackets = parens' '[' ']'

braces :: String -> String
braces = parens' '{' '}'

bracesNL :: Natural -> Natural -> String -> String
bracesNL bf af str = pfxStr bf ++ "{\n" ++ str ++ "\n" ++ pfxStr af ++ "}"

firstUpper :: String -> String
firstUpper [] = undefined
firstUpper (c : cs) = toUpper c : cs

firstLower :: String -> String
firstLower [] = undefined
firstLower (c : cs) = toLower c : cs

spacer :: String
spacer = "\n\n"

dup :: a -> (a,a)
dup x = (x,x)

assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

ppShow :: Show a => a -> String
ppShow = unpack . pShowLightBg

ppPrint :: Show a => a -> IO ()
ppPrint = putStrLn . unpack . pShowLightBg

secondM :: Monad m => (b -> m b') -> (a,b) -> m (a,b')
secondM f (x,y) = fmap (x,) (f y)
