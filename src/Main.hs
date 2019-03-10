module Main where

import Data.Char

-- Combines two predicates to form an OR'd predicate
(|?) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|?) f g x = (f x) || (g x)

-- Combines two predicates to form an AND'd predicate
(&?) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&?) f g x = (f x) && (g x)

data Token = EOF
           | Word       String
           | IntLiteral Int
  deriving(Show)

isspace :: Char -> Bool
isspace c = c `elem`
  [' ', '\t', '\n', '\r']

isdigit :: Char -> Bool
isdigit c = (ord c) >= (ord '0') &&
            (ord c) <= (ord '9')

isalpha :: Char -> Bool
isalpha c = ((ord c) >= (ord 'a') &&
             (ord c) <= (ord 'z')) ||
            ((ord c) >= (ord 'A') &&
             (ord c) <= (ord 'Z'))

readInt :: String -> (Token, String)
readInt s = let (p, r) = span isdigit s
            in (IntLiteral $ read p, r)

readWord :: String -> (Token, String)
readWord (c:tl) = let (p, r) = readRestOfWord tl
                  in (Word (c:p), r)
  where readRestOfWord s = span (isalpha |?
                                 isdigit |?
                                 (==) '_') s

lexString :: String -> [Token]
lexString []  = [EOF]
lexString (c:tl)
  | isspace c = lexString tl
  | isdigit c = let (t, s) = readInt (c:tl)
                in t:(lexString s)
  | (isalpha |? (==) '_') c
              = let (t, s) = readWord (c:tl)
                in t:(lexString s)
  | otherwise = error "Unrecognized char"

main :: IO ()
main = do
  interact $ ((\s -> '\n':(s ++ "\n\n")) . show . lexString)
