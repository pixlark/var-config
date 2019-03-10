module Main where

import Data.Char (ord)

-- Combines two predicates to form an OR'd predicate
(|?) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|?) f g x = (f x) || (g x)

-- Combines two predicates to form an AND'd predicate
(&?) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&?) f g x = (f x) && (g x)

data Token = TokEOF
           | TokColon
           | TokWord       String
           | TokIntLiteral Int
           | TokStrLiteral String
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
            in (TokIntLiteral $ read p, r)

readWord :: String -> (Token, String)
readWord (c:tl) = let (p, r) = readRestOfWord tl
                  in (TokWord (c:p), r)
  where readRestOfWord s = span (isalpha |?
                                 isdigit |?
                                 (==) '_') s

readString :: String -> (Token, String)
readString (c:tl) = let (p, r) = readToEnd tl
                    in (TokString p, r)
  where readToEnd acc ('"':tl) = (acc, tl)

wordToKeywordOrWord :: Token -> Token
wordToKeywordOrWord (TokWord s)
  | otherwise = TokWord s

lexString :: String -> [Token]
lexString []  = [TokEOF]
lexString (c:tl)
  | isspace c = lexString tl
  | isdigit c = let (t, s) = readInt (c:tl)
                in t:(lexString s)
  | (isalpha |? (==) '_') c = let (t, s) = readWord (c:tl)
                              in (wordToKeywordOrWord t):(lexString s)
  | c == '"'  = let (t, s) = readString(c:tl)
                in t:(lexString s)
  | otherwise = case findInMap c symbolMap of
                  Just t  -> t:(lexString tl)
                  Nothing -> error "Unrecognized char"
  where symbolMap = [((head ":"), TokColon)] -- no char lit thanks to
                                             -- emacs messing up
        findInMap _ []     = Nothing
        findInMap c (x:xs) = if c == (fst x)
                             then Just (snd x)
                             else findInMap c xs

data Value = ValInt Int
  deriving(Show)
                     
data Defn = Defn String Value
  deriving(Show)

parse :: [Token] -> [Defn]
parse (TokEOF:[]) = []
parse ((TokWord w):(TokColon):(TokIntLiteral n):tl) =
  (Defn w $ ValInt n):(parse tl)
parse (t:_)  = error $ "Parse error: Unexpected " ++ show t
parse []   = error "Internal error: State should never be reached"

main :: IO ()
main = do
  interact $ ((\s -> '\n':(s ++ "\n\n")) . show . parse . lexString)
