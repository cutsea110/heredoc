{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Heredoc where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.Function (on)
import Data.Monoid ((<>))
import Text.ParserCombinators.Parsec hiding (Line)
import Text.ParserCombinators.Parsec.Error
import Language.Haskell.TH
import Language.Haskell.TH.Quote

heredoc :: QuasiQuoter
heredoc = QuasiQuoter { quoteExp = heredocFromString }

-- | C# code gen
heredocFromString :: String -> Q Exp
heredocFromString
    = either err (concatToQ . arrange) . parse doc "heredoc" . (<>"\n")
    where
      err = infixE <$> Just . pos <*> pure (varE '(++)) <*> Just . msg
      pos = litE <$> (stringL <$> show . errorPos)
      msg = litE <$> (stringL <$> concatMap messageString . errorMessages)

type Indent = Int
type Line' = (Indent, Line)
type ChildBlock = [Line']
type AltFlag = Bool

data InLine = Raw String
            | Quoted [Expr]
              deriving Show

data Line = CtrlForall String [Expr] ChildBlock
          | CtrlMaybe AltFlag String [Expr] ChildBlock ChildBlock
          | CtrlNothing
          | CtrlIf AltFlag [Expr] ChildBlock ChildBlock
          | CtrlElse
          | CtrlCase [Expr] ChildBlock
          | CtrlOf [Expr] ChildBlock
          | CtrlLet String [Expr] ChildBlock
          | Normal [InLine]
            deriving Show

data Expr = S String
          | I Integer
          | V String
          | V' String
          | C String
          | O String
          | O' String
          | E [Expr]
            deriving Show

eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> fail "end of line"

spaceTabs :: Parser String
spaceTabs = many (oneOf " \t")

doc :: Parser [(Indent, Line)]
doc = line `endBy` eol

line :: Parser (Indent, Line)
line = (,) <$> indent <*> contents

indent :: Parser Indent
indent = fmap sum $
         many ((char ' ' >> pure 1) <|>
               (char '\t' >> fail "Tabs are not allowed in indentation"))

contents :: Parser Line
contents = try ctrlForall <|>
           try ctrlMaybe <|>
           try ctrlNothing <|>
           try ctrlIf <|>
           try ctrlElse <|>
           try ctrlCase <|>
           try ctrlOf <|>
           try ctrlLet <|>
           normal

ctrlForall :: Parser Line
ctrlForall = CtrlForall <$> bindVal <*> expr <*> pure []
    where
      bindVal = string "$forall" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlMaybe :: Parser Line
ctrlMaybe = CtrlMaybe <$> pure False <*> bindVal<*> expr <*> pure [] <*> pure []
    where
      bindVal = string "$maybe" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlNothing :: Parser Line
ctrlNothing = string "$nothing" *> spaceTabs >> pure CtrlNothing

ctrlIf :: Parser Line
ctrlIf = CtrlIf <$> pure False <*> (string "$if" *> spaceTabs *> expr <* spaceTabs) <*> pure [] <*> pure []

ctrlElse :: Parser Line
ctrlElse = string "$else" *> spaceTabs >> pure CtrlElse

ctrlCase :: Parser Line
ctrlCase = CtrlCase <$> (string "$case" *> spaceTabs *> expr <* spaceTabs) <*> pure []

ctrlOf :: Parser Line
ctrlOf = CtrlOf <$> (string "$of" *> spaceTabs *> expr <* spaceTabs) <*> pure []

ctrlLet :: Parser Line
ctrlLet = CtrlLet <$> bindVal <*> expr <*> pure []
    where
      bindVal = string "$let" *> spaceTabs *>
                binding
                <* spaceTabs <* string "=" <* spaceTabs

-- TODO: support pattern match
binding :: Parser String
binding = many1 (letter <|> digit <|> char '_')

expr :: Parser [Expr]
expr = spaceTabs *> many1 term
    where
      term :: Parser Expr
      term = (S  <$> str <|>
              O  <$> op <|>
              (try (O' <$> op') <|> try (E  <$> subexp)) <|>
              C  <$> con <|>
              I  <$> integer <|>
              V' <$> var' <|>
              V  <$> var) <* spaceTabs

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> pure '"')

subexp :: Parser [Expr]
subexp = char '(' *> expr <* char ')'

var :: Parser String
var = many1 (letter <|> digit <|> char '_' <|> char '\'')

var' :: Parser String
var' = char '`' *> var <* char '`'

con :: Parser String
con = (:) <$> upper <*> many (letter <|> digit <|> char '_' <|> char '\'')

op :: Parser String
op = many1 (oneOf ":!#$%&*+./<=>?@\\^|-~")

op' :: Parser String
op' = char '(' *> op <* char ')'

normal :: Parser Line
normal = Normal <$> many (try quoted <|> try raw' <|> try raw)

quoted :: Parser InLine
quoted = Quoted <$> (string "${" *> expr <* string "}")

raw' :: Parser InLine
raw' = Raw <$> ((:) <$> (char '$')
                <*> ((:) <$> noneOf "{" <*> many (noneOf "$\n\r")))

raw :: Parser InLine
raw = Raw <$> many1 (noneOf "$\n\r")

----
arrange :: [(Indent, Line)] -> [(Indent, Line)]
arrange = norm . rev . foldl (flip push) []
    where
      isCtrlNothing (_, CtrlNothing) = True
      isCtrlNothing _ = False
      isCtrlElse (_, CtrlElse) = True
      isCtrlElse _ = False

      push x [] = x:[]
      push x ss'@((_, Normal _):_) = x:ss'

      push x@(i, _) ss'@((j, CtrlLet b e body):ss)
          | i > j = (j, CtrlLet b e (push x body)):ss
          | otherwise = x:ss'

      push x@(i, _) ss'@((j, CtrlMaybe flg b e body alt):ss)
          | i > j = if flg
                    then (j, CtrlMaybe flg b e body (push x alt)):ss
                    else (j, CtrlMaybe flg b e (push x body) alt):ss
          | i == j && isCtrlNothing x
              = if flg
                then error "too many $nothing found"
                else (j, CtrlMaybe True b e body alt):ss
          | otherwise = x:ss'
      push x ((j, CtrlNothing):_) = error "orphan $nothing found"

      push x@(i, _) ss'@((j, CtrlIf flg e body alt):ss)
          | i > j = if flg
                    then (j, CtrlIf flg e body (push x alt)):ss
                    else (j, CtrlIf flg e (push x body) alt):ss
          | i == j && isCtrlElse x
              = if flg
                then error "too many $else found"
                else (j, CtrlIf True e body alt):ss
          | otherwise = x:ss'
      push x ((j, CtrlElse):_) = error "orphan $else found"

      rev = foldr (\x xs -> xs ++ [rev' x]) []
      rev' x@(_, Normal _) = x
      rev' (i, CtrlLet b e body)
          = (i, CtrlLet b e (rev body))
      rev' (i, CtrlMaybe flg b e body alt)
          = (i, CtrlMaybe flg b e (rev body) (rev alt))
      rev' (i, CtrlIf flg e body alt)
          = (i, CtrlIf flg e (rev body) (rev alt))

      norm = foldr (\x xs -> norm' x:xs) []
      norm' x@(_, Normal _) = x
      norm' (i, CtrlLet b e body)
          = let j = minimum $ map fst body
                deIndent n = i+(n-j)
            in (i, CtrlLet b e (norm $ map (deIndent *** id) body))
      norm' (i, CtrlMaybe flg b e body alt)
          = let (j, j') = (minimum $ map fst body, minimum $ map fst alt)
                (deIndent, deIndent') = (\n -> i+(n-j), \n -> i+(n-j'))
            in (i, CtrlMaybe flg b e
                     (norm $ map (deIndent *** id) body)
                     (norm $ map (deIndent' *** id) alt))
      norm' (i, CtrlNothing) = error "orphan $nothing found"
      norm' (i, CtrlIf flg e body alt)
          = let (j, j') = (minimum $ map fst body, minimum $ map fst alt)
                (deIndent, deIndent') = (\n -> i+(n-j), \n -> i+(n-j'))
            in (i, CtrlIf flg e
                     (norm $ map (deIndent *** id) body)
                     (norm $ map (deIndent' *** id) alt))
      norm' (i, CtrlElse) = error "orphan $else found"
                             
{--
arrange :: [(Indent, Line)] -> [(Indent, Line)]
arrange [] = []
arrange [x] = [x]
arrange ((i, CtrlForall b e body):(j, next):xs)
    | i < j = arrange $ (i, CtrlForall b e (arrange $ body ++ [(j-i, next)])):xs
    | otherwise = (i, CtrlForall b e body):arrange ((j, next):xs)

arrange ((i, CtrlMaybe False b e body alt):(j, CtrlNothing):xs)
    | i == j = arrange $ (i, CtrlMaybe True b e (arrange body) alt):xs
    | otherwise = error "Couldn't found $maybe statement"
arrange ((i, CtrlMaybe False b e body alt):(j, next):xs)
    | i < j = arrange $ (i, CtrlMaybe False b e (arrange $ body ++ [(j-1, next)]) alt):xs
    | otherwise = (i, CtrlMaybe False b e body alt):arrange ((j, next):xs)
arrange ((i, CtrlMaybe True b e body alt):(j, next):xs)
    | i < j = arrange $ (i, CtrlMaybe True b e body (arrange $ alt ++ [(j-i, next)])):xs
    | otherwise = (i, CtrlMaybe True b e body alt):arrange ((j, next):xs)

arrange ((i, CtrlIf False e body alt):(j, CtrlElse):xs)
    | i == j = arrange $ (i, CtrlIf True e (arrange body) alt):xs
    | otherwise = error "Couldn't found $if statement"
arrange ((i, CtrlIf False e body alt):(j, next):xs)
    | i < j = arrange $ (i, CtrlIf False e (arrange $ body ++ [(j-i, next)]) alt):xs
    | otherwise = (i, CtrlIf False e body alt):arrange ((j, next):xs)
arrange ((i, CtrlIf True e body alt):(j, next):xs)
    | i < j = arrange $ (i, CtrlIf True e body (arrange $ alt ++ [(j-i, next)])):xs
    | otherwise = (i, CtrlIf True e body alt):arrange ((j, next):xs)

arrange ((i, CtrlCase e body):(j, next):xs)
    | i < j = arrange ((i, CtrlCase e (arrange $ body ++ [(j-i, next)])):xs)
    | otherwise = (i, CtrlCase e body):arrange ((j, next):xs)
arrange ((i, CtrlOf e body):(j, next):xs)
    | i < j = arrange ((i, CtrlOf e (arrange $ body ++ [(j-i, next)])):xs)
    | otherwise = (i, CtrlOf e body):arrange ((j, next):xs)

arrange ((i, CtrlLet b e body):(j, next):xs)
    | i < j = arrange ((i, CtrlLet b e (arrange $ body ++ [(j-i, next)])):xs)
    | otherwise = (i, CtrlLet b e body):arrange ((j, next):xs)

arrange ((i, Normal x):xs) = (i, Normal x):arrange xs
--}

class ToQ a where
    toQ :: a -> Q Exp
    concatToQ :: [a] -> Q Exp

instance ToQ Expr where
    toQ (S s) = litE (stringL s)
    toQ (I i) = litE (integerL i)
    toQ (V v) = varE (mkName v)
    toQ (O o) = (varE (mkName o))
    toQ (E e) = concatToQ e
    toQ (C c) = conE (mkName c)

    concatToQ xs = concatToQ' Nothing xs
        where
          concatToQ' (Just acc) [] = acc
          concatToQ' Nothing  [x] = toQ x
          concatToQ' Nothing (x:xs) = concatToQ' (Just (toQ x)) xs
          concatToQ' (Just acc) ((O o):xs)
              = infixE (Just acc)
                       (varE (mkName o))
                       (Just (concatToQ xs))
          concatToQ' (Just acc) ((V' v'):xs)
              = infixE (Just acc)
                       (varE (mkName v'))
                       (Just (concatToQ xs))
          concatToQ' (Just acc) (x:xs)
              = concatToQ' (Just (appE acc (toQ x))) xs

instance ToQ InLine where
    toQ (Raw s) = litE (stringL s)
    toQ (Quoted expr) = concatToQ expr

    concatToQ [] = litE (stringL "")
    concatToQ (x:xs) = infixE (Just (toQ x))
                              (varE '(++))
                              (Just (concatToQ xs))

instance ToQ Line where
    toQ (CtrlForall b e body) = undefined
    toQ (CtrlMaybe flg b e body alt)
        = appE (appE (appE (varE 'maybe)
                           (concatToQ alt))
                     (lamE [varP (mkName b)] (concatToQ body)))
               (concatToQ e)
    toQ (CtrlIf flg e body alt)
        = condE (concatToQ e) (concatToQ body) (concatToQ alt)
    toQ CtrlElse = error "illegal $else found"
    toQ (CtrlCase e body) = undefined
    toQ (CtrlOf e body) = undefined
    toQ (CtrlLet b e body)
        = letE [valD (varP (mkName b)) (normalB $ concatToQ e) []]
               (concatToQ body)
    toQ (Normal xs) = concatToQ xs

    concatToQ (x:[]) = toQ x
    concatToQ (x:xs) = infixE (Just (toQ x))
                              (varE '(++))
                              (Just (concatToQ xs))

instance ToQ Line' where
    toQ (n, x@(Normal _)) = infixE (Just (litE (stringL (replicate n ' '))))
                                   (varE '(++))
                                   (Just (toQ x))
    toQ (n, x) =  toQ x -- Ctrl*

    concatToQ [] = litE (stringL "")
    concatToQ (x@(_, Normal _):xs) = infixE (Just (infixE (Just (toQ x))
                                            (varE '(++))
                                            (Just (litE (stringL "\n")))))
                              (varE '(++))
                              (Just (concatToQ xs))
    concatToQ (x:xs) = infixE (Just (toQ x))
                              (varE '(++))
                              (Just (concatToQ xs))
