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
    = either err (concatToQExp . arrange) . parse doc "heredoc" . (<>"\n")
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
          | CtrlCase [Expr] [([Expr], ChildBlock)]
          | CtrlOf [Expr]
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
ctrlMaybe = CtrlMaybe <$> pure False <*> bindVal <*> expr <*> pure [] <*> pure []
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
ctrlOf = CtrlOf <$> (string "$of" *> spaceTabs *> expr <* spaceTabs)

ctrlLet :: Parser Line
ctrlLet = CtrlLet <$> bindVal <*> expr <*> pure []
    where
      bindVal = string "$let" *> spaceTabs *>
                binding
                <* spaceTabs <* string "=" <* spaceTabs

-- TODO: support pattern match
binding :: Parser String
binding = many1 (letter <|> digit <|> char '_' <|> char '\'')

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
      isCtrlOf (_, CtrlOf _) = True
      isCtrlOf _ = False

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

      push x@(i, _) ss'@((j, CtrlCase e alts):ss)
          | i > j = (j, CtrlCase e (push' x alts)):ss
          | otherwise
            = if isCtrlOf x
              then error "orphan $of found"
              else x:ss'
      push x ((j, CtrlOf _):_) = error "orphan $of found"

      push' x@(i, CtrlOf e) alts = (e, [x]):alts
      push' x [] = error "$of not found"
      push' x ((e, body):alts) = (e, (push x body)):alts

      rev = foldr (\x xs -> xs ++ [rev' x]) []
      rev' x@(_, Normal _) = x
      rev' (i, CtrlLet b e body)
          = (i, CtrlLet b e (rev body))
      rev' (i, CtrlMaybe flg b e body alt)
          = (i, CtrlMaybe flg b e (rev body) (rev alt))
      rev' (i, CtrlIf flg e body alt)
          = (i, CtrlIf flg e (rev body) (rev alt))
      rev' (i, CtrlCase e alts)
          = (i, CtrlCase e (map (id *** rev) $ reverse alts))

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
      norm' (i, CtrlCase e alts) = undefined
      norm' (i, CtrlOf _) = error "orphan $of found"
                             
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

class ToQPat a where
    toQP :: a -> Q Pat
    concatToQP :: [a] -> Q Pat

class ToQExp a where
    toQExp :: a -> Q Exp
    concatToQExp :: [a] -> Q Exp

instance ToQExp Expr where
    toQExp (S s) = litE (stringL s)
    toQExp (I i) = litE (integerL i)
    toQExp (V v) = varE (mkName v)
    toQExp (O o) = (varE (mkName o))
    toQExp (E e) = concatToQExp e
    toQExp (C c) = conE (mkName c)

    concatToQExp xs = concatToQ' Nothing xs
        where
          concatToQ' (Just acc) [] = acc
          concatToQ' Nothing  [x] = toQExp x
          concatToQ' Nothing (x:xs) = concatToQ' (Just (toQExp x)) xs
          concatToQ' (Just acc) ((O o):xs)
              = infixE (Just acc)
                       (varE (mkName o))
                       (Just (concatToQExp xs))
          concatToQ' (Just acc) ((V' v'):xs)
              = infixE (Just acc)
                       (varE (mkName v'))
                       (Just (concatToQExp xs))
          concatToQ' (Just acc) (x:xs)
              = concatToQ' (Just (appE acc (toQExp x))) xs

instance ToQExp InLine where
    toQExp (Raw s) = litE (stringL s)
    toQExp (Quoted expr) = concatToQExp expr

    concatToQExp [] = litE (stringL "")
    concatToQExp (x:xs) = infixE (Just (toQExp x))
                                 (varE '(++))
                                 (Just (concatToQExp xs))

instance ToQExp Line where
    toQExp (CtrlForall b e body) = undefined
    toQExp (CtrlMaybe flg b e body alt)
        = appE (appE (appE (varE 'maybe)
                           (concatToQExp alt))
                     (lamE [varP (mkName b)] (concatToQExp body)))
               (concatToQExp e)
    toQExp (CtrlIf flg e body alt)
        = condE (concatToQExp e) (concatToQExp body) (concatToQExp alt)
    toQExp CtrlElse = error "illegal $else found"
    toQExp (CtrlCase e alts)
        = caseE (concatToQExp e) (map mkMatch alts)
        where
          mkMatch (e', body) = undefined
    toQExp (CtrlOf e) = error "illegal $of found"
    toQExp (CtrlLet b e body)
        = letE [valD (varP (mkName b)) (normalB $ concatToQExp e) []]
               (concatToQExp body)
    toQExp (Normal xs) = concatToQExp xs

    concatToQExp (x:[]) = toQExp x
    concatToQExp (x:xs) = infixE (Just (toQExp x))
                                 (varE '(++))
                                 (Just (concatToQExp xs))

instance ToQExp Line' where
    toQExp (n, x@(Normal _))
        = infixE (Just (litE (stringL (replicate n ' '))))
                 (varE '(++))
                 (Just (toQExp x))
    toQExp (n, x) =  toQExp x -- Ctrl*

    concatToQExp [] = litE (stringL "")
    concatToQExp (x@(_, Normal _):xs)
        = infixE (Just (infixE (Just (toQExp x))
                               (varE '(++))
                               (Just (litE (stringL "\n")))))
                 (varE '(++))
                 (Just (concatToQExp xs))
    concatToQExp (x:xs) = infixE (Just (toQExp x))
                                 (varE '(++))
                                 (Just (concatToQExp xs))
