{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Heredoc where

import Control.Applicative ((<$>), (<*>))
import Text.ParserCombinators.Parsec hiding (Line)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

csharp :: QuasiQuoter
csharp = QuasiQuoter { quoteExp = csharpFromString }

-- | C# code gen
csharpFromString :: String -> Q Exp
csharpFromString = litE . stringL

type Indent = Int

data InLine = Raw String
         | Quoted [Expr]
           deriving Show

data Line = CtrlForall String [Expr]
             | CtrlMaybe String [Expr]
             | CtrlNothing
             | CtrlIf [Expr]
             | CtrlElse
             | CtrlCase [Expr]
             | CtrlOf [Expr]
             | CtrlLet String [Expr]
             | Normal [InLine]
               deriving Show

data Expr = S String
          | I Integer
          | V String
          | O String
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

doc :: Parser [(Indent, [Line])]
doc = line `endBy` eol

line :: Parser (Indent, [Line])
line = (,) <$> indent <*> contents

indent :: Parser Indent
indent = fmap sum $
         many ((char ' ' >> pure 1) <|>
               (char '\t' >> fail "Tabs are not allowed in indentation"))

contents :: Parser [Line]
contents = many (try ctrlForall <|>
                 try ctrlMaybe <|>
                 try ctrlNothing <|>
                 try ctrlIf <|>
                 try ctrlElse <|>
                 try ctrlCase <|>
                 try ctrlOf <|>
                 try ctrlLet <|>
                 normal)

ctrlForall :: Parser Line
ctrlForall = CtrlForall <$> bindVal <*> expr
    where
      bindVal = string "$forall" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlMaybe :: Parser Line
ctrlMaybe = CtrlMaybe <$> bindVal<*> expr
    where
      bindVal = string "$maybe" *> spaceTabs *>
                binding
                <* spaceTabs <* string "<-" <* spaceTabs

ctrlNothing :: Parser Line
ctrlNothing = string "$nothing" *> spaceTabs >> pure CtrlNothing

ctrlIf :: Parser Line
ctrlIf = CtrlIf <$> (string "$if" *> spaceTabs *> expr <* spaceTabs)

ctrlElse :: Parser Line
ctrlElse = string "$else" *> spaceTabs >> pure CtrlElse

ctrlCase :: Parser Line
ctrlCase = CtrlCase <$> (string "$case" *> spaceTabs *> expr <* spaceTabs)

ctrlOf :: Parser Line
ctrlOf = CtrlOf <$> (string "$of" *> spaceTabs *> expr <* spaceTabs)

ctrlLet :: Parser Line
ctrlLet = CtrlLet <$> bindVal <*> expr
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
      term = (S <$> str <|>
              E <$> subexp <|>
              O <$> op <|>
              I <$> integer <|>
              V <$> var) <* spaceTabs

integer :: Parser Integer
integer = read <$> many1 digit

str :: Parser String
str = char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = noneOf "\\\"" <|> try (string "\\\"" >> return '"')

subexp :: Parser [Expr]
subexp = char '(' *> expr <* char ')'

var :: Parser String
var = many1 (letter <|> digit <|> char '_' <|> char '\'')

op :: Parser String
op = many1 (oneOf ":!#$%&*+./<=>?@\\^|-~")

normal :: Parser Line
normal = Normal <$> many1 (try quoted <|> try raw' <|> try raw)

quoted :: Parser InLine
quoted = Quoted <$> (string "${" *> expr <* string "}")

raw' :: Parser InLine
raw' = Raw <$> ((:) <$> (char '$')
                <*> ((:) <$> noneOf "{" <*> many (noneOf "$\n\r")))

raw :: Parser InLine
raw = Raw <$> many1 (noneOf "$\n\r")

----

class ToQ a where
    toQ :: a -> Q Exp
    concatToQ :: [a] -> Q Exp

instance ToQ Expr where
    toQ (S s) = litE (stringL s)
    toQ (I i) = litE (integerL i)
    toQ (V v) = varE (mkName v)

    concatToQ (x:xs) = undefined

instance ToQ InLine where
    toQ (Raw s) = litE (stringL s)
    toQ (Quoted expr) = undefined

    concatToQ (x:xs) = undefined

instance ToQ Line where
    toQ (CtrlForall b e) = undefined
    toQ (CtrlMaybe b e) = undefined
    toQ CtrlNothing = undefined
    toQ (CtrlIf e) = undefined
    toQ CtrlElse = undefined
    toQ (CtrlCase e) = undefined
    toQ (CtrlOf e) = undefined
    toQ (CtrlLet b e) = undefined
    toQ (Normal xs) = undefined

    concatToQ (x:xs) = undefined
