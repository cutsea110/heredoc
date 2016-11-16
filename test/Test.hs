{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test where

import Language.Haskell.TH
import Data.Text as T (Text, unpack)
import Data.ByteString as BS (ByteString)

import Text.Heredoc

-- | Setting up
-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> data Gender = Male | Female | NewHalf deriving Show
-- >>> data Person = Person String Int Gender deriving Show
-- >>> data Person' = Person' { name :: String, age :: Int, sex :: Gender }
--

-- |
-- >>> [heredoc|Hello,World|]
-- "Hello,World"
--
-- |
-- >>> :{
-- [heredoc|
-- Hello,World
-- |]
-- :}
-- "\n Hello,World\n "
--
{- | >>> :{
let x = 42
in [heredoc|
The number is ${show x}.
|]
:}
"\nThe number is 42.\n"
-}
{- | >>> :{
[heredoc|
Question
  $let x = 42
    ${show x} + 2 = ${show $ x + 2}.
    ${show x} * 2 = ${show $ x * 2}.
    ${show x} ^ 2 = ${show $ x ^ 2}.
|]
:}
"\nQuestion\n  42 + 2 = 44.\n  42 * 2 = 84.\n  42 ^ 2 = 1764.\n"
-}
{- | >>> :{
[heredoc|
Question
  $let x = 42
    $let y = "Katsutoshi"
      ${y}(${show $ x+3}).
|]
:}
"\nQuestion\n  Katsutoshi(45).\n"
-}
{- | >>> :{
let mu = Just "katsutoshi"
in [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|]
:}
"\nHello katsutoshi san\n"
-}
{- | >>> :{
let mu = Nothing
in [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|]
:}
"\nBye\n"
-}
{- | >>> :{
let ma = Nothing :: Maybe Int
in [heredoc|
$maybe a<-ma
  ${show a}
|]
:}
"\n"
-}
{- | >>> :{
let mu = Just "katsutoshi"
    ma = Just 45
in [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
|]
:}
"\nkatsutoshi(45)\n"
-}
{- | >>> :{
let mu = Just "katsutoshi"
    ma = Nothing :: Maybe Int
in [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
  $nothing
    ${u}(age not found)
|]
:}
"\nkatsutoshi(age not found)\n"
-}
{- | >>> :{
[heredoc|
$if True
  OK
$else
  NG
|]
:}
"\nOK\n"
-}
{- | >>> :{
[heredoc|
$if 1==1
  OK
|]
:}
"\nOK\n"
-}
{- | >>> :{
[heredoc|
$if 1==2
  OK
|]
:}
"\n"
-}
{- | >>> :{
[heredoc|
$if 1==2
$else
  False
|]
:}
"\nFalse\n"
-}
{- | >>> :{
let mu = Just "katsutoshi"
    b = True
in [heredoc|
Hello
  $maybe u <- mu
   $if b
     OK => ${u}
   $else
     NG
|]
:}
"\nHello\n  OK => katsutoshi\n"
-}
{- | >>> :{
let mu = Just "katsutoshi"
in [heredoc|
Hello
  $maybe u <- mu
    $if u=="katsutoshi"
      OK => ${u}
    $else
      NG
|]
:}
"\nHello\n  OK => katsutoshi\n"
-}
{- | >>> :{
let b = True
in [heredoc|
Hello
  $if b
    $maybe x <- Just "katsutoshi"
      OK => ${x}.
    $nothing
      Ooops.
  $else
|]
:}
"\nHello\n  OK => katsutoshi.\n"
-}
{- | >>> :{
[heredoc|
$maybe _ <- Just 1
  OK
$nothing
  NG
|]
:}
"\nOK\n"
-}
{- | >>> :{
let x = Female
in [heredoc|
$case x
  $of Male
    Otoko
  $of Female
    Onna
  $of _
    P~~~~~~~
|]
:}
"\nOnna\n"
-}
{- | >>> :{
let mp = Just (Person "Katsutoshi" 45 Male)
in [heredoc|
$maybe Person name age sex <- mp
  $case sex
    $of Male
      ${name}(${show age}) - Otoko
    $of Female
      ${name}(${show age}) - Onna
    $of _
      ${name}(${show age}) - ?
|]
:}
"\nKatsutoshi(45) - Otoko\n"
-}
{- | >>> :{
let p = (Person "katsutoshi" 45 Male, Person "keiko" 44 Female)
in [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = p
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|]
:}
"\nkatsutoshi(45) Male\nkeiko(44) Female\n"
-}
{- | >>> :{
let p = Person "katsutoshi" 45 Male
    p' = Person "keiko" 44 Female
in [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = (p, p')
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|]
:}
"\nkatsutoshi(45) Male\nkeiko(44) Female\n"
-}
{- | >>> :{
[heredoc|
$let x:xs = 1:[]
  ${show x} OK
|]
:}
"\n1 OK\n"
-}
{- | >>> :{
[heredoc|
$let x:y:z = 1:2:3:4:[5,6,7]
  ${show z} OK
|]
:}
"\n[3,4,5,6,7] OK\n"
-}
{- | >>> :{
[heredoc|
$let x:y:z = [1,2,3]:[4]:[5,6,7]:[]
  ${show x} OK
  ${show y} OK
  ${show z} OK
|]
:}
"\n[1,2,3] OK\n[4] OK\n[[5,6,7]] OK\n"
-}
{- | >>> :{
[heredoc|
$let x:_:z = (1:2:[]):(3:4:[]):(5:[6,7]):[]
  ${show x} OK
  ${show z} OK
|]
:}
"\n[1,2] OK\n[[5,6,7]] OK\n"
-}
{- | >>> :{
[heredoc|
$let xss@(x@(_,_):xs) = [(1,2),(3,4),(5,6)]
  ${show $ fst x} and ${show xs} in ${show xss}
|]
:}
"\n1 and [(3,4),(5,6)] in [(1,2),(3,4),(5,6)]\n"
-}
{- | >>> :{
[heredoc|
$maybe p@(Person _ age _) <- Just (Person "katsutoshi" 45 Male)
  ${show p}
|]
:}
"\nPerson \"katsutoshi\" 45 Male\n"
-}
{- | >>> :{
let ps = [Person' "katsutoshi" 45 Male, Person' "keiko" 44 Female]
in [heredoc|
$forall (i, p) <- zip [1,2] ps
  ${show i}
    Name : ${name p}
    Age  : ${show $ age p}
    Sex  : ${show $ sex p}
|]
:}
"\n1\n  Name : katsutoshi\n  Age  : 45\n  Sex  : Male\n2\n  Name : keiko\n  Age  : 44\n  Sex  : Female\n"
-}
{- | >>> :{
let name = "Katsutoshi" :: Text
in [heredoc|Hello, ${T.unpack name} san!|]
:}
"Hello, Katsutoshi san!"
-}
{- | >>> :{
[heredoc|
$let (name1, name2) = ("Keiko", "Nao")
  Hello, ${T.unpack name1} & ${T.unpack name2}
|]
:}
"\nHello, Keiko & Nao\n"
-}
{- | >>> :{
let name = "Katsutoshi" :: Text
in [heredoc|Hello, ${name} (Text) san!|]
:}
"Hello, Katsutoshi (Text) san!"
-}
{- | >>> :{
let name = "Katsutoshi" :: ByteString
in [heredoc|Hello, ${name} (ByteString) san!|]
:}
"Hello, Katsutoshi (ByteString) san!"
-}
