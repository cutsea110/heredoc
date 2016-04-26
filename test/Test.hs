{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test where

import Language.Haskell.TH
import Data.Text as T (Text, unpack)

import Text.Heredoc

-- | Setting up
-- >>> :set -XQuasiQuotes
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
-- |
-- >>> :{
-- let x = 42
-- in [heredoc|
-- The number is ${show x}.
-- |]
-- :}
-- "\n The number is 42.\n "
--
-- |
-- >>> :{
-- [heredoc|
-- Question
--   $let x = 42
--     ${show x} + 2 = ${show $ x + 2}.
--     ${show x} * 2 = ${show $ x * 2}.
--     ${show x} ^ 2 = ${show $ x ^ 2}.
-- |]
-- :}
-- "\n Question\n   42 + 2 = 44.\n   42 * 2 = 84.\n   42 ^ 2 = 1764.\n "
--
-- |
-- >>> :{
-- [heredoc|
-- Question
--   $let x = 42
--     $let y = "Katsutoshi"
--       ${y}(${show $ x+3}).
-- |]
-- :}
-- "\n Question\n   Katsutoshi(45).\n "
--
-- |
-- >>> :{
-- let mu = Just "katsutoshi"
-- in [heredoc|
-- $maybe u <- mu
--   Hello ${u} san
-- $nothing
--   Bye
-- |]
-- :}
-- "\n Hello katsutoshi san\n "
--
-- |
-- >>> :{
-- let mu = Nothing
-- in [heredoc|
-- $maybe u <- mu
--   Hello ${u} san
-- $nothing
--   Bye
-- |]
-- :}
-- "\n Bye\n "
--
-- |
-- >>> :{
-- let ma = Nothing :: Maybe Int
-- in [heredoc|
-- $maybe a<-ma
--   ${show a}
-- |]
-- :}
-- "\n "
--
-- |
-- >>> :{
-- let mu = Just "katsutoshi"
--     ma = Just 45
-- in [heredoc|
-- $maybe u <- mu
--   $maybe a <- ma
--     ${u}(${show a})
-- |]
-- :}
-- "\n katsutoshi(45)\n "
--
-- |
-- >>> :{
-- let mu = Just "katsutoshi"
--     ma = Nothing :: Maybe Int
-- in [heredoc|
-- $maybe u <- mu
--   $maybe a <- ma
--     ${u}(${show a})
--   $nothing
--     ${u}(age not found)
-- |]
-- :}
-- "\n katsutoshi(age not found)\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $if True
--   OK
-- $else
--   NG
-- |]
-- :}
-- "\n OK\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $if 1==1
--   OK
-- |]
-- :}
-- "\n OK\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $if 1==2
--   OK
-- |]
-- :}
-- "\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $if 1==2
-- $else
--   False
-- |]
-- :}
-- "\n False\n "
--
-- |
-- >>> :{
-- let mu = Just "katsutoshi"
--     b = True
-- in [heredoc|
-- Hello
--   $maybe u <- mu
--    $if b
--      OK => ${u}
--    $else
--      NG
-- |]
-- :}
-- "\n Hello\n   OK => katsutoshi\n "
--
-- |
-- >>> :{
-- let mu = Just "katsutoshi"
-- in [heredoc|
-- Hello
--   $maybe u <- mu
--     $if u=="katsutoshi"
--       OK => ${u}
--     $else
--       NG
-- |]
-- :}
-- "\n Hello\n   OK => katsutoshi\n "
--
-- |
-- >>> :{
-- let b = True
-- in [heredoc|
-- Hello
--   $if b
--     $maybe x <- Just "katsutoshi"
--       OK => ${x}.
--     $nothing
--       Ooops.
--   $else
-- |]
-- :}
-- "\n Hello\n   OK => katsutoshi.\n "
--
-- |
-- >>> :{
--  [heredoc|
-- $maybe _ <- Just 1
--   OK
-- $nothing
--   NG
-- |]
-- :}
-- "\n OK\n "
--
-- >>> data Gender = Male | Female | NewHalf deriving Show
-- >>> :{
-- let x = Female
-- in [heredoc|
-- $case x
--   $of Male
--     Otoko
--   $of Female
--     Onna
--   $of _
--     P~~~~~~~
-- |]
-- :}
-- "\n Onna\n "
--
-- |
-- >>> data Person = Person String Int Gender deriving Show
-- >>> :{
-- let mp = Just (Person "Katsutoshi" 45 Male)
-- in [heredoc|
-- $maybe Person name age sex <- mp
--   $case sex
--     $of Male
--       ${name}(${show age}) - Otoko
--     $of Female
--       ${name}(${show age}) - Onna
--     $of _
--       ${name}(${show age}) - ?
-- |]
-- :}
-- "\n Katsutoshi(45) - Otoko\n "
--
-- |
-- >>> :{
-- let p = (Person "katsutoshi" 45 Male, Person "keiko" 44 Female)
-- in [heredoc|
-- $let (Person n1 a1 g1, Person n2 a2 g2) = p
--   ${n1}(${show a1}) ${show g1}
--   ${n2}(${show a2}) ${show g2}
-- |]
-- :}
-- "\n katsutoshi(45) Male\n keiko(44) Female\n "
--
-- |
-- >>> :{
-- let p = Person "katsutoshi" 45 Male
--     p' = Person "keiko" 44 Female
-- in [heredoc|
-- $let (Person n1 a1 g1, Person n2 a2 g2) = (p, p')
--   ${n1}(${show a1}) ${show g1}
--   ${n2}(${show a2}) ${show g2}
-- |]
-- :}
-- "\n katsutoshi(45) Male\n keiko(44) Female\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $let x:xs = 1:[]
--   ${show x} OK
-- |]
-- :}
-- "\n 1 OK\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $let x:y:z = 1:2:3:4:[5,6,7]
--   ${show z} OK
-- |]
-- :}
-- "\n [3,4,5,6,7] OK\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $let x:y:z = [1,2,3]:[4]:[5,6,7]:[]
--   ${show x} OK
--   ${show y} OK
--   ${show z} OK
-- |]
-- :}
-- "\n [1,2,3] OK\n [4] OK\n [[5,6,7]] OK\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $let x:_:z = (1:2:[]):(3:4:[]):(5:[6,7]):[]
--   ${show x} OK
--   ${show z} OK
-- |]
-- :}
-- "\n [1,2] OK\n [[5,6,7]] OK\n "
--
-- >>> :{
-- [heredoc|
-- $let xss@(x@(_,_):xs) = [(1,2),(3,4),(5,6)]
--   ${show $ fst x} and ${show xs} in ${show xss}
-- |]
-- :}
-- "\n 1 and [(3,4),(5,6)] in [(1,2),(3,4),(5,6)]\n "
--
-- |
-- >>> :{
-- [heredoc|
-- $maybe p@(Person _ age _) <- Just (Person "katsutoshi" 45 Male)
--   ${show p}
-- |]
-- :}
--"\n Person \"katsutoshi\" 45 Male\n "
--
-- |
-- >>> data Person' = Person' { name :: String, age :: Int, sex :: Gender }
-- >>> :{
-- let ps = [Person' "katsutoshi" 45 Male, Person' "keiko" 44 Female]
-- in [heredoc|
-- $forall (i, p) <- zip [1,2] ps
--   ${show i}
--     Name : ${name p}
--     Age  : ${show $ age p}
--     Sex  : ${show $ sex p}
-- |]
-- :}
-- "\n 1\n   Name : katsutoshi\n   Age  : 45\n   Sex  : Male\n 2\n   Name : keiko\n   Age  : 44\n   Sex  : Female\n "
--
-- |
-- | Setting up
-- >>> :set -XOverloadedStrings
--
-- >>> let name = "Katsutoshi" :: Text
-- >>> [heredoc|Hello, ${T.unpack name} san!|]
-- "Hello, Katsutoshi san!"
--
