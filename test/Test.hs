{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test where

import Language.Haskell.TH

import Text.Heredoc
{--
servantClientCode :: String
servantClientCode = [heredoc|
using Newtonsoft.Json;
using System.Collection.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type aliases
$forall (name, type) <- types
  using ${name} = ${type}
#endregion

namespace ServantClientBook
{
    class ServantClient : HttpClient
    {
        public ServantClient()
        {
            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
    }

    public class API
    {
        #region fields
        private string server;
        #endregion

        #region properties
        #endregion

        #region Constructor
        public API(string _server)
        {
            this.server = _server;
        }
        #endregion

        #region APIs
        $forall ep <- endpoints
          public async Task<${retType ep}> ${methodName ep}Async(${paramDecl ep})
          {
              var client = new ServantClient();
              var queryparams = new List<string> {
                  $forall qp <- queryparams ep
                    _${qp}.HasValue ? $"_${qp}={_${qp}.Value}" : null,
              }.Where(e => !string.IsNullOrEmpty(e));
              var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
#if DEBUG
                var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
#else
                var jsonObj = JsonConvert.SerializeObject(_obj);
#endif
              $if requestBodyExists ep
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
              $else
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}");
              Debug.WriteLine($">>> {res.RequestMessage}");
              $if requestBodyExists ep
                Debug.WriteLine($"-----");
                Debug.WriteLine(jsonObj);
                Debug.WriteLine($"-----");
              Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
              var content = await res.Content.ReadAsStringAsync();
              Debug.WriteLine($"<<< {content}");
              return JsonConvert.DeserializeObject<${retType ep}>(content);
         }
          public ${retType ep} ${methodName ep}(${paramDecl ep})
          {
              Task<${retType ep}> t = ${methodName ep}Async(${paramArg ep});
              return t.GetAwaiter().GetResult();
          }
        #endregion
    }
}
       |]

main :: IO ()
main = do
  let types = [ ("AddressId", "System.Int64")
              , ("AuthorId", "System.Int64")
              , ("PublisherId", "System.Int64")
              , ("BookId", "System.Int64")
              , ("ISBN", "System.String")
              , ("Postcode", "System.String")
              , ("Tel", "System.String")
              , ("Fax", "System.String")
              , ("Emailaddress", "System.String")
              ]
  putStr servantClientCode

--}
-- |
-- >>> :set -XQuasiQuotes
-- >>> [heredoc|Hello,World|]
-- "Hello,World\n"
--
-- |
-- >>> :{
-- [heredoc|
-- Hello,World
-- |]
-- :}
-- "\n Hello,World\n \n"

test0 :: String
test0 = [heredoc|hello,world|]

test1 :: String
test1 = [heredoc|
Hello, World!
|]

test2 :: String
test2 = let x = 42
        in [heredoc|
The number is ${show x}.
|]

test3 :: String
test3 = [heredoc|
Question
  $let x = 42
    ${show x} + 2 = ${show $ x + 2}.
    ${show x} * 2 = ${show $ x * 2}.
    ${show x} ^ 2 = ${show $ x ^ 2}.
|]

test4 :: String
test4 = [heredoc|
Question
  $let x = 42
    $let y = "Katsutoshi"
      ${y}(${show $ x+3}).
|]

test5 :: String
test5 = let mu = Just "katsutoshi"
        in [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|]

test5' :: String
test5' = let mu = Nothing
         in [heredoc|
$maybe u <- mu
  Hello ${u} san
$nothing
  Bye
|]

test5'' :: String
test5'' = let ma = Nothing :: Maybe Int
          in [heredoc|
$maybe a<-ma
  ${show a}
|]

test6 :: String
test6 = let mu = Just "katsutoshi"
            ma = Just 45
        in [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
|]

test6' :: String
test6' = let mu = Just "katsutoshi"
             ma = Nothing :: Maybe Int
         in [heredoc|
$maybe u <- mu
  $maybe a <- ma
    ${u}(${show a})
  $nothing
    ${u}(age not found)
|]

test7 :: String
test7 = [heredoc|
$if True
  OK
$else
  NG
|]

test8 :: String
test8 = [heredoc|
$if 1==1
  OK
|]

test8' :: String
test8' = [heredoc|
$if 1==2
  OK
|]

test8'' :: String
test8'' = [heredoc|
$if 1==2
$else
  False
|]

test9 :: String
test9 = let mu = Just "katsutoshi"
            b = True
        in [heredoc|
Hello
  $maybe u <- mu
    $if b
      OK => ${u}
    $else
      NG
|]

test9' :: String
test9' = let mu = Just "katsutoshi"
        in [heredoc|
Hello
  $maybe u <- mu
    $if u=="katsutoshi"
      OK => ${u}
    $else
      NG
|]

test9'' :: String
test9'' = let b = True
          in [heredoc|
Hello
  $if b
    $maybe x <- Just "katsutoshi"
      OK => ${x}.
    $nothing
      Ooops.
  $else
|]

test10 :: String
test10 = [heredoc|
$maybe _ <- Just 1
  OK
$nothing
  NG
|]

data Gender = Male | Female | NewHalf deriving Show

test11 :: String
test11 = let x = Female
         in [heredoc|
$case x
  $of Male
    Otoko
  $of Female
    Onna
  $of _
    P~~~~~~~
|]

data Person = Person String Int Gender deriving Show

test12 :: String
test12 = let mp = Just (Person "Katsutoshi" 45 Male)
         in [heredoc|
$maybe Person name age sex <- mp
  $case sex
    $of Male
      ${name}(${show age}) - 男
    $of Female
      ${name}(${show age}) - 女
    $of _
      ${name}(${show age}) - ?
|]

test13 :: String
test13 = let p = (Person "katsutoshi" 45 Male, Person "keiko" 44 Female)
         in [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = p
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|]

test13' :: String
test13' = let p = Person "katsutoshi" 45 Male
              p' = Person "keiko" 44 Female
         in [heredoc|
$let (Person n1 a1 g1, Person n2 a2 g2) = (p, p')
  ${n1}(${show a1}) ${show g1}
  ${n2}(${show a2}) ${show g2}
|]

test14 :: String
test14 = [heredoc|
$let x:xs = 1:[]
  ${show x} OK
|]

test14' :: String
test14' = [heredoc|
$let x:y:z = 1:2:3:4:[5,6,7]
  ${show z} OK
|]

test14'' :: String
test14'' = [heredoc|
$let x:y:z = [1,2,3]:[4]:[5,6,7]:[]
  ${show x} OK
  ${show y} OK
  ${show z} OK
|]

test14''' :: String
test14''' = [heredoc|
$let x:_:z = (1:2:[]):(4:[]):(5:[6,7]):[]
  ${show x} OK
  ${show z} OK
|]

