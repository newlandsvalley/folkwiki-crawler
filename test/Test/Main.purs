module Test.Main where

import Prelude
import Data.Bifunctor (rmap)
import Data.Either (Either(..), isRight)
import Data.List (length)
import Data.String (take)
import Effect (Effect)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import AbcParser (parse)
import JsonBuilder (buildJsonString)

main :: Effect Unit
main = 
  runSpecAndExitProcess [ specReporter] do
    describe "all tests" do
      parserSpec
      jsonSpec

parserSpec :: Spec Unit
parserSpec = do
  describe "parser" do
    it "parses a tune" do
      let 
        eProperties = parse larkan 
      eProperties `shouldSatisfy` isRight
      rmap length eProperties `shouldEqual` Right 5
    it "recognizes a tune with two titles" do
      let 
        eProperties = parse twiceNamed
      rmap length eProperties `shouldEqual` Right 6

jsonSpec :: Spec Unit
jsonSpec = do 
  describe "json builder" do 
    it "builds json" do
      let 
        eJson = rmap buildJsonString $ parse twiceNamed
      case eJson of 
        Left _err -> 
          fail "parse error"
        Right jsonString -> do
          take 15 jsonString  `shouldEqual` """{"name":"Djursd"""  

byssKalle :: String
byssKalle =
  "X:6\r\n" <>
  "T:Polska efter Byss-Kalle (nr 6)\r\n" <>
  "R:Polska\r\n" <>
  "B:[[Notböcker/57 låtar efter Byss-Kalle]] utgivet av Uplands Spelmansförbund (där det även finns en andrastämma)\r\n" <>
  "Z:Nils L, 2007-12-14\r\n" <>
  "O:efter Byss-Kalle, Uppland\r\n" <>
  "M:3/4\r\n" <>
  "L:1/16\r\n" <>
  "K:D\r\n" <>
  "A4 | !tenuto!d4 {de}!tenuto!d4 (cA)ce  | d2d2 B4 (AB)cd | {c2} !tenuto![A4e4] !tenuto![A4e4] (dc)df | e2e2 {cB}c4 A4 |\r\n" <> 
     "A2f2 f4 (dc)de | d2d2 c4 B4 | B2g2 g2e2 e2d2 | (cd)ec A2A2 (BA)Bc | c2d2 d4 :|\r\n" <>
  "A4 | d2f2 f2d2 d2ef | (gf)ef g2e2 (fe)fg | a2f2 f2d2 d2ef | g2e2 e2c2 c2A2 |\r\n" <>
     "d2f2 f2d2 d2ef | (gf)ef g2e2 (fe)fg | a2f2 d2b2 g2e2 | (dc)Bc [d4D4] :|\r\n" 

larkan :: String 
larkan =  
  "X:5\r\n" <>
  "T:Lärkan - Polska efter Byss-Kalle (nr 5)\r\n" <> 
  "R:Polska\r\n" <>
  "B:57 låtar efter Byss-Kalle\r\n" <>
  "S:efter Byss-Kalle\r\n" <>
  "Z:2007-12-14, hämtad från häftet \"57 låter efter Byss-Kalle\" utgivet av Uplands Spelmansförbund (där det även finns en andrastämma)\r\n" <>
  "O:efter Byss-Kalle, Uppland\r\n" <>
  "Z:Nils L\r\n" <>
  "M:3/4\r\n" <>
  "L:1/16\r\n" <>
  "K:Gm\r\n" <>
  "{ABAG} A4|(Bd)cB (AB)cA B2A2|(=ed)cB (AB)cA B2{cB}A2|(Bd)cB (Ac)BA G2a=b|\r\n" <>
    "(c'=b)ag (^fg)=bg d2d2|(_Bd)cB (AB)cA B2{cB}A2|(=ed)cB (AB)cA B2A2|(Bd)cB (Ac)BA G2ac'|\r\n" <>
  "(=ba)g^f {/f}g4 ::[K:G] b4|(d'b)gb (d'b)gb d'2d'2|(d'a)fa (c'a)fa {/a}d'4|\r\n" <>
     "(d'b)gb (d'b)gb d'2d'2|(d'a)fa c'afa {gagf}g4|(g^f)gb e2eg f2fa|d2de c2cg _B2{cB}A2|\r\n" <>
     "(_Bd)cB (AB)cA B2{cB}A2|(ed)c_B (AB)cA B2A2|(_Bd)cB (Ac)BA G2ac'|=bagf g8 :|\r\n"
 
twiceNamed :: String
twiceNamed =
  "X:16\r\n" <>
  "T:Djursdalapolskan\r\n" <>
  "T:Melankoliska polonessen\r\n" <>
  "R:Polska\r\n" <>
  "B:50 Småländska låtar\r\n" <>
  "B:Jämför [[https://katalog.visarkiv.se/lib/views/fmk/ShowRecord.aspx?id=1432004|FMK - katalog Ma6 bild 126]] efter [[Personer/Petter Dufva]]\r\n" <>
  "O:Djursdala, Småland\r\n" <>
  "N:Ur gamla handskrifter från Djursdala\r\n" <>
  "N:nr. 16 i '50 Småländska låtar'\r\n" <>
  "N:Låten förekommer i många spelmansböcker, ofta som \"Melankoliska polonessen\"\r\n" <>
  "M:3/4\r\n" <>
  "L:1/8\r\n" <>
  "K:Dm\r\n" <>
  "DD/2F/2 A^c de |fe/2f/2 g/2f/2e/2d/2 ^cc| d/2^c/2d/2e/2 f/2g/2f/2e/2 df|e/2f/2e/2d/2 c/2d/2c/2B/2 A/2G/2F/2E/2|\r\n" <>
  "DD/2F/2 Ad fa| ag/a/ bb/2a/2 ge|fa/2f/2 eg/2e/2 d/2^c/2d/2f/2|a/2g/2e/2^c/2 cd d2:|\r\n" <>
  "|:ff/2e/2 ff/2e/2 fg|ee/2^c/2 ee/2c/2 ef| dd/2^c/2 dd/2c/2 df|e/2f/2e/2d/2 c/2d/2c/2B/2 A/2G/2F/2E/2|\r\n" <>
  "DD/2F/2 Ad fa|ag/2a/2 bb/2a/2 ge| fa/2f/2 eg/2e/2 d/2^c/2d/2f/2|a/2g/2e/2^c/2 cd d2:|\r\n"
 