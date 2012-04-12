{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Main where

import Text.InterpolatedString.Perl6
import Test.HUnit
import GHC.Exts(fromString)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

data Foo = Foo Int String deriving Show

t1 = "字元"

testEmpty       = assertBool "" ([$qc||] == "")
testCharLiteral = assertBool "" ([$qc|{1+2}|] == "3")
testString      = assertBool "" ([$qc|a string {t1} is here|] == "a string 字元 is here")
testVariable    = assertBool "" ([$qq|a string {t1} $t1 {t1} $t1 is here|] == "a string 字元 字元 字元 字元 is here")
testEscape      = assertBool "" ([$qc|#\{}|] == "#{}" && [$qc|\{}|] == "{}")
testComplex     = assertBool "" ([$qc|
        ok
{Foo 4 "Great!" : [Foo 3 "Scott!"]}
        then
|] == ("\n" ++
    "        ok\n" ++
    "[Foo 4 \"Great!\",Foo 3 \"Scott!\"]\n" ++
    "        then\n"))
testConvert = assertBool "" 
              (([$qc|{fromString "a"::Text} {fromString "b"::ByteString}|] :: String)
               == "a b")


tests = TestList
    [ TestLabel "Empty String" $ TestCase testEmpty
    , TestLabel "Character Literal" $ TestCase testCharLiteral
    , TestLabel "String Variable" $ TestCase testString
    , TestLabel "Dollar Variable" $ TestCase testVariable
    , TestLabel "Escape Sequences" $ TestCase testEscape
    , TestLabel "Complex Expression" $ TestCase testComplex
    , TestLabel "String Conversion" $ TestCase testConvert
    ]

main = runTestTT tests

