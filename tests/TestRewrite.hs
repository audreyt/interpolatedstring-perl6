{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where
import Text.InterpolatedString.Perl6
import Test.HUnit
import Data.ByteString.Char8 as BS(ByteString, pack)
import Data.Text as T(Text, pack)


-- the primary purpose of these tests is to ensure that
-- the Text and ByteString rewrite rules are firing, to avoid
-- needlessly converting string types
testByteString = assertBool "" $ [$qc|{"a" :: ByteString} {"b" :: ByteString}|]
                 == BS.pack ("a b")
testText = assertBool "" $ [$qc|{"a" :: Text} {"b" :: Text}|]
           == T.pack ("a b")

tests = TestList [TestLabel "ByteString Test" $ TestCase testByteString
                 ,TestLabel "Text Test" $ TestCase testText
                 ]
        
main = runTestTT tests