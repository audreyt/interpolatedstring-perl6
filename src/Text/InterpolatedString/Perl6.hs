{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- | QuasiQuoter for interpolated strings using Perl 6 syntax.
--
-- The "q" form does one thing and does it well: It contains a multi-line string with
-- no interpolation at all:
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
-- import Text.InterpolatedString.Perl6 (q)
-- foo :: String
-- foo = [q|
-- 
-- Well here is a
--     multi-line string!
-- 
-- |]
-- @
--
-- The "qc" form interpolates curly braces: Expressions inside {} will be
-- directly interpolated if it's a String, or have 'show' called if it is not.
--
-- Escaping of '{' is done with backslash.
--
-- For interpolating numeric expressions without an explicit type signature,
-- use the ExtendedDefaultRules lanuage pragma, as shown below:
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
-- import Text.InterpolatedString.Perl6 (qc)
-- bar :: String
-- bar = [qc| Well {\"hello\" ++ \" there\"} {6 * 7} |]
-- @
--
-- bar will have the value \" Well hello there 42 \".
--
-- If you want control over how 'show' works on your types, define a custom
-- 'ShowQ' instance:
--
-- @
-- import Text.InterpolatedString.Perl6 (qc, ShowQ(..))
-- instance ShowQ ByteString where
--     showQ = unpack
-- @
--
-- That way you interpolate bytestrings will not result in double quotes or
-- character escapes.
--
-- The "qq" form adds to the "qc" form with a simple shorthand: '$foo' means '{foo}',
-- namely interpolating a single variable into the string.
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
-- import Text.InterpolatedString.Perl6 (qq)
-- baz :: String
-- baz = [qc| Hello, $who |]
--     where
--     who = "World"
-- @
--
-- The ability to define custom "ShowQ" instances is particularly powerful with
-- cascading instances using "qq".
--
-- Below is a sample snippet from a script that converts Shape objects into
-- AppleScript suitable for drawing in OmniGraffle:
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules, NamedFieldPuns, RecordWildCards #-}
-- import Text.InterpolatedString.Perl6
-- @ 
--
-- @ 
-- data Shape = Shape
--     { originX         :: Int
--     , originY         :: Int
--     , width           :: Int
--     , height          :: Int
--     , stroke          :: Stroke
--     , text            :: Text
--     }
-- instance ShowQ Shape where
--     showQ Shape{..} = [qq|
--         make new shape at end of graphics with properties
--             \\{ $text, $stroke, _size, $_origin }
--     |]
--         where         
--         _size   = [qq|size: \{$width, $height}|]
--         _origin = [qq|origin: \{$originX, $originY}|]
-- @ 
--
-- @ 
-- data Stroke = StrokeWhite | StrokeNone
-- instance ShowQ Stroke where
--     showQ StrokeNone = \"draws stroke:false\"
--     showQ StrokeWhite = \"stroke color: {1, 1, 1}\"
-- @ 
--
-- @ 
-- data Text   = Text
--     { txt   :: String
--     , color :: Color
--     }
-- instance ShowQ Text where
--     showQ Text{..} = [qq|text: \\{ text: \"$txt\", $color, alignment: center } |]
-- @ 
--
-- @ 
-- data Color = Color { red :: Float, green :: Float, blue :: Float }
-- instance ShowQ Color where
--     showQ Color{..} = [qq|color: \{$red, $green, $blue}|]
-- @ 
--
-- @ 
-- main :: IO ()
-- main = putStrLn [qq|
--     tell application \"OmniGraffle Professional 5\"
--         tell canvas of front window
--             { makeShape ... }
--         end tell
--     end tell
-- |]
-- @
--
module Text.InterpolatedString.Perl6 (qq, qc, q, ShowQ(..)) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Data.Char (isAlpha, isAlphaNum)

class ShowQ a where
    showQ :: a -> String

instance ShowQ Char where
    showQ = (:[])

instance ShowQ String where
    showQ = id

instance (Show a) => ShowQ a where
    showQ = show

data StringPart = Literal String | AntiQuote String deriving Show

unQC a []          = [Literal (reverse a)]
unQC a ('\\':x:xs) = unQC (x:a) xs
unQC a ('\\':[])   = unQC ('\\':a) []
unQC a ('}':xs)    = AntiQuote (reverse a) : parseQC [] xs
unQC a (x:xs)      = unQC (x:a) xs

parseQC a []           = [Literal (reverse a)]
parseQC a ('\\':x:xs)  = parseQC (x:a) xs
parseQC a ('\\':[])    = parseQC ('\\':a) []
parseQC a ('{':xs)     = Literal (reverse a) : unQC [] xs
parseQC a (x:xs)       = parseQC (x:a) xs

unQQ a []          = [Literal (reverse a)]
unQQ a ('\\':x:xs) = unQQ (x:a) xs
unQQ a ('\\':[])   = unQQ ('\\':a) []
unQQ a ('}':xs)    = AntiQuote (reverse a) : parseQQ [] xs
unQQ a (x:xs)      = unQQ (x:a) xs

parseQQ a []           = [Literal (reverse a)]
parseQQ a ('\\':x:xs)  = parseQQ (x:a) xs
parseQQ a ('\\':[])    = parseQQ ('\\':a) []
parseQQ a ('$':x:xs) | x == '_' || isAlpha x =
    Literal (reverse a) : AntiQuote (x:pre) : parseQQ [] post
    where
    (pre, post) = span isIdent xs
parseQQ a ('{':xs)     = Literal (reverse a) : unQQ [] xs
parseQQ a (x:xs)       = parseQQ (x:a) xs

isIdent '_'  = True
isIdent '\'' = True
isIdent x    = isAlphaNum x

makeExpr [] = [| "" |]
makeExpr ((Literal a):xs)   = TH.appE [| (++) a |] $ makeExpr xs
makeExpr ((AntiQuote a):xs) = TH.appE [| (++) (showQ $(reify a)) |] $ makeExpr xs

reify s = 
    case parseExp s of
        Left s  -> TH.report True s >> [| "" |]
        Right e ->  return e

-- | QuasiQuoter for interpolating '$var' and '{expr}' into a string literal. The pattern portion is undefined.
qq :: QuasiQuoter
qq = QuasiQuoter (makeExpr . parseQQ [] . filter (/= '\r'))
                 (error "Cannot use qq as a pattern")
                 (error "Cannot use qq as a type")
                 (error "Cannot use qq as a dec")

-- | QuasiQuoter for interpolating '{expr}' into a string literal. The pattern portion is undefined.
qc :: QuasiQuoter
qc = QuasiQuoter (makeExpr . parseQC [] . filter (/= '\r'))
                 (error "Cannot use qc as a pattern")
                 (error "Cannot use qc as a type")
                 (error "Cannot use qc as a dec")

-- | QuasiQuoter for a non-interpolating string literal. The pattern portion is undefined.
q :: QuasiQuoter
q = QuasiQuoter ((\a -> [|a|]) . filter (/= '\r'))
                 (error "Cannot use q as a pattern")
                 (error "Cannot use q as a type")
                 (error "Cannot use q as a dec")

