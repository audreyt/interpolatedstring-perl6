Text.InterpolatedString.Perl6

QuasiQuoter for Perl6-style multi-line interpolated strings with "q", "qq" and "qc" support.

Description

QuasiQuoter for interpolated strings using Perl 6 syntax.

The q form does one thing and does it well: It contains a multi-line string with
no interpolation at all:

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.Perl6 (q)
foo :: String -- Text, ByteString etc also works
foo = [q|

Well here is a
    multi-line string!

|]

Any instance of the IsString class is permitted.

The qc form interpolates curly braces: expressions inside {} will be
directly interpolated if it's a Char, String, Text or ByteString, or 
it will have show called if it is not.

Escaping of '{' is done with backslash. 

For interpolating numeric expressions without an explicit type signature,
use the ExtendedDefaultRules lanuage pragma, as shown below:

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.Perl6 (qc)
bar :: String
bar = [qc| Well {"hello" ++ " there"} {6 * 7} |]

bar will have the value " Well hello there 42 ".

If you want control over how show works on your types, define a custom
ShowQ instance:

For example, this instance allows you to display interpolated lists of strings as 
a sequence of words, removing those pesky brackets, quotes, and escape sequences.

{-# LANGUAGE FlexibleInstances #-}
import Text.InterpolatedString.Perl6 (qc, ShowQ(..))
instance ShowQ [String] where
    showQ = unwords

The qq form adds to the qc form with a simple shorthand: '$foo' means '{foo}',
namely interpolating a single variable into the string.

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.Perl6 (qq)
baz :: String
baz = [qc| Hello, $who |]
    where
    who = "World"

Both qc and qq permit output to any types with both IsString and Monoid 
instances.

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import Text.InterpolatedString.Perl6 (qc)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
qux :: ByteString
qux = [qc| This will convert {"Text" :: Text} to {"ByteString" :: ByteString} |]

The ability to define custom ShowQ instances is particularly powerful with
cascading instances using qq.

Below is a sample snippet from a script that converts Shape objects into
AppleScript suitable for drawing in OmniGraffle:

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, NamedFieldPuns, RecordWildCards #-}
import Text.InterpolatedString.Perl6


data Shape = Shape
    { originX         :: Int
    , originY         :: Int
    , width           :: Int
    , height          :: Int
    , stroke          :: Stroke
    , text            :: Text
   }
instance ShowQ Shape where
    showQ Shape{..} = [qq|
        make new shape at end of graphics with properties
            \{ $text, $stroke, $_size, $_origin }
    |]
        where         
        _size   = [qq|size: \{$width, $height}|]
        _origin = [qq|origin: \{$originX, $originY}|]

data Stroke = StrokeWhite | StrokeNone
instance ShowQ Stroke where
    showQ StrokeNone = "draws stroke:false"
    showQ StrokeWhite = "stroke color: {1, 1, 1}"

data Text   = Text
    { txt   :: String
    , color :: Color
    }
instance ShowQ Text where
    showQ Text{..} = [qq|text: \{ text: "$txt", $color, alignment: center } |]

data Color = Color { red :: Float, green :: Float, blue :: Float }
instance ShowQ Color where
    showQ Color{..} = [qq|color: {$red, $green, $blue}|]

main :: IO ()
main = putStrLn [qq|
    tell application "OmniGraffle Professional 5"
        tell canvas of front window
            { makeShape ... }
        end tell
    end tell
|]
