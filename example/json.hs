{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

import JsonData

import Prelude hiding ((.), negate, replicate)
import Control.Isomorphism.Partial.Ext
  (Iso, (<$>), (.), inverse, subset, cons, readShow,
   chrOrd, hex, signumAbs, digitsFloat, floatTripleDigits)
import Text.Syntax.Poly
  ((<|>), (<*>), syntax, syntaxError, token, SyntaxT,
   list, this, between, (*>), (<*), many, some, sepBy, replicate,
   choice, optSpace)
import Text.Syntax.Parser.List.Type (RunAsStringParser, ErrorString, ErrorStack)
import Text.Syntax.Parser.List.Lazy (runAsParser)
import Text.Syntax.Printer.List (RunAsStringPrinter, runAsPrinter)

import System.Environment (getArgs)

type JSyntax a = SyntaxT Char a

s_text :: JSyntax JValue
s_text =  optSpace *> text <|>
          syntaxError "JSON text"  where
  text = jObject <$> s_object <|>
         jArray  <$> s_array

s_series :: Char -> JSyntax a -> Char -> JSyntax [a]
s_series left parser right =
  between
  (this left  <* optSpace)
  (this right <* optSpace)
  ((parser <* optSpace) `sepBy` (this ',' <* optSpace))

s_array :: JSyntax [JValue]
s_array =  s_series '[' s_value ']'

s_object :: JSyntax [(String, JValue)]
s_object =  s_series '{' s_field '}'  where
  s_field :: JSyntax (String, JValue)
  s_field =  s_string <* optSpace <* this ':' <* optSpace <*> s_value


s_value :: JSyntax JValue
s_value =  (jString <$> s_string    <|>
            jNumber <$> s_number    <|>
            jObject <$> s_object    <|>
            jArray  <$> s_array     <|>
            jBool   <$> s_bool      <|>
            jNull   <$> list "null" <|>
            syntaxError "JSON value")   <* optSpace

s_bool   :: JSyntax Bool
s_bool   =  true  <$> list "true"  <|>
            false <$> list "false"

s_digit :: JSyntax Char
s_digit =  subset (`elem` ['0'..'9']) <$> token

s_digit_nz :: JSyntax Char
s_digit_nz =  subset (`elem` ['1'..'9']) <$> token

s_digits0 :: JSyntax String
s_digits0 =  many s_digit

s_digits1 :: JSyntax String
s_digits1 =  some s_digit

s_hexdigit :: JSyntax Char
s_hexdigit =  subset (`elem` (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']))
              <$> token

int :: Iso String Int
int =  readShow

char   :: Char -> JSyntax Char
char c =  this c *> syntax c

s_float :: JSyntax Double
s_float =  digitsFloat . floatTripleDigits
           <$> (this '0' *> syntax "" <|>
                cons <$> s_digit_nz <*> s_digits0)
           <*> (this '.' *> s_digits1 <|> syntax "")
           <*> ((this 'e' <|> this 'E')
                *>  (int <$> (cons <$> char '-' <*> s_digits1 <|>
                              this '+' *> s_digits1           <|>
                              s_digits1)
                    )  <|>
                syntax 0)

s_number :: JSyntax Double
s_number =  signumAbs
            <$> ((this '-' *> syntax (-1)) <|>
                 syntax 1)
            <*> s_float

s_string :: JSyntax String
s_string =  between (this '\"') (this '\"') (many jchar)
  where jchar = this '\\' *> (s_escape <|> s_unicode) <|>
                (subset (`notElem` "\"\\") <$> token)

escapeMap :: [(Char, Char)]
escapeMap =  [('b', '\b'), ('n', '\n'),
              ('f', '\f'), ('r', '\r'),
              ('t', '\t'), ('\\', '\\'),
              ('\"', '\"'), ('/', '/')]

s_escape :: JSyntax Char
s_escape =  choice $ map (uncurry decode) escapeMap
  where decode c r = this c *> syntax r

s_unicode :: JSyntax Char
s_unicode =  inverse chrOrd . hex <$> this 'u' *> replicate 4 s_hexdigit


runStringParser :: RunAsStringParser a ErrorStack
runStringParser =  runAsParser

runStringPrinter :: RunAsStringPrinter a ErrorString
runStringPrinter =  runAsPrinter

main :: IO ()
main =  do (fn:_) <- getArgs
           input <- readFile fn
           case runStringParser s_text input of
             Left e       -> putStrLn $ "Parse error: " ++ show e
             Right parsed ->
               do putStrLn $ "parsed: " ++ show parsed
                  case runStringPrinter s_text parsed of
                    Left e        -> putStrLn $ "Print error: " ++ show e
                    Right printed -> putStrLn $ "printed:\n" ++ printed
