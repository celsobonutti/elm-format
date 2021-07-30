module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Data.Char (digitToInt, isSpace)
import qualified Elm.String as ES
import qualified Parse.Primitives as P
import Parse.ParsecAdapter
import Parse.Helpers (processAs, escaped, expecting, sandwich, betwixt)
import Parse.IParser
import qualified Parse.String

import AST.V0_16


literal :: IParser LiteralValue
literal =
  num <|> (uncurry Str <$> str) <|> (Chr <$> chr)


num :: IParser LiteralValue
num =
  toLiteral <$> (rawNumber <?> "a number")


toLiteral :: String -> LiteralValue
toLiteral n
  | 'x' `elem` n         = IntNum (read n) HexadecimalInt
  | any (`elem` ("eE" :: String)) n = FloatNum (read n) ExponentFloat
  | any (`elem` ("." :: String)) n = FloatNum (read n) DecimalFloat
  | otherwise            = IntNum (read n) DecimalInt


rawNumber :: IParser String
rawNumber =
  concat <$> sequence
    [ option "" minus
    , base16 <|> base10
    ]


base16 :: IParser String
base16 =
  do  _ <- try (string "0x")
      digits <- many1 hexDigit
      return ("0x" ++ digits)


base10 :: IParser String
base10 =
  concat <$> sequence
    [ many1 digit
    , option "" decimals
    , option "" exponent
    ]


minus :: IParser String
minus =
  try $ do
    _ <- string "-"
    _ <- lookAhead digit
    return "-"


decimals :: IParser String
decimals =
  do  _ <- try $ lookAhead (string "." >> digit)
      _ <- string "."
      n <- many1 digit
      return ('.' : n)


exponent :: IParser String
exponent =
  do  _ <- string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)


str :: IParser (String, StringRepresentation)
str =
  let
    toExpectation = newErrorUnknown "Expected a `\"`"

    toError e = newErrorUnknown ("Error parsing string: " ++ show e)
  in
  do  (s, representation) <- Parse.String.string toExpectation toError
      return (ES.toChars s, representation)


-- TODO: Error handling.
chr :: IParser Char
chr =
  let
    toExpecation = newErrorUnknown "Expected `'`"

    toError e = newErrorUnknown ("Error parsing char: " ++ show e)
  in
  do  s <- Parse.String.character newErrorUnknown newErrorUnknown
      case ES.toChars s of
        [ c ] -> return c


--
-- Stuff forked from Text.Parsec.Token
--

charLiteral :: IParser Char
charLiteral     = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar :: IParser Char
characterChar   = charLetter <|> charEscape
                <?> "literal character"

charEscape :: IParser Char
charEscape      = do{ _ <- char '\\'; escapeCode }
charLetter :: IParser Char
charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026') || (c == '\t'))


stringLiteral :: IParser String
stringLiteral   = lexeme (
                  do{ str <- between (char '"')
                                      (char '"' <?> "end of string")
                                      (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar :: IParser (Maybe Char)
stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter :: IParser Char
stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026') || (c == '\t'))

stringEscape :: IParser (Maybe Char)
stringEscape    = do{ _ <- char '\\'
                    ;     do{ _ <- escapeGap  ; return Nothing }
                      <|> do{ _ <- escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: IParser Char
escapeEmpty     = char '&'
escapeGap :: IParser Char
escapeGap       = do{ _ <- many1 space
                    ; char '\\' <?> "end of string gap"
                    }



-- escape codes
escapeCode :: IParser Char
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl :: IParser Char
charControl     = do{ _ <- char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                    }

charNum :: IParser Char
charNum         = do{ code <- decimal
                              <|> do{ _ <- char 'o'; number 8 octDigit }
                              <|> do{ _ <- char 'x'; number 16 hexDigit }
                              <|> do{ _ <- char 'u'; between (char '{') (char '}') (number 16 hexDigit) }
                    ; if code > 0x10FFFF
                      then fail "invalid escape sequence"
                      else return (toEnum (fromInteger code))
                    }

charEsc :: IParser Char
charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc :: (Char, a) -> IParser a
                  parseEsc (c,code)     = do{ _ <- char c; return code }

charAscii :: IParser Char
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii :: (String, a) -> IParser a
                  parseAscii (asc,code) = try (do{ _ <- string asc; return code })


-- escape code tables
escMap :: [(Char, Char)]
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap :: [(String, Char)]
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                    "FS","GS","RS","US","SP"]
ascii3codes :: [String]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                    "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                    "CAN","SUB","ESC","DEL"]

ascii2 :: [Char]
ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                    '\EM','\FS','\GS','\RS','\US','\SP']
ascii3 :: [Char]
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                    '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                    '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


decimal :: IParser Integer
decimal         = number 10 digit

number :: Integer -> IParser Char -> IParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }



lexeme :: IParser a -> IParser a
lexeme p
    = do{ x <- p; whiteSpace; return x  }


--whiteSpace
whiteSpace :: IParser ()
whiteSpace = skipMany (simpleSpace <?> "")

simpleSpace :: IParser ()
simpleSpace =
    skipMany1 (satisfy isSpace)
