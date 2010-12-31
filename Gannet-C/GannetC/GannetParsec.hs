{-# LANGUAGE PolymorphicComponents #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module GannetC.GannetParsec where
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Token
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (uses local universal quantification: PolymorphicComponents)
-- 
-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
-- for a description of how to use it.
-- 
-----------------------------------------------------------------------------
import Data.Char ( digitToInt )

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Token hiding (decimal)

-- Need to parse PCRE escapes, this is difficult as the current parser
-- eats the backslash because it can only return a Char, not a [Char]
regexLiteral l = lexeme l (
                      do{ str <- between (char '/')
                                         (char '/' <?> "end of regex")
                                         (many regexChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "regular expression")

regexChar      =   do{ c <- regexLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

regexLetter    = satisfy (\c -> (c /= '/') && (c /= '\\') && (c > '\026'))

stringLiteral l = (qStringLiteral l) <|> (qqStringLiteral l) <?> "string literal"

qqStringLiteral l  = lexeme l (
                      do{ str <- between (char '"')
                                         (char '"' <?> "end of string")
                                         (many qqstringChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "double-quoted literal string")

qStringLiteral l   = lexeme l (
                      do{ str <- between (char '\'')
                                         (char '\'' <?> "end of string")
                                         (many qstringChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "single-quoted literal string")


qqstringChar      =   do{ c <- qqstringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

qqstringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

qstringChar      =   do{ c <- qstringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

qstringLetter    = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))


stringEscape    = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty     = char '&'
escapeGap       = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }



-- escape codes
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

charNum         = do{ code <- decimal
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          = zip ("abfnrtv/\\\"\'") ("\a\b\f\n\r\t\v/\\\"\'")
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
--naturalOrFloat  = lexeme (natFloat) <?> "number"
--
--float           = lexeme floating   <?> "float"
--integer         = lexeme int        <?> "integer"
--natural         = lexeme nat        <?> "natural"
--
--
---- floats
--floating        = do{ n <- decimal
--                    ; fractExponent n
--                    }
--
--
--natFloat        = do{ char '0'
--                    ; zeroNumFloat
--                    }
--                  <|> decimalFloat
--
--zeroNumFloat    =  do{ n <- hexadecimal <|> octal
--                     ; return (Left n)
--                     }
--                <|> decimalFloat
--                <|> fractFloat 0
--                <|> return (Left 0)
--
--decimalFloat    = do{ n <- decimal
--                    ; option (Left n)
--                             (fractFloat n)
--                    }
--
--fractFloat n    = do{ f <- fractExponent n
--                    ; return (Right f)
--                    }
--
--fractExponent n = do{ fract <- fraction
--                    ; expo  <- option 1.0 exponent'
--                    ; return ((fromInteger n + fract)*expo)
--                    }
--                <|>
--                  do{ expo <- exponent'
--                    ; return ((fromInteger n)*expo)
--                    }
--
--fraction        = do{ char '.'
--                    ; digits <- many1 digit <?> "fraction"
--                    ; return (foldr op 0.0 digits)
--                    }
--                  <?> "fraction"
--                where
--                  op d f    = (f + fromIntegral (digitToInt d))/10.0
--
--exponent'       = do{ oneOf "eE"
--                    ; f <- sign
--                    ; e <- decimal <?> "exponent"
--                    ; return (power (f e))
--                    }
--                  <?> "exponent"
--                where
--                   power e  | e < 0      = 1.0/power(-e)
--                            | otherwise  = fromInteger (10^e)
--
--
---- integers and naturals
--int             = do{ f <- lexeme sign
--                    ; n <- nat
--                    ; return (f n)
--                    }
--
--sign            =   (char '-' >> return negate)
--                <|> (char '+' >> return id)
--                <|> return id
--
--nat             = zeroNumber <|> decimal
--
--zeroNumber      = do{ char '0'
--                    ; hexadecimal <|> octal <|> decimal <|> return 0
--                    }
--                  <?> ""
--
decimal         = number 10 digit
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

