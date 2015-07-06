module HHTTPP.Common where

import           Data.CaseInsensitive (CI, mk)
import           Text.ParserCombinators.Parsec

consume_spaces :: Parser String
consume_spaces = many (char ' ')

consume_eol :: Parser String
consume_eol = string "\r\n" <|> string "\n"

parse_msg_version :: Parser String
parse_msg_version = many (noneOf " \r\n")

parse_msg_header :: Parser (CI String, Maybe String)
parse_msg_header =
  parse_msg_header_key >>= (\key ->
  parse_msg_header_val >>= (\val ->
  consume_eol >>
  return (key, val)))

parse_msg_header_key :: Parser (CI String)
parse_msg_header_key = mk <$> many1 (noneOf ":\r\n")

-- returns nothing if there wasn't even a colon :O
parse_msg_header_val :: Parser (Maybe String)
parse_msg_header_val = option Nothing (fmap Just (char ':' >> consume_spaces >> many (noneOf "\r\n")))

parse_msg_body :: Int -> Parser String
parse_msg_body n = count n anyToken
