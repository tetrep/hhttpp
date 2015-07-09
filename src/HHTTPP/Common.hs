{-# LANGUAGE OverloadedStrings #-}
module HHTTPP.Common where

import Control.Monad (join)
import Data.CaseInsensitive (CI, mk)
import Data.Maybe (fromMaybe)
import HHTTPP.Util (maybe_read)
import Text.Parsec.ByteString (Parser)
import Text.Parsec

type Header = (CI String, Maybe String)

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

get_content_length :: [(CI String, Maybe String)] -> Maybe Int
get_content_length headers = maybe_read =<< join (lookup "Content-Length" headers)

parse_headers_and_body:: Parser ([Header], String)
parse_headers_and_body =
  many parse_msg_header >>= (\headers ->
  consume_eol >>
  option "" (parse_msg_body (fromMaybe 0 (get_content_length headers))) >>= (\body ->
  return (headers, body)))
