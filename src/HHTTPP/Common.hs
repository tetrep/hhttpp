{-# LANGUAGE OverloadedStrings #-}
module HHTTPP.Common where

import Data.CaseInsensitive (CI, mk, original)
import Data.Maybe (fromMaybe)
import HHTTPP.Util (maybe_read)
import Text.Parsec.ByteString (Parser)
import Text.Parsec
import Data.ByteString.Char8 (ByteString, append, pack, unpack)

data Header = Header (CI ByteString, ByteString)

consume_spaces :: Parser ByteString
consume_spaces = fmap pack (many (char ' '))

consume_eol :: Parser ByteString
consume_eol = fmap pack (string "\r\n" <|> string "\n")

parse_msg_version :: Parser ByteString
parse_msg_version = fmap pack (many (noneOf " \r\n"))

parse_msg_header :: Parser Header
parse_msg_header =
  parse_msg_header_key >>= (\key ->
  parse_msg_header_val >>= (\val ->
  consume_eol >>
  return (Header (key, val))))

parse_msg_header_key :: Parser (CI ByteString)
parse_msg_header_key = fmap (mk . pack) (many (noneOf ":\r\n"))

parse_msg_header_val :: Parser ByteString
parse_msg_header_val = fmap pack (char ':' >> consume_spaces >> many (noneOf "\r\n"))

parse_msg_body :: Int -> Parser ByteString
parse_msg_body n = fmap pack (count n anyToken)

lookupHeader :: ByteString -> [Header] -> Maybe ByteString
lookupHeader key = lookup (mk key) . map (\(Header x) -> x)

get_content_length :: [Header] -> Maybe Int
get_content_length headers = maybe_read =<< fmap unpack (lookupHeader "Content-Length" headers)

parse_headers_and_body:: Parser ([Header], ByteString)
parse_headers_and_body =
  many parse_msg_header >>= (\headers ->
  consume_eol >>
  option "" (parse_msg_body (fromMaybe 0 (get_content_length headers))) >>= (\body ->
  return (headers, body)))

instance Print_http Header where
  print_http (Header (key, value)) = append (original key) (append value (pack "\n"))

class Print_http a where
  print_http :: a -> ByteString
