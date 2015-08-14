{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module HHTTPP.Request where

import HHTTPP.Common
import Text.Parsec.ByteString (Parser)
import Text.Parsec
import Data.ByteString (ByteString, append, concat, intercalate)
import Data.ByteString.Char8 (pack, cons)

import Prelude hiding (concat)

data RequestHead = RequestHead {
  verb :: ByteString,
  path :: ByteString,
  query_params :: [(ByteString, Maybe ByteString)],
  request_version :: ByteString
}

data Request = Request {
  prehead :: RequestHead,
  headers :: [Header],
  body :: ByteString
}

parse_request :: Parser Request
parse_request =
  parse_request_head >>= (\head' ->
  consume_eol >>
  parse_headers_and_body >>= (\(headers', body') ->
  return Request { prehead = head', headers = headers', body = body' } ))

parse_request_head :: Parser RequestHead
parse_request_head =
  parse_request_verb >>= (\verb' ->
  consume_spaces >>
  parse_request_path >>= (\path' ->
  consume_spaces >>
  option [] parse_request_query_params >>= (\qp ->
  consume_spaces >>
  parse_msg_version >>= (\version ->
  return RequestHead { verb = verb', path = path', query_params = qp, request_version = version } ))))

parse_request_verb :: Parser ByteString
parse_request_verb = fmap pack (many (noneOf " "))

parse_request_path :: Parser ByteString
parse_request_path = fmap pack (many (noneOf " ?"))

parse_request_query_params :: Parser [(ByteString, Maybe ByteString)]
parse_request_query_params = char '?' >> parse_request_parameters

parse_request_parameters :: Parser [(ByteString, Maybe ByteString)]
parse_request_parameters = parse_one_pair >>= (\first -> fmap (first:) ((many1 (oneOf "&;") >> parse_request_parameters) <|> return []))
  where
    parse_one_pair :: Parser (ByteString, Maybe ByteString)
    parse_one_pair = fmap pack (many (noneOf " =&;")) >>= (\key -> option (key, Nothing) (char '=' >> fmap pack (many (noneOf " &;")) >>= (\val -> return (key, Just val))))

query_param_string :: [(ByteString, Maybe ByteString)] -> ByteString
query_param_string = intercalate "&" . map (\(key, maybe_value) -> append key (maybe "" (cons '=') maybe_value))

instance Print_http RequestHead where
  print_http RequestHead {..} = append verb (append (pack " ") (append path (append (query_param_string query_params) (append (pack " ") request_version))))

instance Print_http Request where
  print_http Request {..} = append (print_http prehead) (append (concat (map print_http headers)) (append (pack "\n") body))
