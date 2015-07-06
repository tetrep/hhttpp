module HHTTPP.Request where

import HHTTPP.Common
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

data RequestHead = RequestHead {
  verb :: String,
  path :: String,
  query_params :: [(String, Maybe String)],
  request_version :: String
} deriving Show

data Request = Request {
  prehead :: RequestHead,
  headers :: [Header],
  body :: String
} deriving Show

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

parse_request_verb :: Parser String
parse_request_verb = many (noneOf " ")

parse_request_path :: Parser String
parse_request_path = many (noneOf " ?")

parse_request_query_params :: Parser [(String, Maybe String)]
parse_request_query_params = char '?' >> parse_request_parameters

parse_request_parameters :: Parser [(String, Maybe String)]
parse_request_parameters = parse_one_pair >>= (\first -> fmap (first:) ((many1 (oneOf "&;") >> parse_request_parameters) <|> return []))
  where
    parse_one_pair :: Parser (String, Maybe String)
    parse_one_pair = many (noneOf " =&;") >>= (\key -> option (key , Nothing) (char '=' >> many (noneOf " &;") >>= (\val -> return (key, Just val))))
