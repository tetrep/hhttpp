module HHTTPP.Response where

import HHTTPP.Common
import Text.ParserCombinators.Parsec

data ResponseHead = ResponseHead {
  http_version :: String,
  status_code :: String,
  status_msg :: String
} deriving Show

data Response = Response {
  prehead :: ResponseHead,
  headers :: [Header],
  body :: String
} deriving Show

parse_response :: Parser Response
parse_response =
  parse_response_head >>= (\head' ->
  consume_eol >>
  parse_headers_and_body >>= (\(headers', body') ->
  return Response { prehead = head', headers = headers', body = body' } ))

parse_response_head :: Parser ResponseHead
parse_response_head =
  parse_msg_version >>= (\version' ->
  consume_spaces >>
  parse_status_code >>= (\status_code' ->
  consume_spaces >>
  parse_status_msg >>= (\status_msg' ->
  return ResponseHead { http_version = version', status_code = status_code', status_msg = status_msg' } )))

parse_status_code :: Parser String
parse_status_code = many (noneOf " \r\n")

parse_status_msg :: Parser String
parse_status_msg = many (noneOf "\r\n")
