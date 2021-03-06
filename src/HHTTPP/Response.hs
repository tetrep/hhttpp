module HHTTPP.Response where

import HHTTPP.Common
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Text.Parsec.ByteString (Parser)
import Text.Parsec

data ResponseHead = ResponseHead {
  http_version :: ByteString,
  status_code :: ByteString,
  status_msg :: ByteString
}

data Response = Response {
  prehead :: ResponseHead,
  headers :: [Header],
  body :: ByteString
}

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

parse_status_code :: Parser ByteString
parse_status_code = fmap pack (many (noneOf " \r\n"))

parse_status_msg :: Parser ByteString
parse_status_msg = fmap pack (many (noneOf "\r\n"))
