{-# LANGUAGE OverloadedStrings #-}
module HHTTPP.Response where

import           Control.Monad (join)
import           Data.CaseInsensitive (CI)
import           Data.Maybe (fromMaybe, listToMaybe)
import           HHTTPP.Common

import Text.ParserCombinators.Parsec

data ResponseHead = ResponseHead {
  http_version :: String,
  status_code :: String,
  status_msg :: String
} deriving Show

data Response = Response {
  prehead :: ResponseHead,
  headers :: [(CI String, Maybe String)],
  body :: String
} deriving Show

maybe_read :: (Read a) => String -> Maybe a
maybe_read = fmap fst . listToMaybe . reads

get_content_length :: [(CI String, Maybe String)] -> Maybe Int
get_content_length headers' = maybe_read =<< join (lookup "Content-Length" headers')

parse_response :: Parser Response
parse_response =
  parse_response_head >>= (\head' ->
  consume_eol >>
  many parse_msg_header >>= (\headers' ->
  consume_eol >>
  option "" (parse_msg_body (fromMaybe 0 (get_content_length headers'))) >>= (\body' ->
  return Response { prehead = head', headers = headers', body = body' } )))

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
