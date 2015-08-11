module HHTTPP.Http_from_socket_bytestring where

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Data.ByteString (ByteString, snoc, head, tail, length)
import Data.ByteString.Char8 (pack)
import Data.Word (Word8)
import Data.Char (ord)
import Control.Lens (preview, ix)

import Prelude hiding (head, tail, length)

ctw8 :: Char -> Word8
ctw8 letter =
  fromInteger (toInteger (ord letter))

read_http_line :: Socket -> IO (ByteString, ByteString)
read_http_line in_socket =
  read_http_line_max_bytes in_socket 4096

read_http_line_max_bytes :: Socket -> Int -> IO (ByteString, ByteString)
read_http_line_max_bytes in_socket max_read_bytes = 
  recv in_socket max_read_bytes >>= (\in_data ->
  return (read_http_line_internal (pack "") in_data))

read_http_line_internal :: ByteString -> ByteString -> (ByteString, ByteString)
read_http_line_internal car cdr =
  if (length cdr) == 0 then
    (car, cdr)
  else if (head cdr) == (ctw8 '\r') || (head cdr) == (ctw8 '\n') then
    (car, cdr)
  else
    read_http_line_internal (snoc car (head cdr)) (tail cdr)

parse_http_head :: ByteString -> (ByteString, ByteString)
parse_http_head input =
  split ' ' input >>= (\spaces ->
  let verb = head spaces in
  split '?' (preview (ix 1) spaces) >>= (\path_params ->
  let path = head path_params in
  
