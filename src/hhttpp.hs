import HHTTPP.Request
import HHTTPP.Response
import HHTTPP.Common
import HHTTPP.Http_from_socket_bytestring

import Text.Parsec (parse)
import Network (listenOn, PortID(..), PortNumber, Socket)
import Network.Socket (accept, connect, SockAddr(..), socket, Family(..), SocketType(..), defaultProtocol, inet_addr)
import Network.Socket.ByteString (send, recv)
import Network.URI (nullURI, parseAbsoluteURI, uriPath, uriAuthority, URIAuth(..))
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Control.Lens ((&))

connection_loop :: Socket -> Socket -> IO ()
connection_loop src_socket dst_socket =
  recv src_socket 1024 >>= (\recv_data ->
  -- TODO make sure all data is sent
  send dst_socket recv_data >>
  connection_loop src_socket dst_socket)

connection_with_head :: String -> PortNumber -> RequestHead -> ByteString -> Socket -> IO ()
connection_with_head host_str port new_head cdr_chunk in_socket =
  --connectTo host (PortNumber port) >>= (\handle_out ->
  socket AF_INET Stream defaultProtocol >>= (\out_socket ->
  inet_addr host_str >>= (\host_addr ->
  connect out_socket (SockAddrInet port host_addr) >>
  send out_socket (print_http new_head) >>
  send out_socket cdr_chunk >>
  connection_loop in_socket out_socket >>
  connection_loop out_socket in_socket >>
  return ()))

connection_handler :: Socket -> IO ()
connection_handler in_socket =
  read_http_line in_socket >>= (\(first_line, cdr_chunk) ->
  parse parse_request_head "" first_line & 
    either (undefined) (\request_head -> 
    let unparsed_uri = path request_head in
    let parsed_uri = fromMaybe nullURI (parseAbsoluteURI (unpack unparsed_uri)) in
    let actual_path = pack (uriPath parsed_uri) in
    let new_head = RequestHead {verb = verb request_head, path = actual_path, query_params = query_params request_head, request_version = request_version request_head} in
    let maybe_uri_auth = uriAuthority parsed_uri in
    let uri_auth = (fromMaybe URIAuth {uriUserInfo = "", uriRegName = "", uriPort = ""} maybe_uri_auth) in
    let host = uriRegName uri_auth in
    let port = fromIntegral (read (uriPort uri_auth) :: Integer) in
    (connection_with_head host port new_head cdr_chunk in_socket) >>
    return ()))

accept_loop :: Socket -> IO ()
accept_loop listen_socket =
  accept listen_socket >>= (\(in_socket, _) ->
  forkIO (connection_handler in_socket) >>
  accept_loop listen_socket)

main :: IO ()
main =
  listenOn (PortNumber 8080) >>= (\socket ->
  accept_loop socket)
{-
main = parse_and_print parse_request "example_http_msg/request1.txt" >>
       parse_and_print parse_responsed "example_http_msg/response1.txt"
-- -}
