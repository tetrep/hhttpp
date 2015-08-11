import HHTTPP.Request
import HHTTPP.Response
import HHTTPP.Http_from_socket_bytestring

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec (parse)
import Text.Show.Pretty (ppShow)
import Network (listenOn, PortID(..), PortNumber, Socket, connectTo, HostName)
import Network.Socket (accept, connect, SockAddr(..), send, recv, socket, Family(..), SocketType(..), defaultProtocol, inet_addr)
import Network.URI (nullURI, parseAbsoluteURI, uriPath, uriAuthority, URIAuth(..))
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Control.Lens ((&))

connection_loop :: Socket -> Socket -> IO ()
connection_loop src_socket dst_socket =
  recv src_socket 1024 >>= (\recv_data ->
  -- TODO make sure all data is sent
  send dst_socket recv_data >>
  connection_loop src_socket dst_socket)

connection_with_head :: String -> PortNumber -> RequestHead -> Socket -> IO ()
connection_with_head host port new_head in_socket =
  --connectTo host (PortNumber port) >>= (\handle_out ->
  socket AF_INET Stream defaultProtocol >>= (\out_socket ->
  connect out_socket (SockAddrInet port (inet_addr host)) >>
  send out_socket (show new_head) (length (show new_head)) >>
  connection_loop in_socket out_socket >>
  connection_loop out_socket in_socket >>
  return ())

connection_handler :: Socket -> IO ()
connection_handler in_socket =
  read_http_line in_socket >>= (\(first_line, cdr_chunk) ->
  parse parse_request_head "" (fromString first_line) & 
    either (undefined) (\request_head -> 
    let unparsed_uri = path request_head in
    let parsed_uri = fromMaybe nullURI (parseAbsoluteURI unparsed_uri) in
    let actual_path = uriPath parsed_uri in
    let new_head = RequestHead {verb = verb request_head, path = actual_path, query_params = query_params request_head, request_version = request_version request_head} in
    let maybe_uri_auth = uriAuthority parsed_uri in
    let uri_auth = (fromMaybe URIAuth {uriUserInfo = "", uriRegName = "", uriPort = ""} maybe_uri_auth) in
    let host = uriRegName uri_auth in
    let port = fromIntegral (read (uriPort uri_auth)) in
    (connection_with_head host port new_head in_socket) >>
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
