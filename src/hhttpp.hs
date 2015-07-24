import HHTTPP.Request
import HHTTPP.Response

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec (parse)
import Text.Show.Pretty (ppShow)
import Network (listenOn, accept, PortID(..), Socket)
import Network.URI (nullURI, parseAbsoluteURI, uriPath)
import System.IO (hGetLine, Handle)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Control.Lens ((&))

connection_handler :: Handle -> IO ()
connection_handler handle =
  hGetLine handle >>= (\firstline ->
  parse parse_request_head "" (fromString firstline) & 
    either (undefined) (\request_head -> 
    let unparsed_uri = path request_head in
    let actual_path = uriPath (fromMaybe nullURI (parseAbsoluteURI unparsed_uri)) in
    let new_fl = RequestHead {verb = verb request_head, path = actual_path, query_params = query_params request_head, request_version = request_version request_head} in
    return ()))

listen_loop :: Socket -> IO ()
listen_loop socket =
  accept socket >>= (\(handle, _, _) ->
  forkIO (connection_handler handle) >>
  listen_loop socket)

main :: IO ()
main =
  listenOn (PortNumber 8080) >>= (\socket ->
  listen_loop socket)
{-
main = parse_and_print parse_request "example_http_msg/request1.txt" >>
       parse_and_print parse_responsed "example_http_msg/response1.txt"
-- -}
