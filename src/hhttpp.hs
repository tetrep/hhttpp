import HHTTPP.Request
import HHTTPP.Response

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Show.Pretty (ppShow)
import Network (listenOn, accept, PortID(..), Socket)
import Network.URI
import System.IO (hGetLine, Handle)
import Control.Concurrent (forkIO)

parse_and_print :: (Show a) => Parser a -> FilePath -> IO ()
parse_and_print p filepath =
  putStrLn . ppShow =<< parseFromFile p filepath

connection_handler :: Handle -> IO ()
connection_handler handle =
  hGetLine handle >>= (\firstline ->
  maybe nullURI (parseAbsoluteURI firstline) >>= (\target_uri ->
  return target_uri))

listen_loop :: Socket -> IO ()
listen_loop socket =
  accept socket >>= (\(handle, _, _) ->
  forkIO (connection_handler handle) >>
  return listen_loop socket)

main :: IO ()
main =
  listenOn (PortNumber 8080) >>= (\socket ->
  return listen_loop socket)
{-
main = parse_and_print parse_request "example_http_msg/request1.txt" >>
       parse_and_print parse_responsed "example_http_msg/response1.txt"
-- -}
