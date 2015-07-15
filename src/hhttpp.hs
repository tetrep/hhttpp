import HHTTPP.Request
import HHTTPP.Response

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Show.Pretty (ppShow)

parse_and_print :: (Show a) => Parser a -> FilePath -> IO ()
parse_and_print p filepath =
  putStrLn . ppShow =<< parseFromFile p filepath

main :: IO ()
main = parse_and_print parse_request_head  "example_http_msg/request1.txt" >>
       parse_and_print parse_response_head "example_http_msg/response1.txt"
