import HHTTPP.Request
import HHTTPP.Response
import HHTTPP.Common (Parser)

import qualified Data.ByteString as BS
import Text.ParserCombinators.Parsec (parse)
import Text.Show.Pretty (ppShow)

parse_and_print :: (Show a) => Parser a -> FilePath -> IO ()
parse_and_print p filepath =
  putStrLn . ppShow =<< parse p filepath <$>
    BS.readFile filepath

main :: IO ()
main = parse_and_print parse_request  "example_http_msg/request1.txt" >>
       parse_and_print parse_response "example_http_msg/response1.txt"
