import HHTTPP.Request
import HHTTPP.Response

import Text.ParserCombinators.Parsec (parse)
import Text.Show.Pretty (ppShow)

main :: IO ()
--main = print (parse http_request "sourcename" "GET /foo?a=b&c=d;e=f&;x;;y HTTP/1.1\nHost: foo.bar.com\nContent-Type: delicious\nContent-Length: 5\n\n")
main = putStrLn . ppShow =<< parse HHTTPP.Response.parse_response "sourcename" <$> (readFile "example_http_msg/response2.txt")

