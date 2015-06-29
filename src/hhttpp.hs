import Text.ParserCombinators.Parsec
import Text.Show.Pretty
import Control.Applicative ((<$>))

data HTTPRequestHead = HTTPRequestHead {
  verb :: String,
  path :: String,
  query_params :: [(String, Maybe String)],
  request_version :: String
} deriving Show

data HTTPRequest = HTTPRequest {
  head_head :: HTTPRequestHead,
  headers :: [(String, Maybe String)],
  body :: String
} deriving Show

data HTTPResponseHead = HTTPResponseHead {
  http_version :: String,
  status_code :: String,
  status_msg :: String
} deriving Show

data HTTPResponse = HTTPResponse {
  head_head_head :: HTTPResponseHead,
  headers_headers :: [(String, Maybe String)],
  body_body :: String
} deriving Show

data HTTPBody = HTTPBody { } deriving Show

consume_spaces :: Parser String
consume_spaces = many (char ' ')

consume_eol :: Parser String
consume_eol = string "\r\n" <|> string "\n"

http_request :: Parser HTTPRequest
http_request =
  http_request_head >>= (\head' ->
  consume_eol >>
  many http_msg_header >>= (\headers' ->
  consume_eol >>
  option "" http_msg_body >>= (\body' ->
  return HTTPRequest { head_head = head', headers = headers', body = body' } )))

http_request_head :: Parser HTTPRequestHead
http_request_head =
  http_request_verb >>= (\verb' ->
  consume_spaces >>
  http_request_path >>= (\path' ->
  consume_spaces >>
  option [] http_request_query_params >>= (\qp ->
  consume_spaces >>
  http_msg_version >>= (\version ->
  return HTTPRequestHead { verb = verb', path = path', query_params = qp, request_version = version } ))))

http_response :: Parser HTTPResponse
http_response =
  http_response_head >>= (\head' ->
  consume_eol >>
  many http_msg_header >>= (\headers' ->
  consume_eol >>
  option "" http_msg_body >>= (\body' ->
  return HTTPResponse { head_head_head = head', headers_headers = headers', body_body = body' } )))

http_response_head :: Parser HTTPResponseHead
http_response_head =
  http_msg_version >>= (\version' ->
  consume_spaces >>
  http_status_code >>= (\status_code' ->
  consume_spaces >>
  http_status_msg >>= (\status_msg' ->
  return HTTPResponseHead { http_version = version', status_code = status_code', status_msg = status_msg' } )))

http_request_verb :: Parser String
http_request_verb = many (noneOf " ")

http_request_path :: Parser String
http_request_path = many (noneOf " ?")

http_request_query_params :: Parser [(String, Maybe String)]
http_request_query_params = char '?' >> http_request_parameters

http_request_parameters :: Parser [(String, Maybe String)]
http_request_parameters = parse_one_pair >>= (\first -> fmap (first:) ((many1 (oneOf "&;") >> http_request_parameters) <|> return []))
  where
    parse_one_pair :: Parser (String, Maybe String)
    parse_one_pair = many (noneOf " =&;") >>= (\key -> option (key , Nothing) (char '=' >> many (noneOf " &;") >>= (\val -> return (key, Just val))))

http_msg_version :: Parser String
http_msg_version = many (noneOf " \r\n")

http_status_code :: Parser String
http_status_code = many (noneOf " \r\n")

http_status_msg :: Parser String
http_status_msg = many (noneOf "\r\n")

http_msg_header :: Parser (String, Maybe String)
http_msg_header =
  http_msg_header_key >>= (\key ->
  http_msg_header_val >>= (\val ->
  consume_eol >>
  return (key, val)))

http_msg_header_key :: Parser String
http_msg_header_key = many1 (noneOf ":\r\n")

http_msg_header_val :: Parser (Maybe String)
http_msg_header_val = option Nothing (fmap Just (char ':' >> consume_spaces >> many (noneOf "\r\n")))

http_msg_body :: Parser String
http_msg_body = many (noneOf "foo") >> return []

main :: IO ()
--main = print (parse http_request "sourcename" "GET /foo?a=b&c=d;e=f&;x;;y HTTP/1.1\nHost: foo.bar.com\nContent-Type: delicious\nContent-Length: 5\n\n")
main = putStrLn . ppShow =<< parse http_response "sourcename" <$> (readFile "example_http_msg/response1.txt")
