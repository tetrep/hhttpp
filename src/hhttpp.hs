import Text.ParserCombinators.Parsec

data HTTPRequest = HTTPRequest { _verb :: String, _path :: String, request_version :: String, request_body :: HTTPBody } deriving Show

data HTTPResponse = HTTPResponse { _response_version :: String, _status_code :: Int,  _status_msg :: String, response_body :: HTTPBody } deriving Show

data HTTPBody = HTTPBody { } deriving Show

-- TODO properly support \r\n
http_request :: GenParser Char st HTTPRequest
http_request =
     http_request_verb >>= (\verb ->
     http_request_path >>= (\path ->
     -- http_request_parameters
     http_msg_version >>= (\version ->
     many http_msg_header >>
     http_msg_body >>
     end_of_http_msg >>
     return HTTPRequest { _verb = verb, _path = path, request_version = version, request_body = undefined })))

end_of_http_msg = undefined
http_request_parameters = undefined

http_first_line_response :: GenParser Char st [[String]]
http_first_line_response =
  do http_msg_version
     http_response_status_code
     http_response_status_msg
     return []

http_request_verb :: GenParser Char st String
http_request_verb = many (noneOf " ")

http_request_path :: GenParser Char st String
http_request_path = many (noneOf " ?")

http_msg_version :: GenParser Char st String
http_msg_version = many (noneOf " \n")

http_response_status_code :: GenParser Char st Int
http_response_status_code = many (noneOf " ") >> return 0

http_response_status_msg :: GenParser Char st [String]
http_response_status_msg = many (noneOf "\n") >> return []

http_msg_header :: GenParser Char st [String]
http_msg_header =
  do key <- http_msg_header_key
     val <- http_msg_header_val
     return [] -- (key : val)

http_msg_header_key :: GenParser Char st [String]
http_msg_header_key = many (noneOf ":\n") >> return []

http_msg_header_val :: GenParser Char st [String]
http_msg_header_val = many (noneOf "\n") >> return []

http_msg_body :: GenParser Char st [String]
http_msg_body = many (noneOf "foo") >> return []

eom :: GenParser Char st [String]
eom = char '\n' >> return []

main = print (parse http_request "sourcename" "GET / HTTP/1.1\nHost: foo.bar.com\nContent-Type: delicious\nContent-Length: 5\n\n")

{-
{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
---}
