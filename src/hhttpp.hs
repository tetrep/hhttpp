import Text.ParserCombinators.Parsec

-- TODO properly support \r\n
http_msg :: GenParser Char st [[String]]
http_msg =
  do result <- http_first_line
     many http_msg_header
     http_msg_body
     end_of_http_msg
     return result

http_first_line :: GenParser Char st [[String]]
http_first_line = http_first_line_response <|> http_first_line_request

-- somehow say it can only be one line
http_first_line_request :: GenParser Char st [[String]]
http_first_line_request =
  do result <- http_request_verb
    http_request_path
    http_request_version

http_first_line_response :: GenParser Char st [[String]]
http_first_line_response =
  do result <- http_msg_version
    http_response_status_code
    http_response_status_msg

http_request_verb :: GenParser Char st [String]
http_request_verb = many (noneOf " ")

http_request_path :: GenParser Char st [String]
http_request_path = many (noneOf " ?")

http_msg_version :: GenParser Char st [String]
http_msg_version = many (noneOf " \n")

http_response_status_code :: GenParser Char st [String]
http_response_status_code = many (noneOf " ")

http_response_status_msg :: GenParser Char st [String]
http_response_status_msg = many (noneOf "\n")

http_msg_header :: GenParser Char st [String]
http_msg_header =
  do key <- http_header_key
     val <- http_header_val
     return (key : val)

http_msg_header_key :: GenParser Char st [String]
http_msg_header_key = many (noneOf ":\n")

http_msg_header_val :: GenParser Char st [String]
http_msg_header_val = many (noneOf "\n")

http_msg_body :: GenParser Char st [String]
http_msg_body = many (noneOf "foo")

eom :: GenParser Char st [String]
eom = char '\n'

main = print (http_msg "GET / HTTP/1.1\nHost: foo.bar.com\nContent-Type: delicious\nContent-Length: 5\n\n")

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
