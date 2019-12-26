module Util.String where

-- Escapes printable characters from a string.
escape :: String -> String
escape [] = []
escape ('\t':s) = '\\':'0':'9':escape s
escape ('\n':s) = '\\':'0':'a':escape s
escape ('\"':s) = '\\':'2':'2':escape s
escape ('\\':s) = '\\':'5':'c':escape s
escape (a:s) = a:escape s

-- Removes boundary double quotes from the string.
unquote :: String -> String
unquote (h:t) =
  if (h /= '\"') || (last t /= '\"') then
    error "missing quotes in a string value"
  else
    init t
unquote _ = error "missing quotes in a string value"
