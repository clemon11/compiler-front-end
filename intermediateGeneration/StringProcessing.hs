{-|
  Convenience methods for processing "String"s.
-}

module StringProcessing where


{-|
  Take a "String" and return a "String" with trailing whitepace 
  and newline removed.
-}
trimNl :: String -> String
trimNl s = (reverse . dropWhile (=='\n') . reverse) ((reverse . dropWhile (==' ') . reverse) s)


{-|
  Take a "String" and return a "String" with leading whitepace
  removed.
-}
trimWs :: String -> String
trimWs = dropWhile (==' ')


{-|
  Take a "String" and return a "String" with leading whitepace,
  and trailing whitespace and newline removed.
-}
trim :: String -> String
trim s = trimNl (trimWs s)

