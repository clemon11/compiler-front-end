module StringProcessing where

---- convenience methods-----
trimNl :: String -> String
trimNl s = (reverse . dropWhile (=='\n') . reverse) ((reverse . dropWhile (==' ') . reverse) s)


trimWs :: String -> String
trimWs = dropWhile (==' ')


trim :: String -> String
trim s = trimNl (trimWs s)

