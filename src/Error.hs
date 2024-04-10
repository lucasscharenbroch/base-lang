module Error where

data Error = Error { message :: String
                   , lineNum :: Int
                   , charNum :: Int
                   }
