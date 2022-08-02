module LogAnalysis where

import Log
import Data.List.Split (splitOn)
import Debug.Trace

--parseMessage :: String -> LogMessage
-- parseMessage line = LogMessage

checkMessageType :: String -> Maybe MessageType

checkMessageType ('I':_) = Just Info
checkMessageType ('W':_) = Just Warning
checkMessageType ('E':xs) = let errorArray = drop 1 (splitOn " " xs) -- drop empty string 
  in Just (Error (read (head errorArray)))
checkMessageType _ = Nothing



