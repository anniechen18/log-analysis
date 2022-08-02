module LogAnalysis where

import Log
import Data.List.Split (splitOn)
import Debug.Trace

checkMessageType :: String -> Maybe MessageType
checkMessageType ('I':_) = Just Info
checkMessageType ('W':_) = Just Warning
checkMessageType ('E':xs) = let errorArray = drop 1 (splitOn " " xs) -- drop empty string 
  in Just (Error (read (head errorArray)))
checkMessageType _ = Nothing

getTimeStamp :: String -> TimeStamp
getTimeStamp ('I':xs) = extractTimeStampFromInfoOrWarning xs 
getTimeStamp ('W':xs) = extractTimeStampFromInfoOrWarning xs
getTimeStamp ('E':xs) = extractTimeStampFromError xs 

extractTimeStampFromInfoOrWarning :: String -> TimeStamp
extractTimeStampFromInfoOrWarning line = read (head (drop 1 (splitOn " " line)))

extractTimeStampFromError :: String -> TimeStamp
extractTimeStampFromError line = read (head (drop 2 (splitOn " " line)))

--parseMessage :: String -> LogMessage
--parseMessage line = LogMessage (checkMessageType line) (getTimeStamp line) 



