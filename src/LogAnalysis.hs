module LogAnalysis where

import Log
import Data.List.Split (splitOn)
import Debug.Trace

checkMessageType :: String -> Maybe MessageType
checkMessageType ('I':_) = Just Info
checkMessageType ('W':_) = Just Warning
checkMessageType ('E':xs) = Just $ Error $ read $ head $ words xs
checkMessageType _ = Nothing

getTimeStamp :: String -> Maybe TimeStamp
getTimeStamp ('I':xs) = Just $ extractTimeStampFromInfoOrWarning xs 
getTimeStamp ('W':xs) = Just $ extractTimeStampFromInfoOrWarning xs
getTimeStamp ('E':xs) = Just $extractTimeStampFromError xs 
getTimeStamp _ = Nothing

extractTimeStampFromInfoOrWarning :: String -> TimeStamp
extractTimeStampFromInfoOrWarning line = read (head $ words line)::Int

extractTimeStampFromError :: String -> TimeStamp
extractTimeStampFromError line = read (head $ drop 1 $ words line)::Int

getString :: String -> String
getString line 
  | head line == 'I' || head line == 'W' = unwords $ drop 2 $ words line
  | head line == 'E' = unwords $ drop 3 $ words line
  | otherwise = line
 
-- Parse one log message
parseMessage :: String -> LogMessage
parseMessage line =
  case checkMessageType line of
    Just msgType -> case getTimeStamp line of
                Just ts -> LogMessage msgType ts (getString line)
                Nothing -> Unknown (getString line)
    Nothing -> Unknown (getString line)

-- Parse entire log file
parse :: String -> [LogMessage]
parse fileContent = let logMessages = lines fileContent
  in map (\a -> parseMessage a) logMessages

     
