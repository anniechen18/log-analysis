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
getTimeStamp ('E':xs) = Just $ extractTimeStampFromError xs 
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
  in map parseMessage logMessages

-- Insert messages in order using binary search tree structure
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree 
insert message Leaf = Node Leaf message Leaf
insert (LogMessage insertMsgType insertTs insertS) (Node leftTree (LogMessage msgType ts s) rightTree) = 
  if (insertTs < ts)
  then Node (insert (LogMessage insertMsgType insertTs insertS) leftTree) (LogMessage msgType ts s) rightTree
  else Node leftTree (LogMessage msgType ts s) (insert (LogMessage insertMsgType insertTs insertS) rightTree)

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) = (inOrder leftTree) ++ [message] ++ (inOrder rightTree) 


 
