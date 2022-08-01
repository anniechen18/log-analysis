module LogAnalysis where

import Log

--parseMessage :: String -> LogMessage

-- parseMessage line = LogMessage

checkMessageType :: String -> MessageType

checkMessageType line = identifyMessageType (take 1 line)
  

identifyMessageType :: String -> MessageType
identifyMessageType "I" = Info
identifyMessageType "W" = Warning
identifyMessageType "E" = Error 0 -- error int is 0 for now


