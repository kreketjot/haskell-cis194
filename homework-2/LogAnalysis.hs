{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- exercise 1
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I" : ts : rest) -> LogMessage Info (read ts) (unwords rest)
  ("W" : ts : rest) -> LogMessage Warning (read ts) (unwords rest)
  ("E" : svrt : ts : rest) -> LogMessage (Error (read svrt)) (read ts) (unwords rest)
  wordsList -> Unknown (unwords wordsList)

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x : xs) = parseMessage x : (parseLines xs)

parse :: String -> [LogMessage]
parse = parseLines . lines

-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage (Node left (Unknown _) right) = Node left logMessage right -- for type system (MessageTree doesn't contain Unknown messages)
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage msgType msgTs msg) (Node left (LogMessage nodeType nodeTs node) right)
  | msgTs > nodeTs = Node left (LogMessage nodeType nodeTs node) (insert (LogMessage msgType msgTs msg) right)
  | otherwise = Node (insert (LogMessage msgType msgTs msg) left) (LogMessage nodeType nodeTs node) right

-- exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ logMessage : inOrder right

-- exercise 5
filterDangeourMessages :: [LogMessage] -> [LogMessage]
filterDangeourMessages [] = []
filterDangeourMessages ((LogMessage (Error svrt) ts msg) : xs)
  | svrt >= 50 = LogMessage (Error svrt) ts msg : filterDangeourMessages xs
filterDangeourMessages (_ : xs) = filterDangeourMessages xs

getCause :: LogMessage -> String
getCause (LogMessage _ _ cause) = cause
getCause (Unknown msg) = msg

getCauses :: [LogMessage] -> [String]
getCauses [] = []
getCauses (x : xs) = getCause x : getCauses xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getCauses . inOrder . build . filterDangeourMessages
