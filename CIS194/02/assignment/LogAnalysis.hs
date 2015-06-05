{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


--Problem 1
--Takes a string and returns the corresponding LogMessage
parseMessage :: String -> LogMessage
parseMessage input = case wordList of 
    ("I":time:msg) -> LogMessage Info (read time) (unwords msg)  
    ("W":time:msg) -> LogMessage Warning (read time) (unwords msg)
    ("E":lvl:time:msg) -> LogMessage (Error (read lvl)) (read time) (unwords msg)
    _              -> Unknown (unwords wordList)
    where wordList = words input
--Parse everything to an array of LogMessages
parse :: String -> [LogMessage]
parse = map parseMessage . lines -- the dot takes the next two things as args

--Problem 2
--Make a function that inserts a node into a sorted tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (msg@(LogMessage _ _ _)) Leaf = Node Leaf msg Leaf -- insert any logmessage into an empty tree this way
insert (msg@(LogMessage _ time _)) (Node left (root@(LogMessage _ rootTime _)) right)
    | time > rootTime = Node left root (insert msg right)
    | otherwise       = Node (insert msg left) root right
insert _ msgTree = msgTree -- includes unknown

--Problem 3
--Make a function that inserts everything
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs) 

--Problem 4
--Make a function that takes a sorted tree and returns a sorted array with inorder traversal
inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left msg right) = (inorder left) ++ [msg] ++ (inorder right)

--Problem 5
--Take an unordered list and return a sorted list of errors with magnitude greater than 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong tree = map (messages) (filter (relevant) (inorder (build tree)))
    where
        relevant :: LogMessage -> Bool
        relevant (LogMessage (Error level) _ _) 
            | level > 50 = True
            | level <=50 = False
        relevant _ = False
        messages :: LogMessage -> String
        messages (LogMessage _ _ msg) = msg
        messages _ = "Something went wrong"
