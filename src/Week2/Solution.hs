module Week2.Solution where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage msg =
    case words msg of
        ("I":timestamp:logMsg) -> LogMessage Info (read timestamp) (unwords logMsg)
        ("W":timestamp:logMsg) -> LogMessage Warning (read timestamp) (unwords logMsg)
        ("E":errCode:timestamp:logMsg) -> LogMessage (Error (read errCode)) (read timestamp) (unwords logMsg)
        _ -> Unknown msg

mapMessages :: (String -> LogMessage) -> [String] -> [LogMessage]
mapMessages _ [] = []
mapMessages func (x:xs) = func x: mapMessages func xs

parse :: String -> [LogMessage]
parse s = mapMessages parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) msgTree = msgTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node left m'@(LogMessage _ ets _) right)
  | ts <= ets = Node (insert m left) m' right
  | otherwise = Node left m' (insert m right)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map msg $ filter important sorted
  where sorted = inOrder $ build ms
        important (LogMessage (Error sev) _ _) = sev >= 50
        important _ = False
        msg (LogMessage _ _ m) = m
        msg (Unknown _) = ""
