{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module LogAnalysis where
   import Log

   member :: (Eq a) => a -> [a] -> Bool
   member _ [] = False
   member x (y:ys)
      | x==y = True
      | otherwise = member x ys

   extractInt :: String -> (String,String)
   extractInt s =
      case s of
         []     -> ([],s)
         (x:xs) -> if member x ['0'..'9'] then
            let (s1,s2) = extractInt xs in (x:s1, s2)
         else ([],s)

   stringToInt :: String -> Int
   stringToInt = read

   parseMessage :: String -> LogMessage
   parseMessage s =
      case s of
         ('I':' ':xs) ->
            let (s1,s2) = extractInt xs in
            LogMessage Info (stringToInt s1) (tail s2)
         ('W':' ':xs) ->
            let (s1,s2) = extractInt xs in
            LogMessage Warning (stringToInt s1) (tail s2)
         ('E':' ':xs) ->
            let (s1,s2) = extractInt xs in
            let (s3,s4) = extractInt (tail s2) in
            LogMessage (Error (stringToInt s1)) (stringToInt s3) (tail s4)
         _            -> Unknown s
   
   parse :: String -> [LogMessage]
   parse s =
      let l = lines s in
      fmap parseMessage l

   getTimeStamp :: LogMessage -> Int
   getTimeStamp m =
      case m of
         LogMessage _ t _ -> t
         _                -> error "No timestamp"   

   insert :: LogMessage -> MessageTree -> MessageTree
   insert l t =
     case l of
        Unknown _ -> t
        _         -> let time = getTimeStamp l in insertTime l t time
        where
           insertTime currLog tree time =
              case tree of
                 Leaf              -> Node Leaf currLog Leaf
                 Node left m right -> let timeRoot = getTimeStamp m in
                    if timeRoot < time then Node left m (insertTime currLog right time)
                    else Node (insertTime currLog left time) m right
   
   build :: [LogMessage] -> MessageTree
   build list =
      case list of 
         []     -> Leaf
         (x:xs) -> let t = build xs in
            insert x t

   inOrder :: MessageTree -> [LogMessage]
   inOrder t =
      case t of 
         Leaf              -> []
         Node left x right -> inOrder left ++ [x] ++ inOrder right
   
   whatWentWrong :: [LogMessage] -> [String]
   whatWentWrong list =
      let t = build list in
      let sortedList = inOrder t in
      fmap (\x -> case x of 
         LogMessage _ _ l -> l
         _                -> error "impossible case")
         (filter (\x -> case x of
            LogMessage (Error e) _ _ -> e >= 50
            _                        -> False) sortedList)