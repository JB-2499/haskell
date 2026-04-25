merr :: String
merr = "Error! Type not supported by this function."

tamanho :: [a] -> Int
tamanho [] = 0
tamanho xs = sum [1 | _ <- xs] 

suc :: Num a => a -> a
suc x = x + 1

seqnum :: Show a => [a] -> IO ()
seqnum [] = return ()
seqnum (x:xs) = do
    putStrLn (show x ++ ",")
    seqnum xs

dlower :: String -> String
dlower x = [c | c <- x, elem c ['A' .. 'Z']]

dupper :: String -> String
dupper x = [c | c <- x, elem c ['a' .. 'z']]
