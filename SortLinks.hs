module SortLinks where

import Formator (format,printf,printfLn,splitBy,catLines)
import System.IO (hFlush, stdout)

-- https://stackoverflow.com/questions/13190314/io-happens-out-of-order-when-using-getline-and-putstr

data ParseState a = Stop a | Quit a deriving (Show) -- how to rebuild by this ?

input s = do putStr s >> hFlush stdout >> getLine 

startInput str f = do
    s <- input str
    case s of
      ":q" -> return ":q"
      ":p" -> return ":p"
      ":s" -> return ":s"
      _    -> f s

linksF s = return $ format "- <$S>" [s]
catLinesF s = return $ catLines s
pairF s = do
    l <- input "Link : "
    return $ format "- $S: <$S>" [s,l]
itemF s = do
    return $ format "- $S" [s]
matchF s = do
    l <- input "Info : "
    return $ format "- $S: $S" [s,l]
formatF s = do
    l <- input "Word List: "
    return $ format s $ splitBy l " "

pairInfo = startInput "Title: " pairF
itemInfo = startInput "Content: " itemF
matchInfo = startInput "Title: " matchF
makelinks = startInput "Link: " linksF
formatStr = startInput "Format String ($S): " formatF
catStr = startInput "Concat the Lines ->\n" catLinesF

selectMode = do 
    m <- input "Mode links/pair/match/item/format/cat/:q/:p/:s/:f\n-> "
    case m of 
        "item" -> return itemInfo
        "pair" -> return pairInfo
        "links" -> return makelinks
        "format" -> return formatStr
        "cat" -> return catStr
        ":q" -> return $ return ":q"
        ":p" -> return $ return ":p"
        ":s" -> return $ return ":s"
        _ -> putStrLn "Wrong choosen" >> selectMode

stopWhen :: IO String -> [[Char]] -> IO (ParseState [[Char]])
stopWhen f strs = do
    s <- f
    case s of 
        ":q" -> return (Quit strs)
        ":s" -> putStrLn "\nSelect New Mode:" >> return (Stop strs)
        ":p" -> putStrLn "\nBuffer Informantion:" >> printStrs (reverse strs) >> stopWhen f strs
        ":f" -> putStrLn "Force Print Out:" >> printStrs (reverse strs) >> stopWhen f []
        _ -> stopWhen f (s:strs)

printStrs [] = putStrLn ""
printStrs (s:ss) = do
    putStrLn s
    printStrs ss
    hFlush stdout

loopProcess status = do
    f <- selectMode
    strs' <- stopWhen f status
    case strs' of
      (Quit []) -> putStrLn "Bye."
      (Quit strs) -> putStrLn "\nFormated String List:" >> printStrs (reverse strs) >> putStrLn "Bye."
      (Stop strs) -> loopProcess strs

main = loopProcess []
