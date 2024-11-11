-- https://stackoverflow.com/questions/61280735/how-to-implement-split-function-haskell
module Formator (
      format
    , formatBy
    , concatWith
    , splitBy
    , printf
    , printfLn
    , nextLine
    , headIs
    , catLines
    , (==?)
    , (%<)
    , (%<<)
) where

keySym = "$S"

catLines "" = ""
catLines (x:xs) = 
    case x of
      '-'  -> case xs of
                ('\n':ys) -> catLines ys
                (' ':ys) -> catLines ys
                ('\r':'\n':ys) -> catLines ys
                _ -> x:catLines xs 
      '\n' -> catLines xs
      _    -> x:catLines xs

join :: String -> [String] -> String
join s [] = ""
join s [x] = x
join s (x:xs) = x ++ s ++ join s xs

splitBy :: String -> String -> [String]
splitBy ""  sym = []
splitBy str sym = splitBy' str "" sym

splitBy' :: String -> String -> String -> [String]
splitBy' "" s  _  = [s]
splitBy' str@(x:xs) curr sym
  | str ==? sym = curr:splitBy' (drop (length sym) str) "" sym
  | otherwise   = splitBy' xs (curr++[x]) sym

headIs "" "" = True
headIs _  "" = True
headIs "" _  = False
headIs (x:xs) (y:ys) 
  | x == y    = headIs xs ys
  | otherwise = False

(==?) = headIs

concatWith :: [String] -> [String] -> String
concatWith []  _  = []
concatWith [s] _  = s
concatWith (x:xs) [] = x ++ keySym ++ concatWith xs []
concatWith (x:xs) (y:ys) = x ++ y ++ concatWith xs ys

formatBy :: String -> String -> [String] -> String
formatBy sym str lst =
  let strLst = splitBy str sym
   in concatWith strLst lst

format :: String -> [String] -> String
format = formatBy keySym

printfLn x y = putStrLn $ format x y
printf x y = putStr $ format x y

nextLine = putStrLn ""

(%<) = format
(%<<) x y = format x [y]

main = do
    print $ "123" ==? ""
    print $ "123" ==? "123"
    print $ "" ==? "123"
    print $ "" ==? ""
    print $ "123" ==? "12"
    print $ splitBy' "123 $S 456 $S 987" "" "$S"
    print $ splitBy' "$S 456 $S 987" "" "$S"
    print $ drop (length "$S") "$S 456 $S 987"
    print $ splitBy "123 $S 456 $S 987" "$S"
    print $ format "123 $S 456 $S 987" ["$$","^^"]
    print $ splitBy "$S is a good $S for $S" "$S"
    print $ format "$S is a good $S for $S" ["He","girl","dressing"]
    print $ format "$S is a good $S for $S $S" ["He","girl","dressing"]
    print $ format "$S is a good $S for $S$S" ["He","girl","dressing"]
    print $ splitBy "$S is a good $S for $S$S" "$S"
    print $ format "$S is a good $S for $S$S" []
    print $ format "AAAAA" []
    print $ "$S is a good $S for $S"%<<"She"%<<"boy"%<<"fighting"
    printf "Ah, $S is a $S" ["He","God"]
    nextLine
    printfLn "Ah, $S is a $S" ["He","God"]
    putStrLn $ join "," ["123","456","789"]
    putStrLn $ catLines "1234-\n567\n89-10"
