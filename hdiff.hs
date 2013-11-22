import System.Environment (getArgs)
import Control.Applicative ((<*>))
import Control.Monad (when, liftM)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Console.ANSI (setSGR, SGR (..), ConsoleLayer (..), ColorIntensity (..), Color (..))
import qualified Data.MemoCombinators as Memo
 
type Line = (Int, String) -- line number and value

lineAnnotate :: [String] -> [Line]
lineAnnotate = zip [1..]


lcs :: [String] -> [String] -> [String]
lcs = let memoString = Memo.list $ Memo.list Memo.char
      in  Memo.memo2 memoString memoString lcs'
    where   lcs' [] _          = []
            lcs' _ []          = []
            lcs' (x:xs) (y:ys) | x == y    = x:lcs xs ys
                               | otherwise = let dx = lcs xs (y:ys)
                                                 dy = lcs (x:xs) ys
                                             in  if length dx > length dy then dx
                                                 else dy


removalsAndAdditions :: [Line] -> [Line] -> [String] -> ([Line], [Line])
removalsAndAdditions a b []     = (a, b)
removalsAndAdditions a b (c:cs) = (removals ++ restRem, additions ++ restAdd)
    where   split               = span $ (/= c) . snd
            (removals, restA)   = split a
            (additions, restB)  = split b
            (restRem, restAdd)  = removalsAndAdditions (tail restA) (tail restB) cs


flattenDiff :: ([Line], [Line]) -> [Line]
flattenDiff (rems, adds) = sortBy (comparing (abs . fst)) (rems' ++ adds)
    where   rems' = map (\(i, s) -> (-i, s)) rems


printFlattenedDiff :: [Line] -> IO ()
printFlattenedDiff []   = return ()
printFlattenedDiff diff = mapM_ printDiff annotatedDiff
    where   lineDiff (a, _) (b, s)  = (abs b - abs a, b, s)
            annotatedDiff           = (2, (fst . head) diff, (snd . head) diff) : (zipWith lineDiff <*> tail $ diff)
            printDiff (d, i, s)     = do
                when (d > 1) $ putStrLn $ '@':show (abs i)
                printLine (i, s)


printLine :: Line -> IO ()
printLine (i, s) = do
    if i < 0 then setSGR [SetColor Foreground Vivid Red] >> putStr "- "
    else setSGR [SetColor Foreground Vivid Green] >> putStr "+ "
    putStrLn s
    setSGR [Reset]


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ error $ "Expected 2 command line arguments, received " ++ (show . length) args
    a <- liftM lines $ (readFile . head) args
    b <- liftM lines $ (readFile . head . tail) args
    printFlattenedDiff $ flattenDiff $ removalsAndAdditions (lineAnnotate a) (lineAnnotate b) (lcs a b)
