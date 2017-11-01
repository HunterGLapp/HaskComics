module Main where
import Catalog
import System.IO

myfile = "comics.csv"
    
inputSingle :: IO ()
inputSingle = do
  putStrLn "Enter Title:"
  t <- getLine
  putStrLn "Enter Issue Number"
  i' <- getLine
  let i = read i':: Int
  putStrLn "Enter Format: Hardcover, Trade, or Loose"
  f' <- getLine
  let f = readF f'
  putStrLn "Enter Publisher name:"
  p <- getLine
  putStrLn "Enter Writers (separate with semicolon)"
  w' <- getLine
  let w = (readL w')
  putStrLn "Enter artists"
  a' <- getLine
  let a = (readL a')
  putStrLn "Enter colorists"
  c' <- getLine
  let c = (readL c')
  putStrLn "Enter Year of Publication"
  y' <- getLine
  let y = readY y'
  let issue = Issue t i f p w a c y
  appendFile myfile (makeCSVLine issue)
{-
inputMultiple :: IO ()
inputMultiple = do
    putStrLn "Enter Title:"
  t <- getLine
  putStrLn "Enter Issue Numbers"
  i' <- getLine
  let i = makeIssueList i'
  putStrLn "Enter Format: Hardcover, Trade, or Loose"
  f' <- getLine
  let f = readF f'
  putStrLn "Enter Publisher name:"
  p <- getLine
  putStrLn "Enter Writers (separate with semicolon)"
  w' <- getLine
  let w = (readL w')
  putStrLn "Enter artists"
  a' <- getLine
  let a = (readL a')
  putStrLn "Enter colorists"
  c' <- getLine
  let c = (readL c')
  putStrLn "Enter Year of Publication"
  y' <- getLine
  let y = readY y'
 -- let issues =  Issue t i f p w a c y
  appendFile myfile (concat (map makeCSVLine issues))
-}             
getComics :: IO [Issue]
getComics = do
  handle <- openFile "comics.csv" ReadMode
  contents <- hGetContents handle
  let issues = map readCSVLine (lines contents)
  return issues
         
main :: IO ()
main = do
  inputSingle
  comics <- getComics
  putStrLn (show comics)
