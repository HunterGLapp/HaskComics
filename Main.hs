module Main where
import Catalog
import Validation
import CSV
import System.IO

myfile = "comics.csv"
    
inputSingle :: IO ()
inputSingle = do
  putStrLn "Enter Title:"
  t' <- getLine
  let t = read_t t'
  putStrLn "Enter Issue Number"
  i' <- getLine
  let i = read_i i'
  putStrLn "Enter Format: Hardcover, Trade, or Loose"
  f' <- getLine
  let f = read_f f'
  putStrLn "Enter Publisher name:"
  p' <- getLine
  let p = read_p p'
  putStrLn "Enter Writers (separate with semicolon)"
  w' <- getLine
  let w = (read_w w')
  putStrLn "Enter artists"
  a' <- getLine
  let a = (read_a a')
  putStrLn "Enter colorists"
  c' <- getLine
  let c = (read_c c')
  putStrLn "Enter Year of Publication"
  y' <- getLine
  let y = read_y y'
  let issue = Issue t i f p w a c y
  appendFile myfile (makeCSVLine issue)
           
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
