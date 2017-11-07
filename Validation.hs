module Validation
    where
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Catalog

readValidate :: Read a => (String -> a) -> (String -> Bool) -> String -> Checked a
readValidate _ _ "" = Empty
readValidate _ _ "N/A" = Empty
readValidate readFun validFun s = case validFun s of
                                    True -> Good (readFun s)
                                    False -> Bad s

readTitle = id
validTitle t = True

read_t :: String -> Checked String
read_t = readValidate readTitle validTitle


readIssueNumber i = read i :: Int

validIssueNumber :: String -> Bool
validIssueNumber i = case readMaybe i :: Maybe Int of
                       Just i -> (i >= 0)
                       Nothing -> False

read_i = readValidate readIssueNumber validIssueNumber

readFormat f = read f :: Format
validFormat f = case readMaybe f :: Maybe Format of
                  Just _ -> True
                  Nothing -> False

read_f = readValidate readFormat validFormat

readPublisher = id
validPublisher _ = True
read_p = readValidate readPublisher validPublisher

readWriters :: String -> [String]
readWriters s = splitOn ";" s
validWriters _ = True
read_w = readValidate readWriters validWriters

readArtists = readWriters
validArtists = validWriters
read_a = readValidate readArtists validArtists

readColorists = readWriters
validColorists = validWriters
read_c = readValidate readColorists validColorists

readYear y = read y :: Int
validYear y = case readMaybe y of
                Just i -> (0 <= i) && (i <= 3000)
                Nothing -> False
read_y = readValidate readYear validYear



getIssueList "" = Nothing
getIssueList s = Just (concat (map myIntParse (splitOn "," s))) where
  myIntParse s = case readMaybe s :: Maybe Int of
    (Just i) -> [i]
    Nothing -> myIntRangeParse s where
      myIntRangeParse s = makeRange (map read (splitOn "-" s) :: [Int]) where
        makeRange [a, b]  = [a..b]
     
  
--getIssueList :: String -> Maybe [Int]


