module Catalog
    where


data Format = Hardcover | Trade | Loose
              deriving (Eq, Show, Read)


data Issue = Issue { title :: String
                   , issueNumber :: Int
                   , format :: (Maybe Format)
                   , publisher :: String
                   , writers :: [String]
                   , artists :: [String]
                   , colorists :: [String]
                   , year :: (Maybe Int)
                   } deriving (Eq, Show)
{-
makeIssueList :: String -> [Int]
makeIssueList "" = []
makeIssueList s = concat
                  (map makeRanges
                           (wordsWhen (== ',') s)) where
                      makeRanges s' = makeRanges' (wordsWhen (== ':') s'
                                                             makeRanges' s'' = 
 -}
showci :: Show a => a -> String
showci s = (show s) ++ ","

showc :: String-> String
showc s = s ++ ","
           
mshowl ::Show a => Maybe a -> String
mshowl Nothing = "N/A"
mshowl (Just s) = show s

sshowl :: [String] -> String
sshowl [] = "N/A,"
sshowl arr = (stripSemi (concat (map (++ "; ") arr))) ++ "," where
    stripSemi s = reverse (tail (tail (reverse s)))

makeCSVLine :: Issue -> String
makeCSVLine (Issue t i f p w a c y ) =
    showc t ++ showci i ++ mshowl f ++ "," ++ showc p ++ sshowl w ++ sshowl a ++ sshowl c ++ mshowl y ++ "\n"

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
readL :: String -> [String]
readL "" = []
readL "N/A" = []
readL s = wordsWhen (== ';') s
          
readY :: String -> Maybe Int
readY "" = Nothing
readY "N/A" = Nothing
readY s = Just (read s :: Int)

readF :: String -> Maybe Format
readF "" = Nothing
readF "N/A" = Nothing
readF "t" = (Just Trade)
readF "h" = (Just Hardcover)
readF "l" = (Just Loose)
readF f = Just (read f :: Format)
                                             
readIssue :: [String] -> Issue
readIssue [t, i, f, p, w, a, c, y] = Issue t (read i :: Int) (readF f) p (readL w) (readL a) (readL c) (readY y) 

                                     
readCSVLine :: String -> Issue
readCSVLine s = readIssue (wordsWhen (== ',') s) 
