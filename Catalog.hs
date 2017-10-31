data Format = Hardcover | Trade | Loose
              deriving (Eq, Show)

data Issue = Issue { title :: String
                   , issueNumber :: Int
                   , format :: Format
                   , publisher :: String
                   , writers :: [String]
                   , artists :: [String]
                   , colorists :: [String]
                   , year :: (Maybe Int)
                   } deriving (Eq, Show)

showci :: Show a => a -> String
showci s = (show s) ++ ", "

showc :: String-> String
showc s = s ++ ", "
           
mshowl ::Show a => Maybe a -> String
mshowl Nothing = "N/A"
mshowl (Just s) = show s

sshowl :: [String] -> String
sshowl [] = "N/A, "
sshowl arr = (stripSemi (concat (map (++ "; ") arr))) ++ ", " where
    stripSemi s = reverse (tail (tail (reverse s)))

makeCSVLine :: Issue -> String
makeCSVLine (Issue t i f p w a c y ) =
    showc t ++ showci i ++ showci f ++ showc p ++ sshowl w ++ sshowl a ++ sshowl c ++ mshowl y
          
myComic = Issue "Ms. Marvel" 1 Trade "Marvel" ["Wilson"] [] [] (Just 2017)


