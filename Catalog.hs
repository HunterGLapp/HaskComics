data Format = Hardcover | Trade | Loose
              deriving (Eq, Show)

data Issue = Issue { title :: String
                   , issueNumber :: Int
                   , format :: Format
                   , year :: (Maybe Int)
                   } deriving (Eq, Show)

makeCSVLine :: Issue -> String
makeCSVLine (Issue t i f y ) =
    concat [show t, ", ", show i, ", ", show f,", ", mshow y] where
        mshow Nothing = "N/A"
        mshow (Just year) = show year

--myComic = Issue "Ms. Marvel" 1 Trade (Just 2017)
