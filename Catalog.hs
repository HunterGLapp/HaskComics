module Catalog
    where

import Data.List.Split (splitOn)

data Checked a = Empty | Bad String | Good a deriving (Eq, Show, Read)

data Format = Hardcover | Trade | Loose
              deriving (Eq, Show, Read)


data Issue = Issue { title :: Checked String
                   , issueNumber :: Checked Int
                   , format :: Checked Format
                   , publisher :: Checked String
                   , writers :: Checked [String]
                   , artists :: Checked [String]
                   , colorists :: Checked [String]
                   , year :: Checked Int
                   } deriving (Eq, Show, Read)
