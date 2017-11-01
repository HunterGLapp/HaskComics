module CSV
  where
import Validation
import Catalog
import Data.List.Split (splitOn)

makeCSVLine :: Issue -> String
makeCSVLine i = show i ++ "\n"
                                         
readIssue :: [String] -> Issue
readIssue [t, i, f, p, w, a, c, y] = Issue (read_t t) (read_i i)
                                     (read_f f) (read_p p)
                                     (read_w w) (read_a a)
                                     (read_c c) (read_y y) 

                                     
readCSVLine :: String -> Issue
readCSVLine s = read s :: Issue


