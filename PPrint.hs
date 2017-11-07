module PPrint where
import Catalog

checkedToString :: Show a => Checked a -> String
checkedToString Empty = "N/A"
checkedToString (Bad s) = s ++ ": BADVALUE"
checkedToString (Good x) = show x

listToStr :: Checked [String] -> String
listToStr Empty = "N/A,"
listToStr (Bad s) = "BAD LIST"
listToStr arr = (stripSemi (concat (map (++ "; ") (read (checkedToString arr) :: [String])))) ++ ", " where
    stripSemi s = reverse (tail (tail (reverse s)))

pprintIssue :: Issue -> String
pprintIssue (Issue t i f p w a c y) = (checkedToString t) ++ ", " ++
                                      (checkedToString i) ++ ", " ++
                                      (checkedToString f) ++ ", " ++
                                      (checkedToString p) ++ ", " ++
                                      (listToStr w) ++
                                      (listToStr a) ++
                                      (listToStr c) ++
                                      (checkedToString y) ++ "\n"
