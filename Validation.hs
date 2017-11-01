module Validation
    where

data InputType a = Empty | Bad String | Good a

readValidate :: Read a => (String -> a) -> (String -> Bool) -> String -> InputType a
readValidate readFun validFun "" = Empty
readValidate readFun validFun s = case validFun s of
                                    True -> Good (readFun s)
                                    False -> Bad s

readMaybe :: Read a => (String -> a) -> String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing
                     
validYear :: String -> Bool
validYear "" = undefined
validYear s = case readMaybe s of
                Just i -> (0 <= i) && (i <= 3000)
                Nothing -> False
