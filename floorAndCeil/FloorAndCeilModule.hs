module FloorAndCeilModule where

myFloor x a 
        | smallerNumbers /= [] = Just (maximum smallerNumbers) 
        | otherwise = Nothing 
        where smallerNumbers = [num | num <- a, num < x]

myCeil x a 
        | biggerThanX /= [] = Just (minimum biggerThanX) 
        | otherwise = Nothing
        where biggerThanX = [num | num <- a, num > x]

