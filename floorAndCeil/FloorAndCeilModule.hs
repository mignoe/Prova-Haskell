module FloorAndCeilModule where

-- Find the floor and the ceil from a number x inside from an array a. The number x may not be on the array a.
-- THe floor

-- get all numbers smaller than x, order than and get the biggest

-- >>> Doubt Fl

myFloor x a 
        | smallerNumbers /= [] = Just (maximum smallerNumbers) 
        | otherwise = Nothing 
        where smallerNumbers = [num | num <- a, num < x]

myCeil x a 
        | biggerThanX /= [] = Just (minimum biggerThanX) 
        | otherwise = Nothing
        where biggerThanX = [num | num <- a, num > x]

