-- Find the floor and the ceil from a number x inside from an array a. The number x may not be on the array a.
-- THe floor

-- get all numbers smaller than x, order than and get the biggest

-- >>> Doubt Fl

myFloor x a = if smallerNumbers /= [] then Just (maximum smallerNumbers) else Nothing 
        where smallerNumbers = [num | num <- a, num < x]

myCeill x a = if biggerThanX /= [] then Just (minimum biggerThanX) else Nothing
        where biggerThanX = [num | num <- a, num > x]

