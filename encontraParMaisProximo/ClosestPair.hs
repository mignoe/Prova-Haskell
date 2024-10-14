 module ClosestPair where
 
 -- Given an ordered array and a number x, 
 -- find two numbers that belongs to the array such as (a + b) 
 -- get as near as possible from the value of x.


-- closestPair :: (Ord a, Num a, Show a) => [a] -> a -> String
closestPair ls x = show a ++ " and " ++ show b
    where (_, (a, b)) = closestPairAux ls x



-- closestPairAux :: (Ord a, Num a) => [a] -> a -> (a, (a, a))
closestPairAux [a, b] x = (abs (x - (a + b)), (a, b))
closestPairAux ls x 
                | difference > 0 = min result (closestPairAux (tail ls) x) -- x > (first + last) 
                | difference < 0 = min result (closestPairAux (init ls) x) 
                | otherwise = result
                    where
                        difference = (x - (head ls + last ls))
                        result = (abs difference, (head ls, last ls))
