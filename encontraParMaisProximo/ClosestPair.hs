 module ClosestPair where
 
closestPair [] _ = error "At least two numbers are needed!"
closestPair [_] _ = error "At least two numbers are needed!"
closestPair lista x = show a ++ " and " ++ show b
    where (_, (a, b)) = closestPairAux lista x


closestPairAux [a, b] x = (abs (x - (a + b)), (a, b))
closestPairAux ls x 
                | difference > 0 = min result (closestPairAux (tail ls) x) 
                | difference < 0 = min result (closestPairAux (init ls) x) 
                | otherwise = result
                    where
                        difference = (x - (head ls + last ls))
                        result = (abs difference, (head ls, last ls))
