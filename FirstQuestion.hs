 -- Given an ordered array and a number x, 
 -- find two numbers that belongs to the array such as (a + b) 
 -- get as near as possible from the value of x.

closestNumber [a,b] x = abs (a + b)
closestNumber (l:ls) x = min (minimum differences) (closestNumber ls x) 
                    where differencesAndSums = [abs (x - (l + y)), () | y <- ls]