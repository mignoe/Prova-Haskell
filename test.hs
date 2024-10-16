import Control.Exception (catch, SomeException, throw)

-- Function that returns the head of a list or throws an error for an empty list
myHead :: [a] -> a
myHead [] = Error "empty list")  -- Use throw to raise a user-defined error
myHead (x:_) = x

main :: IO ()
main = do
    -- Attempt to call myHead with an empty list of a specific type
    catch (print (myHead ([] :: [Int]))) handleError

-- Error handler to catch and print only the error message
handleError :: SomeException -> IO ()
handleError e = putStrLn (show ( e == (throw Error "empty list")))  -- Print the error message

