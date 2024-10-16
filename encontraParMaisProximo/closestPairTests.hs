import ClosestPair
import Control.Exception (catch, SomeException)


-- Function to simulate error catching
catchError :: String -> String
catchError action = action

assert testCase expected actual
    | expected == actual = putStrLn ("\nTest case: " ++ testCase ++ " passed!\n")
    | otherwise = putStrLn ("\nTest case: " ++ testCase ++ " failed!\n  - expected " ++ show(expected) ++ "\n  - actual " ++ show(actual) ++ "\n" )


test1 = do
    let testCase = "Teste exemplo Prova final"
    let list = [10, 22, 28, 29, 30, 40]
    let x = 54
    let result = closestPair list x
    let expectedResult = "22 and 30"  
    assert testCase expectedResult result


test2 = do
    let testCase = "Teste segundo exemplo Prova final"
    let list = [1, 3, 4, 7, 10]
    let x = 15
    let result = closestPair list x
    let expectedResult = "4 and 10"  
    assert testCase expectedResult result


test3 = do
    let testCase = "Teste com números negativos"
    let list = [-10, -3, -1, 2, 5]
    let x = -5
    let result = closestPair list x
    let expectedResult = "-10 and 5"  
    assert testCase expectedResult result

test4 = do
    let testCase = "Teste com números decimais"
    let list = [1.2, 3.5, 4.8, 6.7, 10.1]
    let x = 9.5
    let result = closestPair list x
    let expectedResult = "3.5 and 6.7"  
    assert testCase expectedResult result


test5 = do
    let testCase = "Teste com lista vazia"
    let list = []
    let x = 10
    let result = catchError (closestPair list x)
    let expectedResult = "At least two numbers are needed!"
    assert testCase expectedResult result

test6 = do
    let testCase = "Teste com apenas um número na lista"
    let list = [8]
    let x = 8
    let result = catchError (closestPair list x)
    let expectedResult = "At least two numbers are needed!"
    assert testCase expectedResult result


test7 = do
    let testCase = "Teste com todos números negativos"
    let list = [-20, -15, -10, -5, -1]
    let x = -12
    let result = closestPair list x
    let expectedResult = "-10 and -1"  
    assert testCase expectedResult result

test8 = do
    let testCase = "Teste com todos números decimais negativos"
    let list = [-5.5, -3.3, -2.1, -0.9]
    let x = -4.0
    let result = closestPair list x
    let expectedResult = "-3.3 and -0.9"  
    assert testCase expectedResult result


main = do
    test1
    test2
    test3
    test4
    test5
    test6
    test7
    test8
