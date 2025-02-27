import ClosestPair

import Control.Exception (try, evaluate, SomeException)


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

    -- Catching the error
    result <- try (evaluate (closestPair list x)) :: IO (Either SomeException String)

    case result of
        Left _  -> assert testCase True True  -- Espera-se um erro
        Right _ -> assert testCase False True  -- Não deveria alcançar esse código

test6 = do
    let testCase = "Teste com apenas um número na lista"
    let list = [8]
    let x = 8

    -- Catching the error
    result <- try (evaluate (closestPair list x)) :: IO (Either SomeException String)

    case result of
        Left _  -> assert testCase True True  -- Espera-se um erro
        Right _ -> assert testCase False True  -- Não deveria alcançar esse código



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


test9 = do
    let testCase = "Teste com todos números decimais negativos e target positivo"
    let list = [-5.5, -3.3, -2.1, -0.9]
    let x = 4
    let result = closestPair list x
    let expectedResult = "-2.1 and -0.9"  
    assert testCase expectedResult result


test10 = do
    let testCase = "Teste com todos números decimais positivos e target negativo"
    let list = [0.9, 2.1, 3.3, 5.5]
    let x = -4
    let result = closestPair list x
    let expectedResult = "0.9 and 2.1"  
    assert testCase expectedResult result

test11 = do
    let testCase = "Teste com números mistos"
    let list = [-10, -4.5, 2, 4.5, 34]
    let x = 0
    let result = closestPair list x
    let expectedResult = "-4.5 and 4.5"  
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
    test9
    test10
    test11
