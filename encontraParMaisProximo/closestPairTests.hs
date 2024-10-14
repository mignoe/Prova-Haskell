import ClosestPair


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


main = do
    test1
    test2
