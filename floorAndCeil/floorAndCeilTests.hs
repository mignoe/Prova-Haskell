import FloorAndCeilModule

assert testCase expected actual
    | expected == actual = putStrLn ("\nTest case: " ++ testCase ++ " passed!\n")
    | otherwise = putStrLn ("\nTest case: " ++ testCase ++ " failed!\n  - expected " ++ show(expected) ++ "\n  - actual " ++ show(actual) ++ "\n" )

 
test1Floor = do
    let testCase = "Floor com número menor"
    let list = [1, 2, 3, 4]
    let num = 3
    let result = myFloor num list
    let expectedResult = Just 2
    assert testCase expectedResult result


test2Floor = do
    let testCase = "Floor sem número menor"
    let list = [4, 5, 6]
    let num = 3
    let result = myFloor num list
    let expectedResult = Nothing
    assert testCase expectedResult result


test1Ceil = do
    let testCase = "Ceil com número maior"
    let list = [1, 2, 3, 4]
    let num = 3
    let result = myCeil num list
    let expectedResult = Just 4
    assert testCase expectedResult result

test2Ceil = do
    let testCase = "Ceil sem número maior"
    let list = [1, 2, 3]
    let num = 4
    let result = myCeil num list
    let expectedResult = Nothing
    assert testCase expectedResult result


test1FloorAndCeil = do
    let testCase = "Floor e Ceil com número exatamente igual na lista"
    let list = [1, 2, 3, 4, 5]
    let num = 3
    let floorResult = myFloor num list
    let ceilResult = myCeil num list
    let expectedFloorResult = Just 2  -- Floor de 3 is 2
    let expectedCeilResult = Just 4   -- Ceil de 3 is 4
    assert (testCase ++ " - Floor") expectedFloorResult floorResult
    assert (testCase ++ " - Ceil") expectedCeilResult ceilResult


test2FloorAndCeil = do
    let testCase = "Floor and Ceil em lista vazia"
    let lista = []
    let num = 4
    let floorResult = myFloor num lista
    let ceilResult = myCeil num lista
    let expectedFloorResult = Nothing
    let expectedCeilResult = Nothing

    assert (testCase ++ " - Floor") expectedFloorResult floorResult
    assert (testCase ++ " - Ceil") expectedCeilResult ceilResult


test3FloorAndCeil = do
    let testCase = "Floor and Ceil em lista de números negativos"
    let lista = [-10, -5, -1]
    let num = 0
    let floorResult = myFloor num lista
    let ceilResult = myCeil num lista
    let expectedFloorResult = Just (-1)  -- O maior número menor que 0 é -1
    let expectedCeilResult = Nothing     -- Não há número maior que 0 na lista

    assert (testCase ++ " - Floor") expectedFloorResult floorResult
    assert (testCase ++ " - Ceil") expectedCeilResult ceilResult



main = do
    test1Floor
    test2Floor
    test1Ceil
    test2Ceil
    test1FloorAndCeil
    test2FloorAndCeil
    test3FloorAndCeil
