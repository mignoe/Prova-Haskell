import Fila
import Control.Exception (try, evaluate, SomeException)


assert testCase expected actual
    | expected == actual = putStrLn ("\nTest case: " ++ testCase ++ " passed!\n")
    | otherwise = putStrLn ("\nTest case: " ++ testCase ++ " failed!\n  - expected " ++ show(expected) ++ "\n  - actual " ++ show(actual) ++ "\n" )



test1Element = do
    let testCase = "Erro ao acessar elemento em fila vazia"
    let filaEmpty = Fila []
    -- Catch error
    errorResult <- try (evaluate (element filaEmpty)) :: IO (Either SomeException ())

    case errorResult of
        Left _  -> assert testCase True True  -- Esperando um erro
        Right _ -> assert testCase False True  -- Não deve alcançar esse código

test2Element = do

    let testCase = "Element em fila não vazia"
    let fila = Fila [1, 2, 3]
    let result = element fila
    let expectedResult = 1

    assert testCase expectedResult result


test3Element = do

    let testCase = "Element em fila com único número"
    let fila = Fila [1]
    let result = element fila
    let expectedResult = 1

    assert testCase expectedResult result


test4Element = do
    let testCase = "Element em fila com números negativos"
    let fila = Fila [-1, -2, -3]
    let result = element fila
    let expectedResult = -1

    assert testCase expectedResult result


test5Element = do
    let testCase = "Element em fila com números decimais"
    let fila = Fila [1.5, 2.5, 3.5]
    let result = element fila
    let expectedResult = 1.5

    assert testCase expectedResult result

test6Element = do
    let testCase = "Element em fila com strings"
    let fila = Fila ["hello", "world", "test"]
    let result = element fila
    let expectedResult = "hello"

    assert testCase expectedResult result

test1Peek = do
    let testCase = "Peek em fila vazia"
    let filaEmpty = Fila []
    let result = peek filaEmpty
    let expectedResult = Nothing :: Maybe Int

    assert testCase expectedResult result

test2Peek = do
    let testCase = "Peek em fila não vazia"
    let fila = Fila [1, 2, 3]
    let result = peek fila
    let expectedResult = Just 1 :: Maybe Int
    

    assert testCase expectedResult result

test3Peek = do
    let testCase = "Peek em fila com único número"
    let fila = Fila [1]
    let result = peek fila

    let expectedResult = Just 1 :: Maybe Int
    

    assert testCase expectedResult result

test4Peek = do
    let testCase = "Peek em fila com números negativos"
    let fila = Fila [-1, -2, -3]
    let result = peek fila

    let expectedResult = Just (-1):: Maybe Int
   

    assert testCase expectedResult result

test5Peek = do
    let testCase = "Peek em fila com números decimais"
    let fila = Fila [1.5, 2.5, 3.5]
    let result = peek fila


    let expectedResult = Just 1.5 :: Maybe Float
    

    assert testCase expectedResult result

test6Peek = do
    let testCase = "Peek em fila com strings"
    let fila = Fila ["hello", "world", "test"]
    let result = peek fila


    let expectedResult = Just "hello" :: Maybe String

    assert testCase expectedResult result

--------------------
-- Testes para poll
--------------------


test1Poll = do
    let testCase = "Poll em fila vazia"
    let filaEmpty = Fila []
    let (result, newFila) = poll filaEmpty
    let expectedResult = (Nothing, Fila []) :: (Maybe Int, Fila Int)


    assert testCase expectedResult (result, newFila)

test2Poll = do
    let testCase = "Poll em fila não vazia"
    let fila = Fila [1, 2, 3]
    let (result, newFila) = poll fila
    let expectedResult = (Just 1, Fila [2, 3]) :: (Maybe Int, Fila Int)


    assert testCase expectedResult (result, newFila)

test3Poll = do
    let testCase = "Poll em fila com único número"
    let fila = Fila [1]
    let (result, newFila) = poll fila
    let expectedResult = (Just 1, Fila []) :: (Maybe Int, Fila Int)


    assert testCase expectedResult (result, newFila)

test4Poll = do
    let testCase = "Poll em fila com números negativos"
    let fila = Fila [-1, -2, -3]
    let (result, newFila) = poll fila
    let expectedResult = (Just (-1), Fila [-2, -3]) :: (Maybe Int, Fila Int)


    assert testCase expectedResult (result, newFila)

test5Poll = do
    let testCase = "Poll em fila com números decimais"
    let fila = Fila [1.5, 2.5, 3.5]
    let (result, newFila) = poll fila
    let expectedResult = (Just 1.5, Fila [2.5, 3.5]) :: (Maybe Float, Fila Float)


    assert testCase expectedResult (result, newFila)

test6Poll = do
    let testCase = "Poll em fila com strings"
    let fila = Fila ["hello", "world", "test"]
    let (result, newFila) = poll fila
    let expectedResult = (Just "hello", Fila ["world", "test"]) :: (Maybe String, Fila String)


    assert testCase expectedResult (result, newFila)


---------------------
-- Testes de removeFila
---------------------


test1removeFila :: IO ()
test1removeFila = do
    let testCase = "removeFila em fila vazia deve resultar em erro"
    let filaEmpty = Fila []
    -- Catch error
    errorResult <- try (evaluate (removeFila filaEmpty)) :: IO (Either SomeException (Fila Int))

    case errorResult of
        Left _  -> assert testCase True True  -- Esperando um erro
        Right _ -> assert testCase False True  -- Não deve alcançar esse código


test2removeFila :: IO ()
test2removeFila = do
    let testCase = "removeFila em fila não vazia"
    let fila = Fila [1, 2, 3]
    let newFila = removeFila fila
    let expectedResult = Fila [2, 3]  

    assert testCase expectedResult newFila


test3removeFila :: IO ()
test3removeFila = do
    let testCase = "removeFila em fila com único número"
    let fila = Fila [1]
    let newFila = removeFila fila
    let expectedResult = Fila []  

    assert testCase expectedResult newFila

test4removeFila :: IO ()
test4removeFila = do
    let testCase = "removeFila em fila com números negativos"
    let fila = Fila [-1, -2, -3]
    let newFila = removeFila fila
    let expectedResult = Fila [-2, -3]  

    assert testCase expectedResult newFila

test5removeFila :: IO ()
test5removeFila = do
    let testCase = "removeFila em fila com números decimais"
    let fila = Fila [1.5, 2.5, 3.5]
    let newFila = removeFila fila
    let expectedResult = Fila [2.5, 3.5]  

    assert testCase expectedResult newFila



test6removeFila :: IO ()
test6removeFila = do
    let testCase = "removeFila em fila com strings"
    let fila = Fila ["hello", "world", "test"]
    let newFila = removeFila fila
    let expectedResult = Fila ["world", "test"]  

    assert testCase expectedResult newFila

main = do
    test1Element
    test2Element
    test3Element
    test4Element
    test5Element
    test6Element

    test1Peek
    test2Peek
    test3Peek
    test4Peek
    test5Peek
    test6Peek

    test1Poll
    test2Poll
    test3Poll
    test4Poll
    test5Poll
    test6Poll

    test1removeFila
    test2removeFila
    test3removeFila
    test4removeFila
    test5removeFila
    test6removeFila
