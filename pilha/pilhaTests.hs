import Pilha
import Control.Exception (try, evaluate, SomeException)

assert testCase expected actual
    | expected == actual = putStrLn ("\nTest case: " ++ testCase ++ " passed!\n")
    | otherwise = putStrLn ("\nTest case: " ++ testCase ++ " failed!\n  - expected " ++ show(expected) ++ "\n  - actual " ++ show(actual) ++ "\n" )

test1Empty = do
    let testCase = "empty quando pilha está vazia"
    let pilha = Pilha []
    let result = empty pilha
    let expectedResult = True

    assert testCase expectedResult result


test2Empty = do
    let testCase = "empty quando pilha não está vazia"
    let pilha = Pilha [2]
    let result = empty pilha
    let expectedResult = False

    assert testCase expectedResult result

test1Push = do
    let testCase = "Push em uma pilha vazia"
    let pilhaEmpty = Pilha []
    let result = push 1 pilhaEmpty
    let expectedResult = Pilha [1]

    assert testCase expectedResult result


test2Push = do
    let testCase = "Push em uma pilha com único elemento"
    let pilhaEmpty = Pilha [3]
    let result = push 1 pilhaEmpty
    let expectedResult = Pilha [1, 3]

    assert testCase expectedResult result


test3Push = do
    let testCase = "Push em uma pilha com muitos elementos"
    let pilhaEmpty = Pilha [5,4,6,4,3]
    let result = push 1 pilhaEmpty
    let expectedResult = Pilha [1,5,4,6,4,3]

    assert testCase expectedResult result

test1Peek = do
    let testCase = "Peek em uma pilha vazia deve resultar em erro"
    let pilhaEmpty = Pilha []
    -- Captura o erro
    errorResult <- try (evaluate (peek pilhaEmpty)) :: IO (Either SomeException Int)

    case errorResult of
        Left _  -> assert testCase True True  -- Esperando um erro
        Right _ -> assert testCase False True  -- Não deve alcançar esse código

test2Peek = do
    let testCase = "Peek em uma pilha não vazia"
    let pilha = Pilha [1, 2, 3]
    let result = peek pilha
    let expectedResult = 1

    assert testCase expectedResult result

test1Pop = do
    let testCase = "Pop em uma pilha vazia deve resultar em erro"
    let pilhaEmpty = Pilha []
    -- Captura o erro
    errorResult <- try (evaluate (pop pilhaEmpty)) :: IO (Either SomeException (Int, Pilha Int))

    case errorResult of
        Left _  -> assert testCase True True  -- Esperando um erro
        Right _ -> assert testCase False True  -- Não deve alcançar esse código

test2Pop = do
    let testCase = "Pop em uma pilha com elementos"
    let pilha = Pilha [1, 2, 3]
    let (result, newPilha) = pop pilha
    let expectedResult = (1, Pilha [2, 3])

    assert testCase expectedResult (result, newPilha)

-- Teste de search (procurar um elemento na pilha)
test1Search = do
    let testCase = "Search em uma pilha com o elemento no topo"
    let pilha = Pilha [1, 2, 3]
    let result = search 1 pilha
    let expectedResult = 0

    assert testCase expectedResult result

test2Search = do
    let testCase = "Search em uma pilha com o elemento no meio"
    let pilha = Pilha [1, 2, 3]
    let result = search 2 pilha
    let expectedResult = 1

    assert testCase expectedResult result

test3Search = do
    let testCase = "Search em uma pilha sem o elemento"
    let pilha = Pilha [1, 2, 3]
    let result = search 4 pilha
    let expectedResult = -1

    assert testCase expectedResult result


main = do
    test1Empty
    test2Empty

    test1Push
    test2Push
    test3Push
    
    test1Peek
    test2Peek

    test1Pop
    test2Pop

    test1Search
    test2Search
    test3Search