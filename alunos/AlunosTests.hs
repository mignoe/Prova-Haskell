
import AlunosModule


alunos = [ Matricula "12111999" (PrimeiroNome "João") (SegundoNome "Silva") (PeriodoEntrada "2020.1") (CRA 8.5)
    , Matricula "12119443" (PrimeiroNome "Miguel") (SegundoNome "Rodrigues") (PeriodoEntrada "2021.1") (CRA 0)
    , Matricula "12199423" (PrimeiroNome "Rodrigues") (SegundoNome "Miguel") (PeriodoEntrada "2019.2") (CRA 10)
    , Matricula "12210099" (PrimeiroNome "Maria") (SegundoNome "Souza") (PeriodoEntrada "2021.1") (CRA 9.1)
    , Matricula "22111003" (PrimeiroNome "Pedro") (SegundoNome "Oliveira") (PeriodoEntrada "2020.1") (CRA 7.8)
    , Matricula "12993332" (PrimeiroNome "Ana") (SegundoNome "Costa") (PeriodoEntrada "2020.1") (CRA 8.5)
    , Matricula "24112499" (PrimeiroNome "Lucas") (SegundoNome "Almeida") (PeriodoEntrada "2023.1") (CRA 7.5)
    , Matricula "23211111" (PrimeiroNome "Juliana") (SegundoNome "Pereira") (PeriodoEntrada "2021.2") (CRA 9.3)
    , Matricula "12313034" (PrimeiroNome "Rafael") (SegundoNome "Rodrigues") (PeriodoEntrada "2020.1") (CRA 6.7)
    , Matricula "23110343" (PrimeiroNome "Fernanda") (SegundoNome "Lima") (PeriodoEntrada "2019.2") (CRA 8.2)
    , Matricula "21314145" (PrimeiroNome "Bruno") (SegundoNome "Martins") (PeriodoEntrada "2020.1") (CRA 7.1)
    , Matricula "23145123" (PrimeiroNome "Camila") (SegundoNome "Barros") (PeriodoEntrada "2023.2") (CRA 9.0)
    , Matricula "21314567" (PrimeiroNome "Camila") (SegundoNome "Renata") (PeriodoEntrada "2023.2") (CRA 9.0)
    ]


assert testCase expected actual
    | expected == actual = putStrLn ("\nTest case: " ++ testCase ++ " passed!\n")
    | otherwise = putStrLn ("\nTest case: " ++ testCase ++ " failed!\n  - expected " ++ show(expected) ++ "\n  - actual " ++ show(actual) ++ "\n" )


test1Group = do
    let testCase = "Group de um aluno"
    let alunos = [Matricula "12111999" (PrimeiroNome "João") (SegundoNome "Silva") (PeriodoEntrada "2020.1") (CRA 8.5)]
    let alunosAgrupados = groupByCRA alunos
    let expectedResult = [(8.5, [Matricula "12111999" (PrimeiroNome "João") (SegundoNome "Silva") (PeriodoEntrada "2020.1") (CRA 8.5)])]
    assert testCase expectedResult alunosAgrupados

test2Group = do
    let testCase = "Group de lista vazia"
    let alunos = []
    let alunosAgrupados = groupByCRA alunos
    let expectedResult = []
    assert testCase expectedResult alunosAgrupados

test3Group = do
    let testCase = "Group de poucos alunos"
    let alunos = [ Matricula "12111999" (PrimeiroNome "João") (SegundoNome "Silva") (PeriodoEntrada "2020.1") (CRA 8.5)
                    , Matricula "12119443" (PrimeiroNome "Miguel") (SegundoNome "Rodrigues") (PeriodoEntrada "2021.1") (CRA 0)
                    , Matricula "12199423" (PrimeiroNome "Rodrigues") (SegundoNome "Miguel") (PeriodoEntrada "2019.2") (CRA 10)
                    , Matricula "12210099" (PrimeiroNome "Maria") (SegundoNome "Souza") (PeriodoEntrada "2021.1") (CRA 8.5)
                    ]
    let alunosAgrupados = groupByCRA alunos
    let expectedResult = [
          (8.5, [Matricula "12111999" (PrimeiroNome "João") (SegundoNome "Silva") (PeriodoEntrada "2020.1") (CRA 8.5)
                ,  Matricula "12210099" (PrimeiroNome "Maria") (SegundoNome "Souza") (PeriodoEntrada "2021.1") (CRA 8.5) ])
            , (0, [ Matricula "12119443" (PrimeiroNome "Miguel") (SegundoNome "Rodrigues") (PeriodoEntrada "2021.1") (CRA 0)])
            , (10, [ Matricula "12199423" (PrimeiroNome "Rodrigues") (SegundoNome "Miguel") (PeriodoEntrada "2019.2") (CRA 10)])
            ]   
    assert testCase expectedResult alunosAgrupados 


main :: IO ()
main = do
    test1
    test2
    test3
    
