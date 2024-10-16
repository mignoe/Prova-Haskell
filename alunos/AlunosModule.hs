module AlunosModule where

data PrimeiroNome = PrimeiroNome String deriving(Eq,Show)
data SegundoNome = SegundoNome String deriving(Eq, Show)
data CRA = CRA Float deriving(Eq, Show)
data PeriodoEntrada = PeriodoEntrada String deriving(Eq, Show)

data Aluno = Matricula String PrimeiroNome SegundoNome PeriodoEntrada CRA deriving(Eq, Show)


getCRA (Matricula _ _ _ _ (CRA cra)) = cra

mediaCRAs [] = error "empty list"
mediaCRAs alunos = (foldr (\aluno acc-> getCRA aluno + acc) 0 alunos) / fromIntegral len
                where len = length alunos

removeByCRA cra alunos = [aluno | aluno <- alunos, getCRA aluno /= cra]

groupByCRA [] = []
groupByCRA alunos = [(cra, [aluno | aluno <- alunos, getCRA aluno == cra])] ++ groupByCRA (removeByCRA cra alunos)
                where cra = getCRA (head alunos)



