data PrimeiroNome = PrimeiroNome String deriving(Show)
data SegundoNome = SegundoNome String deriving(Show)
data CRA = CRA Float deriving(Show)

data Aluno = Matricula String PrimeiroNome SegundoNome CRA deriving(Show)


nomeCompleto (Matricula _ (PrimeiroNome nome) (SegundoNome sobreNome) _) = nome ++ " " ++ sobreNome