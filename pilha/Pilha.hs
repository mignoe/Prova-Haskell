module Pilha where
-- Esta implementação tenta seguir esta interface do Java:
-- https://docs.oracle.com/javase/8/docs/api/?java/util/Pilha.html
-- exceto pelo método element()

data Pilha a = Pilha [a] deriving(Eq, Show)

empty (Pilha []) = True
empty (Pilha ls) = False

push x (Pilha ls) = Pilha ([x] ++ ls)

peek (Pilha []) = error "Stack is empty"
peek (Pilha (l:ls)) = l

-- retorna o objeto na cabeça e a nova pilha sem ela.
pop (Pilha []) = error "Stack is empty"
pop (Pilha (l:ls)) = (l, Pilha ls)

search x pilha = searchAux x pilha 0

searchAux _ (Pilha []) _ = -1
searchAux x pilha i 
    | pilhaHead == x = i
    | otherwise = searchAux x nextPilha (i + 1)
    where (pilhaHead, nextPilha) = pop pilha

                                
