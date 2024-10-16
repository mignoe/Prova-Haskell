module Fila where

-- Esta implementação tenta seguir esta interface do Java:
-- https://docs.oracle.com/javase/8/docs/api/?java/util/Queue.html
-- exceto pelo método element()


data Fila a = Fila [a] deriving(Eq, Show)

offer x (Fila l) = Fila (l ++ [x])

element (Fila []) = error "Fila is empty"
element (Fila (l:ls)) = l

peek (Fila []) = Nothing
peek (Fila (l:ls)) = Just l

-- Essa função retorna a cabeça e a fila sem a cabeça (simulando a remoção da cabeça)
poll (Fila []) = (Nothing, Fila [])
poll (Fila (l:ls)) = (Just l, Fila ls)

-- remove já é uma função do prelude, então chamei de removeFila
removeFila (Fila []) = error "Fila is empty"
removeFila (Fila (l:ls)) = Fila ls










