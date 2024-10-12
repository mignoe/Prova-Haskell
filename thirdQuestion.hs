-- Implement a queue and its alrorithms in haskell
-- This implementations follows this Java interface:
-- https://docs.oracle.com/javase/8/docs/api/?java/util/Queue.html
-- aside from element()


data Queue a = Queue [a] deriving(Eq, Show)

offer x (Queue l) = Queue (l ++ [x])

element (Queue []) = error "Queue is empty"
element (Queue (l:ls)) = l

peek (Queue []) = Nothing
peek (Queue (l:ls)) = Just l

-- This one returns the head and the Queue without the head
poll (Queue []) = (Nothing, Queue [])
poll (Queue (l:ls)) = (Just l, Queue ls)

remove (Queue []) = error "Queue is empty"
remove (Queue (l:ls)) = Queue ls










