-- https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Stack.html
data Stack a = Stack [a] deriving(Eq, Show)

empty (Stack []) = True
empty (Stack _) = False

push x (Stack ls) = [x] ++ ls

peek (Stack []) = error "Stack is empty"
peek (Stack (l:ls)) = l

-- retrieves the object at the top and returns the new Stack
pop (Stack []) = error "Stack is empty"
pop (Stack (l:ls)) = (l, Stack ls)

search _ (Stack []) = -1
search x stack = if stackHead == x then 0 else 1 + search x nextStack
                        where
                            (stackHead, nextStack) = pop stack  

                                
