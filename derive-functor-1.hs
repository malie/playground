{-# LANGUAGE DeriveFunctor #-}

data M a x = A x
           | B x (N a x)
           deriving (Show, Functor)

data N a x = D x
           | E a x (M a x)
           deriving (Show, Functor)

main = print (fmap f (B 3 (E "abc" 4 (B 5 (E "def" 6 (A 7))))))
       where f = (*2)

