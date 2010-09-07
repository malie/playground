{-# LANGUAGE DeriveFunctor #-}

data M x = A x
         | B x (N x)
           deriving (Show, Functor)

data N x = D x
         | E x (M x)
           deriving (Show, Functor)

main = print (fmap f (B 3 (E 4 (B 5 (E 6 (A 7))))))
       where f = (*2)

