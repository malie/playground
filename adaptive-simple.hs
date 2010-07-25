{- Icremental computation example by Markus Liedl
 - using Control.Monad.Adaptive by Magnus Carlsson
 - install it by `cabal install adaptive' -}
import Control.Monad.Adaptive
import Debug.Trace
main = run $
    do a <- newMod $ return 100
       b <- newMod $ return 23
       c <- newMod $ return 1
       d <- newMod $ return 2
       ab <- newMod $ do va <- readMod a
                         vb <- readMod b
                         trace "calculate ab" $ return $ va+vb
       cd <- newMod $ do vc <- readMod c
                         vd <- readMod d
                         trace "calculate cd" $ return $ vc+vd
       sum <- newMod $ do u <- readMod ab
                          v <- readMod cd
                          trace "calculate sum" $ return $ u+v
       sumstr <- newMod $ do s <- readMod sum
                             trace "stringify" $ return $ show s
       newMod $ do v <- readMod sumstr
                   inM $ putStrLn v
       change a 2
       propagate
{---- output: --------------------
calculate ab
calculate cd
calculate sum
stringify
126
calculate ab
calculate sum
stringify
28
----------------------------------
Note that after `change a 2', the value `cd' is *not* recalculated.
-}