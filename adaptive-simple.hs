{- Icremental computation example by Markus Liedl
 - using Control.Monad.Adaptive by Magnus Carlsson
 - install it by `cabal install adaptive' -}
import Control.Monad.Adaptive
import Debug.Trace
main = run $
    do a <- newMod $ return 2
       b <- newMod $ return 23
       c <- newMod $ return 1
       d <- newMod $ return 2
       ab <- newMod $ do va <- trace "readMod a" $ readMod a
                         vb <- trace "readMod b" $ readMod b
                         trace "calculate ab" $ return $ (va+vb) `mod` 100
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

       inM $ putStrLn "\nchange a 4"
       change a 4
       propagate

       inM $ putStrLn "\nchange a 104"
       change a 104
       propagate

{---- output: --------------------
readMod a
readMod b
calculate ab
calculate cd
calculate sum
stringify
28

change a 4
readMod b
calculate ab
calculate sum
stringify
30

change a 104
readMod b
calculate ab
----------------------------------

Note that after `change a 4', the value `cd' is *not* recalculated.
After `change a 104' the value `ab' is recomputed, but nothing more
since `ab' still contains the value 27. Following computations are
only triggered when one of the read values changes.

After any `change a x' the computation restarts with `readMod b',
thats the continuation of `readMod a', which Adaptive detected
correctly as "having to return once more".

-}