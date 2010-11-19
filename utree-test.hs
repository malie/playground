import UTree
import Pretty

import Debug.Trace

input :: [Int]
input = [1, 2, 7, 4]

dmap x = trace ("dmap " ++ show x)
               (x, [x])

dfold (lx, ls) (rx, rs) = trace ("dfold " ++ show (lx, ls) ++ " " ++ show (rx, rs))
                                (lx+rx, ls ++ rs)

main =
  do let z = updatingZipperOnList input (dmap, dfold)
     pprint $ tree z
     pprint $ tree $ (flip put 100) $ right $ right $ z

{-

dmap 1
dmap 2
dfold (1,[1]) (2,[2])
dmap 7
dmap 4
dfold (7,[7]) (4,[4])
dfold (3,[1,2]) (11,[7,4])

Node (14, [1, 2, 7, 4])
  (Node (3, [1, 2]) (Leaf (1, [1]) 1) (Leaf (2, [2]) 2))
  (Node (11, [7, 4]) (Leaf (7, [7]) 7) (Leaf (4, [4]) 4))

# here the update happens, put 100 at right right, thus replacing 4

dmap 100
dfold (7,[7]) (100,[100])
dfold (3,[1,2]) (107,[7,100])

Node (110, [1, 2, 7, 100])
  (Node (3, [1, 2]) (Leaf (1, [1]) 1) (Leaf (2, [2]) 2))
  (Node (107, [7, 100]) (Leaf (7, [7]) 7) (Leaf (100, [100]) 100))

# see, minimal recalculations

-}

