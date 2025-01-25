module Main (main) where

import              Data.WeakSet.Safe
import              Data.WeakMap.Safe
import              Math.HypergraphRewriting
import              Math.IO.PrettyPrint
import              Data.Simplifiable


main :: IO ()
main = do
    let nodes = set $ [0,1,2 :: Int]
    let edge1 = hyperedge (0  :: Int) [0,1] [1,2  :: Int] "f"
    let edge2 = hyperedge 0 [0,1] [1] "f"
    let edge3 = hyperedge 0 [0,1] [1] "g"
    let h1 = hypergraph nodes (set [edge1,edge2])
    let h2 = hypergraph nodes (set [edge1,edge3])
    putStrLn $ show $ h1
    putStrLn $ show $ h2
    let h = unsafeHypergraph nodes (set [edge1,edge3])
    putStrLn $ show $ isMonogamous h
    putStrLn $ show $ isAcyclic h
    let nodes' = set $ [0,1,2 :: Int]
    let edge1' = hyperedge (0 :: Int) [0] [1] "f"
    let edge2' = hyperedge 0 [1] [2] "g"
    let h' = unsafeHypergraph nodes' (set [edge1',edge2'])
    putStrLn $ show $ isMonogamous h'
    putStrLn $ show $ isAcyclic h'
    let faultyMACospan = maCospan (unsafeHypergraphMorphism (weakMap [(0,0)]) (weakMap []) h') (unsafeHypergraphMorphism (weakMap [(1,1)]) (weakMap []) h')
    let validMACospan = maCospan (unsafeHypergraphMorphism (weakMap [(0,0)]) (weakMap []) h') (unsafeHypergraphMorphism (weakMap [(1,2)]) (weakMap []) h')
    putStrLn $ show $ faultyMACospan
    putStrLn $ show $ validMACospan
    
    let Right example32L = hypergraph (set [0]) (set [])
    let Right example32Kin = hypergraph (set [0]) (set [])
    let Right example32Kout = hypergraph (set [1]) (set [])
    let Right example32R = hypergraph (set [0,1]) (set [hyperedge 0 [0] [1] "a3"])
    let Right example32ain = hypergraphMorphism example32Kin example32L (weakMap [(0,0)]) (weakMap [])
    let Right example32aout = hypergraphMorphism example32Kout example32L (weakMap [(1,0)]) (weakMap [])
    let Right example32bin = hypergraphMorphism example32Kin example32R (weakMap [(0,0)]) (weakMap [])
    let Right example32bout = hypergraphMorphism example32Kout example32R (weakMap [(1,1)]) (weakMap [])
    let Right example32rr = leftConnectedMARewriteRule example32ain example32aout example32bin example32bout
    putStrLn $ show $ example32rr 
    
    let Right example32D = hypergraph (set [0]) (set [hyperedge 0 [] [0] "a1", hyperedge 1 [0] [] "a2"])
    let Right example32f = hypergraphMorphism example32L example32D (weakMap [(0,0)]) (weakMap [])
    putStrLn $ show $ isConvexMatch example32f
    
    let Right nb1L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 0 [0,1] [4] "mu",hyperedge 1 [4,2] [3] "mu"])
    let Right nb1Kin = hypergraph (set [0 :: Int,1,2]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb1Kout = hypergraph (set [3 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb1R = hypergraph (set [0,1,2,3,4]) (set [hyperedge 2 [1,2] [4] "mu",hyperedge 3 [0,4] [3] "mu"])
    let Right nb1ain = hypergraphMorphism nb1Kin nb1L (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1aout = hypergraphMorphism nb1Kout nb1L (weakMap [(3,3)]) (weakMap [])
    let Right nb1bin =  hypergraphMorphism nb1Kin nb1R (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1bout =  hypergraphMorphism nb1Kout nb1R (weakMap [(3,3)]) (weakMap [])
    let Right nb1rr = leftConnectedMARewriteRule nb1ain nb1aout nb1bin nb1bout
    putStrLn $ show $ nb1rr 
    
    -- let Right nm = hypergraph (set [0..5]) (set [])
    let Right d = hypergraph (set [0..8]) (set [hyperedge 0 [0,1] [6] "mu", hyperedge 1 [6,2] [7] "mu", hyperedge 2 [7,3] [8] "mu", hyperedge 0 [8] [4,5] "nu"])
    let Right dcospan = maCospan (unsafeHypergraphMorphism (memorizeFunction id (set [0..3])) mempty d) (unsafeHypergraphMorphism (memorizeFunction id (set [4,5])) mempty d)
    let Right f = hypergraphMorphism nb1L d (weakMap [(0,0),(1,1),(2,2),(3,7),(4,6)]) (weakMap [(hyperedge 0 [0,1] [4] "mu",hyperedge 0 [0,1] [6] "mu"),(hyperedge 1 [4,2] [3] "mu", hyperedge 1 [6,2] [7] "mu")])
    putStrLn $ show $ isConvexMatch f
    
    let (c1,c2) = pushoutComplement (leftCospan nb1rr) dcospan f
    putStrLn $ "\n\n\n"
    putStrLn $ show dcospan
    putStrLn $ "\n\n"
    putStrLn $ show c1
    putStrLn $ show c2
    let push = pushout (rightCospan nb1rr) c2 c1
    putStrLn $ show push
    
    let resultDPO = dpo nb1rr dcospan f
    putStrLn $ show resultDPO
    
    putStrLn "\n\n\n"
    let Right g1 = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right g2 = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [0] [2] "mu",hyperedge 1 [0] [1] "mu"])
    let Right f = hypergraphMorphism g1 g2 (weakMap [(0,1)]) (weakMap [])
    let Right g = hypergraphMorphism g1 g2 (weakMap [(0,2)]) (weakMap [])
    let coeq = coequalize f g
    putStrLn $ show coeq
    
    putStrLn "\n\n\n"
    let Right g1 = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right g2 = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [0] [2] "mu",hyperedge 1 [0] [1] "mu"])
    let coprod = coproduct g1 g2
    putStrLn $ show coprod
    
    
    
    
    
    
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    
    let Right nb1L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 0 [0,1] [4] "mu",hyperedge 1 [4,2] [3] "mu"])
    let Right nb1Kin = hypergraph (set [0 :: Int,1,2]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb1Kout = hypergraph (set [3 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb1R = hypergraph (set [0,1,2,3,4]) (set [hyperedge 2 [1,2] [4] "mu",hyperedge 3 [0,4] [3] "mu"])
    let Right nb1ain = hypergraphMorphism nb1Kin nb1L (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1aout = hypergraphMorphism nb1Kout nb1L (weakMap [(3,3)]) (weakMap [])
    let Right nb1bin =  hypergraphMorphism nb1Kin nb1R (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1bout =  hypergraphMorphism nb1Kout nb1R (weakMap [(3,3)]) (weakMap [])
    let Right nb1rr = leftConnectedMARewriteRule nb1ain nb1aout nb1bin nb1bout
    
    
    let Right nb2L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 0 [0] [4,3] "nu",hyperedge 1 [4] [1,2] "nu"])
    let Right nb2Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb2Kout = hypergraph (set [1,2,3 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb2R = hypergraph (set [0,1,2,3,4]) (set [hyperedge 2 [0] [1,2] "nu",hyperedge 3 [4] [2,3] "nu"])
    let Right nb2ain = hypergraphMorphism nb2Kin nb2L (weakMap [(0,0)]) (weakMap [])
    let Right nb2aout = hypergraphMorphism nb2Kout nb2L (weakMap [(1,1),(2,2),(3,3)]) (weakMap [])
    let Right nb2bin =  hypergraphMorphism nb2Kin nb2R (weakMap [(0,0)]) (weakMap [])
    let Right nb2bout =  hypergraphMorphism nb2Kout nb2R (weakMap [(1,1),(2,2),(3,3)]) (weakMap [])
    let Right nb2rr = leftConnectedMARewriteRule nb2ain nb2aout nb2bin nb2bout
    
    
    let Right nb3L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [] [2] "eta",hyperedge 1 [0,2] [1] "mu"])
    let Right nb3Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb3Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb3R = hypergraph (set [0]) (set [])
    let Right nb3ain = hypergraphMorphism nb3Kin nb3L (weakMap [(0,0)]) (weakMap [])
    let Right nb3aout = hypergraphMorphism nb3Kout nb3L (weakMap [(1,1)]) (weakMap [])
    let Right nb3bin =  hypergraphMorphism nb3Kin nb3R (weakMap [(0,0)]) (weakMap [])
    let Right nb3bout =  hypergraphMorphism nb3Kout nb3R (weakMap [(1,0)]) (weakMap [])
    let Right nb3rr = leftConnectedMARewriteRule nb3ain nb3aout nb3bin nb3bout
    
    
    let Right nb4L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [] [2] "eta",hyperedge 1 [2,0] [1] "mu"])
    let Right nb4Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb4Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb4R = hypergraph (set [0]) (set [])
    let Right nb4ain = hypergraphMorphism nb4Kin nb4L (weakMap [(0,0)]) (weakMap [])
    let Right nb4aout = hypergraphMorphism nb4Kout nb4L (weakMap [(1,1)]) (weakMap [])
    let Right nb4bin =  hypergraphMorphism nb4Kin nb4R (weakMap [(0,0)]) (weakMap [])
    let Right nb4bout =  hypergraphMorphism nb4Kout nb4R (weakMap [(1,0)]) (weakMap [])
    let Right nb4rr = leftConnectedMARewriteRule nb4ain nb4aout nb4bin nb4bout   
    
    
    let Right nb5L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [0] [1,2] "nu",hyperedge 1 [2] [] "epsilon"])
    let Right nb5Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb5Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb5R = hypergraph (set [0]) (set [])
    let Right nb5ain = hypergraphMorphism nb5Kin nb5L (weakMap [(0,0)]) (weakMap [])
    let Right nb5aout = hypergraphMorphism nb5Kout nb5L (weakMap [(1,1)]) (weakMap [])
    let Right nb5bin =  hypergraphMorphism nb5Kin nb5R (weakMap [(0,0)]) (weakMap [])
    let Right nb5bout =  hypergraphMorphism nb5Kout nb5R (weakMap [(1,0)]) (weakMap [])
    let Right nb5rr = leftConnectedMARewriteRule nb5ain nb5aout nb5bin nb5bout
    
    
    let Right nb6L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [0] [2,1] "nu",hyperedge 1 [2] [] "epsilon"])
    let Right nb6Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb6Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb6R = hypergraph (set [0]) (set [])
    let Right nb6ain = hypergraphMorphism nb6Kin nb6L (weakMap [(0,0)]) (weakMap [])
    let Right nb6aout = hypergraphMorphism nb6Kout nb6L (weakMap [(1,1)]) (weakMap [])
    let Right nb6bin =  hypergraphMorphism nb6Kin nb6R (weakMap [(0,0)]) (weakMap [])
    let Right nb6bout =  hypergraphMorphism nb6Kout nb6R (weakMap [(1,0)]) (weakMap [])
    let Right nb6rr = leftConnectedMARewriteRule nb6ain nb6aout nb6bin nb6bout
    
    
    let Right nb7L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [0,1] [2] "mu",hyperedge 1 [2] [] "epsilon"])
    let Right nb7Kin = hypergraph (set [0 :: Int, 1]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb7Kout = hypergraph (set []) (set ([] :: [Hyperedge Int Int String]))
    let Right nb7R = hypergraph (set [0,1]) (set [hyperedge 2 [0] [] "epsilon", hyperedge 3 [1] [] "epsilon"])
    let Right nb7ain = hypergraphMorphism nb7Kin nb7L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb7aout = hypergraphMorphism nb7Kout nb7L (weakMap []) (weakMap [])
    let Right nb7bin =  hypergraphMorphism nb7Kin nb7R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb7bout =  hypergraphMorphism nb7Kout nb7R (weakMap []) (weakMap [])
    let Right nb7rr = leftConnectedMARewriteRule nb7ain nb7aout nb7bin nb7bout
    
    
    let Right nb8L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 0 [] [2] "eta",hyperedge 1 [2] [0,1] "nu"])
    let Right nb8Kin = hypergraph (set []) (set ([] :: [Hyperedge Int Int String]))
    let Right nb8Kout = hypergraph (set [0 :: Int, 1]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb8R = hypergraph (set [0,1]) (set [hyperedge 2 [] [0] "eta", hyperedge 3 [] [1] "eta"])
    let Right nb8ain = hypergraphMorphism nb8Kin nb8L (weakMap []) (weakMap [])
    let Right nb8aout = hypergraphMorphism nb8Kout nb8L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb8bin =  hypergraphMorphism nb8Kin nb8R (weakMap []) (weakMap [])
    let Right nb8bout =  hypergraphMorphism nb8Kout nb8R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb8rr = leftConnectedMARewriteRule nb8ain nb8aout nb8bin nb8bout
    
    
    let Right nb9L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 0 [0,1] [4] "mu", hyperedge 1 [4] [2,3] "nu"])
    let Right nb9Kin = hypergraph (set [0,1 :: Int]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb9Kout = hypergraph (set [2 :: Int, 3]) (set ([] :: [Hyperedge Int Int String]))
    let Right nb9R = hypergraph (set [0,1,2,3,4,5,6,7]) (set [hyperedge 2 [0] [4,5] "nu", hyperedge 3 [1] [6,7] "nu", hyperedge 4 [4,6] [2] "mu", hyperedge 5 [5,7] [3] "mu"])
    let Right nb9ain = hypergraphMorphism nb9Kin nb9L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb9aout = hypergraphMorphism nb9Kout nb9L (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb9bin =  hypergraphMorphism nb9Kin nb9R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb9bout =  hypergraphMorphism nb9Kout nb9R (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb9rr = leftConnectedMARewriteRule nb9ain nb9aout nb9bin nb9bout
    
    
    let Right nb10L = hypergraph (set [0 :: Int]) (set [hyperedge 0 [] [0] "eta", hyperedge 1 [0] [] "epsilon"])
    let Right nb10Kin = hypergraph (set []) (set ([] :: [Hyperedge Int Int String]))
    let Right nb10Kout = hypergraph (set []) (set ([] :: [Hyperedge Int Int String]))
    let Right nb10R = hypergraph (set []) (set [])
    let Right nb10ain = hypergraphMorphism nb10Kin nb10L (weakMap []) (weakMap [])
    let Right nb10aout = hypergraphMorphism nb10Kout nb10L (weakMap []) (weakMap [])
    let Right nb10bin =  hypergraphMorphism nb10Kin nb10R (weakMap []) (weakMap [])
    let Right nb10bout =  hypergraphMorphism nb10Kout nb10R (weakMap []) (weakMap [])
    let Right nb10rr = leftConnectedMARewriteRule nb10ain nb10aout nb10bin nb10bout
    
    let rules = set [nb3rr] -- nb1rr, nb2rr, nb3rr, nb4rr, nb5rr, nb6rr, nb7rr, nb8rr, nb9rr, nb10rr
    
    let precriticalpairs = enumeratePreCriticalPairs rules
    putStrLn $ show $ cardinal precriticalpairs
    putStrLn $ show $ simplify $  precriticalpairs
    
    putStrLn "finished"