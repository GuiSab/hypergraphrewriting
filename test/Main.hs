module Main (main) where

import              Data.WeakSet.Safe
import              Data.WeakMap.Safe
import              Math.Hypergraph


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
    putStrLn "finished"