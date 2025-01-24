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