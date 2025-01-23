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
    let faultyMACospan = maCospan h' (unsafeHypergraphMorphism (weakMap [(0,0)]) (weakMap []) h') (unsafeHypergraphMorphism (weakMap [(1,1)]) (weakMap []) h')
    let validMACospan = maCospan h' (unsafeHypergraphMorphism (weakMap [(0,0)]) (weakMap []) h') (unsafeHypergraphMorphism (weakMap [(1,2)]) (weakMap []) h')
    putStrLn $ show $ faultyMACospan
    putStrLn $ show $ validMACospan