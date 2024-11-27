module Main (main) where

import              Data.WeakSet         (Set)
import qualified    Data.WeakSet       as Set
import              Data.WeakSet.Safe
import              Math.Hypergraph


main :: IO ()
main = do
    let nodes = set $ [0,1,2]
    let edge1 = hyperedge 0 [0,1] [1,2] "f"
    let edge2 = hyperedge 0 [0,1] [1] "f"
    let edge3 = hyperedge 0 [0,1] [1] "g"
    let h1 = hypergraph nodes (set [edge1,edge2])
    let h2 = hypergraph nodes (set [edge1,edge3])
    putStrLn $ show $ h1
    putStrLn $ show $ h2