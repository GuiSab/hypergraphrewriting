{-# LANGUAGE MonadComprehensions #-}

module Main (main) where

import              Data.WeakSet.Safe
import qualified    Data.WeakSet                    as Set
import              Data.WeakMap.Safe
import              Math.HypergraphRewriting
import              Math.IO.PrettyPrint
import              Data.Simplifiable


main :: IO ()
main = do
    
    let Right nb1L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 'f' [0,1] [4] "mu",hyperedge 'g' [4,2] [3] "mu"])
    let Right nb1Kin = hypergraph (set [0 :: Int,1,2]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb1Kout = hypergraph (set [3 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb1R = hypergraph (set [0,1,2,3,4]) (set [hyperedge 'h' [1,2] [4] "mu",hyperedge 'i' [0,4] [3] "mu"])
    let Right nb1ain = hypergraphMorphism nb1Kin nb1L (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1aout = hypergraphMorphism nb1Kout nb1L (weakMap [(3,3)]) (weakMap [])
    let Right nb1bin =  hypergraphMorphism nb1Kin nb1R (weakMap [(0,0),(1,1),(2,2)]) (weakMap [])
    let Right nb1bout =  hypergraphMorphism nb1Kout nb1R (weakMap [(3,3)]) (weakMap [])
    let Right nb1rr = leftConnectedMARewriteRule nb1ain nb1aout nb1bin nb1bout
    
    
    let Right nb2L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 'f' [0] [4,3] "nu",hyperedge 'g' [4] [1,2] "nu"])
    let Right nb2Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb2Kout = hypergraph (set [1,2,3 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb2R = hypergraph (set [0,1,2,3,4]) (set [hyperedge 'h' [0] [1,4] "nu",hyperedge 'i' [4] [2,3] "nu"])
    let Right nb2ain = hypergraphMorphism nb2Kin nb2L (weakMap [(0,0)]) (weakMap [])
    let Right nb2aout = hypergraphMorphism nb2Kout nb2L (weakMap [(1,1),(2,2),(3,3)]) (weakMap [])
    let Right nb2bin =  hypergraphMorphism nb2Kin nb2R (weakMap [(0,0)]) (weakMap [])
    let Right nb2bout =  hypergraphMorphism nb2Kout nb2R (weakMap [(1,1),(2,2),(3,3)]) (weakMap [])
    let Right nb2rr = leftConnectedMARewriteRule nb2ain nb2aout nb2bin nb2bout
    
    
    let Right nb3L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [] [2] "eta",hyperedge 'g' [0,2] [1] "mu"])
    let Right nb3Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb3Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb3R = hypergraph (set [0]) (set [])
    let Right nb3ain = hypergraphMorphism nb3Kin nb3L (weakMap [(0,0)]) (weakMap [])
    let Right nb3aout = hypergraphMorphism nb3Kout nb3L (weakMap [(1,1)]) (weakMap [])
    let Right nb3bin =  hypergraphMorphism nb3Kin nb3R (weakMap [(0,0)]) (weakMap [])
    let Right nb3bout =  hypergraphMorphism nb3Kout nb3R (weakMap [(1,0)]) (weakMap [])
    let Right nb3rr = leftConnectedMARewriteRule nb3ain nb3aout nb3bin nb3bout
    
    
    let Right nb4L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [] [2] "eta",hyperedge 'g' [2,0] [1] "mu"])
    let Right nb4Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb4Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb4R = hypergraph (set [0]) (set [])
    let Right nb4ain = hypergraphMorphism nb4Kin nb4L (weakMap [(0,0)]) (weakMap [])
    let Right nb4aout = hypergraphMorphism nb4Kout nb4L (weakMap [(1,1)]) (weakMap [])
    let Right nb4bin =  hypergraphMorphism nb4Kin nb4R (weakMap [(0,0)]) (weakMap [])
    let Right nb4bout =  hypergraphMorphism nb4Kout nb4R (weakMap [(1,0)]) (weakMap [])
    let Right nb4rr = leftConnectedMARewriteRule nb4ain nb4aout nb4bin nb4bout   
    
    
    let Right nb5L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [0] [1,2] "nu",hyperedge 'g' [2] [] "epsilon"])
    let Right nb5Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb5Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb5R = hypergraph (set [0]) (set [])
    let Right nb5ain = hypergraphMorphism nb5Kin nb5L (weakMap [(0,0)]) (weakMap [])
    let Right nb5aout = hypergraphMorphism nb5Kout nb5L (weakMap [(1,1)]) (weakMap [])
    let Right nb5bin =  hypergraphMorphism nb5Kin nb5R (weakMap [(0,0)]) (weakMap [])
    let Right nb5bout =  hypergraphMorphism nb5Kout nb5R (weakMap [(1,0)]) (weakMap [])
    let Right nb5rr = leftConnectedMARewriteRule nb5ain nb5aout nb5bin nb5bout
    
    
    let Right nb6L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [0] [2,1] "nu",hyperedge 'g' [2] [] "epsilon"])
    let Right nb6Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb6Kout = hypergraph (set [1 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb6R = hypergraph (set [0]) (set [])
    let Right nb6ain = hypergraphMorphism nb6Kin nb6L (weakMap [(0,0)]) (weakMap [])
    let Right nb6aout = hypergraphMorphism nb6Kout nb6L (weakMap [(1,1)]) (weakMap [])
    let Right nb6bin =  hypergraphMorphism nb6Kin nb6R (weakMap [(0,0)]) (weakMap [])
    let Right nb6bout =  hypergraphMorphism nb6Kout nb6R (weakMap [(1,0)]) (weakMap [])
    let Right nb6rr = leftConnectedMARewriteRule nb6ain nb6aout nb6bin nb6bout
    
    
    let Right nb7L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [0,1] [2] "mu",hyperedge 'g' [2] [] "epsilon"])
    let Right nb7Kin = hypergraph (set [0 :: Int, 1]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb7Kout = hypergraph (set []) (set ([] :: [Hyperedge Int Char String]))
    let Right nb7R = hypergraph (set [0,1]) (set [hyperedge 'h' [0] [] "epsilon", hyperedge 'i' [1] [] "epsilon"])
    let Right nb7ain = hypergraphMorphism nb7Kin nb7L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb7aout = hypergraphMorphism nb7Kout nb7L (weakMap []) (weakMap [])
    let Right nb7bin =  hypergraphMorphism nb7Kin nb7R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb7bout =  hypergraphMorphism nb7Kout nb7R (weakMap []) (weakMap [])
    let Right nb7rr = leftConnectedMARewriteRule nb7ain nb7aout nb7bin nb7bout
    
    
    let Right nb8L = hypergraph (set [0 :: Int,1,2]) (set [hyperedge 'f' [] [2] "eta",hyperedge 'g' [2] [0,1] "nu"])
    let Right nb8Kin = hypergraph (set []) (set ([] :: [Hyperedge Int Char String]))
    let Right nb8Kout = hypergraph (set [0 :: Int, 1]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb8R = hypergraph (set [0,1]) (set [hyperedge 'h' [] [0] "eta", hyperedge 'i' [] [1] "eta"])
    let Right nb8ain = hypergraphMorphism nb8Kin nb8L (weakMap []) (weakMap [])
    let Right nb8aout = hypergraphMorphism nb8Kout nb8L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb8bin =  hypergraphMorphism nb8Kin nb8R (weakMap []) (weakMap [])
    let Right nb8bout =  hypergraphMorphism nb8Kout nb8R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb8rr = leftConnectedMARewriteRule nb8ain nb8aout nb8bin nb8bout
    
    
    let Right nb9L = hypergraph (set [0 :: Int,1,2,3,4]) (set [hyperedge 'f' [0,1] [4] "mu", hyperedge 'g' [4] [2,3] "nu"])
    let Right nb9Kin = hypergraph (set [0,1 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb9Kout = hypergraph (set [2 :: Int, 3]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb9R = hypergraph (set [0,1,2,3,4,5,6,7]) (set [hyperedge 'h' [0] [4,5] "nu", hyperedge 'i' [1] [6,7] "nu", hyperedge 'j' [4,6] [2] "mu", hyperedge 'k' [5,7] [3] "mu"])
    let Right nb9ain = hypergraphMorphism nb9Kin nb9L (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb9aout = hypergraphMorphism nb9Kout nb9L (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb9bin =  hypergraphMorphism nb9Kin nb9R (weakMap [(0,0),(1,1)]) (weakMap [])
    let Right nb9bout =  hypergraphMorphism nb9Kout nb9R (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb9rr = leftConnectedMARewriteRule nb9ain nb9aout nb9bin nb9bout
    
    
    let Right nb10L = hypergraph (set [0 :: Int]) (set [hyperedge 'f' [] [0] "eta", hyperedge 'g' [0] [] "epsilon"])
    let Right nb10Kin = hypergraph (set []) (set ([] :: [Hyperedge Int Char String]))
    let Right nb10Kout = hypergraph (set []) (set ([] :: [Hyperedge Int Char String]))
    let Right nb10R = hypergraph (set []) (set [])
    let Right nb10ain = hypergraphMorphism nb10Kin nb10L (weakMap []) (weakMap [])
    let Right nb10aout = hypergraphMorphism nb10Kout nb10L (weakMap []) (weakMap [])
    let Right nb10bin =  hypergraphMorphism nb10Kin nb10R (weakMap []) (weakMap [])
    let Right nb10bout =  hypergraphMorphism nb10Kout nb10R (weakMap []) (weakMap [])
    let Right nb10rr = leftConnectedMARewriteRule nb10ain nb10aout nb10bin nb10bout
    
    let rules = set [nb1rr, nb2rr, nb3rr, nb4rr, nb5rr, nb6rr, nb7rr, nb8rr, nb9rr, nb10rr]
    
    let precriticalpairs = simplify $ enumeratePreCriticalPairs rules
    putStrLn $ "Critical pairs of the rewriting system associated to the theory of non-commutative bimonoids\n"
    putStrLn $ "There are "++ (show $ cardinal precriticalpairs) ++ " critical pairs.\n\n "
    
    putStrLn $ Set.foldr (\str r -> r ++ "=====================================================\n\n" ++ str) "" [pprint 5 (targetHypergraph $ leftHandSideInputNodes r1) ++ "\n\n" ++ pprint 5 (targetHypergraph $ leftHandSideInputNodes r2) ++ "\n\n\n\n" ++ pprint 5 (underlyingHypergraph g) ++ "\n\n\n\n" | (r1,r2,g) <- precriticalpairs]
    
    
    putStrLn $ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn $ "An example where the gluing of nodes matters\\nn"
    
    let Right nb1L = hypergraph (set [0 :: Int,1,2,3]) (set [hyperedge 'f' [0] [1,2] "alpha", hyperedge 'g' [1] [3] "gamma"])
    let Right nb1Kin = hypergraph (set [0 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb1Kout = hypergraph (set [2,3 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb1R = hypergraph (set [0,1,2,3]) (set [hyperedge 'h' [0] [1,2] "xi", hyperedge 'i' [1] [3] "zeta"])
    let Right nb1ain = hypergraphMorphism nb1Kin nb1L (weakMap [(0,0)]) (weakMap [])
    let Right nb1aout = hypergraphMorphism nb1Kout nb1L (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb1bin =  hypergraphMorphism nb1Kin nb1R (weakMap [(0,0)]) (weakMap [])
    let Right nb1bout =  hypergraphMorphism nb1Kout nb1R (weakMap [(2,2),(3,3)]) (weakMap [])
    let Right nb1rr = leftConnectedMARewriteRule nb1ain nb1aout nb1bin nb1bout
    
    let Right nb2L = hypergraph (set [4 :: Int,5,6,7]) (set [hyperedge 'j' [4] [5] "gamma", hyperedge 'k' [5,6] [7] "beta"])
    let Right nb2Kin = hypergraph (set [4,6 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb2Kout = hypergraph (set [7 :: Int]) (set ([] :: [Hyperedge Int Char String]))
    let Right nb2R = hypergraph (set [4 :: Int,5,6,7]) (set [hyperedge 'j' [4] [5] "zeta", hyperedge 'k' [5,6] [7] "upsilon"])
    let Right nb2ain = hypergraphMorphism nb2Kin nb2L (weakMap [(4,4),(6,6)]) (weakMap [])
    let Right nb2aout = hypergraphMorphism nb2Kout nb2L (weakMap [(7,7)]) (weakMap [])
    let Right nb2bin =  hypergraphMorphism nb2Kin nb2R (weakMap [(4,4),(6,6)]) (weakMap [])
    let Right nb2bout =  hypergraphMorphism nb2Kout nb2R (weakMap [(7,7)]) (weakMap [])
    let Right nb2rr = leftConnectedMARewriteRule nb2ain nb2aout nb2bin nb2bout
    
    
    let rules = set [nb1rr, nb2rr]
    
    let precriticalpairs = simplify $ enumeratePreCriticalPairs rules
    
    putStrLn $ Set.foldr (\str r -> r ++ "=====================================================\n\n" ++ str) "" [pprint 5 (targetHypergraph $ leftHandSideInputNodes r1) ++ "\n\n" ++ pprint 5 (targetHypergraph $ leftHandSideInputNodes r2) ++ "\n\n\n\n" ++ pprint 5 (underlyingHypergraph g) ++ "\n\n\n\n" | (r1,r2,g) <- precriticalpairs]
    
    putStrLn "finished"