{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| Module  : HypergraphRewriting
Description : A hypergraph is a finite labeled directed hypergraphs in this library.
Copyright   : Juan Meleiro & Guillaume Sabbagh
License     : GPL-3
Maintainer  : guillaumesabbagh@protonmail.com
Stability   : experimental
Portability : portable

A hypergraph is a finite labeled directed hypergraphs in this library.

-}

module Math.Hypergraph
(
    -- * Hypergraph
    Hyperedge(..),
    hyperedge,
    arity,
    coarity,
    Hypergraph,
    -- ** Getters
    vertices,
    hyperedges,
    -- ** Smart constructors
    HypergraphError(..),
    hypergraph,
    unsafeHypergraph,
    -- ** Monogamous acyclic hypergraph
    isMonogamous,
    isAcyclic,
    
    -- * Hypergraph morphism
    HypergraphMorphism,
    hypergraphMorphism,
    unsafeHypergraphMorphism,
    HypergraphMorphismError(..),
)

where
    import              Data.WeakSet         (Set)
    import qualified    Data.WeakSet       as Set
    import              Data.WeakSet.Safe
    import              Data.WeakMap (Map)
    import qualified    Data.WeakMap as Map
    import              Data.WeakMap.Safe
    import              Data.Simplifiable
    import              Math.IO.PrettyPrint

    import              Math.FiniteCategory
   
    
    import              GHC.Generics

    
    
    
    -- | An 'Hyperedge' is composed of a label in a signature 's', a list of source vertices, a list of target vertices and a hyperedge identifier.
    data Hyperedge n e s = Hyperedge{
                            idHyperedge :: e,
                            sourceHyperedge :: [n],
                            targetHyperedge :: [n],
                            labelHyperedge :: s
                            }
                          deriving (Eq, Show, Generic, Simplifiable)
                          
    -- | Constructor for 'Hyperedge'.
    hyperedge :: e -> [n] -> [n] -> s -> Hyperedge n e s
    hyperedge e ss ts l = Hyperedge{
                            idHyperedge = e,
                            sourceHyperedge = ss,
                            targetHyperedge = ts,
                            labelHyperedge = l
                            }
                          
    -- | The arity of a hyperedge is its number of sources.
    arity :: Hyperedge n e s -> Int
    arity e = length $ sourceHyperedge e
    
    -- | The coarity of a hyperedge is its number of targets.
    coarity :: Hyperedge n e s -> Int
    coarity e = length $ targetHyperedge e
    
    
    instance (PrettyPrint n, PrettyPrint e, PrettyPrint s) => PrettyPrint (Hyperedge n e s) where
        pprint v a = (pprint v $ idHyperedge a)++ " : " ++(pprint v $ sourceHyperedge a)++"-"++(pprint v $ labelHyperedge a)++"->"++(pprint v $ targetHyperedge a)
        

    -- | A 'Hypergraph' is a set of vertices and a set of 'Hyperedge's.
    -- 
    -- 'Hypergraph' is private, use smart constructor 'hypergraph'.
    data Hypergraph n e s = Hypergraph {
                        vertices :: Set n, -- ^ The set of vertices of the hypergraph.
                        hyperedges :: Set (Hyperedge n e s) -- ^ The set of hyperedges of the hypergraph.
                        } deriving (Eq, Generic, PrettyPrint, Simplifiable)
    
    instance (Show n, Show e, Show s) => Show (Hypergraph n e s) where
        show g = "(unsafeHypergraph "++(show $ vertices g)++" "++(show $ hyperedges g)++")"
    
    
    -- | An error when constructing a hypergraph.
    data HypergraphError n e s = UnknownVertex n
                               | IncompatibleArity (Hyperedge n e s) (Hyperedge n e s)
                               deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
    
    -- | Smart constructor of 'Hypergraph'.
    hypergraph :: (Eq n, Eq s) => Set n -> Set (Hyperedge n e s) -> Either (HypergraphError n e s) (Hypergraph n e s)
    hypergraph ns es
        | not $ Set.null $ unknownSources = Left $ UnknownVertex $ anElement unknownSources
        | not $ Set.null $ unknownTargets = Left $ UnknownVertex $ anElement unknownTargets
        | not $ Set.null $ incompatibleHyperedges = Left $ uncurry IncompatibleArity $ anElement incompatibleHyperedges
        | otherwise = Right Hypergraph{vertices=ns, hyperedges=es}
        where
            unknownSources = (set $ Set.concat $ sourceHyperedge <$> es) |-| ns
            unknownTargets = (set $ Set.concat $ targetHyperedge <$> es) |-| ns
            incompatibleHyperedges = [(e1,e2) | e1 <- es, e2 <- es, labelHyperedge e1 == labelHyperedge e2 && (arity e1 /= arity e2 || coarity e1 /= coarity e2)]

    -- | Unsafe constructor of 'Hypergraph', does not check the 'Hypergraph' structure.
    unsafeHypergraph :: Set n -> Set (Hyperedge n e s) -> Hypergraph n e s
    unsafeHypergraph n e = Hypergraph{vertices=n, hyperedges=e}
    
    -- | A hypergraph is monogamous if no node has in or out degree bigger than 1.
    isMonogamous :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Bool
    isMonogamous hg = Set.null $ ((indegree <$> vertices hg) ||| (outdegree <$> vertices hg)) |-| (set [0,1])
        where
            indegree n = sum [length [n | n' <- sourceHyperedge e, n' == n] | e <- Set.setToList $ hyperedges hg]
            outdegree n = sum [length [n | n' <- sourceHyperedge e, n' == n] | e <- Set.setToList $ hyperedges hg]
            
    -- | A hypergraph is acyclic if it contains no cycle. A path is defined as a list of hyperedges e_i such that e_i has at least a target equal to a source of e_i+1.
    isAcyclic :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Bool
    isAcyclic hg = Set.and $ dfs [] <$> vertices hg
        where
            dfs alreadyVisited currentNode
                | currentNode `elem` alreadyVisited = False
                | otherwise = Set.and $ [and $ dfs (currentNode : alreadyVisited) <$> targetHyperedge e | e <- hyperedges hg, currentNode `elem` sourceHyperedge e]
                
    
    

    data HypergraphMorphism n e s = HypergraphMorphism {
                                   onvertices :: Map n n,
                                   onhyperedges :: Map (Hyperedge n e s) (Hyperedge n e s),
                                   targetHypergraph :: Hypergraph n e s
                                   } deriving (Eq, Generic, PrettyPrint, Simplifiable)

    data HypergraphMorphismError n e s = IncompatibleSource (Hyperedge n e s)
                                       | IncompatibleTarget (Hyperedge n e s)
                                       | IncompatibleLabels (Hyperedge n e s)
                                       | MissingEdge (Hyperedge n e s)
                                       | MissingVertex n
                                       deriving (Eq, Show, Generic, PrettyPrint)

    -- | Smart constructor for 'HypergraphMorphism'.
    hypergraphMorphism :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Hypergraph n e s -> Map n n -> Map (Hyperedge n e s) (Hyperedge n e s) -> Either (HypergraphMorphismError n e s) (HypergraphMorphism n e s)
    hypergraphMorphism h h' onns ones
        | not $ Set.null $ incoherentSources = Left $ IncompatibleSource $ anElement incoherentSources
        | not $ Set.null $ incoherentTargets = Left $ IncompatibleTarget $ anElement incoherentTargets
        | not $ Set.null $ incoherentLabels  = Left $ IncompatibleLabels $ anElement incoherentLabels
        | not $ Set.null $ missingEdges      = Left $ MissingEdge        $ anElement missingEdges
        | not $ Set.null $ missingVertices   = Left $ MissingVertex      $ anElement missingVertices
        | otherwise = Right HypergraphMorphism{onvertices=onns, onhyperedges=ones, targetHypergraph = h'}
        where
            incoherentSources = [e | e <- keys' ones, ((onns |!|) <$> (sourceHyperedge e)) /= sourceHyperedge (ones |!| e)]
            incoherentTargets = [e | e <- keys' ones, ((onns |!|) <$> (targetHyperedge e)) /= targetHyperedge (ones |!| e)]
            incoherentLabels  = [e | e <- keys' ones, labelHyperedge e /= labelHyperedge (ones |!| e)]
            missingEdges      = (hyperedges h) |-| keys' ones
            missingVertices   = (vertices h) |-| keys' onns
        


    unsafeHypergraphMorphism :: Hypergraph n e s -> Hypergraph n e s -> Map n n -> Map (Hyperedge n e s) (Hyperedge n e s) -> HypergraphMorphism n e s
    unsafeHypergraphMorphism h h' onns ones = HypergraphMorphism{onvertices=onns, onhyperedges=ones, targetHypergraph = h'}
    
    
    -- | The category of finite hypergraphs on a given signature.
    data FinHyp n e s = FinHyp deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
        
    instance (Eq n, Eq e, Eq s) => Category (FinHyp n e s) (HypergraphMorphism n e s) (Hypergraph n e s) where
        identity _ hg = HypergraphMorphism {onvertices = (idFromSet.vertices) hg, onhyperedges = (idFromSet.hyperedges) hg, targetHypergraph = hg}
        ar _ s t = snd $ Set.catEither [hypergraphMorphism s t onv one | onv <- onvMaps, one <- oneMaps]
            where
                onvMaps = Map.enumerateMaps (vertices s) (vertices t)
                oneMaps = Map.enumerateMaps (hyperedges s) (hyperedges t)
    