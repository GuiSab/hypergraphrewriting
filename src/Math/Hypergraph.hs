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
    
    -- * Hypergraph morphism
    HypergraphMorphism,
    hypergraphMorphism,
    unsafeHypergraphMorphism,
    HypergraphMorphismError(..),
    
    -- ** MA-Cospan
    isMonogamous,
    isAcyclic,
    MACospan,
    MACospanError(..),
    maCospan,
    unsafeMACospan,
    

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

    
    -- HYPERGRAPHS
    
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
    
    
    
    -- HYPERGRAPH MORPHISMS
    
    

    data HypergraphMorphism n e s = HypergraphMorphism {
                                   onVertices :: Map n n,
                                   onHyperedges :: Map (Hyperedge n e s) (Hyperedge n e s),
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
        | otherwise = Right HypergraphMorphism{onVertices=onns, onHyperedges=ones, targetHypergraph = h'}
        where
            incoherentSources = [e | e <- keys' ones, ((onns |!|) <$> (sourceHyperedge e)) /= sourceHyperedge (ones |!| e)]
            incoherentTargets = [e | e <- keys' ones, ((onns |!|) <$> (targetHyperedge e)) /= targetHyperedge (ones |!| e)]
            incoherentLabels  = [e | e <- keys' ones, labelHyperedge e /= labelHyperedge (ones |!| e)]
            missingEdges      = (hyperedges h) |-| keys' ones
            missingVertices   = (vertices h) |-| keys' onns
        

    unsafeHypergraphMorphism :: Map n n -> Map (Hyperedge n e s) (Hyperedge n e s) -> Hypergraph n e s -> HypergraphMorphism n e s
    unsafeHypergraphMorphism onns ones thg = HypergraphMorphism{onVertices=onns, onHyperedges=ones, targetHypergraph = thg}

    instance (Show s, Show n, Show e) => Show (HypergraphMorphism n e s) where
        show hgh = "(unsafeHypergraphMorphism "++(show $ onVertices hgh)++" "++(show $ onHyperedges hgh)++ " " ++ (show $ targetHypergraph hgh) ++")"
    
    
    -- | The category of finite hypergraphs on a given signature.
    data FinHyp n e s = FinHyp deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
    
    instance (Eq n, Eq e, Eq s) => Morphism (HypergraphMorphism n e s) (Hypergraph n e s) where
        source hgh = Hypergraph {vertices = (domain.onVertices) hgh, hyperedges = (domain.onHyperedges) hgh}
        target = targetHypergraph
        (@) hgh2 hgh1 =  HypergraphMorphism {onVertices = (onVertices hgh2) |.| (onVertices hgh1), onHyperedges = (onHyperedges hgh2) |.| (onHyperedges hgh1), targetHypergraph = target hgh2}
    
    
    instance (Eq n, Eq e, Eq s) => Category (FinHyp n e s) (HypergraphMorphism n e s) (Hypergraph n e s) where
        identity _ hg = HypergraphMorphism {onVertices = (idFromSet.vertices) hg, onHyperedges = (idFromSet.hyperedges) hg, targetHypergraph = hg}
        ar _ s t = snd $ Set.catEither [hypergraphMorphism s t onv one | onv <- onvMaps, one <- oneMaps]
            where
                onvMaps = Map.enumerateMaps (vertices s) (vertices t)
                oneMaps = Map.enumerateMaps (hyperedges s) (hyperedges t)
    
    
    -- MA Cospans
    
    -- | The in-degree of a node n is the number of pairs (h,i) where h is an hyperedge and n is the i-th target.
    inDegree :: (Eq n, Eq e, Eq s) =>  Hypergraph n e s -> n -> Int
    inDegree hg n = sum [length [n | n' <- targetHyperedge e, n' == n] | e <- Set.setToList $ hyperedges hg]
    
    -- | The out-degree of a node n is the number of pairs (h,i) where h is an hyperedge and n is the i-th source.
    outDegree :: (Eq n, Eq e, Eq s) =>  Hypergraph n e s -> n -> Int
    outDegree hg n = sum [length [n | n' <- sourceHyperedge e, n' == n] | e <- Set.setToList $ hyperedges hg]
    
    -- | A hypergraph is monogamous if no node has in or out degree bigger than 1.
    isMonogamous :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Bool
    isMonogamous hg = Set.null $ ((inDegree hg <$> vertices hg) ||| (outDegree hg <$> vertices hg)) |-| (set [0,1])
            
    -- | A hypergraph is acyclic if it contains no cycle. A path is defined as a list of hyperedges e_i such that e_i has at least a target equal to a source of e_i+1.
    isAcyclic :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Bool
    isAcyclic hg = Set.and $ dfs [] <$> vertices hg
        where
            dfs alreadyVisited currentNode
                | currentNode `elem` alreadyVisited = False
                | otherwise = Set.and $ [and $ dfs (currentNode : alreadyVisited) <$> targetHyperedge e | e <- hyperedges hg, currentNode `elem` sourceHyperedge e]
                
                
    -- | A 'MACospan' is a monogamous acyclic hypergraph with interface. The input interface are all nodes with in-degree 0 and the output interface are all nodes with out-degree 0.
    -- 
    -- 'MACospan' is private, use smart constructor 'maCospan'.
    data MACospan n e s = MACospan {
                            underlyingHypergraph :: Hypergraph n e s,
                            inputInterface :: HypergraphMorphism n e s,
                            outputInterface :: HypergraphMorphism n e s
                          } deriving (Eq, Generic, PrettyPrint, Simplifiable)
                          
    -- | An error when constructing a 'MACospan'.
    data MACospanError n = NotMonogamous
                         | NotAcyclic
                         | InputNodesIsNotInDegreeZero
                         | OutputNodesIsNotOutDegreeZero
                         | InputInterfaceIsNotDiscrete
                         | OutputInterfaceIsNotDiscrete
                         | InputInterfaceIsNotMono
                         | OutputInterfaceIsNotMono
                         deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
       
    -- | Smart constructor for 'MACospan'.
    maCospan :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> Either (MACospanError n) (MACospan n e s)
    maCospan hg inInterface outInterface
        | not $ isMonogamous hg = Left NotMonogamous
        | not $ isAcyclic hg = Left NotAcyclic
        | not $ isDiscrete inInterface = Left InputInterfaceIsNotDiscrete
        | not $ isDiscrete outInterface = Left OutputInterfaceIsNotDiscrete
        | not $ isMono inInterface = Left InputInterfaceIsNotMono
        | not $ isMono outInterface = Left OutputInterfaceIsNotMono
        | faultyInputNode = Left $ InputNodesIsNotInDegreeZero 
        | faultyOutputNode = Left $ OutputNodesIsNotOutDegreeZero
        | otherwise = Right $ MACospan {underlyingHypergraph = hg, inputInterface = inInterface, outputInterface = outInterface} 
        where
            faultyInputNode = [n | n <- vertices hg, inDegree hg n == 0] /= (Map.values $ onVertices inInterface)
            faultyOutputNode = [n | n <- vertices hg, outDegree hg n == 0] /= (Map.values $ onVertices outInterface)
            isDiscrete gh_ = Map.null $ onHyperedges gh_
            isMono gh_ = null $ [(k1,k2) | (k1,v1) <- al, (k2,v2) <- al, k1 /= k2 && v1 == v2]
                where
                    al = Map.mapToList $ onVertices gh_
    
    -- | Unsafe constructor for 'MACospan'.
    unsafeMACospan :: Hypergraph n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> MACospan n e s
    unsafeMACospan hg inInterface outInterface = MACospan {underlyingHypergraph = hg, inputInterface = inInterface, outputInterface = outInterface} 
    
    instance (Show n, Show e, Show s) => Show (MACospan n e s) where
        show x = "(unsafeMACospan "++(show $ underlyingHypergraph x)++" "++(show $ inputInterface x)++" "++(show $ outputInterface x)++")"
    
    
    
    
    
    -- REWRITE RULES
    
    -- | A rewrite rule is a left hand side, a right hand side, an isomorphism between the interfaces of the left and right hand side.
    -- data RewriteRule n e s = RewriteRule {
                                    -- leftHandSide :: MACospan n e s,
                                    -- rightHandSide :: MACospan n e s,
                                    -- isomorphismInputInterface :: Map n n,
                                    -- isomorphismOutputInterface :: Map n n,
                                -- }
                                -- deriving (Eq, Generic, PrettyPrint, Simplifiable)
    
    
    
    
    -- enumeratePreCriticalPairs :: 