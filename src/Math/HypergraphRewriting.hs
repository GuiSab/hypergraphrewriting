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

module Math.HypergraphRewriting
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
    -- ** Getters
    onVertices,
    onHyperedges,
    targetHypergraph,
    
    -- ** MA-Cospan
    isMonogamous,
    isAcyclic,
    MACospan,
    MACospanError(..),
    maCospan,
    unsafeMACospan,
    -- ** Getters
    underlyingHypergraph,
    inputInterface,
    outputInterface,
    
    -- ** Left connected MA rewrite rule
    LeftConnectedMARewriteRule,
    isStronglyConnected,
    leftConnectedMARewriteRule,
    LeftConnectedMARewriteRuleError(..),
    unsafeLeftConnectedMARewriteRule,
    -- ** Getters
    leftHandSideInputNodes,
    leftHandSideOutputNodes,
    rightHandSideInputNodes,
    rightHandSideOutputNodes,
    leftCospan,
    rightCospan,
    
    
    -- * Convex match
    isConvexMatch,
    
    -- * Categorical constructions
    pushout,
    pushoutComplement,
    dpo,
    coequalize,
    coproduct,
    
    -- * Enumerate pre-critical pairs
    combinations,
    partialPermutations,
    enumerateKMatchingOnBipartiteCompleteGraph,
    enumerateMatchingOnBipartiteCompleteGraph,
    enumeratePreCriticalPairs,
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
   
    
    import              GHC.Generics

    
    -- HYPERGRAPHS
    
    -- | An 'Hyperedge' is composed of a label in a signature 's', a list of source vertices, a list of target vertices and a hyperedge identifier.
    --
    -- The identifier of an hyperedge should be unique among hyperedges with the same label in a given hypergraph.
    data Hyperedge n e s = Hyperedge{
                            labelHyperedge :: s,
                            sourceHyperedge :: [n],
                            targetHyperedge :: [n],
                            idHyperedge :: e
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
                               | SeveralHyperedgesWithSameId (Hyperedge n e s) (Hyperedge n e s)
                               deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
    
    -- | Smart constructor of 'Hypergraph'.
    hypergraph :: (Eq n, Eq e, Eq s) => Set n -> Set (Hyperedge n e s) -> Either (HypergraphError n e s) (Hypergraph n e s)
    hypergraph ns es
        | not $ Set.null $ unknownSources = Left $ UnknownVertex $ anElement unknownSources
        | not $ Set.null $ unknownTargets = Left $ UnknownVertex $ anElement unknownTargets
        | not $ Set.null $ incompatibleHyperedges = Left $ uncurry IncompatibleArity $ anElement incompatibleHyperedges
        | not $ Set.null $ duplicateId = Left $ uncurry SeveralHyperedgesWithSameId $ anElement duplicateId
        | otherwise = Right Hypergraph{vertices=ns, hyperedges=es}
        where
            unknownSources = (set $ Set.concat $ sourceHyperedge <$> es) |-| ns
            unknownTargets = (set $ Set.concat $ targetHyperedge <$> es) |-| ns
            incompatibleHyperedges = [(e1,e2) | e1 <- es, e2 <- es, labelHyperedge e1 == labelHyperedge e2 && (arity e1 /= arity e2 || coarity e1 /= coarity e2)]
            duplicateId = [(e1,e2) | e1 <- es, e2 <- es, labelHyperedge e1 == labelHyperedge e2, idHyperedge e1 == idHyperedge e2, e1 /= e2]

    -- | Unsafe constructor of 'Hypergraph', does not check the 'Hypergraph' structure.
    unsafeHypergraph :: Set n -> Set (Hyperedge n e s) -> Hypergraph n e s
    unsafeHypergraph n e = Hypergraph{vertices=n, hyperedges=e}
    
    
    
    -- HYPERGRAPH MORPHISMS
    
    

    data HypergraphMorphism n1 e1 n2 e2 s = HypergraphMorphism {
                                                   onVertices :: Map n1 n2,
                                                   onHyperedges :: Map (Hyperedge n1 e1 s) (Hyperedge n2 e2 s),
                                                   targetHypergraph :: Hypergraph n2 e2 s
                                                   } deriving (Eq, Generic, PrettyPrint, Simplifiable)

    data HypergraphMorphismError n1 e1 n2 e2 s = IncompatibleSource (Hyperedge n1 e1 s)
                                               | IncompatibleTarget (Hyperedge n1 e1 s)
                                               | IncompatibleLabels (Hyperedge n1 e1 s)
                                               | MissingEdge (Hyperedge n1 e1 s)
                                               | MissingVertex n1
                                               deriving (Eq, Show, Generic, PrettyPrint)

    -- | Smart constructor for 'HypergraphMorphism'.
    hypergraphMorphism :: (Eq n1, Eq e1, Eq n2, Eq e2, Eq s) => Hypergraph n1 e1 s -> Hypergraph n2 e2 s -> Map n1 n2 -> Map (Hyperedge n1 e1 s) (Hyperedge n2 e2 s) -> Either (HypergraphMorphismError n1 e1 n2 e2 s) (HypergraphMorphism n1 e1 n2 e2 s)
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
        

    unsafeHypergraphMorphism :: Map n1 n2 -> Map (Hyperedge n1 e1 s) (Hyperedge n2 e2 s) -> Hypergraph n2 e2 s -> HypergraphMorphism n1 e1 n2 e2 s
    unsafeHypergraphMorphism onns ones thg = HypergraphMorphism{onVertices=onns, onHyperedges=ones, targetHypergraph = thg}

    instance (Show n1, Show e1, Show n2, Show e2, Show s) => Show (HypergraphMorphism n1 e1 n2 e2 s) where
        show hgh = "(unsafeHypergraphMorphism "++(show $ onVertices hgh)++" "++(show $ onHyperedges hgh)++ " " ++ (show $ targetHypergraph hgh) ++")"
    
    
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
                            inputInterface :: HypergraphMorphism n e n e s,
                            outputInterface :: HypergraphMorphism n e n e s
                          } deriving (Eq, Generic, PrettyPrint, Simplifiable)
                          
    -- | An error when constructing a 'MACospan'.
    data MACospanError n = NotMonogamous
                         | NotAcyclic
                         | IncompatibleInterfaces
                         | InputNodesIsNotInDegreeZero
                         | OutputNodesIsNotOutDegreeZero
                         | InputInterfaceIsNotDiscrete
                         | OutputInterfaceIsNotDiscrete
                         | InputInterfaceIsNotMono
                         | OutputInterfaceIsNotMono
                         deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
       
    -- | Smart constructor for 'MACospan'.
    maCospan :: (Eq n, Eq e, Eq s) => HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> Either (MACospanError n) (MACospan n e s)
    maCospan inInterface outInterface
        | targetHypergraph inInterface /= targetHypergraph outInterface = Left IncompatibleInterfaces
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
            hg = targetHypergraph inInterface
            faultyInputNode = [n | n <- vertices hg, inDegree hg n == 0] /= (Map.values $ onVertices inInterface)
            faultyOutputNode = [n | n <- vertices hg, outDegree hg n == 0] /= (Map.values $ onVertices outInterface)
            isDiscrete gh_ = Map.null $ onHyperedges gh_
            isMono gh_ = null $ [(k1,k2) | (k1,v1) <- al, (k2,v2) <- al, k1 /= k2 && v1 == v2]
                where
                    al = Map.mapToList $ onVertices gh_
    
    -- | Unsafe constructor for 'MACospan'.
    unsafeMACospan :: Hypergraph n e s -> HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> MACospan n e s
    unsafeMACospan hg inInterface outInterface = MACospan {underlyingHypergraph = hg, inputInterface = inInterface, outputInterface = outInterface} 
    
    instance (Show n, Show e, Show s) => Show (MACospan n e s) where
        show x = "(unsafeMACospan "++(show $ underlyingHypergraph x)++" "++(show $ inputInterface x)++" "++(show $ outputInterface x)++")"
    
    
    
    
    
    -- REWRITE RULES
    
    
    
    
    -- | A MA-rewrite rule is left connected if its left-hand side is strongly connected.
    data LeftConnectedMARewriteRule n e s = LeftConnectedMARewriteRule {
                                    leftHandSideInputNodes :: HypergraphMorphism n e n e s,
                                    leftHandSideOutputNodes :: HypergraphMorphism n e n e s,
                                    rightHandSideInputNodes :: HypergraphMorphism n e n e s,
                                    rightHandSideOutputNodes :: HypergraphMorphism n e n e s
                                }
                                deriving (Eq, Generic, PrettyPrint, Simplifiable)
    
    -- | The 'MACospan' should really be acyclic, otherwise this function loops indefinitely.
    isStronglyConnected :: (Eq n) => MACospan n e s -> Bool
    isStronglyConnected macospan = Set.and $ [dfs o i | i <- Map.values (onVertices $ inputInterface macospan), o <- Map.values (onVertices $ outputInterface macospan)]
        where
            dfs targetNode currentNode
                | currentNode == targetNode = True
                | otherwise = Set.or $ [or $ dfs targetNode <$> targetHyperedge e | e <- hyperedges (underlyingHypergraph macospan), currentNode `elem` sourceHyperedge e]
    
    -- | An error when building a 'LeftConnectedMARewriteRule'.
    data LeftConnectedMARewriteRuleError n = LeftHandSideIsNotMACospan (MACospanError n)
                                           | RightHandSideIsNotMACospan (MACospanError n)
                                           | LeftHandSideIsNotStronglyConnected
                                           deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
                         
    -- | Smart constructor for 'LeftConnectedMARewriteRule'.
    leftConnectedMARewriteRule :: (Eq n, Eq e, Eq s) => HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> Either (LeftConnectedMARewriteRuleError n) (LeftConnectedMARewriteRule n e s)
    leftConnectedMARewriteRule lhsInputNodes lhsOutputNodes rhsInputNodes rhsOutputNodes
        | null maCospanInput = Left $ LeftHandSideIsNotMACospan errL
        | null maCospanOutput = Left $ RightHandSideIsNotMACospan errR
        | not $ isStronglyConnected maCospanIn2 = Left LeftHandSideIsNotStronglyConnected
        | otherwise = Right $ LeftConnectedMARewriteRule{leftHandSideInputNodes = lhsInputNodes, leftHandSideOutputNodes = lhsOutputNodes, rightHandSideInputNodes = rhsInputNodes, rightHandSideOutputNodes = rhsOutputNodes}
        where
            maCospanInput = maCospan lhsInputNodes lhsOutputNodes
            Left errL = maCospanInput
            Right maCospanIn2 = maCospanInput
            maCospanOutput = maCospan rhsInputNodes rhsOutputNodes
            Left errR = maCospanOutput
    
    unsafeLeftConnectedMARewriteRule :: HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> HypergraphMorphism n e n e s -> LeftConnectedMARewriteRule n e s
    unsafeLeftConnectedMARewriteRule lhsInputNodes lhsOutputNodes rhsInputNodes rhsOutputNodes = LeftConnectedMARewriteRule{leftHandSideInputNodes = lhsInputNodes, leftHandSideOutputNodes = lhsOutputNodes, rightHandSideInputNodes = rhsInputNodes, rightHandSideOutputNodes = rhsOutputNodes}
    
    
    instance (Show n, Show e, Show s) => Show (LeftConnectedMARewriteRule n e s) where
        show x = "(unsafeLeftConnectedMARewriteRule "++(show $ leftHandSideInputNodes x)++" "++(show $ leftHandSideOutputNodes x)++" "++(show $ rightHandSideInputNodes x)++" "++(show $ rightHandSideOutputNodes x)++")"
    
    
    leftCospan :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s
    leftCospan rr = unsafeMACospan (targetHypergraph $ leftHandSideInputNodes rr) (leftHandSideInputNodes rr) (leftHandSideOutputNodes rr)
    
    rightCospan :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s
    rightCospan rr = unsafeMACospan (targetHypergraph $ rightHandSideInputNodes rr) (rightHandSideInputNodes rr) (rightHandSideOutputNodes rr)
    
    -- CONVEX MATCH 
    
    -- | The source and target hypergraphs of the matching should be monogamous acyclic, otherwise this function loops indefinitely. Not optimized yet.
    isConvexMatch :: (Eq n1, Eq e1, Eq n2, Eq e2, Eq s) => HypergraphMorphism n1 e1 n2 e2 s-> Bool
    isConvexMatch matching
        | not $ isMono (onVertices matching) = False
        | not $ isMono (onHyperedges matching) = False
        | otherwise = Set.and $ pathInImage <$> allPaths
        where
            isMono m = null $ [(k1,k2) | (k1,v1) <- al, (k2,v2) <- al, k1 /= k2 && v1 == v2]
                where
                    al = Map.mapToList m
            imageHyperedges = image $ onHyperedges matching
            imageNodes = image $ onVertices matching
            g = targetHypergraph matching
            paths s t
                | s == t = set [[]]
                | otherwise = [(e:p) | e <- hyperedges g, s `elem` (sourceHyperedge e), x <- set $ targetHyperedge e, p <- paths x t]
            allPaths = Set.concat2  $ [paths s t | s <- imageNodes, t <- imageNodes]
            pathInImage p = and $ (\x -> x `isIn` imageHyperedges) <$> p
    
    
    
    -- PUSHOUT
            
            
    -- | Given a ma-cospan i -> L <- j, a ma-cospan n -> G <- m and a convex match f : L -> G, return the pushout complement (i -> C <- j,n -> C <- m) making the following diagram commute:
    --  L <- i+j
    -- f|     |
    --  v ┐   v
    --  G <-  C
    --   ^    ^
    --    \   |
    --     n+m  
    --
    -- Note that the cospan returned are not MACospans, this function is only a single step in the double pushout function.
    pushoutComplement :: (Eq n, Eq e, Eq s) => MACospan n e s -> MACospan n e s -> HypergraphMorphism n e n e s -> (MACospan n e s,MACospan n e s)
    pushoutComplement csp1 csp2 match = (unsafeMACospan complement inputInterface1 outputInterface1, unsafeMACospan complement inputInterface2 outputInterface2)
        where
            nodesToKeep = (image $ onVertices (inputInterface csp1)) ||| (image $ onVertices (outputInterface csp1))
            nodesToRemove = (vertices $ underlyingHypergraph csp1) |-| nodesToKeep
            nodesRemaining = (vertices $ underlyingHypergraph csp2) |-| ([(onVertices match) |!| n | n <- nodesToRemove])
            hyperedgesRemaining = (hyperedges $ underlyingHypergraph csp2) |-| (image $ onHyperedges match)
            complement = unsafeHypergraph nodesRemaining hyperedgesRemaining
            inputInterface1 = unsafeHypergraphMorphism ((onVertices match) |.| (onVertices (inputInterface csp1))) (weakMap []) complement
            outputInterface1 = unsafeHypergraphMorphism ((onVertices match) |.| (onVertices (outputInterface csp1))) (weakMap []) complement
            inputInterface2 = unsafeHypergraphMorphism (onVertices (inputInterface csp2)) (weakMap []) complement
            outputInterface2 = unsafeHypergraphMorphism (onVertices (outputInterface csp2)) (weakMap []) complement
            
    -- | Given a ma-cospan i -> R <- j, a cospan n -> C <- m, a cospan i -> C <- j, return the pushout n -> P <- m such that the following diagram commute:
    -- i+j -> R
    --  |     |
    --  v   ┌ v
    --  C ->  P
    --   ^    ^
    --    \   |
    --     n+m  
    pushout :: (Eq n, Eq e, Eq s) => MACospan n e s -> MACospan n e s -> MACospan n e s -> MACospan (Either n n) (Either e e) s
    pushout cspR cspC1 cspC2 = unsafeMACospan pushout' inputInterfacePushout outputInterfacePushout
        where
            nodesToAdd = ((vertices $ underlyingHypergraph cspR) |-| ((image $ onVertices (inputInterface cspR)) ||| (image $ onVertices (outputInterface cspR))))
            nodesToAdd' = Left <$> nodesToAdd
            interfaceOnVerticesR = (onVertices (inputInterface cspR)) `Map.union` (onVertices (outputInterface cspR))
            interfaceOnVerticesC2 = (onVertices (inputInterface cspC2)) `Map.union` (onVertices (outputInterface cspC2))
            hyperedgesToAdd = [hyperedge (Left $ idHyperedge e) ([if x `Set.elem` nodesToAdd then Left x else Right (interfaceOnVerticesC2 |!| ((pseudoInverse interfaceOnVerticesR) |!| x)) | x <- sourceHyperedge e]) ([if x `Set.elem` nodesToAdd then Left x else Right (interfaceOnVerticesC2 |!| ((pseudoInverse interfaceOnVerticesR) |!| x)) | x <- targetHyperedge e]) (labelHyperedge e)| e <- hyperedges $ underlyingHypergraph cspR]
            
            nodesToKeep = Right <$> (vertices $ underlyingHypergraph cspC2)
            hyperedgesToKeep = [hyperedge (Right $ idHyperedge e) (Right <$> sourceHyperedge e) (Right <$> targetHyperedge e) (labelHyperedge e)| e <- hyperedges $ underlyingHypergraph cspC2]
            
            pushout' = unsafeHypergraph (nodesToKeep ||| nodesToAdd') (hyperedgesToKeep ||| hyperedgesToAdd)
            inputInterfacePushout = unsafeHypergraphMorphism (weakMap [(Right k,Right v) | (k,v) <- Map.mapToList (onVertices $ inputInterface cspC1)]) (weakMap [(hyperedge (Right $ idHyperedge k) (Right <$> sourceHyperedge k) (Right <$> targetHyperedge k) (labelHyperedge k),hyperedge (Right $ idHyperedge v) (Right <$> sourceHyperedge v) (Right <$> targetHyperedge v) (labelHyperedge v)) | (k,v) <- Map.mapToList (onHyperedges $ inputInterface cspC1)]) pushout'
            outputInterfacePushout = unsafeHypergraphMorphism (weakMap [(Right k,Right v) | (k,v) <- Map.mapToList (onVertices $ outputInterface cspC1)]) (weakMap [(hyperedge (Right $ idHyperedge k) (Right <$> sourceHyperedge k) (Right <$> targetHyperedge k) (labelHyperedge k),hyperedge (Right $ idHyperedge v) (Right <$> sourceHyperedge v) (Right <$> targetHyperedge v) (labelHyperedge v)) | (k,v) <- Map.mapToList (onHyperedges $ outputInterface cspC1)]) pushout'
            
    -- | Given a rewrite rule, a ma-cospan n -> G <- m and a convex match f : L -> G, return the double pushout application of the rewrite rule (n -> P <- m) making the following diagram commute:
    --  L <- i+j -> R
    -- f|     |     |
    --  v ┐   v   ┌ v
    --  G  <- C ->  P
    --   ^    ^     ^
    --    \   |   /
    --       n+m  
    dpo :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s -> HypergraphMorphism n e n e s -> MACospan (Either n n) (Either e e) s
    dpo rr csp match = pushout (rightCospan rr) c2 c1
        where
            (c1,c2) = pushoutComplement (leftCospan rr) csp match
            
            
            
            
    -- COEQUALIZER and COPRODUCT
    
    -- | Given two hypergraph morphisms f : G_1 -> G_2 and g : G_1 -> G_2 respecting the matching property, return a hypergraph morphism c : G_2 -> G such that G_1 => G_2 -> G is a coequalizer diagram.
    --
    -- The two given hypergraph morphism should have the same source and target.
    coequalize :: (Eq n1, Eq e1, Eq n2, Eq e2, Eq s) => HypergraphMorphism n1 e1 n2 e2 s -> HypergraphMorphism n1 e1 n2 e2 s -> HypergraphMorphism n2 e2 n2 e2 s
    coequalize f g = morph
        where
            nodesGluedByG = image $ onVertices g
            edgesGluedByG = image $ onHyperedges g
            glueNodes v = if v `isIn` nodesGluedByG then (onVertices f) |!| ((pseudoInverse $ onVertices g) |!| v) else v
            glueHyperedges e = if e `isIn` edgesGluedByG then (onHyperedges f) |!| ((pseudoInverse $ onHyperedges g) |!| e) else e
            newNodes = glueNodes <$> (vertices $ targetHypergraph f)  
            transformHyperedge e = hyperedge (idHyperedge e) (glueNodes <$> sourceHyperedge e) (glueNodes <$> targetHyperedge e) (labelHyperedge e)
            newHyperedges = [transformHyperedge e | e <- glueHyperedges <$> (hyperedges $ targetHypergraph f)]
            newHypergraph = unsafeHypergraph newNodes newHyperedges
            morph = unsafeHypergraphMorphism (memorizeFunction glueNodes (vertices $ targetHypergraph f)) (memorizeFunction (transformHyperedge.glueHyperedges) (hyperedges $ targetHypergraph f)) newHypergraph
            
            
    -- | Given two hypergraphs, return the injections to their coproduct.
    coproduct :: (Eq n, Eq e, Eq s) => Hypergraph n e s -> Hypergraph n e s -> (HypergraphMorphism n e (Either n n) (Either e e) s, HypergraphMorphism n e (Either n n) (Either e e) s)
    coproduct g1 g2 = (leftInjection,rightInjection)
        where
            transformLeftVertices = Left 
            transformRightVertices = Right 
            transformLeftHyperedges e = hyperedge (Left $ idHyperedge e) (Left <$> sourceHyperedge e) (Left <$> targetHyperedge e) (labelHyperedge e)
            transformRightHyperedges e = hyperedge (Right $ idHyperedge e) (Right <$> sourceHyperedge e) (Right <$> targetHyperedge e) (labelHyperedge e)
            coprod = unsafeHypergraph ((transformLeftVertices <$> vertices g1) ||| (transformRightVertices <$> vertices g2)) ((transformLeftHyperedges <$> hyperedges g1) ||| (transformRightHyperedges <$> hyperedges g2))
            leftInjection = unsafeHypergraphMorphism (memorizeFunction transformLeftVertices (vertices g1)) (memorizeFunction transformLeftHyperedges (hyperedges g1)) coprod
            rightInjection = unsafeHypergraphMorphism (memorizeFunction transformRightVertices (vertices g2)) (memorizeFunction transformRightHyperedges (hyperedges g2)) coprod
            
    -- ENUMERATE PRE-CRITICAL PAIRS

    
    -- | Given a set E and a number k, return the different combinations of size k in E.
    combinations :: (Eq e) => Set e -> Int -> Set (Set e)
    combinations es 0 = set [set []]
    combinations es k = set $ Set.setToList $ [Set.insert e comb | comb <- combinations es (k-1), e <- es, not $ e `isIn` comb]
    
    -- | Given a set E and a number k, return the different partial permutations of size k in E.
    partialPermutations :: (Eq e) => Set e -> Int -> Set [e]
    partialPermutations es 0 = set [[]]
    partialPermutations es k = [e:pperm  | e <- es, pperm <- partialPermutations (Set.delete e es) (k-1)]
    
    -- | Given two sets of size a and b, we consider the complete bipartite graph K_{a,b}. This function enumerates all matchings with k edges on K_{a,b}. There are k! C(k,n1) C(k,n2) matchings of size k.
    enumerateKMatchingOnBipartiteCompleteGraph :: (Eq a, Eq b) => Set a -> Set b -> Int -> Set (Set (a,b))
    enumerateKMatchingOnBipartiteCompleteGraph setA setB k = [set $ zip (setToList a) b | a <- combinations setA k, b <- partialPermutations setB k]
    
    -- | Given two sets of size a and b, we consider the complete bipartite graph K_{a,b}. This function enumerates all non-empty matchings on K_{a,b}. There are \sum_{1 \leq k \leq min(a,b)} k! C(k,n1) C(k,n2) non-empty matchings.
    enumerateMatchingOnBipartiteCompleteGraph :: (Eq a, Eq b) => Set a -> Set b -> Set (Set (a,b))
    enumerateMatchingOnBipartiteCompleteGraph setA setB = Set.unions $ enumerateKMatchingOnBipartiteCompleteGraph setA setB <$> [0.. (min (cardinal setA) (cardinal setB))]
            


    -- | Given matchings for every label, combine them in every possible way to product every global matchings.
    combineDifferentMatchings :: (Eq e) =>  Set (Set (Set e)) -> Set (Set e)
    combineDifferentMatchings setOfMatchingsForEachLabel = Set.filter (not.(Set.null)) $ Set.unions <$> (cartesianProductOfSets $ setToList setOfMatchingsForEachLabel)


    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Left x) = Nothing
    eitherToMaybe (Right x) = Just x
    
    -- | Enumerate all pre-critical pairs of a given set of left connected ma rewrite rules. Not optimized algorithm.
    enumeratePreCriticalPairs :: (Simplifiable n, Eq n, Eq e, Eq s) => Set (LeftConnectedMARewriteRule n e s) -> Set (LeftConnectedMARewriteRule n e s,LeftConnectedMARewriteRule n e s,(MACospan (Either n n) (Either e e) s))
    enumeratePreCriticalPairs rules = Set.concat2 $ (uncurry firstLoop) <$> pairsOfRules
        where
            pairsOfRules = [(r1,r2) | r1 <- rules, r2 <- rules]
            firstLoop r1 r2 = [(r1,r2,macspn) | macspn <- Set.catMaybes $ eitherToMaybe <$> (Set.concat2 $ secondLoop <$> hypergraphsToConsider)]
                where
                    l1 = targetHypergraph $ leftHandSideInputNodes r1
                    l2 = targetHypergraph $ leftHandSideInputNodes r2
                    labelsOfHyperedges = [labelHyperedge e | e <- hyperedges l1] ||| [labelHyperedge e | e <- hyperedges l2]
                    bisetsOfHypergraphsWithAGivenLabel = [([e | e <- hyperedges l1, labelHyperedge e == l],[e | e <- hyperedges l2, labelHyperedge e == l]) | l <- labelsOfHyperedges]
                    matchingsHyperedges = combineDifferentMatchings $ uncurry enumerateMatchingOnBipartiteCompleteGraph <$> bisetsOfHypergraphsWithAGivenLabel
                    pairOfHyperedgeToHyperedgeofPair (e1,e2) = hyperedge (idHyperedge e1, idHyperedge e2) (zip (sourceHyperedge e1) (sourceHyperedge e2)) (zip (targetHyperedge e1) (targetHyperedge e2)) (labelHyperedge e1)
                    completeHypergraph g = unsafeHypergraph (simplify $ vertices g ||| (set $ Set.concat [sourceHyperedge e ++ targetHyperedge e | e <- hyperedges g])) (hyperedges g)
                    hypergraphsToConsider = [completeHypergraph (unsafeHypergraph (set []) (pairOfHyperedgeToHyperedgeofPair <$> matchingE)) | matchingE <- matchingsHyperedges]
                    secondLoop g = thirdLoop coequalizerHypergraph <$> hypergraphsToGlueInterfaces
                        where
                            leftProjectHyperedge e = hyperedge (fst $ idHyperedge e) (fst <$> sourceHyperedge e) (fst <$> targetHyperedge e) (labelHyperedge e)
                            rightProjectHyperedge e = hyperedge (snd $ idHyperedge e) (snd <$> sourceHyperedge e) (snd <$> targetHyperedge e) (labelHyperedge e)
                            leftProjection = unsafeHypergraphMorphism pv pe l1
                                where
                                    pv = memorizeFunction fst (vertices g)
                                    pe = memorizeFunction leftProjectHyperedge (hyperedges g)
                            rightProjection = unsafeHypergraphMorphism pv pe l2
                                where
                                    pv = memorizeFunction snd (vertices g)
                                    pe = memorizeFunction rightProjectHyperedge (hyperedges g)
                            
                            (leftInjection, rightInjection) = coproduct (targetHypergraph leftProjection) (targetHypergraph rightProjection)
                            
                            leftComposite = unsafeHypergraphMorphism ((onVertices leftInjection) |.| (onVertices leftProjection)) ((onHyperedges leftInjection) |.| (onHyperedges leftProjection)) (targetHypergraph leftInjection)
                            rightComposite = unsafeHypergraphMorphism ((onVertices rightInjection) |.| (onVertices rightProjection)) ((onHyperedges rightInjection) |.| (onHyperedges rightProjection)) (targetHypergraph rightInjection)
                            
                            coequalizer = coequalize leftComposite rightComposite
                            coequalizerHypergraph = targetHypergraph coequalizer
                            
                            inDegreeZeroVertices = [v | v <- vertices coequalizerHypergraph, inDegree coequalizerHypergraph v == 0]
                            outDegreeZeroVertices = [v | v <- vertices coequalizerHypergraph, outDegree coequalizerHypergraph v == 0]
                            
                            i1 = inDegreeZeroVertices |&| (image $ (onVertices leftInjection) |.| (onVertices $ leftHandSideInputNodes r1))
                            i2 = inDegreeZeroVertices |&| (image $ (onVertices rightInjection) |.|(onVertices $ leftHandSideInputNodes r2))
                            o1 = outDegreeZeroVertices |&| (image $ (onVertices leftInjection) |.| (onVertices $ leftHandSideInputNodes r1))
                            o2 = outDegreeZeroVertices |&| (image $ (onVertices leftInjection) |.| (onVertices $ leftHandSideInputNodes r2))
                            
                            nodesMatchings = enumerateMatchingOnBipartiteCompleteGraph i1 o2 ||| enumerateMatchingOnBipartiteCompleteGraph i2 o1
                            
                            hypergraphsToGlueInterfaces = [unsafeHypergraph matchingN (set []) | matchingN <- nodesMatchings]
                            
                            thirdLoop s g' = candidateMACospan
                                where
                                    leftProjectHyperedge' e = hyperedge (fst $ idHyperedge e) (fst <$> sourceHyperedge e) (fst <$> targetHyperedge e) (labelHyperedge e)
                                    rightProjectHyperedge' e = hyperedge (snd $ idHyperedge e) (snd <$> sourceHyperedge e) (snd <$> targetHyperedge e) (labelHyperedge e)
                                    leftProjection' = unsafeHypergraphMorphism pv pe s
                                        where
                                            pv = memorizeFunction fst (vertices g')
                                            pe = memorizeFunction leftProjectHyperedge' (hyperedges g')
                                    rightProjection' = unsafeHypergraphMorphism pv pe s
                                        where
                                            pv = memorizeFunction snd (vertices g')
                                            pe = memorizeFunction rightProjectHyperedge' (hyperedges g')
                                            
                                    coequalizer' = coequalize leftProjection' rightProjection'
                                    coequalizerHypergraph' = targetHypergraph coequalizer'
                                    
                                    inDegreeZeroVertices' = [v | v <- vertices coequalizerHypergraph', inDegree coequalizerHypergraph' v == 0]
                                    outDegreeZeroVertices' = [v | v <- vertices coequalizerHypergraph', outDegree coequalizerHypergraph' v == 0]
                                    
                                    inInterface = unsafeHypergraphMorphism (memorizeFunction id inDegreeZeroVertices') (weakMap []) coequalizerHypergraph'
                                    outInterface = unsafeHypergraphMorphism (memorizeFunction id outDegreeZeroVertices') (weakMap []) coequalizerHypergraph'
                                    candidateMACospan = maCospan inInterface outInterface
                            
                            