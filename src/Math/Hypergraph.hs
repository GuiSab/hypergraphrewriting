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
    
    -- ** Left connected MA rewrite rule
    LeftConnectedMARewriteRule,
    isStronglyConnected,
    leftConnectedMARewriteRule,
    LeftConnectedMARewriteRuleError(..),
    unsafeLeftConnectedMARewriteRule,
    leftCospan,
    rightCospan,
    
    -- * Convex match
    isConvexMatch,
    
    -- * Pushout
    pushout,
    pushoutComplement,
    dpo,
    
    -- * Equalizer
    
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
                         | IncompatibleInterfaces
                         | InputNodesIsNotInDegreeZero
                         | OutputNodesIsNotOutDegreeZero
                         | InputInterfaceIsNotDiscrete
                         | OutputInterfaceIsNotDiscrete
                         | InputInterfaceIsNotMono
                         | OutputInterfaceIsNotMono
                         deriving (Eq, Show, Generic, PrettyPrint, Simplifiable)
       
    -- | Smart constructor for 'MACospan'.
    maCospan :: (Eq n, Eq e, Eq s) => HypergraphMorphism n e s -> HypergraphMorphism n e s -> Either (MACospanError n) (MACospan n e s)
    maCospan inInterface outInterface
        | target inInterface /= target outInterface = Left IncompatibleInterfaces
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
            hg = target inInterface
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
    
    
    
    
    -- | A MA-rewrite rule is left connected if its left-hand side is strongly connected.
    data LeftConnectedMARewriteRule n e s = LeftConnectedMARewriteRule {
                                    leftHandSideInputNodes :: HypergraphMorphism n e s,
                                    leftHandSideOutputNodes :: HypergraphMorphism n e s,
                                    rightHandSideInputNodes :: HypergraphMorphism n e s,
                                    rightHandSideOutputNodes :: HypergraphMorphism n e s
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
    leftConnectedMARewriteRule :: (Eq n, Eq e, Eq s) => HypergraphMorphism n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> Either (LeftConnectedMARewriteRuleError n) (LeftConnectedMARewriteRule n e s)
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
    
    unsafeLeftConnectedMARewriteRule :: HypergraphMorphism n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> LeftConnectedMARewriteRule n e s
    unsafeLeftConnectedMARewriteRule lhsInputNodes lhsOutputNodes rhsInputNodes rhsOutputNodes = LeftConnectedMARewriteRule{leftHandSideInputNodes = lhsInputNodes, leftHandSideOutputNodes = lhsOutputNodes, rightHandSideInputNodes = rhsInputNodes, rightHandSideOutputNodes = rhsOutputNodes}
    
    
    instance (Show n, Show e, Show s) => Show (LeftConnectedMARewriteRule n e s) where
        show x = "(unsafeLeftConnectedMARewriteRule "++(show $ leftHandSideInputNodes x)++" "++(show $ leftHandSideOutputNodes x)++" "++(show $ rightHandSideInputNodes x)++" "++(show $ rightHandSideOutputNodes x)++")"
    
    
    leftCospan :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s
    leftCospan rr = unsafeMACospan (target $ leftHandSideInputNodes rr) (leftHandSideInputNodes rr) (leftHandSideOutputNodes rr)
    
    rightCospan :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s
    rightCospan rr = unsafeMACospan (target $ rightHandSideInputNodes rr) (rightHandSideInputNodes rr) (rightHandSideOutputNodes rr)
    
    -- CONVEX MATCH 
    
    -- | The source and target hypergraphs of the matching should be monogamous acyclic, otherwise this function loops indefinitely. Not optimized yet.
    isConvexMatch :: (Eq n, Eq e, Eq s) => HypergraphMorphism n e s -> Bool
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
            g = target matching
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
    pushoutComplement :: (Eq n, Eq e, Eq s) => MACospan n e s -> MACospan n e s -> HypergraphMorphism n e s -> (MACospan n e s,MACospan n e s)
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
    dpo :: (Eq n, Eq e, Eq s) => LeftConnectedMARewriteRule n e s -> MACospan n e s -> HypergraphMorphism n e s -> MACospan (Either n n) (Either e e) s
    dpo rr csp match = pushout (rightCospan rr) c2 c1
        where
            (c1,c2) = pushoutComplement (leftCospan rr) csp match