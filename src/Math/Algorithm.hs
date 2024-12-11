-- Functions for computing steps of the main algorithm
-- Feel free to move to main file, or reorder in any way

-- Definition of E_s in line 2
edgesWithSameLabel :: (Eq s) => Hypergraph n e s -> Hypergraph n' e' s -> Set (e, e')
edgesWithSameLabel g g' = [(e, e') | e <- hyperedges g, e' <- hyperedges g', labelHyperedge e == labelHyperedge e']

-- coequalizer of the diagram:
--
-- f, g : a -> b
--
coeq :: Hypergraph n e s -> Hypergraph n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> (HypergraphMorphism n e s, Hypergraph n e s)
coeq a b f g = (hypergraphMorphism b c mn me, c) where
	c :: Hypergraph n e s
	c = hypergraph undefined undefined
	mn :: Map n n
	mn = undefined
	me :: Map (Hyperedge n e s) (Hyperedge n e s) 
	me = undefined

-- complement of the diagram:
--    f      g
-- a ---> b ---> c
complement :: Hypergraph n e s -> Hypergraph n e s -> Hypergraph n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> (Hypergraph n e s, HypergraphMorphism n e s)
complement a b c f g = (k, hypergraphMorphism k c mn me) where
	k :: Hypergraph n e s
	k = undefined
	mn :: Map n n
	mn = undefined
	me :: Map (Hyperedge n e s) (Hyperedge n e s)
	me = undefined

-- pullback of the diagram:
--
--     f       g
--  l ---> c <--- r
--
pullback :: Hypergraph n e s -> Hypergraph n e s -> Hypergraph n e s -> HypergraphMorphism n e s -> HypergraphMorphism n e s -> (Hypergraph n e s, HypergraphMorphism n e s, HypergraphMorphism n e s)
pullback l r c f g = (p, hypergraphMorphism p l toln tole, hypergraphMorphism p r torn tore) where
	p :: Hypergraph n e s
	p = hypergraph undefined undefined
	toln :: Map n n
	toln = undefined
	torn :: Map n n
	torn = undefined
	tole :: Map (Hyperedge n e s) (Hyperedge n e s)
	tole = undefined
	tore :: Map (Hyperedge n e s) (Hyperedge n e s)
	tore = undefined
