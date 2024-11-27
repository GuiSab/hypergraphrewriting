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

)

where
    import              Data.WeakSet         (Set)
    import qualified    Data.WeakSet       as Set
    import              Data.WeakSet.Safe
    import              Data.WeakMap         (Map)
    import qualified    Data.WeakMap       as Map
    import              Data.WeakMap.Safe
    import              Data.Simplifiable
    import              Data.Maybe           (isJust, isNothing, fromJust)
    import              Data.Either          (isLeft)

    import              Math.FiniteCategory
   
    
    import              GHC.Generics

    