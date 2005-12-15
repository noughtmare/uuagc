module SequentialComputation (
    computeSequential, 
    Info(Info), 
    SequentialResult(SequentialResult,DirectCycle,InducedCycle), 
    tdpToTds, tdsToTdp, tdpNt, lmh
) where

import SequentialTypes
import CommonTypes

import Data.Graph
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import List(partition,(\\),sort,transpose)

-- The updatable graph of attribute occurrences
type STGraph s = STArray s Vertex [Vertex]
type Tdp s = (STGraph s, STGraph s)
type Tds s = STGraph s 
type Comp s = (Tds s, Tdp s)

type Interfaces = [Interface]
type Interface = [([Vertex],[Vertex])]

-- Information about the vertices
data Info = Info { tdpToTds :: Table Vertex   -- Mapping attribute occurrences to attributes
                 , tdsToTdp :: Table [Vertex] -- Mapping attributes to all attribute occurrences
                 , tdpNt    :: Table (Name,Name,Name,Name) -- Mapping attribute occurrences to their rhs nt, lhs nt, constructor, and fieldnames
                 , lmh      :: [(Int,Int,Int)] -- Division of tds. l<=inherited<m, m<=synthesized<=h
                 }

-- Result of sequential computation
data SequentialResult = SequentialResult [[[[Vertex]]]] [(Vertex,ChildVisit)] [Interface] -- Succeeded, with visit sub-sequences, a table of childvisits, and interfaces
                      | DirectCycle [Edge]  -- Failed because of a cycle in the direct dependencies (type-2)
                      | InducedCycle [Edge] -- No direct cycle, but the computed interface generated a cycle (type-3)


-- Compute the Tds and Tdp graphs, given Info and the direct dependencies between attribute occurrences
computeSequential :: Info -> [Edge] -> SequentialResult
computeSequential info dpr
  = runST (do (tds,tdp) <- tdsTdp info dpr
              cycles2 <- cycles tds dpr info
              if (not (null cycles2))
               then return (DirectCycle cycles2)
               else do tds' <- freeze tds
                       tdsT <- thaw (transposeG tds') 
                       inters <- makeInterfaces tdsT (lmh info)
                       let idp = concatMap (concatMap (uncurry carthesian)) inters
                       mapM_ (insertTds info (tds,tdp)) idp
                       cycles3 <- cycles tds dpr info
                       if (not (null cycles3))
                        then return (InducedCycle cycles3)
                        else do let dprgraph = buildG (Data.Array.bounds (tdpToTds info)) dpr
                                    newindex = snd (Data.Array.bounds (tdpToTds info)) + 1
                                    (vsgraph,table)  = visitssGraph info dprgraph newindex inters
                                    vs = visitss info vsgraph inters
                                return (SequentialResult vs table inters)
          )

-- Initialise computation, and add all direct dependencies. This will trigger the whole of the computation
tdsTdp :: Info -> [Edge] -> ST s (Comp s)
tdsTdp info dpr = do tds  <- newArray (Data.Array.bounds (tdsToTdp info)) []
                     tdpN <- newArray (Data.Array.bounds (tdpToTds info)) []
                     tdpT <- newArray (Data.Array.bounds (tdpToTds info)) []
                     let comp = (tds,(tdpN,tdpT))
                     mapM_ (insertTdp info comp) dpr
                     return comp

-- Induces dependencies: Given a Tdp edge, add the corresponding Tds edge when applicable
-- Applicable for non-local attributes with equal field names
induce :: Info -> Comp s -> Edge -> ST s ()
induce info comp (v1,v2)
  = let v1' = tdpToTds info ! v1
        v2' = tdpToTds info ! v2
    in when (v1' /= -1 && v2' /= -1 && tdpNt info ! v1 == tdpNt info ! v2)
            (insertTds info comp (v1',v2'))

-- Inserts an edge to Tds. This induces dependencies on Tdp.
insertTds :: Info -> Comp s -> Edge -> ST s ()
insertTds info (tds,tdp) (v1,v2)
  = do e1 <- readArray tds v1
       when (not (v2 `elem` e1))
            (do writeArray tds v1 (v2:e1)
                occur info (tds,tdp) (v1,v2))

-- Addes all induced dependencies of an Tds-edge to the corresponding Tdp-graphs
occur :: Info -> Comp s -> Edge -> ST s ()
occur info comp (v1,v2)
  = let v1s = tdsToTdp info ! v1
        v2s = tdsToTdp info ! v2
    in sequence_ [ insertTdp info comp (v1,v2) | v1 <- v1s, v2 <-  v2s, tdpNt info ! v1 == tdpNt info ! v2 ]

-- Add an edge to Tdp and transitively re-close it.
insertTdp :: Info -> Comp s -> Edge -> ST s ()
insertTdp info comp@(_,(tdpN,tdpT)) (v1,v2)
  = do e1 <- readArray tdpN v1
       when (not (v2 `elem` e1))
            (do inc <- readArray tdpT v1
                out <- readArray tdpN v2
                let edges = carthesian (v1:inc) (v2:out)
                mapM_ (addTdpEdge info comp) edges)

-- Add an edge to Tdp. This induces dependencies on Tds
addTdpEdge :: Info -> Comp s -> Edge -> ST s ()
addTdpEdge info comp@(_,(tdpN,tdpT)) (v1,v2)
  = do e <- readArray tdpN v1
       when (not (v2 `elem` e))
            (do writeArray tdpN v1 (v2:e)
                e' <- readArray tdpT v2
                writeArray tdpT v2 (v1:e')
                induce info comp (v1,v2))

-------------------------------------------------------------------------------
-- Interfaces
-------------------------------------------------------------------------------
makeInterfaces :: Tds s -> [(Int,Int,Int)] -> ST s Interfaces
makeInterfaces tds = mapM (makeInterface tds)

makeInterface :: Tds s -> (Int,Int,Int) -> ST s Interface
makeInterface tds lmh
  = do (iwork,swork) <- sinks tds lmh []
       if null iwork && null swork 
         then return [([],[])]
         else makeInter tds lmh iwork swork

makeInter :: Tds s -> (Int,Int,Int) -> [Vertex] -> [Vertex] -> ST s Interface
makeInter tds lmh []    [] = return []
makeInter tds lmh iwork swork
  = do (ipart,swork') <- predsI tds lmh iwork
       (spart,iwork') <- predsS tds lmh (swork++swork')
       rest <- makeInter tds lmh iwork' []
       return ((ipart,spart):rest)

-- Finds the sinks-components, given initial sinks
predsI,predsS :: Tds s -> (Int,Int,Int) -> [Vertex] -> ST s ([Vertex],[Vertex])
-- The inherited sinks-component and the resulting synthesized sinks
predsI tds lmh []   = return ([],[])
predsI tds lmh work = do (inh,syn) <- sinks tds lmh work
                         (ipart,swork) <- predsI tds lmh inh
                         return (work ++ ipart,syn ++ swork)
-- The synthesized sinks-component and the resulting inherited sinks
predsS tds lmh []   = return ([],[])
predsS tds lmh work = do (inh,syn) <- sinks tds lmh work
                         (spart,iwork) <- predsS tds lmh syn
                         return (work ++ spart,inh ++ iwork)

-- Removes edges from vs, and returns resulting sinks, partitioned into inherited and synthesized
sinks :: Tds s -> (Int,Int,Int) -> [Vertex] -> ST s ([Vertex],[Vertex])
sinks tds (l,m,h) vs
  = liftM (partition (<m) . concat) $ mapM f' [l..h]
     where f' i = do e <- readArray tds i
                     if null e
                      then if null vs then return [i] else return []
                      else do let e' = e \\ vs
                              writeArray tds i e'
                              if null e' && not (i `elem` vs) then return [i] else return []

-------------------------------------------------------------------------------
-- Cycles
-------------------------------------------------------------------------------
cycles :: Tds s -> [Edge] -> Info -> ST s [Edge]
cycles tds dpr info = liftM (filter directOnly) $ concatMapM (cycles' tds) (lmh info)
                        where -- We only want cycles for which there exists a direct dependency
                              directOnly (v1,v2) = not . null $ [(v1',v2') | v1' <- tdsToTdp info ! v1, v2' <- tdsToTdp info ! v2] \\ dpr 

cycles' :: Tds s -> (Int,Int,Int) -> ST s [Edge]
cycles' tds (l,m,h) 
  = concatMapM (cyc tds) [m..h]
      where cyc :: Tds s -> Vertex -> ST s [Edge]
            cyc tds i = do e <- readArray tds i
                           concatMapM (toSelf tds i) e
            toSelf :: Tds s -> Vertex -> Vertex -> ST s [Edge]
            toSelf tds i j | j < m     = do e' <- readArray tds j
                                            if i `elem` e' then return [(i,j)] else return []
                           | otherwise = return []
                      

-------------------------------------------------------------------------------
-- Visit sub-sequences - Graph
-------------------------------------------------------------------------------
visitssGraph :: Info -> Graph -> Vertex -> [Interface] -> (Graph,[(Vertex,ChildVisit)])
visitssGraph info tdp v inters
  = let (rhsedges,fs,v') = rhsEdges info v inters
        tdp' = buildG (0,v'-1) (rhsedges ++ edges tdp)
        visitedges = visitEdges info tdp' v (v'-1)
        tdp'' = transposeG $ buildG (0,v'-1) (visitedges ++ edges tdp')
    in (tdp'',fs)

rhsEdges :: Info -> Vertex -> [Interface] -> ([Edge],[(Vertex,ChildVisit)],Vertex)
rhsEdges info v [] = ([],[],v)
rhsEdges info v (inter:inters) 
  = let (edges,fs,v') = rhsEdge info 0 v inter 
        (rest,fs',v'') = rhsEdges info v' inters
    in (edges++rest,fs++fs',v'')

rhsEdge :: Info -> Int -> Vertex -> Interface -> ([Edge],[(Vertex,ChildVisit)],Vertex)
rhsEdge info n v [] = ([],[],v)
rhsEdge info n v ((inh,syn):inter)
  = let rhsinh = map (\x -> (x,True))  $ concatMap (tdsToTdp info !) inh
        rhssyn = map (\x -> (x,False)) $ concatMap (tdsToTdp info !) syn
        classes = eqClasses comp $ sort $ filter lhs $ map (\(v,b) -> (tdpNt info ! v,v,b)) $ rhsinh ++ rhssyn
        comp (a,_,_) (a',_,_) = a == a'
        lhs ((_,_,_,field),_,_) | field == _LHS = False
                                | otherwise = True
        islast = null inter
        childvisits = zip [v..] $ map ((\((_,_,_,field),_,_) -> ChildVisit field n islast) . head) classes
        edges = makeEdges v classes
        l = length classes
        (rest,fs,v') = rhsEdge info (n+1) (v+l) inter
    in (edges ++ rest,childvisits ++ fs,v')
        
makeEdges :: Int -> [[(a,Vertex,Bool)]] -> [Edge]
makeEdges n [] = []
makeEdges n (x:xs) = makeEdges' n x ++ makeEdges (n+1) xs

makeEdges' :: Int -> [(a,Vertex,Bool)] -> [Edge]
makeEdges' n xs = map (makeEdge n) xs

makeEdge :: Int -> (a,Vertex,Bool) -> Edge
makeEdge n (_,v,True) = (v,n)
makeEdge n (_,v,False) = (n,v)

-- The edges between visits: Visit n+1 depends on visit n
visitEdges :: Info -> Graph -> Int -> Int -> [Edge]
visitEdges info tdp l h 
  = concatMap list2edges $ eqClasses comp $ sort $ map (\x -> (x, tdpNt info ! head (tdp ! x))) [l..h]
      where comp (_,a) (_,a') = a == a'
            list2edges []        = []
            list2edges [a]       = []
            list2edges ((a,_):(b,x):abs) = (a,b):list2edges ((b,x):abs)

-------------------------------------------------------------------------------
-- Visit sub-sequences
-------------------------------------------------------------------------------
-- For each Nt, for each prod, for each visit, a subsequence
visitss :: Info -> Graph -> [Interface] -> [[[[Vertex]]]]
visitss info vsgraph inters = map (transpose . map (visitss' info vsgraph)) inters

visitss' :: Info -> Graph -> ([Vertex],[Vertex]) -> [[Vertex]]
visitss' info vsgraph (inh,syn) 
  = let sortFrom = map (map snd) $ eqClasses comp $ sort $ filter lhs $ map (\x -> (tdpNt info ! x,x)) $ concatMap (tdsToTdp info !) syn
        lhs ((_,_,_,x),_) = x == _LHS
        comp (a,_) (a',_) = a == a'
    in map (topSort' vsgraph) sortFrom

-------------------------------------------------------------------------------
-- Graph-like functions
-------------------------------------------------------------------------------
postorder (Node a ts) = postorderF ts ++ [a]
postorderF = concatMap postorder
postOrd g = postorderF . dfs g
topSort' g = postOrd g

-------------------------------------------------------------------------------
-- Prelude-like functions
-------------------------------------------------------------------------------
-- Gives equivalence classes, given an ORDERED list
eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses p [] = []
eqClasses p (a:as) 
  = eqC [a] as
     where eqC as [] = [as]
           eqC as (a:as') | p a (head as) = eqC (a:as) as'
                          | otherwise     = reverse as : eqC [a] as'

concatMapM f xs = liftM concat $ mapM f xs
carthesian xs ys = [(x,y) | x <- xs, y <- ys]