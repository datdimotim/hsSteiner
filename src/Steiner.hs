{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Steiner where

import Control.Lens hiding (to, from)
import Data.Foldable (minimumBy, toList)
import Data.Ord (comparing)
import Control.Monad (guard)


select1 :: [a] -> [(a, [a])]
select1 [a] = [(a, [])]
select1 (a:as) = (a, as) : over (mapped . _2) (a:)  (select1 as)


select2 :: [a] -> [((a, a), [a])]
select2 [a, b] = [((a, b), [])]
select2 (a:as) = over (mapped . _1) (a,) (select1 as) ++ over (mapped . _2) (a:) (select2 as)


data Vertex = Vertex {_idx :: String, _x :: Double, _y :: Double}
makeLenses ''Vertex


data Edge = Edge {_from :: Vertex, _to :: Vertex}
makeLenses ''Edge


data Net = Net {_vs :: [Vertex], _es :: [Edge]}
makeLenses ''Net


getVertEdges :: Vertex -> Net -> [Edge]
getVertEdges (Vertex idx _ _) (Net _ edges) =
  let
    predicate (Edge (Vertex fIdx _ _) (Vertex tIdx _ _)) = fIdx == idx || tIdx == idx
  in
    filter predicate edges


toVec :: Edge -> (Double, Double)
toVec (Edge (Vertex _ afx afy) (Vertex _ atx aty)) = (atx - afx, aty - afy)


cosEdgesAngle :: Edge -> Edge -> Double
cosEdgesAngle a b =
  let
    (ax, ay) = toVec a
    (bx, by) = toVec b
  in
    (ax * bx + ay * by) / sqrt ((ax * ax + ay * ay) * (bx * bx + by * by))


normEdgeDir :: Edge -> Vertex -> Edge
normEdgeDir e v =
  if e ^. from . idx == v ^. idx
  then e
  else Edge (e ^. to) (e ^. from)


isAddable :: Vertex -> Vertex -> Net -> Bool
isAddable cur v net =
  let
    es = getVertEdges v net
    curEdge = Edge v cur
  in
    length es <= 2 && all ((< (-0.5 + 0.001)) . cosEdgesAngle curEdge . (`normEdgeDir` v)) es


distSquare :: Vertex -> Vertex -> Double
distSquare a b = dx * dx + dy * dy where
  (dx, dy) = toVec $ Edge a b    


edgeWeight :: Edge -> Double
edgeWeight (Edge f t) = sqrt $ distSquare f t


evalWeight :: Net -> Double
evalWeight (Net _ es) = sum . map edgeWeight $ es


findOptNet :: [Net] -> Net
findOptNet = minimumBy (comparing evalWeight)


genTriangles :: Vertex -> Vertex -> [Vertex]
genTriangles a b = [m1, m2] where
  (dx, dy) = toVec $ Edge a b
  sin60 = (sqrt 3) / 2
  cos60 = 0.5
  x1 = a ^. x + cos60 * dx - sin60 * dy
  x2 = a ^. x + cos60 * dx + sin60 * dy
  y1 = a ^. y + sin60 * dx + cos60 * dy
  y2 = a ^. y - sin60 * dx + cos60 * dy
  m1 = Vertex (a ^. idx ++ "-" ++ b ^. idx ++ "-1") x1 y1
  m2 = Vertex (a ^. idx ++ "-" ++ b ^. idx ++ "-2") x2 y2
  
  
replaceVertex :: Vertex -> Vertex -> Edge -> Edge
replaceVertex pat new (Edge f t) = 
  let
    rpl v = if v ^. idx == pat ^. idx then new else v 
  in
    Edge (rpl f) (rpl t)
    

genExtVertex :: Vertex -> Vertex -> Vertex -> Vertex -> Maybe Vertex
genExtVertex s1 s2 m k = 
  let
    sx = (s1 ^. x + s2 ^. x + m ^. x) / 3
    sy = (s1 ^. y + s2 ^. y + m ^. y) / 3
    
    mcx = m ^. x - sx
    mcy = m ^. y - sy
    
    kcx = k ^. x - sx
    kcy = k ^. y - sy
    
    dx = kcx - mcx
    dy = kcy - mcy
    
    t = (-2) * (dx * mcx + dy * mcy) / (dx * dx + dy * dy)
    
    ecx = dx * t + mcx
    ecy = dy * t + mcy
    
    ex = ecx + sx
    ey = ecy + sy
    extId = "(" ++ s1 ^. idx ++ ") - (" ++ s2 ^. idx ++ ") -> (" ++ k ^. idx ++ ")"
  in
    if t > 1 || t < 0
    then Nothing
    else Just $ Vertex extId ex ey

getBackVertexPair :: Net -> Vertex -> Vertex -> Vertex -> Maybe Net
getBackVertexPair sln m s1 s2 = do   
  let (e:empt) = getVertEdges m sln
  guard $ null empt
  let k = if e ^. from . idx == m ^. idx then e ^. to else e ^. from
  ext <- genExtVertex s1 s2 m k 
  let a1 = Edge ext s1
  let a2 = Edge ext s2
  let c = Edge ext k
  guard $ cosEdgesAngle a1 a2 < 0 && cosEdgesAngle a1 c < 0
  let e1 = Edge s1 ext
  let e2 = Edge s2 ext
  let es' = map (replaceVertex m ext) (sln ^. es)
  return $ Net (ext : _vs sln)  (e1 : e2 : es')
       

steiner :: [Vertex] -> [Net]
steiner [_] = [Net [] []]
steiner [a, b] = [Net [] [Edge a b]]
steiner ps = fstVars ++ sndVars where
  fstVars = do
    (cur, vs) <- select1 ps
    let closest = minimumBy (comparing $ distSquare cur) vs
    let optNet = minimumBy (comparing evalWeight) (steiner vs)
    guard $ isAddable cur closest optNet
    return $ Net (_vs optNet) (Edge cur closest : _es optNet)
  
  sndVars = do
    ((s1, s2), vs) <- select2 ps
    m <- genTriangles s1 s2
    let subNet = minimumBy (comparing evalWeight) $ steiner (m : vs)
    toList $ getBackVertexPair subNet m s1 s2
-----------

{-

const testData = [
    {"id": "A", "pos": {"x": 69.2520325203252, "y": 236.64634146341456}},
    {"id": "B", "pos": {"x": 48.47154471544715, "y": 651.1016260162601}},
    {"id": "C", "pos": {"x": 501.0243902439024, "y": 501.020325203252}},
    {"id": "D", "pos": {"x": 756.1626016260163, "y": 363.6382113821138}},
    {"id": "E", "pos": {"x": 863.5284552845529, "y": 793.1016260162602}},
    {"id": "F", "pos": {"x": 243.5772357723577, "y": 133.89837398373976}},
];

-}

testData :: [Vertex]
testData = 
  [
    Vertex "A" 69.2520325203252 236.64634146341456,
    Vertex "B" 48.47154471544715 651.1016260162601,
    Vertex "C" 501.0243902439024 501.020325203252,
    Vertex "D" 756.1626016260163 363.6382113821138,
    Vertex "E" 863.5284552845529 793.1016260162602,
    Vertex "F" 243.5772357723577 133.89837398373976,
    Vertex "G" 203.5772357723577 233.89837398373976
    --Vertex "H" 253.5772357723577 33.89837398373976
  ]  

bench :: (Net, Double)
bench = 
  let
    net = minimumBy (comparing evalWeight) (steiner testData)
  in
    (net, evalWeight net)