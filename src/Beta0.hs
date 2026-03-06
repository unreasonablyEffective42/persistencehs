module Beta0 (beta0) where

import Data.List (foldl', sortOn, tails)
import qualified Data.Map as Map

type Point = (Double, Double, Double, Double, Double, Double)

type Edge = (Point, Point)

data DSU = DSU
  { parent :: Map.Map Point Point,
    size :: Map.Map Point Int
  }

initDSU :: [Point] -> DSU
initDSU =
  foldl'
    ( \d p ->
        d
          { parent = Map.insert p p (parent d),
            size = Map.insert p 1 (size d)
          }
    )
    (DSU Map.empty Map.empty)

find :: DSU -> Point -> (Maybe Point, DSU)
find dsu p =
  case Map.lookup p (parent dsu) of
    Nothing -> (Nothing, dsu)
    Just par ->
      if par == p
        then (Just p, dsu)
        else
          let (rootM, dsu') = find dsu par
              dsu'' = case rootM of
                Nothing -> dsu'
                Just root -> dsu' {parent = Map.insert p root (parent dsu')}
           in (rootM, dsu'')

union :: Point -> Point -> DSU -> DSU
union p q dsu =
  case (rp, rq) of
    (Just rp, Just rq) ->
      if rp == rq
        then dsu2
        else case (Map.lookup rp (size dsu2), Map.lookup rq (size dsu2)) of
          (Just sp, Just sq) ->
            if sp > sq
              then dsu2 {parent = Map.insert rq rp (parent dsu2), size = Map.insert rp (sp + sq) (size dsu2)}
              else dsu2 {parent = Map.insert rp rq (parent dsu2), size = Map.insert rq (sp + sq) (size dsu2)}
          _ -> dsu2
    (Nothing, Just rq) -> dsu2
    (Just rp, Nothing) -> dsu
    (Nothing, Nothing) -> dsu
  where
    (rp, dsu1) = find dsu p
    (rq, dsu2) = find dsu1 q

distance :: Point -> Point -> Double
distance (x1, x2, x3, x4, x5, x6) (y1, y2, y3, y4, y5, y6) = sqrt (dx1 * dx1 + dx2 * dx2 + dx3 * dx3 + dx4 * dx4 + dx5 * dx5 + dx6 * dx6)
  where
    dx1 = y1 - x1
    dx2 = y2 - x2
    dx3 = y3 - x3
    dx4 = y4 - x4
    dx5 = y5 - x5
    dx6 = y6 - x6

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

edgesWithValue :: [Point] -> [(Double, Edge)]
edgesWithValue pts = sortOn fst [(distance a b, (a, b)) | (a, b) <- uniquePairs pts]

edgesAtScale :: Double -> [(Double, Edge)] -> [(Double, Edge)]
edgesAtScale eps = filter (\(d, _) -> d <= eps)

components :: DSU -> [Point] -> Map.Map Point [Point]
components dsu pts = Map.fromListWith (++) [(r, [p]) | p <- pts, Just r <- [fst (find dsu p)]]

numComponents :: DSU -> [Point] -> Int
numComponents dsu pts = Map.size (components dsu pts)

beta0 :: Double -> [Point] -> Int
beta0 eps pts =
  let d0 = initDSU pts
      es = edgesAtScale eps (edgesWithValue pts)
      dF = foldl' (\d (_, (u, v)) -> union u v d) d0 es
   in numComponents dF pts

r0 :: Double
r0 = 1

r1 :: Double
r1 = 2

-- >>> beta0 0.20000731001 points
-- 31
--
-- >>> beta0 0.20000731002 points
-- 1
