import           Data.List                      ( break )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Map                      as M

data Direction a = U a
                 | R a
                 | D a
                 | L a

type Coordinate = Int
type Input = (Wire, Wire)
type Point = (Coordinate, Coordinate)
type Steps = Int
type Wire = [Direction Int]

-- | Solve example input for part 1
--
-- Examples:
--
-- >>> part1 ([R 8,U 5,L 5,D 3], [U 7,R 6, D 4,L 4])
-- 6
--
-- >>> part1 ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83])
-- 159
--
-- >>> part1 ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51],[U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7])
-- 135
part1 :: Input -> Int
part1 = minimum . map (manhattan (0, 0)) . getIntersections
 where
  getIntersections (w1, w2) = M.keys $ M.intersection (toMap w1) (toMap w2)
  manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Solve example input for part 2
--
-- Examples:
--
-- >>> part2 ([R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72], [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83])
-- 610
--
-- >>> part2 ([R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51],[U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7])
-- 410
--
part2 :: Input -> Int
part2 (w1, w2) =
  minimum . M.elems $ M.intersectionWith (+) (toMap w1) (toMap w2)

toMap :: Wire -> M.Map Point Steps
toMap = M.fromList . flip zip [1 ..] . scanl1 addPoints . concatMap getPoint
 where
  addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  getPoint (U n) = replicate n (0, 1)
  getPoint (R n) = replicate n (1, 0)
  getPoint (D n) = replicate n (0, -1)
  getPoint (L n) = replicate n (-1, 0)

prepare :: String -> Input
prepare = tuplify . map (mapMaybe parse . split ',') . lines

tuplify :: [a] -> (a, a)
tuplify [w1, w2] = (w1, w2)
tuplify _        = error "This program can only handle two wires"

parse :: String -> Maybe (Direction Int)
parse ('U' : x) = Just $ U $ read x
parse ('R' : x) = Just $ R $ read x
parse ('D' : x) = Just $ D $ read x
parse ('L' : x) = Just $ L $ read x
parse _         = Nothing

split :: Char -> String -> [String]
split chr str = case break (== chr) str of
  (a, chr' : b) -> a : split chr' b
  (a, ""      ) -> [a]

main :: IO ()
main = interact $ show . ((,) <$> part1 <*> part2) . prepare
