type Input = [Int]

fuel :: Int -> Int
fuel mass = (mass `quot` 3) - 2

-- | Solver for part 1
--
-- Examples:
--
-- >>> part1 [12]
-- 2
--
-- >>> part1 [14]
-- 2
--
-- >>> part1 [1969]
-- 654
--
-- >>> part1 [100756]
-- 33583
part1 :: Input -> Int
part1 = sum . map fuel

-- | Solver for part 2
--
-- Examples:
--
-- >>> part2 [14]
-- 2
--
-- >>> part2 [1969]
-- 966
--
-- >>> part2 [100756]
-- 50346
part2 :: Input -> Int
part2 = sum . map fuel'
  where fuel' = sum . tail . takeWhile (> 0) . iterate fuel

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = interact $ show . ((,) <$> part1 <*> part2) . prepare
