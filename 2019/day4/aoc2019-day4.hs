import           Data.List                      ( break
                                                , group
                                                )

type Range = (Int, Int)

part1 :: Range -> Int
part1 = solve [sixDigits, hasAdjacentChars, onlyIncreasing]

part2 :: Range -> Int
part2 = solve [sixDigits, exactlyTwoAdjChars, onlyIncreasing]

solve :: [String -> Bool] -> Range -> Int
solve conditions range =
  length $ filter (isValidPassword conditions) (map show $ pwRange range)

-- | Solve example input
--
-- Examples:
--
-- >>> isValidPassword [sixDigits, hasAdjacentChars, onlyIncreasing] "111111"
-- True
--
-- >>> isValidPassword [sixDigits, hasAdjacentChars, onlyIncreasing] "223450"
-- False
--
-- >>> isValidPassword [sixDigits, hasAdjacentChars, onlyIncreasing] "123789"
-- False
--
-- >>> isValidPassword [sixDigits, exactlyTwoAdjChars, onlyIncreasing] "112233"
-- True
--
-- >>> isValidPassword [sixDigits, exactlyTwoAdjChars, onlyIncreasing] "123444"
-- False
--
-- >>> isValidPassword [sixDigits, exactlyTwoAdjChars, onlyIncreasing] "111122"
-- True

isValidPassword :: [String -> Bool] -> String -> Bool
isValidPassword conditions p = all ($ p) conditions

sixDigits :: String -> Bool
sixDigits = (== 6) . length

hasAdjacentChars :: String -> Bool
hasAdjacentChars (x : y : rest) = (x == y) || hasAdjacentChars (y : rest)
hasAdjacentChars _              = False

exactlyTwoAdjChars :: String -> Bool
exactlyTwoAdjChars = elem 2 . map length . group

onlyIncreasing :: String -> Bool
onlyIncreasing (x : y : rest) = (x <= y) && onlyIncreasing (y : rest)
onlyIncreasing _              = True

pwRange :: Range -> [Int]
pwRange (min, max) = [min .. max]

prepare :: String -> Range
prepare = tuplify . map read . split '-' where tuplify [x, y] = (x, y)

split :: Char -> String -> [String]
split chr str = case break (== chr) str of
  (a, chr : b) -> a : split chr b
  (a, ""     ) -> [a]

main :: IO ()
main = interact $ show . ((,) <$> part1 <*> part2) . prepare
