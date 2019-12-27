{-# LANGUAGE LambdaCase #-}

import           Data.List                      ( break )
import qualified Data.IntMap                   as IM

type Address = Int
type Program = IM.IntMap Int

data Instruction = Add Address Address Address
                 | Mult Address Address Address
                 | Halt

part1 :: Program -> Maybe Int
part1 program = run 0 (initialize 12 2 program) >>= IM.lookup 0

part2 :: Program -> [Int]
part2 program = do
  noun <- [0 .. 99]
  verb <- [0 .. 99]
  case run 0 (initialize noun verb program) >>= IM.lookup 0 of
    Just 19690720 -> [100 * noun + verb]
    _             -> []

initialize :: Int -> Int -> Program -> Program
initialize noun verb = IM.insert 2 verb . IM.insert 1 noun

-- | Solve example input
--
-- Examples:
--
-- >>> run 0 (prepare "1,0,0,0,99") >>= IM.lookup 0
-- Just 2
--
-- >>> run 0 (prepare "2,3,0,3,99") >>= IM.lookup 0
-- Just 2
--
-- >>> run 0 (prepare "2,4,4,5,99,0") >>= IM.lookup 0
-- Just 2
--
-- >>> run 0 (prepare "1,1,1,4,99,5,6,0,99") >>= IM.lookup 0
-- Just 30
run :: Address -> Program -> Maybe Program
run ip program = interpret ip program >>= \inst -> case inst of
  Halt -> pure program
  _    -> calculate inst program >>= run (ip + 4)

interpret :: Address -> Program -> Maybe Instruction
interpret address program = IM.lookup address program >>= \case
  1  -> binOp Add
  2  -> binOp Mult
  99 -> pure Halt
  _  -> Nothing
 where
  at pos = IM.lookup pos program
  binOp inst =
    inst <$> at (address + 1) <*> at (address + 2) <*> at (address + 3)

calculate :: Instruction -> Program -> Maybe Program
calculate inst program = case inst of
  Halt               -> pure program
  Add  src1 src2 dst -> calc (+) src1 src2 >>= save dst
  Mult src1 src2 dst -> calc (*) src1 src2 >>= save dst
 where
  calc op src1 src2 = op <$> IM.lookup src1 program <*> IM.lookup src2 program
  save dst result = pure $ IM.insert dst result program

prepare :: String -> Program
prepare = IM.fromList . zip [0 ..] . map read . split ','

split :: Char -> String -> [String]
split chr str = case break (== chr) str of
  (a, chr' : b) -> a : split chr' b
  (a, ""      ) -> [a]

main :: IO ()
main = interact $ show . ((,) <$> part1 <*> part2) . prepare
