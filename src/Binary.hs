module Binary (fromString,
               randomBinary,
               flipBit,
               pad,
               Binary(..),
               Bit(..),
               toBinary,
               fromBinary) where

import System.Random
import Control.Monad (sequence, replicateM)

data Bit = One | Zero
  deriving Eq

instance Show Bit where
  show One = "1"
  show Zero = "0"

data Binary = Binary [Bit]
  deriving Eq

instance Show Binary where
  show (Binary b) = concatMap show b


toBinary :: Integral a => a -> Binary
toBinary 0 = Binary [Zero]
toBinary dec = Binary $ go dec []
  where
    go 0 bits = bits
    go d bits = let (d', r) = divMod d 2
                in go d' $ numToBit r : bits

    numToBit 0 = Zero
    numToBit 1 = One
    numToBit _ = error "Not a 1 or 0"

fromBinary :: Integral a => Binary -> a
fromBinary (Binary bits) = let indexed = zip [0..] (map bitToNum . reverse $ bits)
                               raised = map (\(power, value) -> 2^power * value) indexed
                           in sum raised
  where bitToNum Zero = 0
        bitToNum One = 1


-- cheating just to get a working model for now
-- TODO: actually implement binary math funcs
instance Num Binary where
  (+) = binOp (+)
  (-) = binOp (-)
  (*) = binOp (*)
  fromInteger = toBinary
  signum (Binary b) = toBinary . length $ b
  abs = id

binOp :: Integral a => (a -> a -> a) -> Binary -> Binary -> Binary
binOp fun b1 b2 = toBinary (fromBinary b1 `fun` fromBinary b2)

type Width = Int
pad :: Binary -> Width -> Bit -> Binary
pad (Binary bits) w b = let numPad = w - length bits
                        in Binary $ replicate numPad b ++ bits

-- compress :: [Binary] -> Binary
-- compress = foldl (\(Binary b) (Binary b') -> Binary (b++b')) (Binary [])

randomBit :: IO Bit
randomBit = do
  r <- randomRIO (0,1) :: IO Int
  return $ case r of
    0 -> Zero
    1 -> One
    _ -> error "You Shouldn't be seeing this"

-- produce a sequence of random bits of the given length
randomBinary :: Int -> IO Binary
randomBinary n = Binary <$> replicateM n randomBit

fromString :: String -> Binary
fromString s = Binary $ go s
  where go [] = [] :: [Bit]
        go ('0':ss) = Zero:go ss
        go ('1':ss) = One:go ss


flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero
