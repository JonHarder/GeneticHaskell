module Chromosome (showChromosome, Chromosome, decodeChromosome, eval) where

import Binary

import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

data Operator = Add | Sub | Mult | Div

instance Show Operator where
  show Add = " + "
  show Sub = " - "
  show Mult = " * "
  show Div = " / "

data Gene = Number Float
          | Op Operator

instance Show Gene where
  show (Number n) = show n
  show (Op o) = show o

type Chromosome = [Gene]

showChromosome :: Chromosome -> String
showChromosome = concatMap show

encodeGene :: Gene -> Binary
encodeGene (Number n) = if n <= 9 then pad (toBinary $ truncate n) 4 Zero
                        else error "Number not encodable"
encodeGene (Op Add) = toBinary 10
encodeGene (Op Sub) = toBinary 11
encodeGene (Op Mult) = toBinary 12
encodeGene (Op Div) =  toBinary 13

decodeGene :: Binary -> Maybe Gene
decodeGene b = let n = fromIntegral $ fromBinary b
               in if n < 10 && n >= 0 then
                    Just (Number n)
                  else
                    case n of
                      10 -> Just (Op Add)
                      11 -> Just (Op Sub)
                      12 -> Just (Op Mult)
                      13 -> Just (Op Div)
                      _ -> Nothing

-- encodeChromosome :: Chromosome -> Binary
-- encodeChromosome = compress . map encodeGene

justP :: Maybe a -> Bool
justP (Just _) = True
justP _ = False

onlyJusts :: [Maybe a] -> [a]
onlyJusts = map fromJust . filter justP

decodeChromosome :: Binary -> Chromosome
decodeChromosome (Binary b) = let cs = map (decodeGene . Binary) $ chunksOf 4 b
                              in normalize $ onlyJusts cs


-- looks for sequences of genes of the form 'X + Y * Z / W'
-- ignores any malformed sequenecs up till either a correctly
-- formed sequence is discovered or there is nothing left
-- BUG: recodes the result of each operation back as a number
--      which (un)conveniently can only represent whole numbers.
--      this means 5/2 gets stored as nothing becasue it isnt 2 or 3

-- should instead fold chromosome with float accumulator, reducing
-- the remaining chromosome until only acc is left leaving the
-- true result
evalChromosome :: Chromosome -> Float
evalChromosome [] = 1/0
evalChromosome [Number n] = n
evalChromosome (Number n1:Op Add:Number n2:rest) = evalChromosome (Number (n1+n2):rest)
evalChromosome (Number n1:Op Sub:Number n2:rest) = evalChromosome (Number (n1-n2):rest)
evalChromosome (Number n1:Op Mult:Number n2:rest) = evalChromosome (Number (n1*n2):rest)
evalChromosome (Number n1:Op Div:Number n2:rest) = evalChromosome (Number (n1/n2):rest)
evalChromosome _ = error "Hit edge case in `evalChromosome`"

normalize :: Chromosome -> Chromosome
normalize [] = []
normalize c@[Number _] = c
normalize (Op _:rest) = normalize rest
normalize [Number n,Op _] = [Number n]
normalize (Number n:Number _:rest) = normalize $ Number n:rest
normalize (Number n:Op o:Op _:rest) = normalize $ Number n:Op o:rest
normalize (Number n:Op o:Number m:rest) = Number n:Op o:normalize (Number m:rest)

eval :: Binary -> Float
eval b = evalChromosome . decodeChromosome $ b
