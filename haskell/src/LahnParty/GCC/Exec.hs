
module LahnParty.GCC.Exec where

import Control.Lens
import Control.Monad.Except

import LahnParty.GCC.Syntax
import LahnParty.GCC.State
import LahnParty.GCC.Monad


--
-- * Program execution
--

-- | Apply a function to the first two integers popped off the data stack,
--   pushing the result.
intOp :: Monad m => (Int -> Int -> Int) -> GCCM m ()
intOp f = do a <- popInt; b <- popInt; pushD (Lit (f a b))

-- | Apply a function to the first two integers popped off the data stack,
--   push 1 if the result is true, otherwise 0.
boolOp :: Monad m => (Int -> Int -> Bool) -> GCCM m ()
boolOp f = intOp (\a b -> if f a b then 1 else 0)


-- | Execute a single instruction.
inst :: Monad m => Inst -> GCCM m ()

-- Load
inst (LDC n)  = pushD (Lit n) >> incPC
inst (LD n i) = envGet n i >>= pushD >> incPC

-- Arithmetic
inst ADD = intOp (+) >> incPC
inst SUB = intOp (-) >> incPC
inst MUL = intOp (*) >> incPC
inst DIV = do b <- popInt; a <- popInt;
              when (b == 0) (throwError DivByZero)
              pushD (Lit (a `div` b))
              incPC

-- Logic
inst CEQ  = boolOp (==) >> incPC
inst CGT  = boolOp (>)  >> incPC
inst CGTE = boolOp (>=) >> incPC
inst ATOM = do v <- popD
               case v of
                 Lit _ -> pushD (Lit 1)
                 _     -> pushD (Lit 0)
               incPC

-- Pairs
inst CONS = liftM2 Pair popD popD >> incPC
inst CAR  = popPair >>= pushD . fst >> incPC
inst CDR  = popPair >>= pushD . snd >> incPC

-- Branching
inst JOIN      = popC >>= setPC
inst (SEL t f) = do b <- popInt
                    use pc >>= pushC . (+1)
                    if b == 0
                      then setPC f
                      else setPC t
