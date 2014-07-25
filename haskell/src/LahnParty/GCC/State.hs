{-# LANGUAGE TemplateHaskell #-}

module LahnParty.GCC.State where

import Control.Lens
import Data.List (splitAt)

import LahnParty.GCC.Syntax


--
-- * Values
--

-- | Values in the data stack or environment frame.
data Value 
  =  Lit  Int
  |  Pair Value Value
  |  Clos Frame Addr
  deriving (Eq,Show)


--
-- * Stacks
--

-- | A stack.
type Stack a = [a]

-- | Errors by underflowing stacks.
data StackError = EmptyDataStack | EmptyControlStack
  deriving (Eq,Show)


--
-- * Environment
--

-- | An environment frame is a fixed-length list of slots for values.
type Frame = [Maybe Value]

-- | An environment is a list of frames.
type Env = [Frame]

-- | Two kinds of errors when getting/setting values from the environment.
--   Either the there is no slot at the give frame and element number,
--   or the slot is unitialized (error on get only).
data EnvError = OutOfBounds | Uninitialized
  deriving (Eq,Show)

-- | Create a new frame of the specified size.
newFrame :: Int -> Frame
newFrame n = replicate n Nothing

-- | Create a new frame initialized with the given values.
newFrameWith :: [Value] -> Frame
newFrameWith = map Just

-- | Get a value from the environment.
_envGet :: Int -> Int -> Env -> Either EnvError Value
_envGet 0 sn (f:_)
  | sn < length f = maybe (Left Uninitialized) Right (f !! sn)
  | otherwise     = Left OutOfBounds
_envGet fn sn (_:fs) = _envGet (fn-1) sn fs
_envGet _  _  _      = Left OutOfBounds

-- | Set a value in the environment.
_envSet :: Int -> Int -> Value -> Env -> Either EnvError Env
_envSet 0 sn v (f:fs) = case splitAt sn f of
  (as,_:bs) -> Right ((as ++ Just v : bs) : fs)
  _         -> Left OutOfBounds
_envSet fn sn v (f:fs) = fmap (f:) $ _envSet (fn-1) sn v fs


--
-- * Program state
--

-- | State of the GCC machine.
data GCC = GCC {
  _pc     :: Addr,
  _stackD :: Stack Value,
  _stackC :: Stack Addr,
  _env    :: Env
}

$(makeLenses ''GCC)
