
module LahnParty.GCC.Syntax where

-- | A pointer into the heap.
type Ptr = Int

-- | Absolute instruction address.
type Addr = Int

-- | GCC instruction set.
data Inst
  =  LDC  Int       -- ^ load constant (literal)
  |  LD   Int  Int  -- ^ load from environment (frame number, element in frame)
  |  ADD            -- ^ integer addition
  |  SUB            -- ^ integer subtraction
  |  MUL            -- ^ integer multiplication
  |  DIV            -- ^ integer division
  |  CEQ            -- ^ compare equal
  |  CGT            -- ^ compare greater than
  |  CGTE           -- ^ compare greater than equal
  |  ATOM           -- ^ test if value is an integer
  |  CONS           -- ^ allocate a CONS cell
  |  CAR            -- ^ extract first element from CONS cell
  |  CDR            -- ^ extract second element from CONS cell
  |  SEL  Addr Addr -- ^ conditional branch (true, false)
  |  JOIN           -- ^ return from branch
  |  LDF  Addr      -- ^ load function
  |  AP   Int       -- ^ call function (number of args)
  |  RTN            -- ^ return from function call
  |  DUM  Int       -- ^ create empty environment frame (size of frame)
  |  RAP  Int       -- ^ recursive environment call function (# args)
  |  STOP           -- ^ terminate co-processor execution
  
  -- Tail call extensions
  |  TSEL Addr Addr -- ^ tail-call conditional branch (true, false)
  |  TAP  Int       -- ^ tail-call function (# args)
  |  TRAP Int       -- ^ recursive environment tail-call function (# args)
  
  -- Pascal extensions
  |  ST   Int  Int  -- ^ store to environment (frame number, element in frame)
  
  -- Debug extensions
  |  DBUG           -- ^ printf debugging
  |  BRK            -- ^ breakpoint debugging
  deriving (Eq,Show)
