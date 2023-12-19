{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solution (whitespace) where

import Control.Lens
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Data.Array
import Data.Char (chr, ord)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe
import Text.Parsec hiding (labels)
import Text.Parsec.String

type ErrorMsg = String

type Result = Either ErrorMsg String

type Label = String

data Instruction
  = Push Int
  | Ref Int
  | Slide Int
  | Dup
  | Swap
  | Discard
  | Plus
  | Sub
  | Mult
  | Div
  | Mod
  | Store
  | Retrieve
  | Label Label
  | Call Label
  | Jump Label
  | JumpZ Label
  | JumpN Label
  | Return
  | End
  | PutChar
  | PutNum
  | GetChar
  | GetNum
  deriving (Show, Eq)

type Program = Array Int Instruction

type Labels = M.Map Label Int

data Env = Env
  { program :: Program,
    labels :: Labels
  }

type Stack = [Int]

type Heap = M.Map Int Int

data VMState = VMState
  { _valStack :: Stack,
    _callStack :: Stack,
    _heap :: Heap,
    _pcounter :: Int,
    _input :: String
  }

makeLenses ''VMState

type VM = RWST Env String VMState (Either ErrorMsg)

vm :: VM ()
vm = do
  pc <- use pcounter
  inst <- getInst pc
  unless (inst == End) $ do
    exec inst
    pcounter += 1
    vm

getInst :: Int -> VM Instruction
getInst pc = do
  maxPc <- asks (snd . bounds . program)
  if pc <= maxPc
    then asks ((! pc) . program)
    else throwError "The program needs a terminator."

push :: Lens' VMState Stack -> Int -> VM ()
push stack n = stack %= (n :)

pop :: Lens' VMState Stack -> VM Int
pop stack = do
  s <- use stack
  case s of
    (x : s') -> do
      stack .= s'
      return x
    _ -> throwError "Stack pop error"

jump :: Label -> VM ()
jump label = do
  mpc <- asks $ (M.!? label) . labels
  case mpc of
    Nothing -> throwError "Label not exist."
    Just pc -> pcounter .= pc

exec :: Instruction -> VM ()
exec (Push n) = push valStack n
exec (Ref n) = do
  len <- uses valStack length
  if n >= len || n < 0
    then throwError "Invalid n error."
    else do
      x <- uses valStack (!! n)
      push valStack x
exec (Slide n) = do
  x <- pop valStack
  if n < 0
    then valStack .= []
    else valStack %= drop n
  push valStack x
exec Dup = do
  x <- pop valStack
  replicateM_ 2 $ push valStack x
exec Swap = do
  x <- pop valStack
  y <- pop valStack
  mapM_ (push valStack) [x, y]
exec Discard = do
  void $ pop valStack
exec Plus = do
  x <- pop valStack
  y <- pop valStack
  push valStack (y + x)
exec Sub = do
  x <- pop valStack
  y <- pop valStack
  push valStack (y - x)
exec Mult = do
  x <- pop valStack
  y <- pop valStack
  push valStack (y * x)
exec Div = do
  x <- pop valStack
  when (x == 0) $ throwError "Can't divide by zero."
  y <- pop valStack
  push valStack (y `div` x)
exec Mod = do
  x <- pop valStack
  when (x == 0) $ throwError "Can't divide by zero."
  y <- pop valStack
  push valStack (y `mod` x)
exec Store = do
  x <- pop valStack
  addr <- pop valStack
  heap . at addr ?= x
exec Retrieve = do
  addr <- pop valStack
  mx <- use $ heap . at addr
  case mx of
    Nothing -> throwError "Heap error."
    Just x -> push valStack x
exec (Label _) = return ()
exec (Call label) = do
  pc <- use pcounter
  push callStack pc
  jump label
exec (Jump label) = jump label
exec (JumpZ label) = do
  x <- pop valStack
  when (x == 0) (jump label)
exec (JumpN label) = do
  x <- pop valStack
  when (x < 0) (jump label)
exec Return = do
  pc <- pop callStack
  pcounter .= pc
exec PutChar = do
  x <- pop valStack
  tell [chr x]
exec PutNum = do
  x <- pop valStack
  tell (show x)
exec GetChar = do
  s <- use input
  case s of
    "" -> throwError "Input error."
    (c : s') -> do
      input .= s'
      addr <- pop valStack
      heap . at addr ?= ord c
exec GetNum = do
  s <- use input
  case reads s of
    [(x, s')] -> do
      input .= s'
      addr <- pop valStack
      heap . at addr ?= x
    _ -> throwError "Input error."

pNum :: Parser Int
pNum = do
  sign <- pSign
  bits <- many $ (char ' ' $> 0) <|> (char '\t' $> 1)
  char '\n'
  return $ sign * foldl' (\n b -> n * 2 + b) 0 bits
  where
    pSign = (char ' ' $> 1) <|> (char '\t' $> (-1))

pLabel :: Parser Label
pLabel = many (oneOf " \t") <* char '\n'

pProgram :: Parser Program
pProgram = (\ls -> listArray (0, length ls - 1) ls) <$> many pInstruction

pInstruction :: Parser Instruction
pInstruction =
  try (char ' ' *> pStack)
    <|> try (string "\t " *> pArith)
    <|> try (string "\t\t" *> pHeap)
    <|> try (string "\t\n" *> pIO)
    <|> (char '\n' *> pControl)

pStack :: Parser Instruction
pStack =
  try (char ' ' $> Push <*> pNum)
    <|> try (string "\t " $> Ref <*> pNum)
    <|> try (string "\t\n" $> Slide <*> pNum)
    <|> try (string "\n " $> Dup)
    <|> try (string "\n\t" $> Swap)
    <|> (string "\n\n" $> Discard)

pArith :: Parser Instruction
pArith =
  try (string "  " $> Plus)
    <|> try (string " \t" $> Sub)
    <|> try (string " \n" $> Mult)
    <|> try (string "\t " $> Div)
    <|> (string "\t\t" $> Mod)

pHeap :: Parser Instruction
pHeap = (char ' ' $> Store) <|> (char '\t' $> Retrieve)

pIO :: Parser Instruction
pIO =
  try (string "  " $> PutChar)
    <|> try (string " \t" $> PutNum)
    <|> try (string "\t " $> GetChar)
    <|> (string "\t\t" $> GetNum)

pControl :: Parser Instruction
pControl =
  try (string "  " $> Label <*> pLabel)
    <|> try (string " \t" $> Call <*> pLabel)
    <|> try (string " \n" $> Jump <*> pLabel)
    <|> try (string "\t " $> JumpZ <*> pLabel)
    <|> try (string "\t\t" $> JumpN <*> pLabel)
    <|> try (string "\t\n" $> Return)
    <|> (string "\n\n" $> End)

parseCode :: String -> Either ErrorMsg Program
parseCode code =
  case parse pProgram "" code of
    Left _ -> Left "Parse error."
    Right prog -> Right prog

getLabels :: Program -> Either ErrorMsg Labels
getLabels prog = filterLabels M.empty (assocs prog)
  where
    filterLabels ls [] = Right ls
    filterLabels ls ((i, Label l) : ps)
      | isJust (ls M.!? l) = Left "Labels must be unique."
      | otherwise = filterLabels (M.insert l i ls) ps
    filterLabels ls (_ : ps) = filterLabels ls ps

whitespace :: String -> String -> Result
whitespace code input = do
  let code' = filter (`elem` " \t\n") code
  program <- parseCode code'
  labels <- getLabels program
  let env = Env {..}
      initState =
        VMState
          { _valStack = [],
            _callStack = [],
            _heap = M.empty,
            _pcounter = 0,
            _input = input
          }
  (_, output) <- execRWST vm env initState
  return output
