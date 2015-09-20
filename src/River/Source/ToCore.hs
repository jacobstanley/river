{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module River.Source.ToCore where

import           Control.Applicative
import           Control.Monad.Trans.State

import           Data.Text (Text)

import qualified River.Core.Syntax as C
import           River.Source.Syntax

------------------------------------------------------------------------

data Name n =
    Name    !n
  | NameMod !n !Int
  | NameNew    !Int
  deriving (Eq, Ord, Read, Show)

data FreshContext = FreshContext
  { ctxSupply  :: ![Int]
  }

newtype Fresh a = Fresh (State FreshContext a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m (FreshContext [1..])

nextFresh :: Fresh Int
nextFresh = Fresh $ do
    FreshContext (f:fs) <- get
    put (FreshContext fs)
    return f

freshen :: Name n -> Fresh (Name n)
freshen (Name    n)   = NameMod n <$> nextFresh
freshen (NameMod n _) = NameMod n <$> nextFresh
freshen (NameNew   _) = newFresh

newFresh :: Fresh (Name n)
newFresh = NameNew <$> nextFresh

------------------------------------------------------------------------

coreOfProgram :: Program a -> C.Program (Maybe a) (Name Text)
coreOfProgram = \case
  Program a ss -> C.Program (Just a) (runFresh (coreOfStatements ss))

------------------------------------------------------------------------

coreOfStatements :: [Statement a] -> Fresh (C.Term (Maybe a) (Name Text))

coreOfStatements []     = pure (C.Return Nothing (C.Copy Nothing []))
coreOfStatements (s:ss) = case s of
  Declaration _ _ Nothing
   -> coreOfStatements ss

  Declaration _ (Identifier n) (Just x)
   -> do term_let <- coreOfExpression (Name n) x
         term_ss  <- coreOfStatements ss
         pure (term_let term_ss)

  Assignment _ (Identifier n) Nothing x
   -> do term_let <- coreOfExpression (Name n) x
         term_ss  <- coreOfStatements ss
         pure (term_let term_ss)

  Assignment a i@(Identifier n) (Just op) x
   | bx <- Binary a op (Variable a i) x
   -> do term_let <- coreOfExpression (Name n) bx
         term_ss  <- coreOfStatements ss
         pure (term_let term_ss)

  Return a x
   -> do n        <- newFresh
         term_let <- coreOfExpression n x
         pure . term_let
              . C.Return (Just a)
              $ C.Copy   (Just a) [C.Variable (Just a) n]

------------------------------------------------------------------------

coreOfExpression :: Name Text
                 -> Expression a
                 -> Fresh ( C.Term (Maybe a) (Name Text)
                         -> C.Term (Maybe a) (Name Text) )

coreOfExpression n0 = \case
  Literal a x
   -> pure . C.Let  (Just a) [n0]
           $ C.Copy (Just a) [C.Immediate (Just a) x]

  Variable a (Identifier n1)
   -> pure . C.Let  (Just a) [n0]
           $ C.Copy (Just a) [C.Variable (Just a) (Name n1)]

  Unary a op x
   -> do n1       <- freshen n0
         term_let <- coreOfExpression n1 x

         let tail_op = C.Unary (Just a)
                               (coreOfUnaryOp op)
                               (C.Variable (Just a) n1)

         pure (term_let . C.Let (Just a) [n0] tail_op)

  Binary a op x y
   -> do n1        <- freshen n0
         n2        <- freshen n0
         term_letx <- coreOfExpression n1 x
         term_lety <- coreOfExpression n2 y

         let tail_op = C.Binary (Just a)
                                (coreOfBinaryOp op)
                                (C.Variable (Just a) n1)
                                (C.Variable (Just a) n2)

         pure (term_letx . term_lety . C.Let (Just a) [n0] tail_op)

------------------------------------------------------------------------

coreOfUnaryOp :: UnaryOp -> C.UnaryOp
coreOfUnaryOp = \case
  Neg -> C.Neg

coreOfBinaryOp :: BinaryOp -> C.BinaryOp
coreOfBinaryOp = \case
  Add -> C.Add
  Sub -> C.Sub
  Mul -> C.Mul
  Div -> C.Div
  Mod -> C.Mod
