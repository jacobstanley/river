{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- | The core language, almost all transformations and optimisations take place
--   on this form.
--
--   River Core is a functional programming language whose syntactic structure
--   guarantees that programs are always in Administrative Normal Form [1].
--
--   This particular flavour of ANF is heavily inspired by the one presented in
--   [2] where a formal mapping from Static Single Assignment (SSA) form [3] to
--   ANF is presented. [2] also demonstrates that algorithms which work over
--   the SSA form can similarly be translated to work over ANF.
--
--   1. The Essence of Compiling with Continuations
--      Cormac Flanagan, Amr Sabry, Bruce F. Duba and Matthias Felleisen (1993)
--      https://users.soe.ucsc.edu/~cormac/papers/pldi93.pdf
--
--   2. A Functional Perspective on SSA Optimisation Algorithms
--      Manuel M. T. Chakravarty, Gabriele Keller and Patryk Zadarnowski (2003)
--      https://www.jantar.org/papers/chakravarty03perspective.pdf
--
--   3. Efficiently computing static single assignment form and the control dependence graph.
--      Ron Cytron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, and Kenneth F. Zadeck (1991)
--      http://www.cs.utexas.edu/~pingali/CS380C/2010/papers/ssaCytron.pdf
--
module River.Core.Syntax (
    Program(..)
  , Term(..)
  , Tail(..)
  , Atom(..)
  , Bindings(..)
  , Binding(..)
  ) where

import           Control.DeepSeq (NFData)

import           Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)


data Program p n a =
    Program !a !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Term p n a =
    Return !a !(Tail p n a)
  | If !a !(Atom n a) !(Term p n a) !(Term p n a)
  | Let !a ![n] !(Tail p n a) !(Term p n a)
  | LetRec !a !(Bindings p n a) !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Tail p n a =
    Copy !a ![Atom n a]
  | Call !a !n ![Atom n a]
  | Prim !a !p ![Atom n a]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Atom n a =
    Immediate !a !Integer
  | Variable !a !n
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Bindings p n a =
    Bindings !a [(n, Binding p n a)]
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

data Binding p n a =
    Lambda !a ![n] !(Term p n a)
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Data, Typeable, Generic, NFData)

$(deriveBifunctor ''Program)
$(deriveBifoldable ''Program)
$(deriveBitraversable ''Program)
$(deriveBifunctor ''Term)
$(deriveBifoldable ''Term)
$(deriveBitraversable ''Term)
$(deriveBifunctor ''Tail)
$(deriveBifoldable ''Tail)
$(deriveBitraversable ''Tail)
$(deriveBifunctor ''Atom)
$(deriveBifoldable ''Atom)
$(deriveBitraversable ''Atom)
$(deriveBifunctor ''Bindings)
$(deriveBifoldable ''Bindings)
$(deriveBitraversable ''Bindings)
$(deriveBifunctor ''Binding)
$(deriveBifoldable ''Binding)
$(deriveBitraversable ''Binding)
