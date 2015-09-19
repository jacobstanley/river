{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module River.Test.Parser (tests) where

import Test.QuickCheck

------------------------------------------------------------------------

prop_foo = True

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $quickCheckAll
