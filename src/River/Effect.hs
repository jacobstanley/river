module River.Effect (
    HasEffect(..)
  ) where


class HasEffect p where
  hasEffect :: p -> Bool
