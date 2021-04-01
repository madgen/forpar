module Language.Fortran.Util
  ( fromJustMsg
  ) where

-- | Retrieve a Just from a Maybe, or fail with a descriptive error message.
fromJustMsg :: String -> Maybe a -> a
fromJustMsg _ (Just x) = x
fromJustMsg msg _      = error msg
