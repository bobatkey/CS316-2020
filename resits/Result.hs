module Result where

data Result a
  = Ok a
  | Error String
  deriving (Show, Eq)

{- It is a monad... -}
instance Monad Result where
  return x = Ok x
  Ok a      >>= k = k a
  Error msg >>= _ = Error msg

instance Functor Result where
  fmap f (Ok a)      = Ok (f a)
  fmap f (Error msg) = Error msg

instance Applicative Result where
  pure x = Ok x
  Ok f      <*> Ok a      = Ok (f a)
  Error msg <*> _         = Error msg
  _         <*> Error msg = Error msg
