{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ErrOr where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data ErrOr a = Err String | Ok a deriving (Show, Generic, NFData)

instance Functor ErrOr where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Err err) = Err err

instance Applicative ErrOr where
  pure = Ok
  (<*>) (Err err) _ =  Err err
  (<*>) (Ok f) x = fmap f x

instance Monad ErrOr where
  (>>=) (Err err) _ = Err err
  (>>=) (Ok x) f = f x
