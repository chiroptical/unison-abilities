#!/usr/bin/env stack
-- stack --resolver lts-16.5 script --package free --package transformers

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Free where

import Control.Monad.Free (Free (..), iterM, liftF)
import qualified Control.Monad.Trans.State as S

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

data StackF a k
    = Get ([a] -> k)
    | Put a k
    | Pop (Maybe a -> k)
    deriving (Functor)

type Stack a = Free (StackF a)

get :: Stack a [a]
get = liftF (Get id)

put :: a -> Stack a ()
put x = liftF (Put x ())

pop :: Stack a (Maybe a)
pop = liftF (Pop id)

runStack :: Stack a b -> S.State [a] b
runStack = iterM $ \case
  Get k -> S.get >>= k
  Put x k -> S.modify (x:) >> k
  Pop k -> S.gets safeHead >>= \case
    Nothing -> k Nothing
    jx -> S.modify tail >> k jx

one :: Stack Int [Int]
one = do
    put 1
    put 2
    get

two :: Stack Int [Int]
two = do
  put 2
  put 4
  mx <- pop
  my <- pop
  case (mx, my) of
    (Just x, Just y) -> put (x + y)
    _ -> pure ()
  get

three :: Stack Int (Maybe Int)
three = pop

main :: IO ()
main = do
  print $ S.evalState (runStack one) []
  print $ S.evalState (runStack two) []
  print $ S.evalState (runStack three) []
