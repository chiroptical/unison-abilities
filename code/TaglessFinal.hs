#!/usr/bin/env stack
-- stack --resolver lts-16.5 script --package transformers

{-# language AllowAmbiguousTypes #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeApplications #-}

module TaglessFinal where

import Control.Monad.Trans.State (State, get, modify, evalState)

class Monad m => Stack repr m a where
  sGet :: repr (m a)
  sPut :: a -> repr ()
  sPop :: repr (Maybe a)

instance Stack (State [Int]) [] Int where
  sGet = get
  sPut x = modify (x:)
  sPop = do
    xs <- get
    case xs of
      (y:_) -> modify tail >> pure (Just y)
      _ -> pure Nothing

one :: State [Int] [Int]
one = do
  sPut @(State [Int]) @[] @Int 1
  sPut @(State [Int]) @[] @Int 2
  sGet @(State [Int]) @[] @Int

two :: State [Int] [Int]
two = do
  sPut @(State [Int]) @[] @Int 2
  sPut @(State [Int]) @[] @Int 4
  mx <- sPop @(State [Int]) @[] @Int
  my <- sPop @(State [Int]) @[] @Int
  case (mx, my) of
    (Just x, Just y) -> sPut @(State [Int]) @[] @Int (x + y)
  sGet @(State [Int]) @[] @Int

three :: State [Int] (Maybe Int)
three = do
  sPop @(State [Int]) @[] @Int

main :: IO ()
main = do
  print $ evalState one []
  print $ evalState two []
  print $ evalState three []
