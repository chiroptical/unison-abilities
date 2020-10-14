#!/usr/bin/env stack
-- stack --resolver lts-16.5 script --package transformers

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TaglessFinal where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as S

class Stack repr a where
    sGet :: repr [a]
    sPut :: a -> repr ()
    sPop :: repr (Maybe a)

instance Stack (State [Int]) Int where
    sGet = S.get
    sPut x = S.modify (x :)
    sPop = S.get >>= \case
        (x : xs) -> S.put xs >> pure (Just x)
        _ -> pure Nothing

put :: Int -> State [Int] ()
put = sPut @(State [Int]) @Int

get :: State [Int] [Int]
get = sGet @(State [Int]) @Int 

pop :: State [Int] (Maybe Int)
pop = sPop @(State [Int]) @Int

one :: State [Int] [Int]
one = put 1 >> put 2 >> get

two :: State [Int] [Int]
two = do
    put 2
    put 4
    tup <- (,) <$> pop <*> pop
    case tup of
        (Just x, Just y) -> put (x + y)
        _ -> pure ()
    get

three :: State [Int] (Maybe Int)
three = pop

main :: IO ()
main = do
    print $ S.evalState one []
    print $ S.evalState two []
    print $ S.evalState three []
