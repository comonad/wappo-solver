{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}


module View (
    View(), view
) where

import Control.Monad.RWS

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe

import Types
import Arena
import State
import Tools




data View x = WithA Arena x
instance Show (View Situation) where
    show (WithA a Situation{..}) = let ?arena=a in show (player,Map.keys monsters)
instance Show (View State) where
    show (WithA a s@State{endOrSituation=Left x,..}) = let ?arena=a in show path ++"\n"++ show x ++ "\n"
    show (WithA a State{..}) = let ?arena=a in show path ++ (either show (\s->show $ updateArena s a) endOrSituation)


view :: Arena -> State -> View State
view = WithA
