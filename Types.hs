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


module Types  where

import Control.Monad.RWS

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe


data Rank = Rank2|Rank3
    deriving (Eq,Ord,Show,Enum,Bounded)

data Direction = North|East|South|West
    deriving (Eq,Ord,Show,Enum,Bounded)

data Pos = Pos !Int !Int
    deriving (Eq,Ord,Show)

data Props = Wall Direction|HasTrap|HasGoal|HasWarp|StartPosPlayer|StartPosMonster Rank
    deriving (Eq,Ord,Show)

type Field = Set Props


applyDir :: Direction -> Pos -> Pos
applyDir North (Pos x y) = Pos x (y-1)
applyDir East  (Pos x y) = Pos (x+1) y
applyDir South (Pos x y) = Pos x (y+1)
applyDir West  (Pos x y) = Pos (x-1) y


data MonsterState = WaitRounds !Int | StepsToGo !Int
    deriving (Eq,Ord,Show)


getStartPosMonster :: Field -> Maybe Rank
getStartPosMonster f = listToMaybe [r|(StartPosMonster r) <- Set.toList f]



data Situation = Situation
    { monsters :: Map Pos (Rank,MonsterState)
    , player :: Pos
    }
    deriving (Eq,Ord,Show)




