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


module State (
    Path(), emptyPath, addToPath, 
    EndState(..), isDesiredEndState,
    State(..), startState,
    StateDistinction, stateDistinction
) where

import Control.Monad.RWS

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe

import Types
import Arena


newtype Path = Path_ [Direction]
    deriving (Eq,Ord)

instance Show Path where show (Path_ ds) = ["↑→↓←"!!fromEnum d|d<-List.reverse ds] 
emptyPath :: Path
emptyPath = Path_ []
addToPath :: Direction -> Path -> Path
addToPath d (Path_ p) = Path_ (d:p)



data EndState = EWon|ELost|EUnknown String
    deriving (Eq,Ord,Show)

isDesiredEndState :: EndState -> Bool
isDesiredEndState ELost = False
isDesiredEndState _ = True



data State = State { path :: !Path
                   , endOrSituation :: !(Either EndState Situation)
                   }
    deriving (Eq,Ord,Show)


startState :: (?arena :: Arena) => State
startState = State {path=Path_[], endOrSituation = Right startSituation }
    where
        startSituation = Situation {..}
        monsters :: Map Pos (Rank,MonsterState)
        monsters = fmap (,WaitRounds 0) $ Map.mapMaybe getStartPosMonster $ allFields
        player :: Pos
        (player:_) = [p|(p,f)<-Map.toList allFields, StartPosPlayer `Set.member` f]



newtype StateDistinction = StateDistinction_ (Either EndState Integer)
    deriving (Eq,Ord)
stateDistinction :: State -> StateDistinction
stateDistinction State{..} = StateDistinction_ $ fmap packSituation endOrSituation

packSituation :: Situation -> Integer
packSituation Situation{..} = read $ packPos player ++ mconcat
                                        [packPos p++show r++show n
                                        |(p,(fromEnum->r,WaitRounds n))<-Map.toList monsters
                                        ] 
packPos (Pos x y) = show x ++ show y


