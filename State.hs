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
    EndState(..),
    State(..), startState, playerMoves, updateSituation, hasDesiredEndState,
    walledStep,
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

hasDesiredEndState :: State -> Bool
hasDesiredEndState State {endOrSituation = Left(isDesiredEndState->True)} = True
hasDesiredEndState _ = False

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
        doorStates = Map.fromList[(doorId d,isClosed d)|fs<-Map.elems allFields, (HasDoor _ d)<-Set.toList fs]


updateSituation :: (Situation -> Either EndState Situation) -> State -> State
updateSituation f s@State{endOrSituation = Right situation} = s{endOrSituation = f situation}

walledStep :: (?arena :: Arena) => Situation -> Pos -> Direction -> Maybe Pos
walledStep Situation{doorStates} pos dir = do
    let f = Arena.getField pos
    when (Wall dir `Set.member` f) $ Nothing
    let md = fmap  doorId $ hasDoor dir f
    let iscloseddoor = maybe False (doorStates Map.!) md
    when (iscloseddoor) $ Nothing    
    Just (applyDir dir pos)


playerMoves :: (?arena :: Arena) => Direction -> State -> Maybe State
playerMoves d state = do
    (State {path, endOrSituation=Right Situation{..}}) <- return state
    player' <- walledStep Situation{..} player d
    let path' = addToPath d path
    let monsters' = fmap prepareMonsterStep monsters
    return $ State {path=path',endOrSituation=Right Situation{monsters=monsters',player=player',doorStates}}

prepareMonsterStep :: (Rank,MonsterState) -> (Rank,MonsterState)
prepareMonsterStep (!r,WaitRounds 0) = (r,StepsToGo $ case r of {Rank2->2 ; Rank3->3})
prepareMonsterStep (!r,WaitRounds n) = (r,WaitRounds (n-1))
prepareMonsterStep x = error $ "prepareMonsterStep " ++ show x





newtype StateDistinction = StateDistinction_ (Either EndState Integer)
    deriving (Eq,Ord)
stateDistinction :: State -> StateDistinction
stateDistinction State{..} = StateDistinction_ $ fmap packSituation endOrSituation

packSituation :: Situation -> Integer
packSituation Situation{..} = read $ packPos player ++ mconcat
                                        [packPos p++show r++show n
                                        |(p,(fromEnum->r,WaitRounds n))<-Map.toList monsters
                                        ] ++ [if b then '1' else '0'|b<-Map.elems doorStates]
packPos (Pos x y) = show x ++ show y


