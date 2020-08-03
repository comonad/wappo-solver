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


module Main where

import Control.Monad.RWS

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe

import Types
import Arena
import State
import Tools
import View








directionMonsterToPlayer :: Pos -> Pos -> [Direction]
directionMonsterToPlayer (Pos mx my) (Pos px py)
    = [West|px < mx]++[East|mx < px]++[North|my > py]++[South|py > my]


evalPlayerSituation, maybeWarpPlayer, runMonsterStep :: (?arena :: Arena) => State -> State

type Moved = Bool
joinMonsters :: Either String ((Rank,MonsterState),Moved) -> Either String ((Rank,MonsterState),Moved) -> Either String ((Rank,MonsterState),Moved)
joinMonsters (Right((Rank2,_),_)) (Right((Rank2,_),_)) = Right ((Rank3,WaitRounds 0),True)
joinMonsters a b = Left $ "joinMonsters " ++ show a ++ " " ++ show b

runMonsterStep s@State{endOrSituation=Left _} = s
runMonsterStep s@State{endOrSituation=Right Situation{..},..}
    | List.null moved = s
    | not (List.null unknowns) = s{endOrSituation=Left (EUnknown unknowns)}
    | otherwise = runMonsterStep $ s{endOrSituation=Right Situation{monsters=newmonsters,..}}
    where
        sitting :: [(Pos,((Rank,MonsterState),Moved))]
        sitting = [ (pos,(rm,False)) | x@(pos,rm@(_,WaitRounds _)) <- Map.toList monsters]
        moved = [ (newpos,((r,StepsToGo (s-1)),newpos/=oldpos))
                | (oldpos,(r,StepsToGo s)) <- Map.toList monsters
                , let newpos = List.head $ [np|(walledStep oldpos->Just np)<-directionMonsterToPlayer oldpos player] ++ [oldpos]
                ]
        joined = Map.fromListWith joinMonsters $ fmap (fmap Right) $ sitting++moved
        unknowns = intercalate " " [s|Left s <- Map.elems joined]
        newmonsters = Map.mapWithKey triggerMonsterEvent joined
        triggerMonsterEvent pos (Right ((r@Rank2,StepsToGo n),True)) | HasTrap `Set.member` Arena.getField pos = (r,WaitRounds 3)
        triggerMonsterEvent pos (Right ((r,StepsToGo 0),_)) = (r,WaitRounds 0)
        triggerMonsterEvent pos (Right (rm,_)) = rm

maybeWarpPlayer s@State{endOrSituation=Left _} = s
maybeWarpPlayer s = if HasWarp `Set.member` Arena.getField player 
                    then s{endOrSituation=Right Situation{player=warpedPlayer,..}}
                    else s
    where
        (Right Situation{..}) = endOrSituation s
        warpedPlayer = List.head [w|w<-Arena.allWarps,w/=player]

evalPlayerSituation s@State{endOrSituation=Left _} = s
evalPlayerSituation State{..} = State{endOrSituation=maybe endOrSituation Left maybeEndState,..}  
    where
        (Right Situation{..}) = endOrSituation
        x = Arena.getField player
        maybeEndState = listToMaybe . mconcat $
                [ [ELost|Map.member player monsters]
                , [EWon|Set.member HasGoal x]
                , [ELost|Set.member HasTrap x]
                ]

runSteps :: (?arena :: Arena) => State -> [State]
runSteps state = do
    d<-[minBound..maxBound::Direction]
    (Just s)<-[playerMoves d state]
    return $ evalPlayerSituation . runMonsterStep . evalPlayerSituation . maybeWarpPlayer . evalPlayerSituation $ s

solveGame :: Arena -> State
solveGame arena = List.head [s|s@(State {endOrSituation=Left e})<-states, isDesiredEndState e]
    where
        states = distinctOn stateDistinction $ let ?arena = arena in startState : (states >>= runSteps)
solveGame' :: Arena -> [State]
solveGame' arena = List.reverse $ List.head $ [s|s@(State {endOrSituation=Left e}:_)<-states, isDesiredEndState e]
    where
        states = distinctOn (stateDistinction.head) $ let ?arena = arena in [startState] : [n:ss|ss@(s:_)<-states, n<-runSteps s]


----------------------------------------------------------------------------------------------------

inferno14 = Game
    ["  2  __ "
    ," *Ig I_*  "
    ," I_ _I "
    ,"__ II I"
    ,"     LI"
    ," I_X 2  I"
    ,"    p   "
    ]
inferno15 = Game
    ["      X "
    ," _2   LI"
    ,"    II "
    ," XI*I _ I"
    ,"__   _ "
    ," gI II_* "
    ,"  2    p "
    ]
classic149 = Game
    [" 2 g_ X  "
    ," 2_I  p "
    ,"  I   "
    ,"   _  "
    ,"_   _IX"
    ,"   _  "
    ,"     X "
    ,"      "
    ]

readGame :: IO Arena
readGame = return $ getArena classic149

main :: IO ()
main = do
    arena <- readGame
    print $ fmap (view arena) $ solveGame' arena









