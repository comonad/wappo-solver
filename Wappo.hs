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

import Control.Monad
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


evalPlayerMonsterSituation, playerTriggersEvents, runMonsterSteps :: (?arena :: Arena) => Situation -> Either EndState Situation

type Moved = Bool
joinMonsters :: Either String ((Rank,MonsterState),Moved) -> Either String ((Rank,MonsterState),Moved) -> Either String ((Rank,MonsterState),Moved)
joinMonsters (Right((Rank2,_),_)) (Right((Rank2,_),_)) = Right ((Rank3,WaitRounds 0),True)
joinMonsters a b = Left $ "joinMonsters " ++ show a ++ " " ++ show b

runMonsterSteps situation@Situation{..}
    | List.null moved = Right situation
    | not (List.null unknowns) = Left (EUnknown unknowns)
    | otherwise = runMonsterSteps $ Situation{monsters=newmonsters,..}
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

playerTriggersEvents situation@Situation{..} =
    let fieldContains = Set.member `flip` Arena.getField player
        warpedPlayer = List.head [w|w<-Arena.allWarps,w/=player]
     in List.head $ mconcat
            [ [Left EWon|fieldContains HasGoal]
            , [Left ELost|fieldContains HasTrap]
            , [Right situation{player=warpedPlayer}|fieldContains HasWarp]
            , [Right Situation{..}]
            ]

evalPlayerMonsterSituation situation@Situation{..} =
    if player `Map.member` monsters then Left ELost else Right situation

runSteps :: (?arena :: Arena) => State -> [State]
runSteps state0 = do
    d <- [minBound..maxBound::Direction]
    state1 <- maybeToList $ playerMoves d state0
    --updateSituation :: (Situation -> Either EndState Situation) -> State -> State
    return $ (`State.updateSituation` state1) $ foldr1 (>=>)
        [ evalPlayerMonsterSituation
        , playerTriggersEvents
        , evalPlayerMonsterSituation
        , runMonsterSteps
        , evalPlayerMonsterSituation
        ]


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









