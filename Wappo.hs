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
        sitting, moved :: [(Pos,((Rank,MonsterState),Moved))]
        sitting = [ (pos,(rm,False)) | x@(pos,rm@(_,WaitRounds _)) <- Map.toList monsters]
        moved = [ (newpos,((r,StepsToGo (s-1)),newpos/=oldpos))
                | (oldpos,(r,StepsToGo s)) <- Map.toList monsters
                , let newpos = List.head $ [np|(walledStep situation oldpos->Just np)<-directionMonsterToPlayer oldpos player] ++ [oldpos]
                ]
        joined = Map.fromListWith joinMonsters $ fmap (fmap Right) $ sitting++moved
        unknowns = intercalate " " [s|Left s <- Map.elems joined]
        newmonsters = Map.mapWithKey triggerMonsterEvent joined
        triggerMonsterEvent pos (Right ((r@Rank2,StepsToGo n),True)) | HasTrap `Set.member` Arena.getField pos = (r,WaitRounds 3)
        triggerMonsterEvent pos (Right ((r,StepsToGo 0),_)) = (r,WaitRounds 0)
        triggerMonsterEvent pos (Right (rm,_)) = rm

playerTriggersEvents situation@Situation{..}
    | fieldContains HasGoal = Left EWon
    | fieldContains HasTrap = Left ELost
    | fieldContains HasWarp = Right situation{player=warpedPlayer}
    | hasSwitch = Right (flipSwitch switch situation)
    | otherwise = Right situation
    where
        playerfield = Arena.getField player
        fieldContains = Set.member `flip` playerfield
        switches = [d|HasSwitch d<-Set.toList playerfield]
        hasSwitch = switches/=[]
        [switch] = switches
        warpedPlayer = List.head [w|w<-Arena.allWarps,w/=player]

evalPlayerMonsterSituation situation@Situation{..}
    | player `Map.member` monsters = Left ELost
    | otherwise = Right situation

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
solveGame arena = List.head [s|s<-states, hasDesiredEndState s]
    where
        states = distinctOn stateDistinction $ let ?arena = arena in startState : (states >>= runSteps)
solveGame' :: Arena -> [State]
solveGame' arena = List.reverse $ List.head [s|s<-states, hasDesiredEndState (head s)]
    where
        states = distinctOn (stateDistinction.head) $ let ?arena = arena in [startState] : [n:ss|ss@(s:_)<-states, n<-runSteps s]


----------------------------------------------------------------------------------------------------

inferno14 = Game
    ["  2  __ "
    ," *IG I_*  "
    ," I_ _I "
    ,"__ II I"
    ,"     LI"
    ," I_X 2  I"
    ,"    P   "
    ]
inferno15 = Game
    ["      X "
    ," _2   LI"
    ,"    II "
    ," XI*I _ I"
    ,"__   _ "
    ," GI II_* "
    ,"  2    P "
    ]
inferno16 = Game
    [" Izza  Iza2I"
    ," 2IbP zzb Id "
    ," I  L_zc"
    ," Izzc  zzdI G"
    ]
inferno46 = Game
    ["____IG"
    ," 2 * 2La "
    ," _  _X"
    ,"   X I"
    ," P _ I"
    ,"   __"
    ,"  I  *"
    ," zza    "
    ]
inferno55 = Game
    ["_____IG"
    ,"  *    "
    ," 2  P  _"
    ,"_X   L "
    ,"_     "
    ,"    X * "
    ," 2 II I"
    ]
inferno57 = Game
    ["  *    2"
    ,"_I  _ "
    ,"      "
    ,"_IL  * X"
    ,"  I_  "
    ," _2___X_P"
    ," GI    "
    ]

classic149 = Game
    [" 2 G_ X  "
    ," 2_I  P "
    ,"  I   "
    ,"   _  "
    ,"_   _IX"
    ,"   _  "
    ,"     X "
    ,"      "
    ]


readGame :: IO Arena
readGame = return $ getArena inferno57

main :: IO ()
main = do
    arena <- readGame
    print $ fmap (view arena) $ solveGame' arena









