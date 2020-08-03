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


prepareMonsterStep :: (Rank,MonsterState) -> (Rank,MonsterState)
prepareMonsterStep (!r,WaitRounds 0) = (r,StepsToGo $ case r of {Rank2->2 ; Rank3->3})
prepareMonsterStep (!r,WaitRounds n) = (r,WaitRounds (n-1))
prepareMonsterStep x = error $ "prepareMonsterStep " ++ show x

newtype Game = Game [String]


data MonsterState = WaitRounds !Int | StepsToGo !Int
    deriving (Eq,Ord,Show)

getStartPosMonster :: Field -> Maybe Rank
getStartPosMonster f = listToMaybe [r|(StartPosMonster r) <- Set.toList f]

data Props = Wall Direction|HasTrap|HasGoal|HasWarp|StartPosPlayer|StartPosMonster Rank
    deriving (Eq,Ord,Show)
type Field = Set Props
showField1 f = if Wall North `Set.member` f then "--+" else "  +"
showField2 f = (if StartPosMonster Rank2 `Set.member` f then "2" else if StartPosMonster Rank3 `Set.member` f then "3" else if StartPosPlayer `Set.member` f then "p" else " ") ++ (if HasTrap `Set.member` f then "X" else if HasGoal `Set.member` f then "g" else if HasWarp `Set.member` f then "*" else " ") ++ (if Wall East `Set.member` f then "|" else " ")

data Arena = Arena
    { fields :: Map Pos Field
    , warps :: [Pos]
    }
    deriving (Eq,Ord)

instance Show Arena where
    show Arena{..} = "\n" ++ intercalate "\n" lines' ++ "\n"
        where
            lines' = lines ++ [head lines]
            lines = mconcat $ Map.elems $
                        fmap (\x->[x>>=showField1,x>>=showField2]) $
                        fmap (fmap snd . List.sortOn fst) $
                        Map.fromListWith(++)[(y,[(x,f)])|(Pos x y,f) <- Map.toList fields]


getArena :: Game -> Arena
getArena (Game g) = Arena {fields, warps}
    where
        ps = [(Pos x y,p::Set Props)
             |(l::String,y::Int)<-zip g [1..]
             ,(c::Char,x::Int)<-zip l $ List.tail $ List.scanl (\x c->if c `List.elem` " I_L" then x+1 else x) 0 l
             ,let m :: Map Char (Set Props)
                  m = fmap Set.fromList $ Map.fromList $ " I_L23Xgp*" `zip` [[],[Wall West],[Wall South],[Wall West,Wall South],[StartPosMonster Rank2],[StartPosMonster Rank3],[HasTrap],[HasGoal],[StartPosPlayer],[HasWarp]]
             ,let p = m Map.! c
             ]
        fields0 = Map.fromListWith(Set.union) ps
        hasneighbourwall :: Direction -> Pos -> Bool
        hasneighbourwall direction p =
            let oppositedir = toEnum $ (fromEnum direction + 2) `mod` 4
                neighbour = (applyDir direction p) `Map.lookup` fields0
             in maybe True (Set.member $ Wall oppositedir) neighbour
        fields = Map.unionWith (Set.union) fields0 $
                 Map.fromListWith (Set.union) [ (p,w)
                                              | d<-[minBound..maxBound]
                                              , let w = Set.singleton $ Wall d
                                              , p <- Map.keys fields0
                                              , hasneighbourwall d p
                                              ]
        warps = [p|(p,f)<-Map.toList fields, HasWarp `Set.member` f]




data State = State { path :: !Path
                   , endOrSituation :: !(Either EndState Situation)
                   }
    deriving (Eq,Ord,Show)




data Situation = Situation
    { monsters :: Map Pos (Rank,MonsterState)
    , player :: Pos
    }
    deriving (Eq,Ord,Show)

data WithA x = WithA Arena x
instance Show (WithA Situation) where
    show (WithA a Situation{..}) = let ?arena=a in show (player,Map.keys monsters)
instance Show (WithA State) where
    --show (WithA a s@State{..}) = let ?arena=a in show (updateArena s a) ++"\n"++ show path ++ either show (show . WithA a) endOrSituation ++"\n"
    show (WithA a s@State{endOrSituation=Left x,..}) = let ?arena=a in show path ++"\n"++ show x ++ "\n"
    show (WithA a s@State{..}) = let ?arena=a in show path ++ show (updateArena s a)

updateArena :: State -> Arena -> Arena
updateArena State{..} Arena{..} = Arena{fields=fields',..}
    where
        fields',fields0 :: Map Pos Field
        fields0 = fmap (Set.\\ Set.fromList[StartPosPlayer,StartPosMonster Rank2,StartPosMonster Rank3]) fields
        fields' = either (const fields0) ff endOrSituation
        ff :: Situation -> Map Pos Field
        ff Situation{..} = Map.unionWith(Set.union) fields0 $ Map.insertWith(Set.union) player (Set.singleton StartPosPlayer) $ fmap(Set.singleton . StartPosMonster . fst) monsters

startState :: (?arena :: Arena) => State
startState = State {path=Path_[], endOrSituation = Right startSituation }
    where
        startSituation = Situation {..}
        monsters :: Map Pos (Rank,MonsterState)
        monsters = fmap (,WaitRounds 0) $ Map.mapMaybe getStartPosMonster $ fields ?arena
        player :: Pos
        (player:_) = [p|(p,f)<-Map.toList $ fields ?arena, StartPosPlayer `Set.member` f]

getWarps :: (?arena :: Arena) => [Pos]
getWarps = warps ?arena

getField :: (?arena :: Arena) => Pos -> Field
getField p = fields ?arena Map.! p

walledStep :: (?arena :: Arena) => Pos -> Direction -> Maybe Pos
walledStep pos dir = if Wall dir `Set.member` getField pos
                      then Nothing
                      else Just (applyDir dir pos)

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
        triggerMonsterEvent pos (Right ((r@Rank2,StepsToGo n),True)) | HasTrap `Set.member` getField pos = (r,WaitRounds 3)
        triggerMonsterEvent pos (Right ((r,StepsToGo 0),_)) = (r,WaitRounds 0)
        triggerMonsterEvent pos (Right (rm,_)) = rm

maybeWarpPlayer s@State{endOrSituation=Left _} = s
maybeWarpPlayer s = if HasWarp `Set.member` getField player 
                    then s{endOrSituation=Right Situation{player=warpedPlayer,..}}
                    else s
    where
        (Right Situation{..}) = endOrSituation s
        warpedPlayer = List.head [w|w<-getWarps,w/=player]

evalPlayerSituation s@State{endOrSituation=Left _} = s
evalPlayerSituation State{..} = State{endOrSituation=maybe endOrSituation Left maybeEndState,..}  
    where
        (Right Situation{..}) = endOrSituation
        x = getField player
        maybeEndState = listToMaybe . mconcat $
                [ [ELost|Map.member player monsters]
                , [EWon|Set.member HasGoal x]
                , [ELost|Set.member HasTrap x]
                ]


runSteps :: (?arena :: Arena) => State -> [State]
runSteps (State { endOrSituation=Left _}) = []
runSteps (State {path, endOrSituation=Right Situation{..}}) = do
    d<-[minBound..maxBound::Direction]
    Just player' <-[walledStep player d]
    let path' = addToPath d path
    
    let monsters' = fmap prepareMonsterStep monsters
    return $ evalPlayerSituation . runMonsterStep . evalPlayerSituation . maybeWarpPlayer . evalPlayerSituation
           $ State {path=path',endOrSituation=Right Situation{monsters=monsters',player=player'}}


solveGame :: Arena -> WithA State
solveGame arena = (WithA arena) $ List.head [s|s@(State {endOrSituation=Left e})<-states, isDesiredEndState e]
    where
        states = distinctOn stateDistinction $ let ?arena = arena in startState : (states >>= runSteps)
solveGame' :: Arena -> [WithA State]
solveGame' arena = List.reverse $ fmap (WithA arena) $ List.head $ [s|s@(State {endOrSituation=Left e}:_)<-states, isDesiredEndState e]
    where
        states = distinctOn (stateDistinction.head) $ let ?arena = arena in [startState] : [n:ss|ss@(s:_)<-states, n<-runSteps s]

isDesiredEndState :: EndState -> Bool
isDesiredEndState ELost = False
isDesiredEndState _ = True

data EndState = EWon|ELost|EUnknown String
    deriving (Eq,Ord,Show)


data Rank = Rank2|Rank3
    deriving (Eq,Ord,Show,Enum,Bounded)

data Pos = Pos !Int !Int
    deriving (Eq,Ord,Show)


data Direction = North|East|South|West
    deriving (Eq,Ord,Show,Enum,Bounded)

newtype Path = Path_ [Direction]
    deriving (Eq,Ord)

instance Show Path where show (Path_ ds) = ["↑→↓←"!!fromEnum d|d<-List.reverse ds] 
emptyPath :: Path
emptyPath = Path_ []
addToPath :: Direction -> Path -> Path
addToPath d (Path_ p) = Path_ (d:p)

applyDir :: Direction -> Pos -> Pos
applyDir North (Pos x y) = Pos x (y-1)
applyDir East  (Pos x y) = Pos (x+1) y
applyDir South (Pos x y) = Pos x (y+1)
applyDir West  (Pos x y) = Pos (x-1) y



stateDistinction :: State -> Either EndState Integer
stateDistinction State{..} = fmap packSituation endOrSituation
packSituation :: Situation -> Integer
packSituation Situation{..} = read $ packPos player ++ mconcat
                                        [packPos p++show r++show n
                                        |(p,(fromEnum->r,WaitRounds n))<-Map.toList monsters
                                        ] 
packPos (Pos x y) = show x ++ show y


distinct :: Ord a => [a] -> [a]
distinct = distinct' Set.empty
    where
        distinct' s [] = []
        distinct' s (a:ar) = [a|not $ a `Set.member` s]++distinct' (Set.insert a s) ar

distinctOn :: Ord b => (a->b) -> [a] -> [a]
distinctOn f = distinct' Set.empty
    where
        distinct' s [] = []
        distinct' s (a:ar) = let b=f a in [a|not $ b `Set.member` s]++distinct' (Set.insert b s) ar



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
    game <- readGame
    print $ solveGame' game









