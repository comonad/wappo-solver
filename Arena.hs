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


module Arena (
    allWarps, getField, allFields,
    Arena(), Game(..), getArena,
    updateArena
) where

import Control.Monad.RWS

import Data.Set as Set
import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
import Data.Function (on)
import Data.Char as Char (isLower)

import Types



allWarps :: (?arena :: Arena) => [Pos]
allWarps = warps ?arena

getField :: (?arena :: Arena) => Pos -> Field
getField = (allFields Map.!)

allFields :: (?arena :: Arena) => Map Pos Field
allFields = fields ?arena


data Arena = Arena_
    { fields :: Map Pos Field
    , warps :: [Pos]
    }
    deriving (Eq,Ord)

hasWall :: Direction -> Field -> Bool
hasWall = Set.member . Wall

showNorthArenaWall, showSouthArenaWall :: [Field] -> String
showNorthArenaWall fs = '┏':mconcat[if hasWall East f then "━━━┳" else "━━━━" |f<-init fs]++"━━━┓"
showSouthArenaWall fs = '┗':mconcat[if hasWall East f then "━━━┻" else "━━━━" |f<-init fs]++"━━━┛"
showHorizArenaWall :: [Field] -> [Field] -> String
showHorizArenaWall above below = (if hasWall North $ head below then '┣' else '┃'):mconcat
 [ (
     ["   ","━━━","┄┄┄","┅┅┅"]!!wd
   )++(
     [ "·" -- "    "
     , "╺" -- "   →"
     , "╻" -- "  ↓ "
     , "┏" -- "  ↓→"
     , "╹" -- " ↑  "
     , "┗" -- " ↑ →"
     , "┃" -- " ↑↓ "
     , "┣" -- " ↑↓→"
     , "╸" -- "←   "
     , "━" -- "←  →"
     , "┓" -- "← ↓ "
     , "┳" -- "← ↓→"
     , "┛" -- "←↑  "
     , "┻" -- "←↑ →"
     , "┫" -- "←↑↓ "
     , "╋" -- "←↑↓→"
     ]!!(w+n+s+e)
   )
 | (a,b)<-above `zip` (tail below ++ [Set.singleton $ Wall West])
 , let wd = wallOrDoorNr South a
 , let w=if hasWall South a then 8 else 0
 , let n=if hasWall East a then 4 else 0
 , let s=if hasWall West b then 2 else 0
 , let e=if hasWall North b then 1 else 0
 ]

wallOrDoorNr :: Direction -> Field -> Int
wallOrDoorNr dir f = case (hasWall dir f,fmap isClosed $ hasDoor dir f) of 
                (True,_) -> 1
                (_,Just True) -> 3
                (_,Just False) -> 2
                _ -> 0

showArenaStride fs = '┃':mconcat[ showFieldContent f ++ [" ┃┊┋" !! wallOrDoorNr East f] | f<-fs ]

showFieldContent f = mconcat $ fmap (head . mconcat)
    [ [ ["⛝"|HasTrap `Set.member` f]
      , ["⚐"|HasGoal `Set.member` f]
      , ["⚝"|HasWarp `Set.member` f]
      , ["⊙"|hasSwitch f]
      , [" "]
      ]
    , [ ["🙋"|StartPosMonster Rank2 `Set.member` f]
      , ["🙌"|StartPosMonster Rank3 `Set.member` f]
      , ["🛉 "|StartPosPlayer `Set.member` f]
      , ["  "]
      ]
    ]
-- ⊙
-- ┄┄┅┅┊┋
-- ━┃┏┓┗┛┣┫┳┻╋╸╹╺╻·

instance Show Arena where
    show Arena_{..} = "\n" ++ intercalate "\n" lines ++ "\n"
        where
            --lines' = lines ++ [head lines]
            --lines = mconcat $ 
            --            fmap (\x->zipWith(++)[" ┣"," ┃"][x>>=showField1,x>>=showField2]) $
            --                fs
            rows = fmap showArenaStride fs
            inbetween = zipWith showHorizArenaWall fs (tail fs)
            no = showNorthArenaWall (head fs)
            so = showSouthArenaWall (last fs)
            lines = no:mconcat(zipWith(\a b->[a,b])rows(inbetween++[so]))
            

            fs :: [[Field]]
            fs = fmap (fmap snd . List.sortOn fst) $
                    Map.elems . Map.fromListWith(++) $
                    [(y,[(x,f)])|(Pos x y,f) <- Map.toList fields]


newtype Game = Game [String]

getArena :: Game -> Arena
getArena (Game g) = Arena_ {fields, warps}
    where
        ps = [(Pos x y,sp::Set Props)
             |(l::String,y::Int)<-zip g [1..]
             ,let (cx::[(Char,Int)]) = zip l $ List.tail $ List.scanl (\x c->if c `List.elem` " I_L" then x+1 else x) 0 l
             ,(cs::String,x::Int) <-fmap (\csxs->(fmap fst csxs,snd.head$csxs)) $ groupBy ((==)`on`snd) cx

             ,(c:(fmap DoorId . takeWhile isLower->doorids))<-tails cs
             , not . Char.isLower $ c
             ,let m :: Map Char (Set Props)
                  m = fmap Set.fromList $ Map.fromList $ " I_L23XGP*" `zip` [[],[Wall West],[Wall South],[Wall West,Wall South],[StartPosMonster Rank2],[StartPosMonster Rank3],[HasTrap],[HasGoal],[StartPosPlayer],[HasWarp]]
             ,let p = m Map.! c
             ,let mkdoor d i = HasDoor d$Door{isClosed=Wall d`Set.member`p,doorId=i}
             ,let pp = [f x|(f,x)<-zip[mkdoor West,mkdoor South,HasSwitch] doorids,x/=DoorId 'z']
             ,let nowalls = p Set.\\ Set.fromList[Wall d|(HasDoor d _)<-pp]
             ,let sp = Set.fromList pp `Set.union` nowalls 
             ]
        fields0 = Map.fromListWith(Set.union) ps
        hasneighbourwall :: Direction -> Pos -> Bool
        hasneighbourwall direction p =
            let neighbour = (applyDir direction p) `Map.lookup` fields0
             in maybe True (Set.member $ Wall $ oppositeDirection direction) neighbour
        hasneighbourdoor :: Direction -> Pos -> Maybe Door
        hasneighbourdoor direction p = do
            neighbour <- (applyDir direction p) `Map.lookup` fields0
            hasDoor (oppositeDirection direction) neighbour

        fields = Map.unionWith (Set.union) fields0 $
                 Map.fromListWith (Set.union) [ (p,w)
                                              | d<-[minBound..maxBound]
                                              , p <- Map.keys fields0
                                              , w<-[Set.singleton $ Wall d|hasneighbourwall d p] ++
                                                   [Set.singleton $ HasDoor d door|door<-maybeToList $ hasneighbourdoor d p]
                                              ]
        warps = [p|(p,f)<-Map.toList fields, HasWarp `Set.member` f]



updateDoor :: Map DoorId IsClosed -> Field -> Field
updateDoor ds f = Set.map u f
    where
        u (HasDoor di Door{..}) = HasDoor di Door{isClosed=ds Map.! doorId,..}
        u x = x
    


updateArena :: Situation -> Arena -> Arena
updateArena Situation{..} Arena_{..} = Arena_{fields=fields2,..}
    where
        fields1,fields0 :: Map Pos Field
        fields0 = fmap (Set.\\ Set.fromList[StartPosPlayer,StartPosMonster Rank2,StartPosMonster Rank3]) fields
        fields1 = Map.unionWith(Set.union) fields0 $ Map.insertWith(Set.union) player (Set.singleton StartPosPlayer) $ fmap(Set.singleton . StartPosMonster . fst) monsters
        fields2 = fmap (updateDoor doorStates) fields1







