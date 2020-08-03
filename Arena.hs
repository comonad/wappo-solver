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
showNorthArenaWall fs = '┏':mconcat[if hasWall East f then "━━┳" else "━━━" |f<-init fs]++"━━┓"
showSouthArenaWall fs = '┗':mconcat[if hasWall East f then "━━┻" else "━━━" |f<-init fs]++"━━┛"
showHorizArenaWall :: [Field] -> [Field] -> String
showHorizArenaWall above below = (if hasWall North $ head below then '┣' else '┃'):mconcat
 [ [ "  ·" -- "    "
   , "  ╺" -- "   →"
   , "  ╻" -- "  ↓ "
   , "  ┏" -- "  ↓→"
   , "  ╹" -- " ↑  "
   , "  ┗" -- " ↑ →"
   , "  ┃" -- " ↑↓ "
   , "  ┣" -- " ↑↓→"
   , "━━╸" -- "←   "
   , "━━━" -- "←  →"
   , "━━┓" -- "← ↓ "
   , "━━┳" -- "← ↓→"
   , "━━┛" -- "←↑  "
   , "━━┻" -- "←↑ →"
   , "━━┫" -- "←↑↓ "
   , "━━╋" -- "←↑↓→"
   ]!!(w+n+s+e)
 | (a,b)<-above `zip` (tail below ++ [Set.singleton $ Wall West])
 , let w=if hasWall South a then 8 else 0
 , let n=if hasWall East a then 4 else 0
 , let s=if hasWall West b then 2 else 0
 , let e=if hasWall North b then 1 else 0
 ]

showArenaStride fs = '┃':mconcat[ showFieldContent f ++ if hasWall East f then "┃" else " " | f<-fs ]

showFieldContent f = fmap (head . mconcat)
    [ [ ['X'|HasTrap `Set.member` f]
      , ['g'|HasGoal `Set.member` f]
      , ['*'|HasWarp `Set.member` f]
      , [' ']
      ]
    , [ ['2'|StartPosMonster Rank2 `Set.member` f]
      , ['3'|StartPosMonster Rank3 `Set.member` f]
      , ['p'|StartPosPlayer `Set.member` f]
      , [' ']
      ]
    ]

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






updateArena :: Situation -> Arena -> Arena
updateArena Situation{..} Arena_{..} = Arena_{fields=fields',..}
    where
        fields',fields0 :: Map Pos Field
        fields0 = fmap (Set.\\ Set.fromList[StartPosPlayer,StartPosMonster Rank2,StartPosMonster Rank3]) fields
        fields' = Map.unionWith(Set.union) fields0 $ Map.insertWith(Set.union) player (Set.singleton StartPosPlayer) $ fmap(Set.singleton . StartPosMonster . fst) monsters








