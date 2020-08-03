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


module Tools  where

--import Control.Monad.RWS

import Data.Set as Set
--import Data.Map as Map
--import Data.List as List
--import Data.Maybe as Maybe





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


