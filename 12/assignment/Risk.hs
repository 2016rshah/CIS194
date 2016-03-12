{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sort)
import Control.Applicative

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

-- | rolls specified number of dice
dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                   deriving (Show)

getAvailibleTroops :: Battlefield -> (Army, Army)
getAvailibleTroops (Battlefield attackers defenders) = (as, ds)
  where as = if attackers > 3 then 3 else attackers - 1
        ds = if defenders > 1 then 2 else 1

getRolls :: Army -> Rand StdGen [DieValue]
getRolls n = fmap (reverse . sort) (dice n)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield allAs allDs) = afterKilling >>= \ (remAs, remDs) -> return (Battlefield (remAs) (remDs))
  where
    (avAs, avDs) = getAvailibleTroops bf
    aRolls = getRolls avAs
    dRolls = getRolls avDs
    matchups = zip <$> aRolls <*> dRolls -- Rand StdGen [(DieValue, DieValue)]
    afterKilling = fmap (foldr killSoldier (allAs, allDs)) matchups -- Rand StdGen (Army, Army)

killSoldier :: (DieValue, DieValue) -> (Army, Army) -> (Army, Army)
killSoldier (x, y) (a, b) = if x > y then (a, b-1) else (a-1, b)

-- threeInts :: Rand StdGen (Int, Int, Int)
-- threeInts =
--   getRandom >>= \i1 ->
--   getRandom >>= \i2 ->
--   getRandom >>= \i3 ->
--   return (i1,i2,i3)
--evalRandIO is a way to print Rand StdGen stuff


--s <- newStdGen
--evalRand (battle (Battlefield 2 4)) s

--Exercise 2
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = battle bf >>= \ rbf@(Battlefield remAs remDs) -> if remDs > 0 && remAs > 1 then invade rbf else return rbf

success :: [Battlefield] -> Rand StdGen Double
success bfs = return ((fromIntegral successfulBattles) / (fromIntegral allBattles))
  where
    allBattles = (length bfs)
    successfulBattles = length (filter ((==) 0 . defenders) bfs)
    
successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= success
