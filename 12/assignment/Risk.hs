{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                   deriving (Show)

getAvailibleTroops :: Battlefield -> (Army, Army)
getAvailibleTroops (Battlefield attackers defenders) = (as, ds)
  where as = if attackers > 3 then 3 else attackers - 1
        ds = if defenders > 1 then 2 else 1

-- battle :: Battlefield -> Rand StdGen Battlefield                                
-- battle (Battlefield attackers defenders) = 
