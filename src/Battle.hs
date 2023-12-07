{-# LANGUAGE GADTs #-}

-- Import necessary modules
import Data.List (cycle)

-- Define a typeclass for common fighter properties
class Fighter a where
  -- Function to get the current health of the fighter
  getHealth :: a -> Int
  -- Function to apply damage to the fighter
  applyDamage :: Int -> a -> a
  -- Function to check if the fighter is still alive
  isAlive :: a -> Bool
  isAlive fighter = getHealth fighter > 0
  -- Function to perform the next action in the sequence
  nextAction :: a -> (a, Action)

-- Define the actions available to fighters
data Action = Attack Int | DrinkPotion Int | CastSpell Int | RunAway deriving (Show)

-- Define the data type for a knight
data Knight = Knight {
  knightHealth :: Int,
  knightDefense :: Int,
  knightActions :: [Action]
} deriving (Show)

-- Make Knight an instance of the Fighter typeclass
instance Fighter Knight where
  getHealth = knightHealth
  applyDamage dmg knight = knight { knightHealth = max 0 (knightHealth knight - dmg + knightDefense knight) }
  nextAction knight@(Knight _ _ actions) = (knight, head actions)

-- Define the data type for a monster
data Monster = Monster {
  monsterHealth :: Int,
  monsterAttack :: Int,
  monsterActions :: [Action]
} deriving (Show)

-- Make Monster an instance of the Fighter typeclass
instance Fighter Monster where
  getHealth = monsterHealth
  applyDamage dmg monster = monster { monsterHealth = max 0 (monsterHealth monster - dmg) }
  nextAction monster@(Monster _ _ actions) = (monster, head actions)

-- Function to simulate a battle between two fighters
battle :: (Fighter a, Fighter b) => a -> b -> Either a b
battle fighter1 fighter2 = go (cycleActions fighter1) (cycleActions fighter2)
  where
    -- Cycle the actions of the fighters indefinitely
    cycleActions fighter = cycle $ knightActions fighter
    -- Recursive function to simulate each turn of the battle
    go f1@(nextAction -> (nf1, action1)) f2@(nextAction -> (nf2, action2))
      | not (isAlive nf1) = Right nf2
      | not (isAlive nf2) = Left nf1
      | otherwise = case action1 of
          Attack dmg -> go nf1 (applyDamage dmg f2)
          DrinkPotion amount -> go (nf1 { knightHealth = knightHealth nf1 + amount }) f2
          CastSpell def -> go (nf1 { knightDefense = knightDefense nf1 + def }) f2
          RunAway -> Right nf2 -- If the monster runs away, the knight wins

-- Example usage:
-- Create a knight with 100 health, 10 defense, and a sequence of actions
let knight = Knight 100 10 [Attack 20, DrinkPotion 10, CastSpell 5]
-- Create a monster with 120 health, 15 attack, and a sequence of actions
let monster = Monster 120 15 [Attack 15, RunAway]
-- Simulate the battle
battle knight monster