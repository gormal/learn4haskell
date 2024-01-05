module Battle where

-- Define a typeclass for fighters
class Fighter a where
    getName :: a -> String
    getHealth :: a -> Int
    getAttack :: a -> Int
    performAction :: Fighter b => a -> b -> a

-- Define data types for Knight and Monster
data Knight = Knight { kName :: String, kHealth :: Int, kAttack :: Int, kDefence :: Int, kActions :: [KnightAction] }
data Monster = Monster { mName :: String, mHealth :: Int, mAttack :: Int, mActions :: [MonsterAction] }

-- Define data types for Knight actions and Monster actions
data KnightAction = KnightAttack | DrinkPotion | CastSpell
data MonsterAction = MonsterAttack | RunAway

-- Implement Fighter instance for Knight
instance Fighter Knight where
    getName = kName
    getHealth = kHealth
    getAttack = kAttack
    performAction knight@(Knight name health attack defence actions) opponent =
        case cycleActions of
            (KnightAttack:rest) -> Knight name (health - getOpponentAttack defence opponent) attack defence rest
            (DrinkPotion:rest) -> Knight name (health + 10) attack defence rest
            (CastSpell:rest) -> Knight name health attack (defence + 1) rest
        where 
            cycleActions = cycle $ actions
            getOpponentAttack :: Fighter b => Int ->  b -> Int
            getOpponentAttack defense opponent
              | defense > getAttack opponent = 0
              | otherwise = getAttack opponent - defense

-- Implement Fighter instance for Monster
instance Fighter Monster where
    getName = mName
    getHealth = mHealth
    getAttack = mAttack
    performAction monster@(Monster name health attack actions) opponent =
        case cycleActions of
            (MonsterAttack:rest) -> Monster name (health - getAttack opponent) attack rest
            (RunAway:rest) -> Monster name 0 attack rest
        where 
          cycleActions = cycle $ actions

fight :: (Fighter a, Fighter b) => a -> b -> String
fight fighter1 fighter2
    | getHealth fighter1 <= 0 = getName fighter2 ++ " wins!"
    | getHealth fighter2 <= 0 = getName fighter1 ++ " wins!"
    | otherwise =
        let fighter1' = performAction fighter1 fighter2
            fighter2' = performAction fighter2 fighter1
        in fight fighter2' fighter1'
