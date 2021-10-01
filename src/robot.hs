-- closure
robot (name, attack, hp) = \f -> f (name, attack, hp)

-- helpers
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h

-- getters
getHP aRobot = aRobot hp
getName aRobot = aRobot name
getAttack aRobot = aRobot attack

-- setters
setHp aRobot newHP = aRobot (\(n,a,h) -> robot (n, a, newHP))
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n, newAttack, h))

-- methods
printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack:" ++ (show a) ++
                                        " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) ->
                                      robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0

-- Examples
--
-- >>> killerRobot = robot ("Bobo", 10, 100)
-- >>> getHp killerRobot => 100
-- >>> getName killerRobot => Bobo
-- >>> getAttack killerRobot => 10
--
-- >>> nicerRobot = setName killerRobot "kitty"
--
-- >>> killerRobot = robot ("Bobo", 25, 200)
-- >>> gentleGiant = robot ("Mr. Friendly", 10, 300)
--
-- >>> gentleGiantRound1 = fight killerRobot gentleGiant
-- >>> killerRobotRound1 = fight gentleGiant killerRobot
-- >>> gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
-- >>> killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
-- >>> gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
-- >>> killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
--
-- >>> printRobot gentleGiantRound3
-- >>> "Mr. Friendly attack:10 hp:225"
-- >>> printRobot killerRobotRound3
-- >>> "Bobo attack:25 hp:170"
