{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

getCoordonates :: Field -> Dimension
getCoordonates field = fst field

getEntity:: Field -> Entity
getEntity field = snd field


data Entity = BlankEntity | HunterEntity | TargetEntity{target :: Target} | ObstacleEntity | GatewayEntity {gatePosition :: (Position, Position), hasTarget :: Bool, hasHunter :: Bool}
    deriving (Eq, Ord)

type Dimension = (Int, Int)
type Field = (Position,Entity)


data Game = Game {  dimension :: Dimension,
                    gameBoard :: [[Field]],
                    targetList :: [Target],
                    hunterPosition :: Position,
                    moveTargetsList :: [Target]
                } deriving(Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

showEntity :: Field -> String
showEntity (position, entity) = show entity

gameAsString :: Game -> String
gameAsString game = intercalate "\n" (map (\x -> concatMap showEntity x)  (gameBoard game))

instance Show Entity
    where
        show BlankEntity = " "
        show HunterEntity = "!"
        show (TargetEntity _) = "*"
        show ObstacleEntity = "@"
        show (GatewayEntity _ True _) = "*"
        show (GatewayEntity _ _ True) = "!"
        show (GatewayEntity _ False False) = "#"
instance Show Game where
    show game = gameAsString game

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame rows columns = Game { dimension = (rows,columns),
                                gameBoard = [  [if x == 0 || x == rows - 1 || y == 0 || y == columns - 1 then  ((x,y), ObstacleEntity) 
                                                                                                                else   if  x == 1 && y == 1  
                                                                                                                    then ((x,y), HunterEntity) 
                                                                                                                    else ((x,y), BlankEntity) 
                                                                                                            | y <- [0..columns-1] ] | x <- [0..rows-1] ],
                                targetList = [],
                                hunterPosition = (1,1),
                                moveTargetsList = []
                                }

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

vanishOldHunter :: Game -> Game
vanishOldHunter game  = Game {dimension = (dimension game)
                             ,gameBoard = (map (\x -> (map (\y -> if (snd y) == (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game))))
                                            then 
                                                if ((snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game)))) == GatewayEntity{ gatePosition = (gatePosition (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game))))), hasTarget = (hasTarget (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game))))),  hasHunter = (hasHunter (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game)))))}) then ((getCoordonates y), GatewayEntity{ gatePosition = (gatePosition (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game))))), hasTarget = (hasTarget (snd ((gameBoard game) !! (fst (hunterPosition game)) !! (snd (hunterPosition game))))),  hasHunter = False}) else ((getCoordonates y),BlankEntity)
                                            else  y  ) x ) ) (gameBoard game))
                             ,targetList = (targetList game)
                             ,hunterPosition = (-1,-1)
                             ,moveTargetsList = (moveTargetsList game)
                            }

checkEmpty :: Position -> Game -> Bool
checkEmpty position game = if snd ((gameBoard game) !! (fst position) !! (snd position)) == BlankEntity || show (snd ((gameBoard game) !! (fst position) !! (snd position))) == "#" ||  show (snd ((gameBoard game) !! (fst position) !! (snd position))) == "*" then True else False

targetGateway :: Position -> Entity -> Game -> Game
targetGateway position entity game = if (snd ((gameBoard game) !! (fst position) !! (snd position))) == GatewayEntity{ gatePosition = (gatePosition (snd ((gameBoard game) !! (fst position) !! (snd position)))) , hasTarget = (hasTarget (snd ((gameBoard game) !! (fst position) !! (snd position)))),  hasHunter = (hasHunter (snd ((gameBoard game) !! (fst position) !! (snd position))))}
                                        then ------
                                            Game {dimension = (dimension game)
                                             ,gameBoard = (map (\x -> (map (\y -> if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) then ((getCoordonates y), GatewayEntity{ gatePosition = (gatePosition (snd y)) , hasTarget = True, hasHunter = False} ) else ((getCoordonates y), (getEntity y))  ) x ) ) (gameBoard game))
                                             ,targetList = (targetList game) ++ [target entity]
                                             ,hunterPosition =  (hunterPosition game)
                                             ,moveTargetsList = (moveTargetsList game)
                                            }
                                        else    
                                            Game {dimension = (dimension game)
                                             ,gameBoard = (map (\x -> (map (\y -> if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) then ((getCoordonates y), entity) else ((getCoordonates y), (getEntity y))  ) x ) ) (gameBoard game))
                                             ,targetList = (targetList game) ++ [target entity]
                                             ,hunterPosition =  (hunterPosition game)
                                             ,moveTargetsList = (moveTargetsList game)
                                            }

hunterGateway :: Position -> Entity -> Game -> Game
hunterGateway position entity game = if (snd ((gameBoard game) !! (fst position) !! (snd position))) == GatewayEntity{ gatePosition = (gatePosition (snd ((gameBoard game) !! (fst position) !! (snd position)))) , hasTarget = (hasTarget (snd ((gameBoard game) !! (fst position) !! (snd position)))),  hasHunter = (hasHunter (snd ((gameBoard game) !! (fst position) !! (snd position))))}
                                        then ------
                                            Game {dimension = (dimension game)
                                             ,gameBoard = (map (\x -> (map (\y -> if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) then ((getCoordonates y), GatewayEntity{ gatePosition = (gatePosition (snd y)) , hasTarget = False, hasHunter = True} ) else ((getCoordonates y), (getEntity y))  ) x ) ) (gameBoard game))
                                             ,targetList = (targetList game)
                                             ,hunterPosition =  position
                                             ,moveTargetsList = (moveTargetsList game)
                                            }
                                        else    
                                            Game {dimension = (dimension game)
                                             ,gameBoard = (map (\x -> (map (\y -> if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) then ((getCoordonates y), entity) else ((getCoordonates y), (getEntity y))  ) x ) ) (gameBoard game))
                                             ,targetList = (targetList game)
                                             ,hunterPosition =  position
                                             ,moveTargetsList = (moveTargetsList game)
                                            }

addHelper :: Position -> Entity -> Game -> Game
addHelper position entity game 
    | (show entity) == "*" = (targetGateway position entity game)
    | (show entity) == "!" = (hunterGateway position entity game)
    | otherwise =    Game {dimension = (dimension game)
                        ,gameBoard = (map (\x -> (map (\y -> if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) then ((getCoordonates y), entity) else ((getCoordonates y), (getEntity y))  ) x ) ) (gameBoard game))
                        ,targetList = (targetList game)
                        ,hunterPosition =  (hunterPosition game)
                        ,moveTargetsList = (moveTargetsList game)
                    }

checkOutOFBounds :: Position -> Game -> Bool
checkOutOFBounds position game = if (fst position) > (fst (dimension game)-1) || (snd position) > (snd (dimension game)-1) || (fst position) < 0  || (snd position) < 0 then False else True

addEntity :: Position -> Entity -> Game -> Game
addEntity position entity game = if (checkOutOFBounds position game) == False  
                                then game  
                                else if (checkEmpty position game) == True then (addHelper position entity game) 
                                                                            else game 
addEntityNoEmptyCheck :: Position -> Entity -> Game -> Game
addEntityNoEmptyCheck position entity game = if (fst position) > (fst (dimension game)-1) || (snd position) > (snd (dimension game)-1) || (fst position) < 0  || (snd position) < 0  
                                then game  
                                else if entity == HunterEntity  then  (addHelper position entity (vanishOldHunter game)) else (addHelper position entity game)
                                                                            

addHunter :: Position -> Game -> Game
addHunter position game = if (checkOutOFBounds position game) == True && (checkEmpty position game) == True then Game   {dimension = (dimension (addHelper position HunterEntity (vanishOldHunter game)))
                                                                                                                        ,gameBoard = (gameBoard (addHelper position HunterEntity (vanishOldHunter game)))
                                                                                                                        ,targetList = (targetList (addHelper position HunterEntity (vanishOldHunter game)))
                                                                                                                        ,hunterPosition = position
                                                                                                                        ,moveTargetsList = (moveTargetsList (addHelper position HunterEntity (vanishOldHunter game))) }
     else game


{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Tar get-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget targetBehavior targetPosition game = addEntity targetPosition TargetEntity{target = Target {position = targetPosition, behavior = targetBehavior } } game 

                                                                                                                                                                    

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
addGateway :: (Position, Position) -> Game -> Game
addGateway positions game = addEntityNoEmptyCheck (snd positions) (GatewayEntity {gatePosition = ((fst positions),(snd positions)), hasTarget = False, hasHunter = False}) (addEntityNoEmptyCheck (fst positions)  (GatewayEntity {gatePosition = ((snd positions),(fst positions)), hasTarget = False, hasHunter = False}) game) 
    
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle  position game = addEntity position ObstacleEntity game 

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

checkEntity :: Position -> Game -> Entity -> Maybe Position
checkEntity position game entity
    | entity == BlankEntity = Just (position)
    | entity == GatewayEntity {gatePosition = (gatePosition  (snd ((gameBoard game) !! (fst position) !! (snd position))) ) , hasTarget = (hasTarget  (snd ((gameBoard game) !! (fst position) !! (snd position))) ), hasHunter = (hasHunter  (snd ((gameBoard game) !! (fst position) !! (snd position))) )}  = Just (fst (gatePosition  (snd ((gameBoard game) !! (fst position) !! (snd position))) ))
    | entity == ObstacleEntity = Nothing
    | otherwise = Nothing

checkEmptyMove :: Position -> Game -> Maybe Position
checkEmptyMove position game =  checkEntity position game (snd ((gameBoard game) !! (fst position) !! (snd position)))

attemptMove :: Position -> Game -> Maybe Position
attemptMove position game = if (fst position) > (fst (dimension game)-1) || (snd position) > (snd (dimension game)-1) || (fst position) < 0  || (snd position) < 0  
                                then Nothing else checkEmptyMove position game
                                                    


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

goSomewhere :: Direction -> Behavior
goSomewhere direction position game
    | direction == East && (attemptMove ((fst position),(snd position) + 1) game) == Nothing = Target {
                                                                                                            position = position,
                                                                                                            behavior = goEast
                                                                                                      }
                                                                                 
    | direction == East && (attemptMove ((fst position),(snd position) + 1) game) /= Nothing = Target {
                                                                                                            position = ((fst position),(snd position) + 1),
                                                                                                            behavior = goEast
                                                                                                      } 
   
    | direction == West && (attemptMove ((fst position),(snd position) - 1) game) == Nothing = Target {
                                                                                                            position = position,
                                                                                                            behavior = goWest
                                                                                                      }
                                                                                 
    | direction == West && (attemptMove ((fst position),(snd position) - 1) game) /= Nothing = Target {
                                                                                                            position = ((fst position),(snd position) - 1),
                                                                                                            behavior = goWest
                                                                                                      }
   
    | direction == North && (attemptMove ((fst position) - 1,(snd position)) game) == Nothing = Target {
                                                                                                            position = position,
                                                                                                            behavior = goNorth
                                                                                                      }
                                                                                 
    | direction == North && (attemptMove ((fst position) - 1,(snd position)) game) /= Nothing = Target {
                                                                                                            position = ((fst position) - 1,(snd position)),
                                                                                                            behavior = goNorth
                                                                                                      }

    | direction == South && (attemptMove ((fst position) + 1,(snd position)) game) == Nothing = Target {
                                                                                                            position = position,
                                                                                                            behavior = goSouth
                                                                                                      }
                                                                                 
    | direction == South && (attemptMove ((fst position) + 1,(snd position)) game) /= Nothing = Target {
                                                                                                            position = ((fst position) + 1,(snd position)),
                                                                                                            behavior = goSouth
                                                                                                      }


goEast :: Behavior
goEast position game = goSomewhere East position game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest position game = goSomewhere West position game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth position game = goSomewhere North position game

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth position game = goSomewhere South position game
{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior 
bounce targetMovement position game
                                                                              
    | (attemptMove ((fst position) - 1, (snd position)) game) == Nothing && targetMovement == (-1) = Target { -- trebuie sa vedem daca ajunge pe un gateway deci tre sa folosim attempt 
                                                                                    position =  (fst position + 1, snd position),
                                                                                    behavior = (bounce (1))
                                                                                }
    | (attemptMove ((fst position) + 1, (snd position)) game) == Nothing && targetMovement == (1) = Target  {
                                                                                    position = (fst position - 1, snd position),
                                                                                    behavior = (bounce (-1))
                                                                                }
    | (attemptMove ((fst position) - 1, (snd position)) game) /= Nothing && targetMovement == (-1) = Target {
                                                                                    position = (fst position - 1, snd position),
                                                                                    behavior = (bounce (-1))
                                                                                }
    | (attemptMove ((fst position) + 1, (snd position)) game) /= Nothing && targetMovement == (1) = Target  {
                                                                                    position = (fst position + 1, snd position),
                                                                                    behavior = (bounce (1))
                                                                                }
{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
clearTargets :: Game -> Game --JEGU STERGE GATEWAY-UL SI SARACU NU STIE CE MAI FACE
clearTargets game = Game {dimension = (dimension game)
                         ,gameBoard = (map (\x -> (map (\y -> if (show (snd y)) == "*" then if (snd y ) == GatewayEntity {gatePosition = gatePosition (snd y), hasTarget = hasTarget (snd y) , hasHunter = hasHunter (snd y)} then ((getCoordonates y), GatewayEntity {gatePosition = gatePosition (snd y), hasTarget = False, hasHunter = (hasHunter (snd y))}) else ((getCoordonates y), BlankEntity) else ((getCoordonates y), (getEntity y)) ) x ) ) (gameBoard game) )
                         ,targetList = (targetList game)
                         ,hunterPosition = (hunterPosition game)
                         ,moveTargetsList = (moveTargetsList game)
                         }

addAllTargetsHelper :: Game -> Game -- Daca e bounce tu tre sa misti daca rahat de se blocheacheaza tre sa ramana pe respecitiva -- DA CACA
addAllTargetsHelper game  =  Game {dimension = (dimension game)
                                  ,gameBoard = (gameBoard (addTarget  ( (behavior (((behavior (head (moveTargetsList game))))  (position (head (moveTargetsList game))) game)) )  (fromJust (attemptMove (position (((behavior (head (moveTargetsList game))))  (position (head (moveTargetsList game))) game)) game)) game) )
                                  ,targetList = (targetList (addTarget  ( (behavior (((behavior (head (moveTargetsList game))))  (position (head (moveTargetsList game))) game))) (fromJust (attemptMove (position (((behavior (head (moveTargetsList game))))  (position (head (moveTargetsList game))) game)) game)) game) )
                                  ,hunterPosition = (hunterPosition game)
                                  ,moveTargetsList = drop 1 (moveTargetsList game)
                                  }

 

addAllTargetsEmptyList :: Game -> Game
addAllTargetsEmptyList game = if [] == (moveTargetsList game) then game else (addAllTargetsEmptyList (addAllTargetsHelper game)) 


moveTargets :: Game -> Game
moveTargets game = addAllTargetsEmptyList Game {dimension = (dimension game)
                                               ,gameBoard = (gameBoard (clearTargets game)) --la urmatorul apel noi avem * in loc de # d-aia se impute
                                               ,targetList = []
                                               ,hunterPosition = (hunterPosition game)
                                               ,moveTargetsList = (targetList game)
                                               }

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}

isTargetKilled :: Position -> Target -> Bool
isTargetKilled hunterPosition target
    |  (fst (position target)) == (fst hunterPosition) && (snd (position target)) - (snd hunterPosition) <= 1  && (snd (position target)) - (snd hunterPosition) >= -1  = True
    |  (snd (position target)) == (snd hunterPosition) && (fst (position target)) - (fst hunterPosition) <= 1  && (fst (position target)) - (fst hunterPosition) >= -1  = True
    | otherwise = False

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}



hunterMove :: Direction -> Game -> Game
hunterMove direction game    
    | direction == North && (attemptMove ((fst(hunterPosition game))-1,(snd(hunterPosition game))) game) /= Nothing = (addHunter (fromJust (attemptMove ((fst(hunterPosition game))-1,(snd(hunterPosition game))) game))  game)
    | direction == South && (attemptMove ((fst(hunterPosition game))+1,(snd(hunterPosition game))) game) /= Nothing = (addHunter (fromJust (attemptMove ((fst(hunterPosition game))+1,(snd(hunterPosition game))) game)) game)
    | direction == East && (attemptMove ((fst(hunterPosition game)),(snd(hunterPosition game)+1)) game) /= Nothing = (addHunter (fromJust (attemptMove ((fst(hunterPosition game)),(snd(hunterPosition game))+1) game)) game )
    | direction == West && (attemptMove ((fst(hunterPosition game)),(snd(hunterPosition game)-1)) game) /= Nothing = (addHunter (fromJust (attemptMove ((fst(hunterPosition game)),(snd(hunterPosition game))-1) game)) game)
    | otherwise = (addHunter (hunterPosition game)  game)
 

--TREBUIE SA ITEREZ PRIN LISTA SA VAD CE NAIBA OMOARA CE NAIBA NU OMOARA
--DACA NU OMOARA BAG IN LISTA DE TARGET LIST DACA IL OMOARA NU MAI ADAUG NIMIC ACOLO 
--Salvam in move targets si la final trebuie sa o facem [] si pe targetList trebuie sa o facem moveTargetList
clearCertainTarget :: Position -> Game -> [[(Dimension, Entity)]]
clearCertainTarget position game = (map (\x -> (map (\y ->  if (fst (getCoordonates y)) == (fst position) && (snd (getCoordonates y)) == (snd position) 
                                                            then 
                                                                if (getEntity y) == GatewayEntity {gatePosition = (gatePosition (getEntity y)) , hasTarget = (hasTarget (getEntity y)) , hasHunter = (hasHunter (getEntity y))}
                                                                then ((getCoordonates y), GatewayEntity {gatePosition = (gatePosition (getEntity y)) , hasTarget = False , hasHunter = (hasHunter (getEntity y))})
                                                                else ((getCoordonates y), BlankEntity)
                                                            else                                                                    
                                                            ((getCoordonates y), (getEntity y))  
                                                            ) x ) ) (gameBoard game))

killTargetHelpOfHelp :: Game -> Game
killTargetHelpOfHelp game  =  if (isTargetKilled (hunterPosition game) 
    (head (targetList game))) == True --daca il nenoroceste
                                then    Game {dimension = (dimension game)
                                        ,gameBoard = (clearCertainTarget (position ((head (targetList game))) ) game)
                                        ,targetList = drop 1 (targetList game) 
                                        ,hunterPosition = (hunterPosition game)
                                        ,moveTargetsList = (drop 1 (targetList game))
                                            }
                                else    Game {dimension = (dimension game)
                                        ,gameBoard = (gameBoard game)
                                        ,targetList = drop 1 (targetList game)
                                        ,hunterPosition = (hunterPosition game)
                                        ,moveTargetsList = (targetList game)
                                        }    

killTargetHelper :: Game -> Game
killTargetHelper game = if (targetList game) == [] then game else (killTarget (killTargetHelpOfHelp game))
                                  

killTarget :: Game -> Game
killTarget game = Game {dimension = (dimension game)
                             ,gameBoard = (gameBoard (killTargetHelper game))
                             ,targetList = (moveTargetsList (killTargetHelper game)) 
                             ,hunterPosition = (hunterPosition game)
                             ,moveTargetsList = (moveTargetsList game)
                             }


--deplasez hunterul omor targeturi mut targeturi omor
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction bool game
    | bool == True = killTarget (moveTargets ( killTarget (hunterMove direction game)))
    | bool == False = hunterMove direction game

{-
    ***  TODO ***
    Daca NU GASESTE NIMIC RETURNEAZA FALSE
    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = (elem True (concatMap (\x-> x)  (map (\x -> (map (\y -> if (show (getEntity y)) == "*" then True else False ) x ) ) (gameBoard game))))

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

helper :: Game -> Bool
helper game = if (filter (\x -> (isTargetKilled (hunterPosition game) x) == True) (targetList game) ) == [] then True else False

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = gameStates 
        where
            gameSouth = (South, advanceGameState South False game)
            gameNorth = (North, advanceGameState North False game)
            gameEast = (East, advanceGameState East False game)
            gameWest = (West, advanceGameState West False game)
            gameStates = [gameSouth, gameNorth, gameEast, gameWest]
            
    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = elem True (map (\x -> (isTargetKilled (hunterPosition game) x)) (targetList game))
    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game =if (helper game) == True then 0.0 else hEuclidean (hunterPosition game) (position (head (filter (\x -> (isTargetKilled (hunterPosition game) x) == True) (targetList game) ))) 

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
