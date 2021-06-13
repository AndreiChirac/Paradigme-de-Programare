{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {nodeStateN :: s
                    ,nodeParentN :: Maybe (Node s a)
                    ,nodeDepthN :: Int
                    ,nodeChildrenN :: [Node s a]
                    ,nodeHeuristicN :: Float
                    ,nodeActionN :: Maybe a
                    }

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    Node nodeState1 _ _ _ _ _  == Node nodeState2 _ _ _ _ _  = nodeState1 == nodeState2

instance Ord s => Ord (Node s a) where
    Node nodeState1 _ _ _ _ _  <= Node nodeState2 _ _ _ _ _  = nodeState1 <= nodeState2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState myNode = (nodeStateN myNode)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent myNode = (nodeParent myNode)

nodeDepth :: Node s a -> Int
nodeDepth myNode = (nodeDepthN myNode)

nodeChildren :: Node s a -> [Node s a]
nodeChildren myNode = (nodeChildrenN myNode)

nodeHeuristic :: Node s a -> Float
nodeHeuristic myNode = (nodeHeuristicN myNode)

nodeAction :: Node s a -> Maybe a
nodeAction myNode = (nodeActionN myNode)

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> a ->Node s a -> Int -> Node s a  
createStateSpaceHelper state action parent depth = myNode
    where 
        myNode = Node {nodeStateN = state
                      ,nodeParentN = (Just parent)
                      ,nodeDepthN = depth
                      ,nodeChildrenN = childrenNodes
                      ,nodeHeuristicN = (h state)
                      ,nodeActionN = (Just action)
                    }
        childrenNodes = map (\(state,action) -> createStateSpaceHelper action state myNode (depth + 1)) (successors state)

        

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = myNode
    where
        myNode =  Node {nodeStateN = initialState
                        ,nodeParentN = Nothing
                        ,nodeDepthN = 0
                        ,nodeChildrenN = childrenNodes
                        ,nodeHeuristicN = (h initialState)
                        ,nodeActionN =  Nothing
                        }

        childrenNodes = map (\(state,action) -> createStateSpaceHelper action state myNode  1) (successors initialState)

-- initialNode

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}
suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\states -> (S.member (nodeStateN states) visited) /= True)  (nodeChildrenN node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}


insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = if PQ.lookup node frontier == Nothing then PQ.insert node ((nodeHeuristic node) + (fromIntegral (nodeDepthN node))) frontier else 
    if (fromJust (PQ.lookup node frontier)) > ((nodeHeuristic node) + (fromIntegral (nodeDepthN node))) then PQ.insertWith (+) node (((nodeHeuristic node) + (fromIntegral (nodeDepthN node))) - (fromJust (PQ.lookup node frontier))) frontier else frontier  -- newFrontier

{-
   Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insert :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> [Node s a] -> (PQ.PSQ (Node s a) Float)
insert frontier list = insertSucc frontier (head list)

insertSuccsHelper :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> [Node s a] -> (PQ.PSQ (Node s a) Float)
insertSuccsHelper frontier list = if list == [] then frontier else insertSuccsHelper (insert frontier list) (drop 1 list) 

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = insertSuccsHelper frontier (suitableSuccs node visited)--newFrontier


{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = if isGoal (nodeStateN (fst (deleteFindMin frontier))) == False then (astar' (S.insert (nodeStateN (fst (deleteFindMin frontier))) visited) (insertSuccs (fst (deleteFindMin frontier))  (snd (deleteFindMin frontier)) (S.insert (nodeStateN (fst (deleteFindMin frontier))) visited)) ) else  (fst (deleteFindMin frontier))-- goalNode


{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' (S.insert (nodeStateN initialNode) S.empty) (PQ.insert initialNode (nodeHeuristicN initialNode + fromIntegral(nodeDepthN initialNode)) PQ.empty)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = foldl (\acc n -> (fromJust(nodeActionN n), nodeStateN n):acc) []
    (takeWhile (isJust . nodeParentN) $ iterate (fromJust . nodeParentN) goalNode)