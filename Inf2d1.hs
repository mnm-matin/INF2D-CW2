-- Inf2d Assignment 1 2017-2018
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases. 
badNodesList = []

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= gridLength_search*gridWidth_search
-- Why did you choose this number?
-- Thats the maximum length of a search that is possible.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch = [
     node:branch | node <- [(r, c+1), (r, c-1), (r+1, c), (r-1, c)] -- all possible movements in the grid attached to branch
     , notElem node branch -- nodes that are part of the branch are not included
     , notElem node badNodesList -- nodes that are faulty are not included
     , elem node validList -- nodes that are part of the valid list pass through
    ]
    where
        (r, c) = head branch 
        validList = [(x,y) | x <- [1..gridWidth_search], y <- [1..gridLength_search]] -- used in conjunction with node to determine validity



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch destination next branches exploredList =
	if (elem destination badNodesList) -- checking if destination is faulty state
	then Nothing
	else if not (null cur_Branch)
		then Just (head cur_Branch) -- checking if head of current brach in the stack is at destination
    	else if all (`elem` exploredList) (map head next_Branch_Heads) -- checking if all branches explored
    		then Nothing
    		else breadthFirstSearch destination next next_Branch_Heads ((map head next_Branch_Heads) ++ exploredList)
    		-- recursive call using next branches (i.e dropping to the next level of search)
         	-- also adding the branch heads to the exploredlist
    where
    	cur_Branch :: [Branch]
        cur_Branch = filter (\x -> checkArrival destination (head x)) branches -- checking if branch head at destination
        next_Branch_Heads' = map next branches
        next_Branch_Heads = myConcat next_Branch_Heads' -- uses myConcat in auxillary functions to reduce clutter here
    

             



-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch

depthFirstSearch destination next branches exploredList=
    if (elem destination badNodesList) -- checking if destination is faulty state
    then Nothing
    else if not (all (`elem` exploredList) (map head next_Branch_Heads))  -- checking if all branches explored
         then depthFirstSearch destination next next_Branch_Heads ((map head next_Branch_Heads) ++ exploredList)
         -- recursive call using next branches (i.e dropping to the next level of search)
         -- also adding the branch heads to the exploredlist
         else if not (null cur_Branch)
               then Just (head cur_Branch) -- checking if head of current brach in the stack is at destination
               else Nothing
    where
    	cur_Branch :: [Branch]
        cur_Branch = filter (\x -> checkArrival destination (head x)) branches -- checking if branch head at destination
        next_Branch_Heads' = map next branches
        next_Branch_Heads = myConcat next_Branch_Heads' -- uses myConcat in auxillary functions to reduce clutter here

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next branches d exploredList=
	if (elem destination badNodesList)-- checking if destination is faulty state
	then Nothing
	else if (&&) (d > 0) (not (all (`elem` exploredList) (map head next_Branch_Heads)))  -- checking if all branches explored and limit of depth
		then depthLimitedSearch destination next next_Branch_Heads (d-1) ((map head next_Branch_Heads) ++ exploredList)
		-- recursive call using next branches (i.e dropping to the next level of search until depth limit reached
         -- also adding the branch heads to the exploredlist
    	else if not (null cur_Branch)
    		then Just (head cur_Branch) -- checking if head of current brach in the stack is at destination
    		else Nothing
    where
    	cur_Branch :: [Branch]
        cur_Branch = filter (\x -> checkArrival destination (head x)) branches -- checking if branch head at destination
        next_Branch_Heads' = map next branches
        next_Branch_Heads = myConcat next_Branch_Heads' -- uses myConcat in auxillary functions to reduce clutter here

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d =
	if (elem destination badNodesList)-- checking if destination is faulty state
	then Nothing
	else if search == Nothing -- checking if search not successfull
		then iterDeepSearch destination next initialNode (d+1) -- deepening the level of search
    	else search -- returns successful search
    where
        search = depthLimitedSearch destination next [[initialNode]] d []

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = (abs (a_p - a_d)) + (abs (b_p - b_d)) -- simple sum of differences of coor
	where
		(a_p,b_p)= position
		(a_d,b_d)= destination

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList= traverse_r (lowest_H_value branches) exploredList
    where
        lowest_H_value :: [Branch] -> [Branch]
        lowest_H_value = sortBy (\x y -> compare (heuristic (head x)) (heuristic (head y)))
        
        traverse_r :: [Branch] -> [Node] -> Maybe Branch
        traverse_r branches discovered=
        	if (elem destination badNodesList) -- checking if destination is faulty state
			then Nothing
            else if not (null cur_Branch) 
            	then Just (head cur_Branch) -- checking if head of current brach in the stack is at destination
            	else if not (all (`elem` discovered) (map head next_H_value)) 
            		then traverse_r next_H_value ((map head next_H_value) ++ discovered)
            		else Nothing
            
            where
            	next_H_value = lowest_H_value (next_branches')
                cur_Branch = filter (\x -> checkArrival destination (head x)) branches -- checking if branch head at destination
            	next_branches = map next branches
            	next_branches' = myConcat next_branches -- uses myConcat in auxillary functions to reduce clutter here
    
	
-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList= traverse_r (lowest_H_value branches) exploredList  
    where
        lowest_H_value :: [Branch] -> [Branch]
        lowest_H_value = sortBy (\x y -> compare (heuristic_cost x) (heuristic_cost y))
        
        heuristic_cost :: Branch -> Int
        heuristic_cost a = (cost a) + (heuristic (head a)) --returns the total cost

        traverse_r :: [Branch] -> [Node] -> Maybe Branch
        traverse_r branches discovered =
        	if (elem destination badNodesList) -- checking if destination is faulty state
			then Nothing 
            else if not (null cur_Branch) 
            	then Just (head cur_Branch) -- checking if head of current brach in the stack is at destination
                else if not (all (`elem` discovered) (map head next_H_value))
            	     then traverse_r next_H_value ((map head next_H_value) ++ discovered)
                     else Nothing
            where
            	next_branches = map next branches
            	next_branches' = myConcat next_branches -- uses myConcat in auxillary functions to reduce clutter here
                next_H_value = lowest_H_value (next_branches')
                cur_Branch = filter (\x -> checkArrival destination (head x)) branches -- checking if branch head at destination

    
	
-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch -- -1 ?


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches. 



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state. 
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game =
	if checkWin game humanPlayer
	then 1
	else if checkWin game compPlayer
		then -1
		else 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state. 

minimax:: Game->Player->Int
minimax game player =
	if terminal game 
	then eval game
    else if (maxPlayer player)
    	then maximum score_list
    	else minimum score_list
    where 
    	game_list = moves game player
    	score_list = [minimax game' (switch player) | game' <- game_list]
    	--applies minimax on each possible game while switching players
	

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 

alphabeta:: Game->Player->Int
alphabeta game player =
	if maxPlayer player 
	then max_min [game] (2) (-2) "max_"
	else max_min [game] (2) (-2) "min_"
    
    where
    	max_min gs a b q'=
    		if (&&) (length gs == 1) (terminal (head gs)) 
    		then eval (head gs)
    		else if q'== "max_"
    			then max_min_v gs a b (-2) "max_V"
    			else max_min_v gs a b (2) "min_V"
        
    	max_min_v gs a b v q'=
    		if gs==[]
    		then v
    		else if q' == "max_V"
    			then if v' >= b 
    				then v'
    				else max_min_v xs (max v' a) b v' "max_V"
    			else if w' <= a 
    				then w'
    				else max_min_v xs a (min w' b) w' "min_V"
    		where 
    			(x:xs) =gs
    			v' = max v $ max_min (moves x player) a b "min_"
    			w' = min v $ max_min (moves x (switch player)) a b "max_"
    

-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state. 
-- It should return 1 if either of the move types is in the correct winning position. 
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =
	if (checkWin game 1 || checkWin game 0)
	then 1
	else if terminal game then 0 else 0

-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward. 
-- If the min player sent the game into a terminal state you should give -1 reward. 

alphabetaWild:: Game->Player->Int
alphabetaWild game player =
	if maxPlayer player 
	then max_min [game] (2) (-2) "max_"
	else max_min [game] (2) (-2) "min_"
    
    where
    	max_min gs a b q'=
    		-- the changed part for wild tic tac toe that ...
    		if (length gs == 1) && (terminal (head gs)) && (q'== "max_")  
    		then (1) -- gives +1 for the max player that makes the winning move
    		else if (length gs == 1) && (terminal (head gs))&& (q'== "min_")
    		then (-1) -- gives -1 for the min player that makes the winning move
    		else if q'== "max_"
    			then max_min_v gs a b (-2) "max_V"
    			else max_min_v gs a b (2) "min_V"
        
    	max_min_v gs a b v q'=     -- the function that restricts the looping of the minmax algorithm
    		if gs==[]
    		then v
    		else if q' == "max_V"
    			then if v' >= b 
    				then v'
    				else max_min_v xs (max v' a) b v' "max_V"
    			else if w' <= a 
    				then w'
    				else max_min_v xs a (min w' b) w' "min_V"
    		where 
    			(x:xs) =gs
    			v' = max v $ max_min (movesWild x player) a b "min_"
    			w' = min v $ max_min (movesWild x (switch player)) a b "max_"
    
	
	
-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.

		
-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning). 
-- The evalWild function should be used to get the value of a terminal state. 

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined
	

			
			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
 
myConcat :: [[Branch]] -> [Branch] -- converts a list of list of branches to a list of branch
myConcat x = concat [y| y <- x]	 

