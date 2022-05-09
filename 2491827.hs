--Belousov Daniil 2491827
--I have read and accept the submission rules 
--This work is my own work which is fully done by myself

--DO NOT FORGET TO CHANGE THE NAME OF THE FILE WITH YOUR OWN STUDENTID!

--Your data, you can expand or do changes on the Tree if necessary. Do not change the included names
--You must at least complete the Node according to the given explanation.

data TernaryTree = Empty | NotReachable | NodeExists | NodeNotFound | Node String TernaryTree TernaryTree TernaryTree deriving (Show, Eq, Ord)

-------------------------------------------------------------------
--All the helper functions 
-------------------------------------------------------------------
-- compares two stings 
compareArray [] [] = 1
compareArray [] _ = 0
compareArray _ [] = 0
compareArray (a:as) (b:bs) = if (a==b) then compareArray as bs else 0 

-- the function is needed for the insert function as it checks if all the rulls are followed. this function outputs wether the node exsists or is not reachable

search (Node as leftSide midSide rightSide) a = if (compareArray as a == 1) then NodeExists else 
  if (((charToInt (a!!(length as +1))) == 1) && (length a == (length as  +2)) && (leftSide==Empty)) then Empty else
    if (((charToInt (a!!(length as +1))) == 1) && (length a >= (length as  +2)) && (leftSide/=Empty)) then search leftSide a else 
      if (((charToInt (a!!(length as +1))) == 2) && (length a == (length as  +2)) && (midSide==Empty)) then Empty else  
        if (((charToInt (a!!(length as +1))) == 2) && (length a >= (length as  +2)) && (midSide/=Empty)) then search midSide a else 
          if (((charToInt (a!!(length as +1))) == 3) && (length a == (length as  +2)) && (rightSide==Empty)) then Empty else
            if (((charToInt (a!!(length as +1))) == 3) && (length a >= (length as  +2)) && (rightSide/=Empty)) then search rightSide a else
              NotReachable

-- this function inserts the node into the correct place assuming that the node is possiable to insert (the search function does the checking)

insertion Empty a = Node a Empty Empty Empty

insertion (Node as leftSide midSide rightSide) a = if (((charToInt (a!!(length as +1))) == 1) && ((length a == (length as  +2)) || leftSide /= Empty ))  then (Node as (insertion leftSide a) midSide rightSide) else 
    if (((charToInt (a!!(length as +1))) == 2) && ((length a == (length as  +2))   ||  midSide /= Empty ))  then (Node as leftSide (insertion midSide a) rightSide) else 
      if (((charToInt (a!!(length as +1))) == 3) && ((length a == (length as  +2))   ||  rightSide /= Empty )) then (Node as leftSide midSide (insertion rightSide a)) else 
        error "Something's wrong)"

-------------------------------------------------------------------

-------------------------------------------------------------------

--Part One
-- as read requires a string i make a string from a cerrecter and assighn it to integer 
charToInt::Char->Int
charToInt a = read (a:"") :: Int

--Part Two
-- this function does the insertion opperation by the use of 2 helper functions mentioned above (search and insertion)
insertNode::TernaryTree->[Char]->TernaryTree   

insertNode Empty a = Node a Empty Empty Empty

insertNode (Node as leftSide midSide rightSide) a = if ((search (Node as leftSide midSide rightSide) a) == NotReachable || (search (Node as leftSide midSide rightSide) a ) == NodeExists) then (search (Node as leftSide midSide rightSide) a) else 
  (insertion (Node as leftSide midSide rightSide) a)


--Part Three
-- recursivle checking evary nodes children and then do the summing, the 1 stands to count the current node
totalNodes::TernaryTree->Int
totalNodes Empty = 0
totalNodes (Node as leftSide midSide rightSide)  = 1 + totalNodes leftSide + totalNodes midSide + totalNodes rightSide


--Part Four

-- in this function I check the hight by finding the max among the 3 children of a node and then increment it by 1 (adding the current node)
height::TernaryTree->Int
height Empty = 0
height (Node as leftSide midSide rightSide)= 1 + max (max (height leftSide) (height rightSide)) (height midSide)


--Part Five
-- recursivle going to each branch of the tree and see if the node is their then i increment otherwise not, in the end summing all the branches results
levelcount::TernaryTree->Int->Int
levelcount Empty b = 0
levelcount a 0 = 1
levelcount (Node as leftSide midSide rightSide) b = (levelcount leftSide (b-1)) + (levelcount midSide (b-1)) + (levelcount rightSide (b-1))


--Part Six
-- in this function I check the nodes based on the needed string and if i find the similaar one, outut it, otherwise continue checking 
-- if no node is found, output that it is not in the tree
findNode::TernaryTree->[Char]->TernaryTree

findNode Empty a = NodeNotFound

findNode (Node as leftSide midSide rightSide) str = if (compareArray as str == 1 )   then Node as leftSide midSide rightSide 
  else if (((charToInt (str!!(length as +1))) == 1) && (length str == (length as  +2)   ||  leftSide /= Empty )) then findNode leftSide str 
    else if (((charToInt (str!!(length as +1))) == 2) && (length str == (length as  +2)   ||  midSide /= Empty )) then findNode midSide str 
      else if (((charToInt (str!!(length as +1))) == 3) && (length str == (length as  +2)   ||  rightSide /= Empty )) then findNode rightSide str else 
        error "Something's wrong)"
