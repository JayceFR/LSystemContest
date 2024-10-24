module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2
                , expandLSystem, commandMap ) where

import IC.Colour
import Graphics.UI.GLUT (Color(color))

type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
data Command = F | L | R | B [Command] deriving Show
type ColouredLine = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem a _ _ )  = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ b _) = b

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ c) = c 

--
-- Pre: the character has a binding in the Rules list
--
-- type Rules a = [(Char, [a])]
lookupChar :: Rules a -> Char -> [a]
lookupChar rules character = go rules
  where 
    go (x : xs) = case x of 
      (char, ys) -> if char == character then ys else go xs 

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne rules axioms = go axioms
  where 
    go [] = []
    go (x : xs) = lookupChar rules x ++ go xs 

--
-- Expand command string s n times using rule table r
--
expand :: [Char] -> Int -> Rules Char -> [Char]
expand axiom 0 _  = axiom
expand axiom n rules = expand (expandOne rules axiom) (n-1) rules

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.

-- type Vertex = (Float, Float)
-- type TurtleState = (Vertex, Float)


move :: Command -> Float -> TurtleState -> TurtleState
move comm rot (all@(x,y), z) = case comm of 
  F -> (pos z, z)
  L -> (all, rot + z)
  R -> (all, z - rot)
  where pos angle = (x + cos(degreesToRadians angle), y + sin (degreesToRadians angle))


degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

parse :: Rules Command -> [Char] -> [Command]
parse rules string = fst (go string)
  where 
    go :: [Char] -> ([Command], [Char])
    go [] = ([], [])
    go (']' : xs) = ([], xs)
    go (x : xs) = case x of 
      '[' -> (B a : a', xss')
      n -> (lookupChar rules x ++ a , xss)
      where 
        (a, xss) = go xs 
        (a', xss') = go xss 

-- data Command = F | L | R | B [Command] deriving Show
-- type ColouredLine = (Vertex, Vertex, Colour)
-- move :: Command -> Float -> TurtleState -> TurtleState
-- type Vertex = (Float, Float)
-- type TurtleState = (Vertex, Float)

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 commands angle color = go commands ((0, 0), 90)
  where 
    go :: [Command] -> TurtleState -> [ColouredLine]
    go [] _ = []
    go (B cmd : cmds) state = go cmd state ++ go cmds state 
    go (x : xs) state@(vert, float) = case x of 
      F -> (vert, evert, color) : go xs state'
      _ -> go xs state' 
      where state'@(evert, _) = move x angle state 


-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 commands angle color = go commands [] [] ((0,0),90)
  where 
    go :: [Command] -> [([Command], TurtleState)] -> [ColouredLine] -> TurtleState -> [ColouredLine]
    go [] [] acc _ = reverse acc -- Used reverse to avoid ++ for performance gains
    go [] ((comm, state''): xs) acc state = go comm xs acc state''
    go (B cmd : cmds) stack acc state = go cmd ((cmds, state) : stack) acc state
    go (x : xs) stack acc state@(vert, float) = case x of 
      F -> go xs stack ((vert, evert, color) : acc) state'
      _ -> go xs stack acc state'
      where state'@(evert, _) = move x angle state 

-- Provided Functions
------------------------------------------------------------------------------

expandLSystem :: LSystem -> Int -> [Command]
-- expandLSystem = undefined
expandLSystem (LSystem _ axiom rs) n = parse commandMap (expand axiom n rs)

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
