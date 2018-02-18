module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as G2
import System.IO  
import System.Random 
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 20
width = 800
height = 500 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100

window = InWindow "Collision" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
    p1 :: Particle,
    p2 :: Particle,
    paused :: Bool,
    gen :: StdGen
  } deriving Show 

data Particle = Particle
  {
    pos :: (Int, Int),
    vel :: (Int, Int),
    mass :: Int,
    radius :: Float,
    col :: G2.Color
  } deriving Show

particle pos vel mass radius col = Particle { pos = pos, vel = vel, mass = mass, radius = radius, col = col }

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderParticles g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text "Dashboard"

renderParticles g = pictures $ [renderParticle (p1 g), renderParticle (p2 g)]

renderParticle :: Particle -> Picture
renderParticle p
 = translate x' y' $ G2.color (col p) $ circleSolid (radius p)
  where
    (x, y) = pos p
    (x', y') = (fromIntegral x, fromIntegral y)
   

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys _ game = game

togglePaused g = g { paused   = not (paused g) }

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game) = game
 | otherwise     = updateGame game

updateGame g = g { p1 = p1', p2 = p2' }
  where (p1', p2') = updateParticles (p1 g) (p2 g)

updateParticles p1 p2 = (updateParticle p1 p2, updateParticle p2 p1)
  where
    updateParticle p1 p2
      | isCollision p1 p2 = p1 { vel = (-u1, 0) } -- (- quot (abs m1*u1 + abs m2*u2) (m1+m2), 0) }
      | inRange p1 = p1 { pos = add (pos p1) (vel p1) }
      | otherwise = p1
        where (u1, _) = vel p1
              (u2, _) = vel p2
              m1 = mass p1
              m2 = mass p2

isCollision p1 p2 = d <= (radius p2)
  where
    d = sqrt $ fromIntegral ((px-cx)*(px-cx) + (py-cy)*(py-cy))
    (px, py) = add (pos p1) (vel p1)
    (cx, cy) = add (pos p2) (vel p2)

inRange p = -w <= x && x <= w && -h <= y && y <= h
  where (x, y) = pos p
        w = quot width 2
        h = quot height 2

add (a,b) (c,d) = (a+c,b+d)


initGame = do 
  stdGen <- newStdGen
  let particle1 = particle (-100, 0) ( 5, 0) 10 10 blue
  let particle2 = particle ( 100, 0) (-5, 0) 10 10 red
  let initialState = Game { paused = False, p1 = particle1, p2 = particle2, gen = stdGen }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
