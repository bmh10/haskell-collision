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
width = 800 :: Float
height = 500 + dashboardHeight :: Float -- 31 * 15
dashboardHeight = 20 :: Float
offset = 100

window = InWindow "Collision" (round width, round height) (offset, offset)
background = black

data Game = Game
  { 
    particles :: [ParticlePair],
    paused :: Bool,
    gen :: StdGen,
    cr :: Float -- Coefficient of resitution
  } deriving Show 

data ParticlePair = ParticlePair
  {
    p1 :: Particle,
    p2 :: Particle
  } deriving Show

data Particle = Particle
  {
    pos :: (Float, Float),
    vel :: (Float, Float),
    mass :: Float,
    radius :: Float,
    col :: G2.Color
  } deriving Show

particle pos vel mass radius col = Particle { pos = pos, vel = vel, mass = mass, radius = radius, col = col }

-- Rendering
render :: Game -> Picture 
render g = pictures [renderParticles g, 
                     renderDashboard g]

renderDashboard :: Game -> Picture
renderDashboard g = G2.color white $ translate (-300) (-height/2 + 5) $ scale 0.1 0.1 $ text $ "Coefficient of restitution: " ++ show (cr g)

renderParticles g = pictures $ map renderParticlePair (particles g)

renderParticlePair :: ParticlePair -> Picture
renderParticlePair pp = pictures $ [renderParticle (p1 pp), renderParticle (p2 pp)]

renderParticle :: Particle -> Picture
renderParticle p
 = translate x y $ G2.color (col p) $ circleSolid (radius p)
  where
    (x, y) = pos p
   

-- Event handling
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys (EventKey (Char 'r') Down _ _) g = reset g
handleKeys (EventKey (Char 'q') Down _ _) g = g  {cr = (cr g) + 0.1 }
handleKeys (EventKey (Char 'a') Down _ _) g = g  {cr = (cr g) - 0.1 }
handleKeys _ game = game

togglePaused g = g { paused   = not (paused g) }

update :: Float -> Game -> Game
update secs game
 | (paused game) = game
 | otherwise     = updateGame game

updateGame g = g { particles = map (updateParticlePair (cr g)) (particles g) }

updateParticlePair cr pp = pp { p1 = updateParticle (p1 pp) (p2 pp) cr,
                             p2 = updateParticle (p2 pp) (p1 pp) cr }
  where
    updateParticle p1 p2 cr
      | isCollision p1 p2 = p1 { vel = (calcVelocity u1 u2 m1 m2 cr, 0) }
      | inRange p1 = p1 { pos = add (pos p1) (vel p1) }
      | otherwise = p1
        where (u1, _) = vel p1
              (u2, _) = vel p2
              m1 = mass p1
              m2 = mass p2

calcVelocity u1 u2 m1 m2 cr = 
  (cr*m2*(u2-u1) + m1*u1 + m2*u2) / (m1+m2)
  --quot ((u1*(m1-m2)) + 2*m2*u2) (m1+m2)

isCollision p1 p2 = d <= (radius p2)
  where
    d = sqrt $ (px-cx)*(px-cx) + (py-cy)*(py-cy)
    (px, py) = add (pos p1) (vel p1)
    (cx, cy) = add (pos p2) (vel p2)

inRange p = -w <= x && x <= w && -h <= y && y <= h
  where (x, y) = pos p
        w = width / 2
        h = height / 2

add (a,b) (c,d) = (a+c,b+d)

reset g = g { paused = False, particles = particlePairs}

particle1 = particle (-100, 0) ( 5, 0) 10 10 blue
particle2 = particle ( 100, 0) (-5, 0) 20 15 red

particle3 = particle (-100, 50) ( 10, 0) 10 10 blue
particle4 = particle ( 100, 50) (-10, 0) 10 10 red

particle5 = particle (-100, 100) (10, 0) 10 10 blue
particle6 = particle ( 100, 100) (0, 0)  20 15 red

particlePairs = [ParticlePair { p1 = particle1, p2 = particle2 },
                 ParticlePair { p1 = particle3, p2 = particle4 },
                 ParticlePair { p1 = particle5, p2 = particle6 }]

initGame = do 
  stdGen <- newStdGen
  let initialState = Game { paused = False, particles = particlePairs, gen = stdGen, cr = 0.5 }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
