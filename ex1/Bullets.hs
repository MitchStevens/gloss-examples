import Graphics.Gloss
import Tuple2
import Data.Monoid

type Pos  = Tuple2 Float
type Vel  = Tuple2 Float
data Ball = Ball Pos Vel
data Model = Model [Ball]

height, width :: Int
height = 768
width  = 1024

tstep, radius :: Float
tstep = 1000.0/24.0 --rate in which
radius = 10

gravity :: Vel
gravity = Tuple2 0 (-9.8)

(|>) :: a -> (a -> a) -> a
x |> f = f x

class ModelElement t where
		tick :: Model -> t -> t
		draw :: t -> Picture

instance ModelElement Model where
		tick _ m@(Model balls) = Model (map (tick m) balls)
		draw (Model balls) = Pictures (map draw balls)

-- BALL METHODS
instance Tickable Ball where
		tick _ (Ball pos vel) = Ball (pos <> vel) vel
		draw (Ball pos _) =

calc_pos :: Ball ->

apply_gravity :: Ball -> Ball
apply_gravity (Ball pos vel) = Ball pos (vel <> gravity)
