import Graphics.Gloss
import Tuple2
import Data.Monoid

type Pos  = Tuple2 Float
type Vel  = Tuple2 Float
type Acc  = Tuple2 Float
data Ball = Ball Pos Vel Acc
data Model = Model [Ball]

fps :: Int
fps = 24

height, width, tstep, radius :: Float
height = 768.0
width  = 1024.0
tstep = 1000.0/(fromIntegral fps)
radius = 10.0

origin = (Float, Float)

gravity :: Acc
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
instance ModelElement Ball where
		tick _ ball = ball |> calc_pos |> calc_vel |> calc_acc |> apply_collisions
		draw (Ball (Tuple2 x y) _ _) = translate x y $ Circle radius

calc_pos, calc_vel, calc_acc :: Ball -> Ball
calc_pos (Ball p v a) = Ball (p |+| (v |*| tstep)) v a
calc_vel (Ball p v a) = Ball p (v |+| (a |*| tstep)) a
calc_acc (Ball p v a) = Ball p v gravity

apply_collisions :: Model -> Ball -> Ball
apply_collisions _ b@(Ball (Tuple2 px py) (Tuple2 vx vy) a) =
	if (y < 0)
		then Ball (Tuple2 px radius) (Tuple2 vx (-vy)) a
		else b

Model model = Model [Ball (Tuple2 500, 500) zero zero]
main = simulate red fps model draw (\_ _ m -> tick m)





















--
