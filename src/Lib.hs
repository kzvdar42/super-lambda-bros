module Lib where


-- Map
data Tile = Ground | Brick | BonusBlock | Empty
type Map = [[Tile]]
-- State = HP mapNumber Maybe nextMap
type State = Int Int Maybe Int
data World = World [Map] Player [MovingObject] State


-- Objects
type Vector2 = (Double, Double)
type Position = Vector2
type Velocity = Vector2
type Acceleration = Vector2
data PlayerKind = Small | Big | Dead
data EnemyKind = Gumba | Cherepakha
data ItemKind = Mushroom | Star | Shell


data MovingObject
    = Player Position Velocity Acceleration PlayerKind
    | Enemy Position Velocity Acceleration EnemyKind
    | Item Position Velocity Acceleration ItemKind

initWorld :: World
init
