{-
    Asteroids clone in Elm
    Copyright (C) 2016  Juho Sepänmaa

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Html exposing (Html)
import Html.App as App
import Html.Attributes as HtmlA
import Svg.Lazy as SvgL
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)
import AnimationFrame
import Keyboard exposing (..)
import Random 
import Char exposing (fromCode)
import Set exposing (..)

screenWidth = 800
screenHeight = 600

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every millisecond Tick
              , AnimationFrame.diffs Frame
              , Keyboard.downs KeyDown
              , Keyboard.ups KeyUp
              ]   
        


-- MODEL

type GameState = Ready | Running | Over

    
type AsteroidType = BigAsteroid | SmallAsteroid

    
type alias Asteroid =
    { pos : (Int, Int)
    , vel : (Int, Int)
    , shape : List (Int, Int)
    , health : Int
    , size : AsteroidType
    }

    
type alias Bullet =
    { pos : (Int, Int)
    , vel : (Int, Int)
    , health : Int
    , damage : Int
    , lifeTime : Time
    }

    
type alias Ship =
    { pos : (Int, Int)
    , vel : (Int, Int)
    , angle : Float
    , health : Int
    , damage : Int
    , lives : Int
    }

    
type alias Model =
    { time : Time
    , ship : Ship
    , shootDelay : Float
    , accelDelay: Float
    , asteroids : List (Asteroid)
    , bullets : List (Bullet)
    , starfield : List (Int, Int)
    , score : Int
    , noCollision : Float
    , state : GameState
    , keys : Set KeyCode
    }
    
    
init : (Model, Cmd Msg)
init =
    ({ time = 0
     , ship = Ship (400, 400) (0, 0) (-pi/2) 100 100 3                  
     , shootDelay = 0
     , accelDelay = 0
     , asteroids = [] 
     , bullets = []                       
     , starfield = [] 
     , score = 0
     , noCollision = 0
     , state = Ready
     , keys = Set.empty
     }, Cmd.batch
         [ Random.generate NewAsteroids (asteroidListGen Nothing 5 BigAsteroid)
         , Random.generate NewStarfield (starfieldGen 15)
         ])



-- UPDATE

        
type Msg
    = Tick Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Frame Time
    | NewAsteroids (List (Asteroid))
    | NewStarfield (List (Int, Int))

      
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.state of
        Over ->
            case msg of
                KeyDown key ->                
                    case Char.fromCode key of
                        'R' -> init
                        _ -> (model, Cmd.none)
                _ -> (model, Cmd.none)
        Running ->
            case msg of
                Frame dt ->
                    if model.ship.lives == 0
                    then ({ model | state = Over }, Cmd.none)
                    else model
                        |> control
                        |> updatePositions
                        |> detectCollisions
                        |> updateModel
                Tick newTime ->
                    ({model | time = newTime}, Cmd.none)
                KeyDown key ->
                    ({model | keys = Set.insert key model.keys}, Cmd.none)
                KeyUp key ->
                    ({model | keys = Set.remove key model.keys}, Cmd.none)
                NewAsteroids asts ->
                    ({model | asteroids = asts ++ model.asteroids}, Cmd.none)
                _ -> (model, Cmd.none)
        Ready ->
            case msg of
                KeyDown key ->
                    ({model | state = Running}, Cmd.none)
                NewAsteroids asts ->
                    ({model | asteroids = asts}, Cmd.none)
                NewStarfield sf ->
                    ({model | starfield = sf}, Cmd.none)
                _ -> (model, Cmd.none)

                     
control : Model -> Model
control model =
    let move key ship = case key of
                            37 -> {ship | angle = ship.angle - 0.08}
                            39 -> {ship | angle = ship.angle + 0.08}
                            _ -> ship
        model' = { model | ship = Set.foldr (\k s -> move k s) model.ship model.keys }
    in model' |> shoot |> accelerate


accelerate : Model -> Model
accelerate model =
    if model.accelDelay < model.time && Set.member 38 model.keys
    then { model |
               ship = updateVelocity model.ship,
               accelDelay = model.time + 200
         }
    else model


shoot : Model -> Model
shoot model =
    let vel = (round (10*cos(model.ship.angle)), round (10*sin(model.ship.angle)))
    in if model.shootDelay < model.time && model.noCollision < model.time && Set.member 32 model.keys
       then { model |
                  bullets = (Bullet model.ship.pos vel 1 35 (model.time+700)) :: model.bullets,
                  shootDelay = model.time + 200
            }
       else model
        

hit : {a | pos:(Int, Int), damage:Int} -> Asteroid -> Bool
hit a ast =
    let offset = case ast.size of
                     SmallAsteroid -> 8
                     BigAsteroid -> 16
        (astx, asty) = ast.pos
        (x, y) = a.pos 
        xd = toFloat <| abs <| x - (astx+offset)
        yd = toFloat <| abs <| y - (asty+offset)
        dist = sqrt ((xd*xd) + (yd*yd))
    in dist < 25


detectCollisions : Model -> Model
detectCollisions model =
    let kill a = {a | health = 0}
        bulletCollision b a' = if hit b a' then {a' | health = a'.health - b.damage} else a'
        shipCollision a (s, asts) = if hit s a then (kill s, kill a :: asts) else (s, a :: asts)
        asteroids = List.map (\a -> List.foldr bulletCollision a model.bullets) model.asteroids       
        (ship', asteroids') = List.foldr shipCollision (model.ship, []) asteroids
        bullets' = List.filter (\b -> not (List.any (\a -> hit b a) asteroids')) model.bullets
    in if model.noCollision > model.time
       then model
       else { model |
                  asteroids = asteroids',
                  bullets = bullets',
                  ship = ship',
                  noCollision = if ship'.health <= 0 then model.time + 1000 else 0
            }


updateVelocity : Ship -> Ship
updateVelocity ship =
    let (xVel, yVel) = ship.vel
        newXVel = round <| (toFloat xVel) + (cos ship.angle)
        newYVel = round <| (toFloat yVel) + (sin ship.angle)
    in { ship | vel = (newXVel, newYVel) }
        

updatePosition : {a | pos:(Int, Int), vel:(Int, Int)} -> {a | pos:(Int, Int), vel:(Int, Int)}
updatePosition a =
    let (x, y) = a.pos
        (xVel, yVel) = a.vel
        margin = 24
        newCoord n limit = if n > limit+margin
                           then -margin
                           else if n < -margin then limit+margin else n
    in { a | pos = (newCoord (x+xVel) screenWidth, newCoord (y+yVel) screenHeight) }


updateModel : Model -> (Model, Cmd Msg)
updateModel model =
    let (alive, dead) = List.partition (\a -> a.health > 0) model.asteroids
        dead' = List.filter (\a -> a.size == BigAsteroid) dead
        s = model.ship
        ship' = if s.health <= 0 then { s | health = 100, lives = s.lives - 1 } else s
        fragments ast = Random.generate NewAsteroids (asteroidListGen (Just ast.pos) 3 SmallAsteroid)
        cmd = if not (List.isEmpty dead')
              then Cmd.batch (List.map fragments dead')
              else if List.isEmpty alive
                   then Random.generate NewAsteroids (asteroidListGen Nothing 5 BigAsteroid)
                   else Cmd.none
    in ({ model |
             ship = ship',
             asteroids = alive,
             bullets = List.filter (\b -> b.lifeTime > model.time && b.health > 0) model.bullets,
             score = model.score + (List.length dead)
       }, cmd)
        

updatePositions : Model -> Model
updatePositions model =
    { model |
          ship = updatePosition model.ship,
          asteroids = List.map updatePosition model.asteroids,
          bullets = List.map updatePosition model.bullets
    }    
        

asteroidGen : AsteroidType -> Random.Generator Asteroid
asteroidGen size =
    let octagon = [(0, 16), (0, 8), (8, 0), (16, 0), (24, 8), (24, 16), (16, 24), (8, 24)]
        smallOctagon = List.map (\(x, y) -> (round(x/2), round(y/2))) octagon
        pos = Random.pair (Random.int 0 screenWidth) (Random.int 0 screenHeight)
        vel = Random.pair (Random.int -3 3) (Random.int -3  3)
        (shape, health) = case size of
                             BigAsteroid -> (octagon, 100)
                             SmallAsteroid -> (smallOctagon, 70)
        offsets = Random.list 8 (Random.pair (Random.int -3 3) (Random.int -3 3))
        addTuples xs ys = List.map2 (\(xa,xb) (ya,yb) -> (xa+ya, xb+yb)) xs ys        
    in  Random.map3 (\o p v -> Asteroid p v (addTuples o shape) health size) offsets pos vel


asteroidListGen : Maybe (Int, Int) -> Int -> AsteroidType -> Random.Generator (List Asteroid)
asteroidListGen position n size =
    let asts = (Random.list n (asteroidGen size))
    in case position of
           Just (x, y) -> Random.map (\a -> List.map (\a' -> {a' | pos = (x,y)}) a) asts
           Nothing -> asts    


starfieldGen : Int -> Random.Generator (List (Int, Int))
starfieldGen n = Random.list n <| Random.pair (Random.int 0 screenWidth) (Random.int 0 screenHeight)



-- VIEW


asteroidPolygon : Asteroid -> Svg a
asteroidPolygon ast =
    let (ax, ay) = ast.pos
        lineColor = if ast.health < 100 && ast.health > 50
                    then "yellow"
                    else if ast.health < 50
                         then "red"
                         else "white"                             
        addPoint (x,y) s = s ++ (toString <| ax+x) ++ "," ++ (toString <| ay+y) ++ " "
        ps = List.foldr addPoint "" ast.shape
    in polygon [ stroke lineColor
               , strokeWidth "2"
               , fill "white"
               , points ps] []

        
bulletCircle : Bullet -> Svg a
bulletCircle b =
    let (x, y) = b.pos
    in circle [ fill "white", cx (toString x), cy (toString y), r "3" ] []
        

starCircle : (Int, Int) -> Svg a
starCircle (x, y) =
    circle [ fill "white", cx (toString x), cy (toString y), r "1"] []
        

shipPolygon : (Int, Int) -> String -> Svg a
shipPolygon (x, y) fillColor =
    let toPoint px py = (toString px) ++ "," ++ (toString py)
        ps = (toPoint (x-10) (y-10)) ++ " " ++ (toPoint (x-10) (y+10)) ++ " " ++ (toPoint (x+15) y)
    in  polygon [ fill fillColor, points ps ] []

        
statusStyle =
    HtmlA.style
        [ ("top", "0")
        , ("left", "0")
        , ("width", "100%")
        , ("position", "absolute")
        , ("color", "white")
        ]

        
messageStyle =
    HtmlA.style
        [ ("top", "10%")
        , ("left", "10%")
        , ("position", "absolute")
        , ("color", "white")
        , ("fontSize", "5vh")
        ]
        

welcomeMsg =
    Html.div [ messageStyle ]
        [ Html.text "Press any key to start"
        , Html.br [] []
        , Html.text "Use arrow keys to move and space to shoot" ]
        

gameOverMsg score =
    Html.div [ messageStyle ]
        [ Html.text "Game Over"
        , Html.br [] []
        , Html.text ("Score: " ++ toString score)
        , Html.br [] []
        , Html.text "Press R to restart"
        ]
        

statusText score lives =
    Html.div [ statusStyle ]
        [ Html.text ("Score: " ++ toString score)
        , Html.br [] []
        , Html.text ("Lives: " ++ toString lives)
        ]        
        
    
view : Model -> Html Msg
view model =
  let background = rect [ x "0", y "0", width "100%", height "100%", fill "black" ] []
      (shipX, shipY) = model.ship.pos
      fillColor = if model.noCollision > model.time
                  then if (round (sin model.time)) % 2 == 0 then "white" else "black"
                  else "white"
      angle = toString <| model.ship.angle * 180/pi
      pos = (toString shipX) ++ " " ++ (toString shipY)
      rotation = transform <| "rotate(" ++ angle ++ " " ++ pos ++ ")"
      ship = g [ rotation ] [ shipPolygon (shipX, shipY) fillColor ]
  in Html.div [ HtmlA.style [("height","100%")] ]
      [ case model.state of
           Ready -> welcomeMsg
           Over -> gameOverMsg model.score
           _ -> Html.div [] []
      , statusText model.score model.ship.lives
      , svg [ viewBox "0 0 800 600", height "100%", preserveAspectRatio "xMinYMin"]
          ([ background, ship ]
               ++ List.map (SvgL.lazy asteroidPolygon) model.asteroids
               ++ List.map (SvgL.lazy bulletCircle) model.bullets
               ++ List.map (SvgL.lazy starCircle) model.starfield)
      ]
          

          
         
    
