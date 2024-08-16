{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro
import Control.Monad.State
import Brick
import Brick.BChan
import Brick.Main
    ( App(..)
    , customMainWithDefaultVty
    , defaultMain
    , halt, neverShowCursor
    )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Types
    ( Widget
    )
import Brick.Widgets.Core
    ((<=>)
    ,(<+>)
    ,str
    ,padLeftRight
    ,padTop
    ,Padding
    )
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Bifunctor (bimap, Bifunctor (first))
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intersect, sort, sortBy, sortOn, foldl', find)
import Data.Containers.ListUtils (nubInt)
import qualified Data.Ord as Ord
import System.Random.Shuffle
import System.Random
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate (Day)
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import qualified Brick.Widgets.Center as B

type Coord = (Int, Int)
type Board = [Tile]

addCoord :: Coord -> Coord -> Coord
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)
negCoord :: Coord -> Coord
negCoord = bimap negate negate
subCoord :: Coord -> Coord -> Coord
subCoord a = addCoord a . negCoord

data MinoType = I | J | L | O | S | Z | T | Mono | Ghost MinoType | Empty | None deriving (Show, Eq)
data Rotation = Rot0 | RotR | Rot2 | RotL deriving (Show, Eq, Enum)
data Tile = Tile { tilePosition :: Coord, tileType :: MinoType } deriving (Show, Eq)
data Piece = Piece { pieceType :: MinoType, pieceTiles :: [Tile], piecePosition :: Coord, pieceRotation :: Rotation } deriving (Show, Eq)

coordListToTiles :: [Coord] -> MinoType -> [Tile]
coordListToTiles locs t = map (\l -> Tile { tilePosition = l, tileType = t }) locs
tilesToCoordList :: [Tile] -> [Coord]
tilesToCoordList = map tilePosition

getPiece :: MinoType -> Piece
getPiece minotype = Piece { pieceType = minotype, pieceTiles = coordListToTiles tilePositions minotype, piecePosition = (0, 0), pieceRotation = Rot0 }
    where tilePositions = case unGhost minotype of
            I -> [(-1,0), (0,0), (1,0), (2,0)]
            J -> [(-1,1),(-1,0),(0,0), (1,0)]
            L -> [(-1,0), (0,0), (1,0), (1,1)]
            O -> [(0,1), (0,0), (1,1),(1,0)]
            S -> [(-1,0), (0,0), (0,1),(1,1)]
            T -> [(-1,0), (0,0), (0,1),(1,0)]
            Z -> [(-1,1),(0,1),(0,0), (1,0)]
            Mono -> [(0,0)]
            Empty -> []
            None -> []
            Ghost _ -> []

offsetTable :: MinoType -> Rotation -> [Coord]
offsetTable O Rot0 = [(0,0)]
offsetTable O RotR = [(0,-1)]
offsetTable O Rot2 = [(-1,-1)]
offsetTable O RotL = [(-1,0)]
offsetTable I Rot0 = [(0,0),(-1,0),(2,0),(-1,0),(2,0)]
offsetTable I RotR = [(-1,0),(0,0),(0,0),(0,1),(0,-2)]
offsetTable I Rot2 = [(-1,1),(1,1),(-2,1),(1,0),(-2,0)]
offsetTable I RotL = [(0,1),(0,1),(0,1),(0,-1),(0,2)]
offsetTable _ Rot0 = [(0,0),(0,0),(0,0),(0,0),(0,0)]
offsetTable _ RotR = [(0,0),(1,0),(1,-1),(0,2),(1,2)]
offsetTable _ Rot2 = [(0,0),(0,0),(0,0),(0,0),(0,0)]
offsetTable _ RotL = [(0,0),(-1,0),(-1,-1),(0,2),(-1,2)]

kickTable :: MinoType -> Rotation -> Rotation -> [Coord]
kickTable typ startRot endRot = zipWith subCoord (offsetTable typ startRot) (offsetTable typ endRot)

rotationDiff :: Rotation -> Rotation -> Rotation
rotationDiff r1 r2
    | r1 == r2 = Rot0
    | r1 == Rot0 = r2
    | rs RotR Rot2 || rs Rot2 RotL || rs RotL Rot0 = RotR
    | rs RotR RotL || rs Rot2 Rot0 || rs RotL RotR = Rot2
    | rs RotR Rot0 || rs Rot2 RotR || rs RotL Rot2 = RotL
    where rs rot1 rot2 = r1 == rot1 && r2 == rot2
rotationDiff r1 r2 = error ("no rotation difference for " ++ show r1 ++ " " ++ show r2)

addRotation :: Rotation -> Rotation -> Rotation
addRotation r1 Rot0 = r1
addRotation RotL r2 = addRotation Rot0 (pred r2)
addRotation r1 r2 = addRotation (succ r1) (pred r2)

pureRotateAround0 :: [Tile] -> Rotation -> Rotation -> [Tile]
{-pureRotateAround0 ls r1 r2 = rot ls (rotationDiff r1 r2)
    where
        rot l Rot0 = l
        rot l r = map ((\c -> Tile { tilePosition = c, tileType = tileType $ head l }) . rotateTup r . tilePosition) l
            where rotateTup Rot0 t = t
                  rotateTup RotR (x,y) = (y, -x)
                  rotateTup Rot2 (x,y) = (-x,-y)
                  rotateTup RotL (x,y) = (-y, x)
-}
pureRotateAround0 = pureRotateAround (0,0)
pureRotateAround :: Coord -> [Tile] -> Rotation -> Rotation -> [Tile]
pureRotateAround origin ls r1 r2 = offsetTiles (rot (offsetTiles ls (negCoord origin)) (rotationDiff r1 r2)) origin
    where
        rot l Rot0 = l
        rot l r = map ((\c -> Tile { tilePosition = c, tileType = tileType $ head l }) . rotateTup r . tilePosition) l
            where rotateTup Rot0 t = t
                  rotateTup RotR (x,y) = (y, -x)
                  rotateTup Rot2 (x,y) = (-x,-y)
                  rotateTup RotL (x,y) = (-y, x)

tilesInBoard :: Board -> [Tile] -> Bool
tilesInBoard brd tls = null (tilesToCoordList brd `intersect` tilesToCoordList tls)
                       && all ((\(x,y) -> y >= 0 && x >= 0 && x < 10) . tilePosition) tls

rotatePiece :: Board -> Piece -> Rotation -> Piece
rotatePiece board piece@Piece{pieceType=pTyp,pieceRotation=pRot,piecePosition=pPos} rot = if null firstOffset then piece else piece { piecePosition=pPos `addCoord` head firstOffset, pieceRotation=addRotation pRot rot }
    where rotatedTiles = pureRotateAround pPos (extractPieceTiles piece) pRot (pRot `addRotation` rot)
          firstOffset = take 1 . dropWhile (not . tilesInBoard board . offsetTiles rotatedTiles) $ kickTable pTyp pRot (addRotation pRot rot)

offsetTiles :: [Tile] -> Coord -> [Tile]
offsetTiles tls offset = map (\t@Tile{tilePosition=pos} -> t { tilePosition=pos `addCoord` offset }) tls

movePiece :: Board -> Piece -> Coord -> Piece
movePiece board piece@Piece{pieceType=pTyp,pieceRotation=pRot,piecePosition=pPos} movement = if isValid then piece { piecePosition=movement `addCoord` pPos } else piece
    where movedTiles = offsetTiles (extractPieceTiles piece) movement
          isValid = tilesInBoard board movedTiles

extractPieceTiles :: Piece -> [Tile]
extractPieceTiles Piece{pieceTiles=pTls,pieceRotation=pRot,piecePosition=pPos,pieceType=pTyp} = offsetTiles (pureRotateAround0 coloredTiles Rot0 pRot) pPos
    where coloredTiles = map (\t -> t {tileType=pTyp}) pTls

unGhost :: MinoType -> MinoType
unGhost (Ghost m) = m
unGhost m = m

isColoredGhost :: MinoType -> Bool
isColoredGhost (Ghost (Ghost _)) = False
isColoredGhost (Ghost Empty) = False
isColoredGhost (Ghost _) = True
isColoredGhost _ = False

tileWidget :: MinoType -> Widget ()
tileWidget Empty = withAttr (attrName "pieceTypeEmpty") $ str "[]"
tileWidget None = str "  "
tileWidget m@(Ghost mtype) = withAttr (attrName ghostAttrName) $ str "░░"
    where ghostAttrName = "pieceType" ++ (if isColoredGhost m then show mtype else "Ghost")
tileWidget mtype = withAttr (attrName $ "pieceType" ++ show mtype) $ str "██"

renderSinglePiece :: Piece -> Widget ()
renderSinglePiece p = vBox $ map (\vi -> hBox $ map (\hi -> tileWidget $ minoAtRelativePositionByTiles tiles (hi, vi)) [-1..2]) [2,1.. -1]
    where tiles = pieceTiles p

--minoAtRelativePosition :: Piece -> Coord -> MinoType
--minoAtRelativePosition p c = fromMaybe Empty $ lookup c (map (\(Tile crd tp) -> (crd, tp)) (pieceTiles p))
              --where offsetPosition = bimap (x -) (y -) (piecePosition p)
minoAtRelativePositionByTiles :: [Tile] -> Coord -> MinoType
minoAtRelativePositionByTiles t c = fromMaybe Empty $ lookup c (map (\(Tile crd tp) -> (crd, tp)) t)
--minoAtAbsolutePosition :: Piece -> Coord -> MinoType
--minoAtAbsolutePosition p (x,y) = fromMaybe Empty $ lookup offsetPosition (map (\(Tile crd tp) -> (crd, tp)) (pieceTiles p))
              ----where offsetPosition = bimap (subtract x) (subtract y) (piecePosition p)
              --where offsetPosition = bimap (subtract x) (subtract y) (piecePosition p)

renderBoard :: Board -> Widget ()
renderBoard l = vBox $ map (\vi -> hBox $ map (\hi -> tileWidget $ minoAtRelativePositionByTiles l (hi, vi)) [0..9]) [23,22..0]

clearLines :: Board -> (Board, Int)
clearLines board = first concat . foldl' (\(b,offset) l -> if length l < 10 then (moveLineDownBy offset l:b,offset) else (b,offset+1)) ([],0) $ tilesByLine
    where allLineIndexes = sort (nubInt $ map (snd . tilePosition) board)
          tilesByLine :: [[Tile]]
          tilesByLine = map (\i -> filter ((i ==) . snd . tilePosition) board) [0..maximum allLineIndexes]
          moveLineDownBy offset = map (\t@(Tile{tilePosition=(x,y)}) -> t {tilePosition=(x,y-offset)})

dropPiece :: Board -> Piece -> Piece
dropPiece board piece@Piece{piecePosition=(x,y)} = piece{piecePosition=(x,y-(firstBad-1))}
    where firstBad = fromJust $ find (\offset -> not $ tilesInBoard board (extractPieceTiles piece{piecePosition=(x,y-offset)})) [0..]



data FailData = NotFailed | Failed { failScore :: Int }
data GameState = MenuState { _menuTime :: UTCTime, _failed :: FailData } | GameState { _score :: Int, _lastDrop :: UTCTime, _currentTime :: UTCTime }
makeLenses ''GameState
data St = St { _board :: Board, _queue :: [Piece], _hold :: Piece, _currentPiece :: Piece, _gameState :: GameState }
makeLenses ''St
data CustomEvent = Tick UTCTime


menuToGame :: GameState -> GameState
menuToGame gmSt@GameState{} = gmSt
menuToGame mSt@MenuState{_menuTime=tm} = GameState { _score=0, _lastDrop=tm, _currentTime=tm }
shouldDrop :: GameState -> Bool
shouldDrop MenuState{} = False
shouldDrop GameState{_score=scr, _lastDrop=dropTime, _currentTime=currTime} = currTime `diffUTCTime` dropTime > 1

eventHandler :: BrickEvent () CustomEvent -> EventM () St ()
eventHandler (AppEvent (Tick t)) = modify (handleGravity t)
eventHandler (VtyEvent (V.EvKey V.KEsc [])) = halt
eventHandler (VtyEvent (V.EvKey V.KLeft [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=movePiece brd curr (-1,0) })
eventHandler (VtyEvent (V.EvKey V.KRight [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=movePiece brd curr (1,0) })
eventHandler (VtyEvent (V.EvKey V.KDown [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=movePiece brd curr (0,-1) })
eventHandler (VtyEvent (V.EvKey V.KUp [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=dropPiece brd curr })
eventHandler (VtyEvent (V.EvKey (V.KChar 'a') [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=rotatePiece brd curr Rot2 })
eventHandler (VtyEvent (V.EvKey (V.KChar 'd') [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=rotatePiece brd curr RotR })
eventHandler (VtyEvent (V.EvKey (V.KChar 's') [])) = modify (\st@St{_board=brd, _currentPiece=curr} -> st { _currentPiece=rotatePiece brd curr RotL })
eventHandler (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify handleDrop
eventHandler (VtyEvent (V.EvKey V.KEnter [])) = modify handleMenu
eventHandler (VtyEvent (V.EvKey (V.KChar '<') [])) = modify handleHold
--eventHandler (VtyEvent (V.EvKey V.KRight [])) = i %= (*2)
eventHandler _ = return ()

handleGravity :: UTCTime -> St -> St
handleGravity time st@St{_gameState=mSt@MenuState{}} = st { _gameState=mSt{_menuTime=time} }
handleGravity time st@St{_board=brd, _currentPiece=curr, _gameState=gmSt } =
    let (newCurr, newGameState) = if shouldDrop gmSt then (movePiece brd curr (0,-1), gmSt{_lastDrop=time, _currentTime=time}) else (curr, gmSt{_currentTime=time})
    in  st{_currentPiece=newCurr, _gameState=newGameState}
handleHold :: St -> St
handleHold st@St{_queue=q, _currentPiece=curr, _hold=hld} = if tilesInBoard (_board newState) (extractPieceTiles $ _currentPiece newState) then newState else newState { _gameState=MenuState{_menuTime=_currentTime currGameState, _failed=Failed{failScore=_score currGameState}} }
    where (newQueue, newPiece, newHold) = if pieceType hld == Empty
              then (drop 1 q, head q, curr)
              else (q, hld{piecePosition=(4,21), pieceRotation=Rot0}, curr)
          newState = st { _queue=newQueue, _currentPiece=newPiece, _hold=newHold }
          currGameState = _gameState st
handleDrop :: St -> St
handleDrop st@St{_board=brd, _queue=q, _currentPiece=curr} = if tilesInBoard (_board newState) (extractPieceTiles $ _currentPiece newState) then newState else newState { _gameState=MenuState{_menuTime=_currentTime currGameState, _failed=Failed{failScore=_score currGameState}} }
    where (clearedBoard, clearedLines) = clearLines $ extractPieceTiles (dropPiece brd curr) ++ brd
          newState = st { _board=clearedBoard, _queue=drop 1 q, _currentPiece=head q, _gameState=currGameState{_score=currScore+scoreForClear clearedLines} }
          currGameState = _gameState st
          currScore = _score currGameState

-- spins soon(tm)
scoreForClear :: Int -> Int
scoreForClear 1 = 50
scoreForClear 2 = 100
scoreForClear 3 = 200
scoreForClear 4 = 400
scoreForClear x = 100 * x
           
--(\st@St{_gameState=gmSt} -> case gmSt of mSt@MenuState{_failed=failData} -> (case failData of NotFailed -> st { _gameState=menuToGame mSt }; Failed{} -> st);  GameState{} -> st)
handleMenu :: St -> St
handleMenu st@St{_gameState=mSt@MenuState{_failed=NotFailed}} = updatedState{_gameState=menuToGame mSt}
    where updatedState = st { _currentPiece=head (_queue st), _queue = drop 1 (_queue st)}
handleMenu st@St{_gameState=MenuState{_failed=Failed{}}} = st
handleMenu st@St{_gameState=GameState{}} = st


pieceWithPos :: MinoType -> Coord -> Piece
pieceWithPos t c = (getPiece t) { piecePosition = c }

draw :: St -> [Widget ()]
draw (St{_gameState=MenuState{_failed=NotFailed{}}}) = [C.center $ introLine <=> str "Press Enter to start"]
    where introLine = ((withAttr (attrName "haskellColor") $ str "λ") <+> str " -> " <+> (withAttr (attrName "haskellColor") $ str "ha") <+> str "tris")
draw (St{_gameState=MenuState{_failed=Failed{ failScore=finalScore }}}) = [C.center $ str ("You lost!\nYour final score was " ++ show finalScore ++ ".")]
draw (St{_currentPiece=c@Piece{pieceType=t}, _board=brd, _queue=q, _hold=hld, _gameState=GameState{_score=currScore}}) =
    [C.center $ hBox [
                    padTop (Pad 1) $ widgetBorders "Hold" $ renderSinglePiece hld,
                    padLeftRight 1 $ (str ("Score: " ++ show currScore)) <=> (widgetBorders "Board" $ renderBoard (extractPieceTiles c
                                ++ extractPieceTiles ((dropPiece brd c){pieceType=Ghost t})
                                ++ brd)),
                    padTop (Pad 1) $ widgetBorders "Queue" $ vBox (map renderSinglePiece (take 5 q))
                ]]
--draw (St _ [] _ _) = [C.center $ str "no more piece!"]

widgetBorders :: String -> Widget () -> Widget ()
widgetBorders label = withBorderStyle BS.unicodeRounded . B.borderWithLabel (str label)

mainAttrMap :: AttrMap
mainAttrMap = attrMap V.defAttr [
     (attrName "haskellColor", fg (V.linearColor 0x5e 0x51 0x84) `V.withStyle` V.bold)
    ,(attrName "pieceTypeI", fg V.cyan)
    ,(attrName "pieceTypeJ", fg V.blue)
    ,(attrName "pieceTypeL", fg V.yellow `V.withStyle` V.dim)
    ,(attrName "pieceTypeO", fg V.yellow)
    ,(attrName "pieceTypeS", fg V.green)
    ,(attrName "pieceTypeT", fg V.magenta)
    ,(attrName "pieceTypeZ", fg V.red)
    ,(attrName "pieceTypeMono", fg V.white)
    ,(attrName "pieceTypeGhost", fg V.black)
    ,(attrName "pieceTypeEmpty", fg V.black{- `V.withStyle` V.dim-})]

app :: App St CustomEvent ()
app = App { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = eventHandler
    , appStartEvent = return ()
    , appAttrMap = const mainAttrMap
}

generateBags :: RandomGen g => [MinoType] -> g -> [MinoType]
generateBags _bag _gen = genb _bag _gen
    where len = length _bag
          genb bag gen = shuffle' bag len gen1 ++ genb bag gen2
               where (gen1, gen2) = split gen


main :: IO ()
main = do
    stdGen <- initStdGen
    bchan <- newBChan 10
    currTime <- getCurrentTime

    void . forkIO $ forever $ do
        t <- getCurrentTime
        writeBChan bchan (Tick t)
        threadDelay 10000
    --let initialState = St [] (cycle $ map getPiece [I,J,L,O,S,T,Z,Ghost I,Ghost J,Ghost L,Ghost O,Ghost S,Ghost T,Ghost Z, Ghost Empty]) (getPiece O)
    let initialState = St { _board=[], _queue=(map (`pieceWithPos` (4,21)) (generateBags [I,J,L,O,S,T,Z] stdGen)), _hold=(getPiece Empty), _currentPiece=(getPiece Empty), _gameState=(MenuState {_menuTime=currTime, _failed=NotFailed})}
    _ <- customMainWithDefaultVty (Just bchan) app initialState
    return ()
