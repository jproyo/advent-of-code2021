module Day2 where

import           Control.Lens
import           Data.Text                      ( breakOn
                                                , toTitle
                                                )
import           Prelude                        ( read )
import           Relude                  hiding ( Down )


data CommandT = Forward | Down | Up
  deriving Read

type Command = (CommandT, Int)

data Position = Position
  { _horizontal :: Int
  , _depth      :: Int
  , _aim        :: Int
  }

makeLenses ''Position

initialPos :: Position
initialPos = Position 0 0 0

parseCommandT :: Text -> CommandT
parseCommandT = read . toString . toTitle

parseInput :: Text -> [Command]
parseInput = map (bimap parseCommandT (read . toString) . breakOn " ") . lines

finalPosition :: Position -> Int
finalPosition Position {..} = _horizontal * _depth

doState :: MonadState Position m
        => (Command -> Position -> Position)
        -> Command
        -> m ()
doState c = modify . c

evalCommand1 :: Command -> Position -> Position
evalCommand1 (Forward, x) = over horizontal (+ x)
evalCommand1 (Down   , x) = over depth (+ x)
evalCommand1 (Up     , x) = over depth (flip (-) x)

updatePosition :: Int -> State Position ()
updatePosition x = do
  a <- use aim
  depth += a * x
  horizontal += x

evalCommand2 :: Command -> Position -> Position
evalCommand2 (Forward, x) = execState (updatePosition x)
evalCommand2 (Down   , x) = over aim (+ x)
evalCommand2 (Up     , x) = over aim (flip (-) x)

solveDay2 :: (Command -> Position -> Position) -> FilePath -> IO Int
solveDay2 eval =
  fmap
      ( finalPosition
      . flip execState initialPos
      . mapM_ (doState eval)
      . parseInput
      )
    . readFileText

day2p1 :: FilePath -> IO Int
day2p1 = solveDay2 evalCommand1

day2p2 :: FilePath -> IO Int
day2p2 = solveDay2 evalCommand2
