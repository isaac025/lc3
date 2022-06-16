module Ex where

import Control.Monad (unless)
import Control.Monad.ST
import Control.Monad.State (liftIO)
import Data.STRef
import Data.IORef

data Status = Halted
            | Running
            deriving (Show, Eq)


type Machine = ST RealWorld Status

mutation :: Machine -> Machine
mutation machine = do curr <- newSTRef Running :: ST RealWorld (STRef RealWorld Status)
                      readSTRef curr 


computation mach = undefined

main :: IO ()
main = do let machine = newIORef Running
          --unless ((readSTRef machine) == Halted) $ do
           -- computation machine
          print "Still works"
