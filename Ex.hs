module Ex where

import Control.Monad.ST
import Control.Monad.State (liftIO)
import Data.STRef

data Status = Halted
            | Running
            deriving (Show, Eq)


type Machine = ST RealWorld Status

mutation :: Machine -> Machine
mutation machine = do curr <- newSTRef Running :: ST RealWorld (STRef RealWorld Status)
                      readSTRef curr 


computation = do curr <- stToIO $ (newSTRef Running >>= readSTRef :: Machine)
                 x <- newSTRef (-1)
                 y <- 

main :: IO ()
main = do computation 
          print "Program Halted"
