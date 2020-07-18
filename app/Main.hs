    
import Lib
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as ST
-- import qualified Control.Monad.ST as
import Control.Monad.Trans.Class
-- client functions

routeHandler1 :: ActionT ()
routeHandler1 = do
    request <- getRequest
    let st = ST.modify (\_ -> ": 200 request in handler1 =" ++ request)
    lift . lift $ st

routeHandler2 :: ActionT ()
routeHandler2 = do
    request <- getRequest
    lift . lift $ ST.modify (\s -> s ++ ": 200 request in handler2 =" ++ request)

routeHandler3 :: ActionT ()
routeHandler3 = do
    request <- getRequest
    lift . lift $ ST.modify (\s -> s ++ ": 200 request in handler3 =" ++ request)

buggy :: ActionT ()
buggy = Exc.throwE "Error from buggy"

myApp :: AppStateT ()
myApp = do
    addRoute routeHandler1 (== "handler1")
    addRoute routeHandler2 (== "handler2")
    addRoute routeHandler3 (== "handler3")
    addRoute buggy         (== "buggy")

main :: IO ()
main = myScotty myApp

