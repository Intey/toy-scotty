    
import Lib
import qualified Control.Monad.Trans.Except as Exc
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
-- client functions
constructResponse :: [String] -> ActionT
constructResponse = Exc.except . Right . unwords 

routeHandler1 :: ActionT
routeHandler1 = do
    request <- lift ask 
    return $ "200 request in handler1 =" ++ request

routeHandler2 :: ActionT
routeHandler2 = do
    request <- lift ask
    return $ "200 request in handler2 =" ++ request

routeHandler3 :: ActionT
routeHandler3 = do
    request <- lift ask
    return $ "200 request in handler3 =" ++ request

buggy :: ActionT
buggy = do
    Exc.throwE "Error from buggy"

myApp :: AppStateT ()
myApp = do
    addRoute routeHandler1 (== "handler1")
    addRoute routeHandler2 (== "handler2")
    addRoute routeHandler3 (== "handler3")
    addRoute buggy         (== "buggy")

main :: IO ()
main = myScotty myApp

