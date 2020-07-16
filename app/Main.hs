    
import Lib
import qualified Control.Monad.Trans.Except as Exc

-- client functions
constructResponse :: [String] -> ActionT
constructResponse = Exc.except . Right . Just . unwords 

routeHandler1 :: Handler
routeHandler1 request = constructResponse [
    request, "request in handler1"]

routeHandler2 :: Handler
routeHandler2 request =  constructResponse [
    request, "request in handler2"]

routeHandler3 :: Handler
routeHandler3 request = constructResponse [
    request, "request in handler3"]

buggy :: Handler
buggy _ = Exc.throwE "Error from buggy"

myApp :: AppStateT ()
myApp = do
    addRoute routeHandler1 (== "handler1")
    addRoute routeHandler2 (== "handler2")
    addRoute routeHandler3 (== "handler3")
    addRoute buggy         (== "buggy")

main :: IO ()
main = myScotty myApp

