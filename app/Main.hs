    
import Lib

-- client functions
-- constructResponse :: [String] -> String
constructResponse = unwords

routeHandler1 :: Handler
routeHandler1 request = constructResponse [
    "request in handler1 got: '" ++ request ++ "'"]

routeHandler2 :: Handler
routeHandler2 request = constructResponse [
    "request in handler2 got: '" ++ request ++ "'"]

routeHandler3 :: Handler
routeHandler3 request = constructResponse [
    "request in handler3 got: '" ++ request ++ "'"]

myApp :: AppStateT ()
myApp = do
    addRoute routeHandler1 (== "handler1")
    addRoute routeHandler2 (== "handler2")
    addRoute routeHandler3 (== "handler3")

main :: IO ()
main = myScotty myApp

