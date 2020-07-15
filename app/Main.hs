    
import Lib

-- client functions
constructResponse :: [String] -> String
constructResponse = unwords

routeHandler1 :: String -> String
routeHandler1 request = constructResponse [
    "request in handler1 got: '" ++ request ++ "'"]

routeHandler2 :: String -> String
routeHandler2 request = constructResponse [
    "request in handler2 got: '" ++ request ++ "'"]

routeHandler3 :: String -> String
routeHandler3 request = constructResponse [
    "request in handler3 got: '" ++ request ++ "'"]

myApp :: AppStateT ()
myApp = do
    addRoute routeHandler1
    addRoute routeHandler2
    addRoute routeHandler3

main :: IO ()
main = myScotty myApp

