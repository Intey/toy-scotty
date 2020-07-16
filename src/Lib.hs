module Lib
    ( Route
    , AppState
    , AppStateT
    , Handler
    , Response
    , Request
    , ActionT
    , ActionError
    , addRoute
    , myScotty
    ) where

import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad
import qualified Control.Monad.Trans.Except as Exc

type Response = Maybe String
type Request = String

type ActionError = String
type ActionT = Exc.Except ActionError Response

type Handler = Request -> ActionT
type Route = Handler -> Handler

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState

-- stolen from client, he-he
constructResponse :: [String] -> ActionT
constructResponse = Exc.except . Right . Just . unwords 

runHandler :: Handler -> Request -> ActionT
runHandler h req = (h req) `Exc.catchE` errorHandler

route :: Handler -> (String -> Bool) -> Route
route handler1 cond handler2 input_string =
  if cond input_string then
    runHandler handler1 input_string
  else
    runHandler handler2 input_string

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute :: Handler -> (String -> Bool) -> AppStateT ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

-- runMyApp :: Monad m => Handler -> AppState -> String -> m String
runMyApp :: (Monad m) => Handler -> AppState -> Request -> m ActionT
runMyApp defaultHandler app_state request = do
  let output = foldl (flip ($)) defaultHandler (routes app_state) request
  return output

defaultHandler :: Request -> ActionT
defaultHandler request = constructResponse ["not found handler for request: '", request, "'"]

errorHandler :: ActionError -> ActionT
errorHandler error = Exc.except . Right . Just $ "Got exception: '" ++ error ++ "'"

userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultHandler app_state request :: Maybe ActionT
    print response
    putStrLn ""
    userInputLoop app_state

myScotty :: AppStateT () -> IO ()
myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state  