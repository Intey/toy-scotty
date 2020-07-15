module Lib
    ( Route
    , AppState
    , AppStateT
    , Handler
    , Response
    , Request
    , addRoute
    , myScotty
    ) where

import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad

type Response = Maybe String
type Request = String
type Handler = Request -> Response
type Route = Handler -> Handler

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState


route :: Handler -> (String -> Bool) -> Route
route handler1 cond handler2 input_string =
  if cond input_string then
    case handler1 input_string of
      -- if one handler "raises error" - pass to next. Why?
      Nothing -> errorHandler input_string 
      Just a -> Just a
  else
    handler2 input_string


addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute :: Handler -> (String -> Bool) -> AppStateT ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

-- runMyApp :: Monad m => Handler -> AppState -> String -> m String
runMyApp defaultHandler app_state request = do
  let output = foldl (flip ($)) defaultHandler (routes app_state) request
  return output

defaultHandler :: Request -> Response
defaultHandler request = Just $ unwords [request, "not found handler for request"]

errorHandler :: Request -> Response
errorHandler request = Just $ unwords [
  request, "Nothing returned from one of the handlers"]

-- defaultRoute request = unwords ["default router got: '" ++ request ++ "'"]

userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultHandler app_state request
    case response of
      Just x -> print x
      Nothing -> print $ errorHandler request
    userInputLoop app_state

myScotty :: AppStateT () -> IO ()
myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state  