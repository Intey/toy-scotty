module Lib
    ( Route
    , AppState
    , AppStateT
    , Handler
    , addRoute
    , myScotty
    ) where

import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad

type Handler = String -> String
type Route = Handler -> Handler

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState

-- Inside: Possible replace
-- route = flip select
-- import Control.Conditional from package cond
-- for remove flip, we should declare route as @(String -> Bool) -> Handler -L Route@
route :: Handler -> (String -> Bool) -> Route
route handler1 cond handler2 input_string =
  if cond input_string then
    handler1 input_string
  else
    handler2 input_string


addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute :: Handler -> (String -> Bool) -> AppStateT ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

runMyApp :: Monad m => Handler -> AppState -> String -> m String
runMyApp def app_state request = do
  let output = foldl (flip ($)) def (routes app_state) request
  return output

defaultRoute request = unwords ["default router got: '" ++ request ++ "'"]

userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop app_state


myScotty :: AppStateT () -> IO ()
myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state  