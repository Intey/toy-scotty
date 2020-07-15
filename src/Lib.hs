module Lib
    ( Route
    , AppState
    , AppStateT
    , addRoute
    , myScotty
    ) where

import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad

type Route = String -> String

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

addRoute :: Route -> AppStateT ()
addRoute mf = ST.modify $ \s -> addRoute' mf s

runMyApp :: Monad m => String -> ST.State AppState a -> m String
runMyApp request my_app = do
    -- initialize routes from client
    let s = ST.execState my_app AppState{ routes = []}
    -- apply first route to request, then apply it's response to second route and so on
    let output = foldr ($) request (routes s) 
    return $ output

userInputLoop :: AppStateT () -> IO ()
userInputLoop app_state = do
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp request app_state
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop app_state

myScotty :: AppStateT () -> IO ()
myScotty my_app = do
  userInputLoop my_app
  