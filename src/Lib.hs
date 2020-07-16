module Lib
    ( Route
    , AppState
    , AppStateT
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
import Control.Monad.Trans.Reader
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class (lift)

type Response = String
type Request = String

type ActionError = String
type ActionT = Exc.ExceptT ActionError (Reader Request) Response

type Application = String -> String
type Route = Application -> Application

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState

-- stolen from client, he-he
constructResponse :: [String] -> ActionT
constructResponse = Exc.except . Right . unwords 

-- runHandler :: Handler -> Request -> ActionT
-- runHandler h req = (h req) `Exc.catchE` errorHandler

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: ActionT -> (String -> Bool) -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    runAction mw input_string
  else
    tryNext

runAction :: ActionT -> Request -> Response
runAction action request =
  let response = flip runReader request
                 $ Exc.runExceptT
                 $ action `Exc.catchE` errorHandler
      left  = ("There was an error :" ++)
      right = id
  in
    either left right response

addRoute ::
  Monad m => ActionT -> (String -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

-- runMyApp :: Monad m => Handler -> AppState -> String -> m String
runMyApp :: AppState -> Application -> Application
runMyApp app_state request = foldl (flip ($)) request (routes app_state)

defaultHandler :: Request -> Response
defaultHandler request = "404: '" ++ request ++ "'"

errorHandler :: ActionError -> ActionT
errorHandler error = Exc.except . Right $ "500: '" ++ error ++ "'"

userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  
  putStrLn "Please type in the request"
  putStrLn "(one of 'handler1', 'handler2', 'handler3', 'buggy' or any string for default handling)"
  request <- getLine
  unless (request == "q") $ do
    let response = runMyApp app_state defaultHandler request
    print response
    putStrLn ""
    userInputLoop app_state

myScotty :: AppStateT () -> IO ()
myScotty my_app = do
    let app_state = ST.execState my_app AppState{ routes = []}
    userInputLoop app_state  