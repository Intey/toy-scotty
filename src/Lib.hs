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
    , getRequest
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

-- Ok, Except needs for try...catch. ReaderT, StateT?
-- ReaderT just for access Request (ReaderT Request) in Handler(ActionT) with 
-- do-semantic, like it's global as in flask(python web framework)
{- @@@
routeHandler1 = do
  request <- lift ask 
  ...
@@@ -} 
-- Just for this?
-- StateT for changing Response with middlewares. Ok.
type ActionT a = Exc.ExceptT ActionError (ReaderT Request (ST.State Response)) a

-- and now, routeHandler = do { request <- getRequest; ... }
getRequest :: ActionT Request
getRequest = lift ask 

-- Why Application? I think it's Handler, but ActionT is Handler
type Application = String -> String

type Route = Application -> Application

data AppState = AppState { routes::[Route]}
type AppStateT = ST.State AppState

-- stolen from client, he-he
-- constructResponse :: [String] -> ActionT ()
-- constructResponse = Exc.except . Right . unwords 

-- runHandler :: Handler -> Request -> ActionT
-- runHandler h req = (h req) `Exc.catchE` errorHandler

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: ActionT () -> (String -> Bool) -> Route
route mw pat mw1 input_string =
  let tryNext = mw1 input_string in
  if pat input_string
  then
    fromMaybe "" $ runAction mw input_string
  else
    tryNext

-- oh, shi.
-- firstly run ExceptT action with error handling. result will be ReaderT Request (State Response) (Either ActionError ())
runAction :: ActionT () -> Request -> Maybe Response
runAction action request =
  let (a, s) = flip ST.runState ""
                 $ flip runReaderT request
                 $ Exc.runExceptT
                 $ action `Exc.catchE` errorHandler
      left  = const $ Just "There was an error"
      right = const $ Just s
  in
    either left right a

addRoute ::
  Monad m => ActionT () -> (String -> Bool) -> ST.StateT AppState m ()
addRoute mf pat = ST.modify $ \s -> addRoute' (route mf pat) s

-- runMyApp :: Monad m => Handler -> AppState -> String -> m String
runMyApp :: AppState -> Application -> Application
runMyApp app_state request = foldl (flip ($)) request (routes app_state)

defaultHandler :: Request -> Response
defaultHandler request = "404: '" ++ request ++ "'"

errorHandler :: ActionError -> ActionT ()
errorHandler error = lift . lift
  $ ST.modify (\s -> "500" ++ s ++ error ++ " inside middleware func 3")

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