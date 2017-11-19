{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import System.Environment
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import Data.Maybe
import Data.List
import GHC.Conc.Sync

main :: IO ()
main = withSocketsDo $ do
  port <- getArgs
  sock <- listenOn (PortNumber (fromIntegral (read (head port))))
  server <- newServer sock
  printf "Listening on port %s\n" (head port)
  mainLoop server sock (read (head port))  0

mainLoop :: Server -> Socket -> Int -> Int -> IO ()
mainLoop server sock sockPort joinId = do
  (handle, host, port) <- accept sock
  printf "Accepted connection from %s: %s\n" host (show port)
  forkFinally (talk host sockPort handle server joinId) (\_ -> hClose handle)

  mainLoop server sock sockPort $! joinId + 1

type ClientName = String
type RoomName = String
type RoomRef = Int

data Client = Client
  { clientId       :: Int
  , clientName     :: ClientName
  , clientIP       :: String
  , clientPort     :: Int
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  }

newClient :: Int -> String -> Int -> Handle -> STM Client
newClient id host port handle = do
  c <- newTChan
  return Client { clientId       = id
                , clientName     = ""
                , clientIP       = host
                , clientPort     = port
                , clientHandle   = handle
                , clientSendChan = c
                }

dupClient :: Client -> String -> String -> Int -> STM Client
dupClient Client{..} name ip port    =
  return Client { clientId       = clientId
                  , clientName     = name
                  , clientIP       = ip
                  , clientPort     = port
                  , clientHandle   = clientHandle
                  , clientSendChan = clientSendChan
                  }

data Room = Room
  { roomRef     :: RoomRef
  , roomName    :: RoomName
  , roomClients :: TVar (Map Int Client)
  }

newRoom :: RoomRef -> RoomName -> STM Room
newRoom ref name = do
  c <- newTVar Map.empty
  return Room { roomRef      = ref
               , roomName       = name
               , roomClients  = c
               }

data Server = Server
  { sock      :: Socket
    , nextRef :: TVar RoomRef
    , clients :: TVar (Map Int Client)
    , rooms   :: TVar (Map RoomName Room)
    , refs    :: TVar (Map String RoomName)
  }

newServer :: Socket -> IO Server
newServer s  = do
  c <- newTVarIO Map.empty
  r <- newTVarIO Map.empty
  re <- newTVarIO Map.empty
  ref <- newTVarIO 0
  return Server { sock = s, nextRef = ref, clients = c, rooms = r, refs = re }

data Message = Broadcast String
             | Command String

broadcast :: Server -> Room -> Message -> STM ()
broadcast Server{..} Room{..} msg = do
  clientmap <- readTVar roomClients
  mapM_ (\client -> writeTChan (clientSendChan client) msg) (Map.elems clientmap)

talk :: String -> Int -> Handle -> Server -> Int ->  IO ()
talk host port handle server@Server{..} joinId = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  clientmap <- atomically $ readTVar clients
  client <- atomically $ newClient joinId host port handle
  atomically $ writeTVar clients $ Map.insert joinId client clientmap
  runClient server client
      `finally` atomically ( modifyTVar' clients ( Map.delete joinId))

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- getUserLines clientHandle
    if head  (words msg) == "CHAT:"
    then do
      x <- getMessageLines clientHandle
      atomically $ writeTChan clientSendChan (Command (msg ++ x))
    else atomically $ writeTChan clientSendChan (Command msg)

  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
        continue <- handleMessage serv client msg
        when continue server

getUserLines :: Handle -> IO String
getUserLines hdl = go hdl ""

go :: Handle -> String -> IO String
go hdl contents = do
  line <- hGetLine hdl
  case words line of
               "KILL_SERVICE" : a -> return "KILL_SERVICE"

               "HELO" : a -> return line

               "CLIENT_NAME:" : a -> return (contents ++ line ++ "\n")

               _      -> go hdl (contents ++ line ++ "\n")

getMessageLines :: Handle -> IO String
getMessageLines hdl = gm hdl ""

gm :: Handle -> String -> IO String
gm hdl contents = do
  line <- hGetLine hdl
  case line of
               "" -> return contents

               _      -> gm hdl (contents ++ line ++ "\n")

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Broadcast msg -> do
      hPutStrLn clientHandle msg
      return True
     Command msg ->
       case words msg of

           "JOIN_CHATROOM:" : a -> joinChatroom server client msg

           "LEAVE_CHATROOM:" : a -> leaveChatroom server client msg

           "DISCONNECT:" : a -> disconnect server client msg

           "CHAT:" : a -> chat server client msg

           "HELO" : t : a -> heloText server client t

           "KILL_SERVICE" : a -> killService server client

           _ -> do
               hPrintf clientHandle "Unrecognised command: %s\n"  msg
               return True

joinChatroom :: Server -> Client -> String -> IO Bool
joinChatroom server@Server{..} client@Client{..} a = do
    let l = lines a
    if length l >= 4
    then do
      let name = fromMaybe "" (stripPrefix "JOIN_CHATROOM: " (head l))
      let ip = fromMaybe "" (stripPrefix "CLIENT_IP: " (l !! 1))
      let port = fromMaybe "" (stripPrefix "PORT: " (l !! 2))
      let nick = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 3))

      roomMap <- atomically $ readTVar rooms
      case Map.lookup name roomMap of
       Nothing -> atomically $ do
         ref <- readTVar nextRef
         room <-  newRoom ref name
         refMap <- readTVar refs
         writeTVar refs $ Map.insert (show ref) name refMap
         writeTVar nextRef $! ref + 1
         writeTVar rooms $ Map.insert name room roomMap

         joinChat room ref name ip port nick client server

       Just room -> atomically $ joinChat room (roomRef room) name ip port nick client server

    else hPrintf clientHandle  "Unrecognised command: %s\n" a


    return True

joinChat :: Room -> Int -> String -> String -> String -> String -> Client -> Server -> STM ()
joinChat room ref name ip port nick client@Client{..} server@Server{..} = do
   clientRefs <- readTVar (roomClients room)
   dupClient <- dupClient client name ip (read port)
   writeTVar (roomClients room)  (Map.insert clientId dupClient clientRefs)
   unsafeIOToSTM (hPrintf clientHandle "JOINED_CHATROOM: %s\nSERVER_IP: 89.100.123.193\nPORT: %s\nROOM_REF: %s\nJOIN_ID: %s\n" name (show clientPort) (show (roomRef room)) (show clientId))
   let msg = "CHAT: "++show ref ++"\nCLIENT_NAME: "++nick++"\nMESSAGE: " ++ nick ++ " joined room"
   broadcast server room $(Broadcast  msg)

disconnect :: Server -> Client -> String -> IO Bool
disconnect server Client{..} a = do
  let l = lines a
  if length l >= 3
  then do
    let ip = fromMaybe "" (stripPrefix "DISCONNECT: " (head l))
    let port = fromMaybe "" (stripPrefix "PORT: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))

    return False

  else do
    hPrintf clientHandle  "Unrecognised command: %s\n" a
    return True

leaveChatroom :: Server -> Client -> String -> IO Bool
leaveChatroom server@Server{..} Client{..} a = do
  let l = lines a
  if length l >= 3
  then do
    let ref = fromMaybe "" (stripPrefix "LEAVE_CHATROOM: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let nick = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))
    refMap <- atomically $ readTVar refs
    case Map.lookup ref refMap of
        Nothing -> hPrintf clientHandle "Room does not exist: %s\n" ref
        Just name -> do
                     roomMap <- atomically $ readTVar rooms
                     case Map.lookup name roomMap of
                        Nothing -> hPrintf clientHandle "Room does not exist: " name
                        Just room -> atomically $ do
                          clientRefs <- readTVar (roomClients room)
                          writeTVar (roomClients room)  (Map.delete clientId  clientRefs)
                          let msg = "LEFT_CHATROOM: "++ref ++"\nJOIN_ID: " ++ show clientId
                          broadcast server room $(Broadcast  msg)
                          unsafeIOToSTM $ hPutStrLn clientHandle msg

  else hPrintf clientHandle "Unrecognised command: %s\n" a

  return True

chat :: Server -> Client -> String -> IO Bool
chat server@Server{..} Client{..} a = do
  let l = lines a
  if length l >= 4
  then do
    let ref = fromMaybe "" (stripPrefix "CHAT: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let nick = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))
    let message = fromMaybe "" (stripPrefix "MESSAGE: " (unlines(drop 2 l)))

    refMap <- atomically $ readTVar refs
    case Map.lookup ref refMap of
        Nothing -> hPrintf clientHandle "Room does not exist: %s\n" ref
        Just name -> do
          roomMap <- atomically $ readTVar rooms
          case Map.lookup name roomMap of
             Nothing -> hPrintf clientHandle "Room does not exist: %s\n" name
             Just room -> atomically $ do
              let msg = "CHAT: "++ref ++"\nCLIENT_NAME: "++nick++"\nMESSAGE: "++message
              broadcast server room $(Broadcast  msg)

  else hPrintf clientHandle "Unrecognised command: %s\n" a

  return True

heloText :: Server -> Client -> String -> IO Bool
heloText server@Server{..} Client{..} t = do
  printf "HELO %s\nIP:%s\nPort:%s\nStudentID:13326255\n" t clientIP (show clientPort)
  hPrintf clientHandle "HELO %s\nIP:%s\nPort:%s\nStudentID:13326255\n" t clientIP (show clientPort)
  return True

killService :: Server -> Client  -> IO Bool
killService server@Server{..} Client{..}  = do
  sClose sock
  return True