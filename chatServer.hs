import Data.List.Split
import System.IO
import System.Exit
import System.Environment
import Network.Socket
import Network.BSD
import Network as L
import Control.Concurrent
import Control.Monad
import System.Exit
import Control.Monad(liftM,when)
import Control.Monad.Fix(fix)

myPool = 10


main :: IO ()
main = do
    args <- getArgs
    let port = read $ head args :: Int
    start port

start:: Int -> IO ()
start port = do
    sock <- socket AF_INET Stream 0    -- create socket    
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable
    bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)  
    listen sock 8
    chan<-newChan
    writeChan chan ("", 0)
    (input,output) <- threadPoolIO myPool runConn
    mainLoop sock input chan port 0

mainLoop :: Socket -> Chan (Int,Handle, SockAddr,Chan (String, Int))-> Chan (String,Int) ->Int -> Int -> IO ()
mainLoop sock input h port client = do
    (conn,addr) <- Network.Socket.accept sock     -- accept a connection and handle it
    hdl <- socketToHandle conn ReadWriteMode
    hSetBuffering hdl  $ BlockBuffering (Nothing)
    writeChan input (port, hdl, addr,h)
    mainLoop sock input h port (client+1)


runConn :: (Int,Handle,SockAddr, Chan (String, Int)) -> IO ()
runConn (port,hdl, sa, chan) = do
    (mess, clientID)<-readChan chan
    let j = clientID
    let clientID = j+1
    --writeChan chan (mess, clientID)
    messaging sa hdl chan clientID mess
    
messaging :: SockAddr -> Handle -> Chan (String, Int) -> Int -> String -> IO()
messaging addr hdl chan cID mss = do
    let broadcast msg clientID = writeChan chan (msg,clientID)
    commLine <- dupChan chan
    writeChan commLine (mss,cID)
    reader <- forkIO $ fix $ \loop -> do
        (message,id) <- readChan commLine
        if (id /= cID)
            then hPutStrLn hdl (message)>> hFlush hdl --"Client" ++ show(id) ++ ":" ++
            else putStrLn "\n"
        loop
    fix $ \loop -> do
        mess <- getMessage hdl
        broadcast mess cID
        case (sq (extractInfo mess 0 0)) of
            "KILL_SERVICE" -> cls  hdl
            "HELO" -> infoSplit addr hdl
            "JOIN_CHATROOM" -> joinRoom mess
            "LEAVE_CHATROOM" -> putStrLn "leaving"
            "CHAT" -> putStrLn "message"
            _ -> putStrLn "keep listening">> loop 
    messaging addr hdl chan cID mss

joinRoom :: String -> IO()
joinRoom clientM = do
   let chatName = (sq (extractInfo clientM 0 1))
   putStrLn chatName
   let cIP = (sq (extractInfo clientM 1 1))
   let cPort = (sq (extractInfo clientM 2 1))
   let cName = (sq (extractInfo clientM 3 1))
   putStrLn (chatName ++ cIP ++ cPort ++ cName)



cls:: Handle -> IO ()
cls hdl = do
    hPutStr hdl "gluck"
    hFlush hdl
    hClose hdl
    exitSuccess


infoSplit:: SockAddr -> Handle -> IO()
infoSplit sa hdl = do
    let address = (show sa)
    let a = splitOn ":" address
    hPutStr hdl ("HELO BASE_TEXT\nIP:" ++ (sq (a !! 0)) ++ "\n" ++ "Port:" ++ (sq (a !! 1)) ++"\n" ++ "StudentID:12312629\n")--(show (a !! 1))
    hFlush hdl

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
        | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
        | otherwise      = s
sq s                         = s

   
threadPoolIO :: Int -> (a -> IO b) -> IO (Chan a, Chan b)
threadPoolIO nr mutator = do
    input <- newChan
    output <- newChan
    forM_ [1..nr] $
        \_ -> forkIO (forever $ do
            i <- readChan input
            o <- mutator i
            writeChan output o)
    return (input, output)

extractInfo:: String -> Int -> Int -> String
extractInfo joinReq i j= do
    a <- show((splitOn ": "(lines joinReq!!i))!!j)
    return  a
    
getMessage:: Handle ->  IO String
getMessage hdl =do
    mess <-hGetLine hdl
    if (mess=="")
        then getMessage hdl
        else return mess

--JOIN_CHATROOM: name1\nIP: 1325\nPORT: 34243\nC_NAME: Oisin