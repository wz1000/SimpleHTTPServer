{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network
import System.Environment (getArgs)
import Control.Monad
import Data.Maybe
import System.Directory
import Control.Concurrent ( forkIO )
import System.IO
import Data.Char (isSpace)
import System.IO.Error
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format ( formatTime, defaultTimeLocale)
import qualified Data.ByteString.Char8 as B

data Request = GET FilePath B.ByteString [B.ByteString]
             | HEAD FilePath B.ByteString [B.ByteString]
             | NotImplemented

main :: IO ()
main =
  withSocketsDo $
  do args <- getArgs
     let port =
           fromMaybe (6667 :: Int)
                     (read <$> listToMaybe args :: Maybe Int)
     directory <-
       maybe getCurrentDirectory return (listToMaybe (drop 1 args))
     sock <-
       listenOn $ PortNumber (fromIntegral port)
     putStrLn $
       "Started listening on port " ++
       show port ++ " in directory " ++ show directory
     startServer sock directory

startServer :: Socket -> FilePath -> IO ()
startServer sock dir =
  forever $
  do (handle,hostname,_) <- accept sock
     hSetBuffering handle NoBuffering
     _ <- forkIO $ handleConnection handle dir
     putStrLn $ "Connection from " ++ show hostname
     return ()

handleConnection :: Handle -> FilePath -> IO ()
handleConnection handle path =
  do request <- parseRequest handle
     case request of
       GET resource' _ headers ->
         do let resource =
                  takeWhile (/= '#') $
                  if last resource' == '/'
                     then resource' ++ "index.html"
                     else resource'
            putHeaders (path ++ resource)
                       handle
            putFile (path ++ resource) handle
            B.hPutStrLn handle ""
            if "Connection: close\r" `elem` headers
               then hClose handle >> putStrLn "Closing connection"
               else handleConnection handle path
       HEAD resource _ headers ->
         do putHeaders (path ++ resource)
                       handle
            if "Connection: close\r" `elem` headers
               then hClose handle >> putStrLn "Closing connection"
               else handleConnection handle path
       NotImplemented ->
         B.hPutStrLn handle
                     (httpVersion `B.append` " 501 Not Implemented")
                      `catchIOError` (\e ->
                       if isEOFError e
                           then putStrLn "Host closed connection"
                           else ioError e)


getTime :: IO B.ByteString
getTime =
  do t <- getCurrentTime
     return . B.pack $
       formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" t

parseRequest:: Handle -> IO Request
parseRequest h =
  do initR <- getInitialRequest h
     headers <- getHeaders h
     case B.words initR of
       ["GET",resource,version] ->
         return $ GET (B.unpack resource) version headers
       ["HEAD",resource,version] ->
         return $ GET (B.unpack resource) version headers
       _ -> return NotImplemented

getInitialRequest :: Handle -> IO B.ByteString
getInitialRequest h =
  do s <- B.hGetLine h
     if B.all isSpace s
        then getInitialRequest h
        else return s

getHeaders :: Handle -> IO [B.ByteString]
getHeaders h =
  do s <- B.hGetLine h
     if B.all isSpace s
        then return []
        else liftM (s :) $ getHeaders h


putHeaders :: FilePath -> Handle -> IO ()
putHeaders path h =
  do B.hPutStr h httpVersion
     withFile path
              ReadMode
              (\fileH ->
                 do B.hPutStrLn h " 200 OK"
                    date <- getTime
                    B.hPutStrLn h $ "Date: " `B.append` date
                    cl <- hFileSize fileH
                    B.hPutStrLn h $
                      "Content-Length: " `B.append` (B.pack . show) cl) 
                      `catchIOError` (\e ->
                          if isDoesNotExistError e
                             then do B.hPutStrLn h " 404 Not Found"
                                     date <- getTime
                                     B.hPutStrLn h $ "Date: " `B.append` date
                                     B.hPutStrLn h "Content-Type: text/html"
                                     B.hPutStrLn h $
                                       "Content-Length: " `B.append`
                                       (B.pack . show) (B.length message404)
                             else ioError e)
     hPutStrLn h ""

putFile :: FilePath -> Handle -> IO ()
putFile path h =
  withFile path ReadMode (`copyTo` h) `catchIOError`
       (\e ->
          if isDoesNotExistError e
             then B.hPutStrLn h message404
             else ioError e)

copyTo :: Handle -> Handle -> IO ()
copyTo h1 h2 =
  do contents <- B.hGetContents h1
     B.hPutStr h2 contents

httpVersion :: B.ByteString
httpVersion = "HTTP/1.1"

message404 :: B.ByteString
message404 = "<!DOCTYPE html> <p>The requested URL was not found on this server. </p>"

