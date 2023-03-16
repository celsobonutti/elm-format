{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CommandLine.World.RealWorld (RealWorld, runRealWorld) where

-- \| This is the implementation of `World` that is used by the actual CLI.

import CommandLine.World
import Control.Concurrent
import qualified Control.Concurrent.PooledIO.Final as Pool
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Text.IO
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import System.IO

newtype RealWorld a
    = RealWorld (ReaderT (MVar ()) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runRealWorld :: RealWorld a -> IO a
runRealWorld m = do
    printLock <- newMVar ()
    runWithPrintLock printLock m

runWithPrintLock :: MVar () -> RealWorld a -> IO a
runWithPrintLock printLock (RealWorld m) =
    runReaderT m printLock

withPrintLock :: (a -> IO b) -> a -> RealWorld b
withPrintLock f a = do
    printLock <- RealWorld ask
    liftIO $ withMVar printLock (\() -> f a)

instance World RealWorld where
    readUtf8File path = liftIO (decodeUtf8 <$> BS.readFile path)
    writeUtf8File path content = liftIO (BS.writeFile path $ encodeUtf8 content)

    doesFileExist = liftIO . System.Directory.doesFileExist
    doesDirectoryExist = liftIO . System.Directory.doesDirectoryExist
    listDirectory = liftIO . System.Directory.listDirectory

    getProgName = liftIO $ fmap Text.pack System.Environment.getProgName

    getStdin = liftIO $ decodeUtf8 . Lazy.toStrict <$> Lazy.getContents
    getLine = liftIO Data.Text.IO.getLine
    putStr = withPrintLock Data.Text.IO.putStr
    putStrLn = withPrintLock Data.Text.IO.putStrLn
    writeStdout content = liftIO $ BS.putStr $ encodeUtf8 content
    flushStdout = liftIO $ System.IO.hFlush stdout
    putStrStderr = withPrintLock (Data.Text.IO.hPutStr stderr)
    putStrLnStderr = withPrintLock (Data.Text.IO.hPutStrLn stderr)

    exitFailure = liftIO System.Exit.exitFailure
    exitSuccess = liftIO System.Exit.exitSuccess

    mapMConcurrently f ms = do
        printLock <- RealWorld ask
        let threads = traverse (Pool.fork . runWithPrintLock printLock . f) ms
        liftIO $ Pool.run threads
