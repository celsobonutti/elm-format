module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified ElmFormat.Cli as Cli
import Foreign hiding (void)
import Foreign.C.Types
import qualified System.Environment

main :: IO ()
main =
    mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

formatRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
formatRaw inputPtr inputLen outputPtrPtr = do
    input <-
        decodeUtf8 <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
    case encodeUtf8 <$> Cli.formatSimple input of
        Left err -> pure (-1)
        Right outputBytes ->
            BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
                outputPtr <- mallocBytes len
                poke outputPtrPtr outputPtr
                copyBytes outputPtr buf len
                pure len
