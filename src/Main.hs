{-# LANGUAGE DataKinds #-}

module Main where

import AST.Module (Module)
import AST.Structure
import AST.V0_16
import qualified Data.Text as Text
import qualified ElmFormat.AST.PublicAST as PublicAST
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import ElmVersion
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Reporting.Annotation (Located)
import qualified Reporting.Result as Result

foreign import javascript "((f) => { formatFn = f })"
    setF :: Callback (JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
    x <- syncCallback1' (return . toJSString . Text.unpack . format . Text.pack . fromJSString)
    setF x
  where
    format input =
        case Render.render Elm_0_19 <$> parseModule Elm_0_19 ("", input) of
            Right output -> output
            Left err -> Text.pack "error"
    parseModule ::
        ElmVersion ->
        (FilePath, Text.Text) ->
        Either () (Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK))
    parseModule elmVersion (inputFile, inputText) =
        case Parse.parse elmVersion inputText of
            Result.Result _ (Result.Ok modu) ->
                Right modu
            Result.Result _ (Result.Err errs) ->
                Left ()
