module Data.Either.Extra (collectErrors) where

import Data.List (foldl')

collectErrors :: [Either l r] -> Either [l] [r]
collectErrors list =
    let
        step acc next =
            case (next, acc) of
                (Left l, Right _) ->
                    Left [l]
                (Left l, Left ls) ->
                    Left (l : ls)
                (Right r, Right rs) ->
                    Right (r : rs)
                (Right _, Left ls) ->
                    Left ls
     in
        foldl' step (Right []) list
