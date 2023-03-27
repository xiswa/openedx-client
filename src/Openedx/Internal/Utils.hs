module Openedx.Internal.Utils (
    camelToSnake
  , removePrefix
  ) where

import Data.Char (isUpper, toLower)

camelToSnake :: String -> String
camelToSnake = foldr op [] 
  where
    op c z 
      | isUpper c = '_':toLower c:z
      | otherwise = c:z

removePrefix :: String -> String -> String
removePrefix pref = drop (length pref)
