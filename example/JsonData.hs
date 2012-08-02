{-# LANGUAGE TemplateHaskell #-}

module JsonData where

import Control.Isomorphism.Partial.TH (defineIsomorphisms)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)


$(defineIsomorphisms ''JValue)
$(defineIsomorphisms ''Bool)
