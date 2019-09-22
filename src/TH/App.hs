{-# LANGUAGE TemplateHaskell #-}
module TH.App (a, app) where

import Data.Foldable
import Data.List
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

a :: QuasiQuoter
a = app

app :: QuasiQuoter
app = emptyQuoter { quoteExp = appQuoter }

nameE :: String -> Q Exp
nameE = varE . mkName

names :: String -> [Q Exp]
names = fmap nameE . words

appQuoter :: String -> Q Exp
appQuoter s =
  case names s of
    []       -> error "Empty idioms are not supported."
    [single] -> single
    (f : ws) -> foldl app' (appE (nameE "pure") f) ws
  where
    app' l r = appE (nameE "<*>") l `appE` r

emptyQuoter :: QuasiQuoter
emptyQuoter = QuasiQuoter
  {
    quoteExp  = error "Expressions are not supported by this quoter."
  , quotePat  = error "Patterns are not supported by this quoter"
  , quoteType = error "Types are not supported by this quoter"
  , quoteDec  = error "Declarations are not supported by this quoter"
  }
