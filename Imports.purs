module Imports where

import Prelude (($), map, (<$>), (<#>), (<<<), (>>>), class Show, show, unit, Unit(), bind)

import Control.Bind ( (>>=), (=<<)
                    , (>=>), (<=<)
                    )

import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Data.Maybe (Maybe(Just, Nothing))
import Data.Generic (class Generic, gShow, gEq, gOrd)
import Data.Tuple (Tuple(Tuple))
import Data.StrMap (StrMap())
import Data.StrMap as StrMap
import Data.Foreign as Foreign
import Data.Foreign.Class as Foreign

import Network.HTTP.Affjax          as Ajax
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax.Response as Ajax
import Network.HTTP.MimeType.Common as Mime

import DOM.XHR.FormData (FormData())
import DOM.XHR.FormData             as FormData
