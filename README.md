Running on the files (commited in project) `Imports.purs` and `Code.purs`
produces a subset of imports listed in `Imports.purs` so that it would resolve
all the names that it can resolve in `Code.purs`.
```
$ purescript-importer Imports.purs Code.purs
import Control.Bind ((>=>))
import Control.Monad.Aff (Aff())
import DOM.XHR.FormData as FormData
import Data.Foreign as Foreign
import Data.Foreign.Class as Foreign
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax as Ajax
import Network.HTTP.Affjax.Response as Ajax
import Network.HTTP.MimeType.Common as Mime
import Prelude (($), map, (<$>), (>>>), class Show)
```
