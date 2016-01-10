module Code
  ( Username(..)
  , Password(..)
  , runUserName
  , runPassword
  , LoginResponse(..)
  , login
  ) where

newtype Username = Username String
newtype Password = Password String

derive instance genericUsername :: Generic Username
derive instance genericPassword :: Generic Password

runUserName :: Username -> String
runUserName (Username a) = a

runPassword :: Password -> String
runPassword (Password a) = a

data LoginResponse
  = LoginSuccess
  | LoginFailure

derive instance genericLoginResponse :: Generic LoginResponse

instance showLoginResponse :: Show LoginResponse where show = gShow

instance respondableLoginResponse :: Ajax.Respondable LoginResponse where
  responseType = Tuple (Just Mime.applicationJSON) Ajax.JSONResponse
  fromResponse = Foreign.readString >=> Foreign.parseJSON
             >=> Foreign.readProp "message" >>> map fromMessage
    where
      fromMessage "Login Successful" = LoginSuccess
      fromMessage _                  = LoginFailure


login :: forall eff. Username -> Password -> Aff (ajax :: AJAX | eff) LoginResponse
login username password =
  _.response <$> Ajax.post "http://localhost:8080/backend/auth/page/hashdb/login" formData
  where
    formData = FormData.fromMap
             $ StrMap.insert    "password" (runPassword password)
             $ StrMap.singleton "username" (runUserName username)
