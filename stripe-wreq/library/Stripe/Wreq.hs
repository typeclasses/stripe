{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

{- | Typical use of this module:

1. Run 'get', 'post', or 'delete' to get a 'WreqResponse'.
2. Use 'wreqResponse' to convert the Wreq response to a 'Response'.
3. Use 'responseValue' to obtain the response payload as an Aeson
   'Data.Aeson.Value' (or an 'Error' if the request was not successful). -}

module Stripe.Wreq
  (
  -- * Request
  -- ** GET
    get, get', Get (..)
  -- ** POST
  , post, post', Post (..)
  -- ** DELETE
  , delete, delete', Delete (..)

  -- * Response
  , WreqResponse, Response (..)
  , wreqResponse, responseValue, responseValueError

  -- * Error
  , Error (..), UserMessage (..), LogMessage (..), userError, logError

  -- * Status code
  , StatusCode (..)
  -- ** Predicates
  -- $predicates
  , isSuccess, isError, isClientError, isServerError
  -- ** Client error codes
  -- $constants
  , badRequest400, unauthorized401, requestFailed402
  , notFound404, conflict409, tooManyRequests429

  -- * Re-exports from Wreq
  , FormParam (..), Session, Network.Wreq.Session.newAPISession

  ) where

-- aeson
import qualified Data.Aeson

-- base
import qualified Control.Exception
import           Control.Monad     ((>=>))
import qualified Data.Bifunctor
import qualified Data.Semigroup
import           Data.String       (fromString)
import           Prelude           hiding (userError)

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- lens
import Control.Lens ((&), (.~), (?~), (^.), (<>~))

-- stripe-concepts
import Stripe.Concepts (ApiSecretKey (..), RequestApiVersion (..), ApiVersion (..))

-- text
import           Data.Text (Text)
import qualified Data.Text

-- unordered-containers
import qualified Data.HashMap.Strict

-- wreq
import           Network.Wreq         (FormParam (..))
import qualified Network.Wreq
import           Network.Wreq.Session (Session)
import qualified Network.Wreq.Session

------------------------------------------------------------

{- | An HTTP status code returned by Stripe.

"Stripe uses conventional HTTP response codes to indicate the success or failure
of an API request." - <https://stripe.com/docs/api#errors Stripe> -}

newtype StatusCode = StatusCode Int deriving Eq

{- $predicates

Some basic functions for interpreting status codes. -}

{- | "Codes in the 2xx range indicate success." -
<https://stripe.com/docs/api#errors Stripe> -}

isSuccess :: StatusCode -> Bool
isSuccess (StatusCode x) = x >= 200 && x < 300

{- | @isError x@ is equivalent to @'isClientError' x || 'isServerError' x@. -}

isError :: StatusCode -> Bool
isError (StatusCode x) = x >= 400 && x < 600

{- | "Codes in the 4xx range indicate an error that failed given the information
provided (e.g., a required parameter was omitted, a charge failed, etc.)." -
<https://stripe.com/docs/api#errors Stripe> -}

isClientError :: StatusCode -> Bool
isClientError (StatusCode x) = x >= 400 && x < 500

{- | "Codes in the 5xx range indicate an error with Stripe's servers." -
<https://stripe.com/docs/api#errors Stripe> -}

isServerError :: StatusCode -> Bool
isServerError (StatusCode x) = x >= 500 && x < 600

{- $constants

Constants for each of the error codes enumerated in the Stripe API
documentation, for your convenience. -}

{- | 400 - Bad Request

"The request was unacceptable, often due to missing a required parameter." -
<https://stripe.com/docs/api#errors Stripe> -}

badRequest400 :: StatusCode
badRequest400 = StatusCode 400

{- | 401 - Unauthorized

"No valid API key provided." - <https://stripe.com/docs/api#errors Stripe> -}

unauthorized401 :: StatusCode
unauthorized401 = StatusCode 401

{- | 402 - Request Failed

"The parameters were valid but the request failed." -
<https://stripe.com/docs/api#errors Stripe> -}

requestFailed402 :: StatusCode
requestFailed402 = StatusCode 402

{- | 404 - Not Found

"The requested resource doesn't exist." -
<https://stripe.com/docs/api#errors Stripe> -}

notFound404 :: StatusCode
notFound404 = StatusCode 404

{- | 409 - Conflict

"The request conflicts with another request (perhaps due to using the same
idempotent key)." - <https://stripe.com/docs/api#errors Stripe> -}

conflict409 :: StatusCode
conflict409 = StatusCode 409

{- | 429 - Too Many Requests

"Too many requests hit the API too quickly. We recommend an exponential backoff
of your requests." - <https://stripe.com/docs/api#errors Stripe> -}

tooManyRequests429 :: StatusCode
tooManyRequests429 = StatusCode 429

------------------------------------------------------------

data Get =
  Get
    { getPath     :: [Text]          -- ^ URL path components
    , getParams   :: [(Text, Text)]  -- ^ Query params
    }

data Post =
  Post
    { postPath     :: [Text]         -- ^ URL path components
    , postParams   :: [FormParam]    -- ^ Parameters to send in the request body
    }

data Delete =
  Delete
    { deletePath   :: [Text]         -- ^ URL path components
    , deleteParams :: [(Text, Text)] -- ^ Query params
    }

get :: Session -> ApiSecretKey -> Get -> IO WreqResponse
get session key x = get' session key DefaultApiVersion x

get' :: Session -> ApiSecretKey -> RequestApiVersion -> Get -> IO WreqResponse
get' session key v x = Network.Wreq.Session.getWith opts session url
  where
    url = makeUrl (getPath x)
    opts = wreqDefaults & Network.Wreq.auth ?~ auth key
                        & Network.Wreq.params .~ (getParams x)
                        & Network.Wreq.headers <>~ (requestApiVersionHeaders v)

post :: Session -> ApiSecretKey -> Post -> IO WreqResponse
post session key x = post' session key DefaultApiVersion x

post' :: Session -> ApiSecretKey -> RequestApiVersion -> Post -> IO WreqResponse
post' session key v x = Network.Wreq.Session.postWith opts session url params
  where
    url = makeUrl (postPath x)
    params = postParams x
    opts = wreqDefaults & Network.Wreq.auth ?~ auth key
                        & Network.Wreq.headers <>~ (requestApiVersionHeaders v)

delete :: Session -> ApiSecretKey -> Delete -> IO WreqResponse
delete session key x = delete' session key DefaultApiVersion x

delete' :: Session -> ApiSecretKey -> RequestApiVersion -> Delete -> IO WreqResponse
delete' session key v x = Network.Wreq.Session.deleteWith opts session url
  where
    url = makeUrl (deletePath x)
    opts = wreqDefaults & Network.Wreq.auth ?~ auth key
                        & Network.Wreq.params .~ (deleteParams x)
                        & Network.Wreq.headers <>~ (requestApiVersionHeaders v)

urlBase :: Text
urlBase = Data.Text.pack "https://api.stripe.com/v1"

makeUrl :: [Text] -> String
makeUrl =
    Data.Text.unpack
    . Data.Text.intercalate (Data.Text.pack "/")
    . (urlBase :)

wreqDefaults :: Network.Wreq.Options
wreqDefaults = Network.Wreq.defaults & noCheckResponse

-- When using the default API version, no header is required.
requestApiVersionHeaders DefaultApiVersion = []

-- Overriding with a specific API version requires one header field.
requestApiVersionHeaders (OverrideApiVersion v) = [apiVersionHeader v]

-- The header field for specifying the API version.
apiVersionHeader (ApiVersion v) = (name, value)
  where
    name = fromString "Stripe-Version"
    value = fromString (Data.Text.unpack v)

{- | Set a "response checker" that overrides Wreq's default one which causes
exceptions to be thrown for non-2xx HTTP status codes -}

noCheckResponse :: Network.Wreq.Options -> Network.Wreq.Options
noCheckResponse = Network.Wreq.checkResponse ?~ (\_ _ -> return ())

{- | Represent a Stripe API key as a Wreq 'Network.Wreq.Auth' value.

"Authentication to the API is performed via HTTP Basic Auth. Provide your API
key as the basic auth username value. You do not need to provide a password." -
<https://stripe.com/docs/api#authentication Stripe> -}

auth :: ApiSecretKey -> Network.Wreq.Auth
auth (ApiSecretKey key) = Network.Wreq.basicAuth key Data.ByteString.empty

------------------------------------------------------------

{- | An error message suitable for being shown to a user. -}

newtype UserMessage = UserMessage Text deriving (Eq, Show)

{- | An error message that should go into an error log, /not/ shown to a user.
-}

newtype LogMessage = LogMessage Text deriving (Eq, Show)

data Error =
  Error
    { userMessages :: [UserMessage]
    , logMessages  :: [LogMessage]
    }
    deriving (Eq, Show)

instance Data.Semigroup.Semigroup Error
  where
    Error x y <> Error x' y' =
        Error
            ((Data.Semigroup.<>) x x')
            ((Data.Semigroup.<>) y y')

instance Monoid Error
  where
    mappend = (Data.Semigroup.<>)
    mempty = Error mempty mempty

instance Control.Exception.Exception Error

userError
    :: Text -- ^ An error message intended to be shown to a user.
    -> Error

userError x = Error { userMessages = [UserMessage x], logMessages = [] }

logError
    :: Text -- ^ An error message intended to go into a log file,
            --   /not/ to be shown to a user.
    -> Error

logError x = Error { userMessages = [], logMessages = [LogMessage x] }

------------------------------------------------------------

type WreqResponse = Network.Wreq.Response Data.ByteString.Lazy.ByteString

data Response =
  Response
    { responseBody :: Either Text Data.Aeson.Value
        -- ^ Every Stripe response should have a JSON body; but if not, this
        --   will be a 'Left' value with an error message from the JSON parser.
    , responseCode :: StatusCode
        -- ^ The status code of the HTTP response.
    }

{- | Convert a 'WreqResponse' into a 'Response' by parsing the JSON response
body (the Stripe API always returns JSON) and getting the HTTP status code. -}

wreqResponse :: WreqResponse -> Response
wreqResponse r =
  Response
    { responseBody =
        r ^. Network.Wreq.responseBody
           & Data.Aeson.eitherDecode
           & Data.Bifunctor.first Data.Text.pack
    , responseCode =
        r ^. Network.Wreq.responseStatus
           . Network.Wreq.statusCode
           & StatusCode
    }

{- | Interpret a response, returning 'Right' with the parsed JSON payload if
everything is okay, or 'Left' with an error if the response contains any
indication that something went wrong. -}

responseValue :: Response -> Either Error Data.Aeson.Value
responseValue r =
    case (responseBody r) of
        Left e    -> Left (logError e)
        Right val ->
            case isSuccess (responseCode r) of
                True  -> Right val
                False -> Left (responseValueError val)

{- | If the response object looks like this:

> {
>   "error": {
>       "type": "card_error",
>       "message": "..."
>     }
>   }
> }

then we use the value of the @message@ field as a 'UserMessage'. Otherwise it is
a 'LogMessage'.

"@message@: A human-readable message providing more details about the error. For
card errors, these messages can be shown to your users. [...] Card errors are
the most common type of error you should expect to handle. They result when the
user enters a card that can't be charged for some reason." -
<https://stripe.com/docs/api#errors Stripe> -}

responseValueError :: Data.Aeson.Value -> Error
responseValueError val
    | isCardError  =  foldMap userError (msg val)
    | otherwise    =  foldMap logError  (msg val)
  where

    isCardError = typ val == Just (Data.Text.pack "card_error")

    msg = aesonAttr "error" >=> aesonAttr "message" >=> aesonText
    typ = aesonAttr "error" >=> aesonAttr "type"    >=> aesonText

------------------------------------------------------------

-- Internal Aeson decoding functions

aesonAttr :: String -> Data.Aeson.Value -> Maybe Data.Aeson.Value
aesonAttr x = aesonObject >=> Data.HashMap.Strict.lookup (Data.Text.pack x)

aesonObject :: Data.Aeson.Value -> Maybe Data.Aeson.Object
aesonObject (Data.Aeson.Object x) = Just x
aesonObject _ = Nothing

aesonText :: Data.Aeson.Value -> Maybe Text
aesonText (Data.Aeson.String x) = Just x
aesonText _ = Nothing
