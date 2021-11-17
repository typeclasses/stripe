{-# OPTIONS_GHC -Wall #-}

{- | For a typical webhook server implemented with Scotty, probably the only
thing you need from this module is one of these two actions:

  - 'requireSig' - If your live mode and test mode webhooks use separate
    URLs.

  - 'requireSig_eitherMode' - If your live mode and test mode webhooks use
    the same URL. -}

module Stripe.Scotty
  ( requireSig, requireSig_eitherMode
  , hasValidSig, getSig, getSigText
  ) where

-- aeson
import qualified Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap

-- base
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (MonadIO)

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- http-types
import qualified Network.HTTP.Types.Status

-- scotty
import qualified Web.Scotty.Trans as Scotty

-- stripe-concepts
import qualified Stripe.Concepts as Stripe

-- stripe-signature
import qualified Stripe.Signature as Stripe

-- text
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

{- | Terminates request processing if the request does not contain a valid
Stripe signature header.

This action returns the request body as a strict byte string; it has to fully
evaluate the request body to do the signature check, so we might as well return
this information to you for subsequent use. -}

requireSig
    :: (MonadIO m, Scotty.ScottyError e) =>
    Stripe.WebhookSecretKey
    -> Scotty.ActionT e m Data.ByteString.ByteString

requireSig secret =
  do
    body <- getBody
    okay <- hasValidSig secret body

    when (not okay) invalidSigAction

    return body

{- | Terminates request processing if the request does not contain a valid
Stripe signature header.

This action returns the mode and JSON request body; it has to do this much
parsing to determine the mode so that we can know which secret key to use in the
verification, so we might as well return this information to you for subsequent
use. -}

requireSig_eitherMode
    :: (MonadIO m, Scotty.ScottyError e) =>
    Stripe.BothModes (Maybe Stripe.WebhookSecretKey)
    -> Scotty.ActionT e m (Stripe.Mode, Data.Aeson.Value)

requireSig_eitherMode secrets =
  do
    body   <- getBody
    value  <- parseBody body
    mode   <- getMode value
    secret <- chooseSecret mode secrets
    okay   <- hasValidSig secret body

    when (not okay) invalidSigAction

    return (mode, value)

getBody :: (MonadIO m, Scotty.ScottyError e) =>
    Scotty.ActionT e m Data.ByteString.ByteString
getBody = Data.ByteString.Lazy.toStrict <$> Scotty.body

invalidSigAction :: (Monad m, Scotty.ScottyError e) => Scotty.ActionT e m a
invalidSigAction =
  do
    Scotty.status Network.HTTP.Types.Status.forbidden403
    Scotty.text (Data.Text.Lazy.pack "Invalid Stripe signature")
    Scotty.finish

missingKeyAction :: (Monad m, Scotty.ScottyError e) =>
    Stripe.Mode -> Scotty.ActionT e m a
missingKeyAction mode =
  do
    Scotty.status Network.HTTP.Types.Status.internalServerError500
    Scotty.text message
    Scotty.finish
  where
    message =
        Data.Text.Lazy.Builder.toLazyText $
            foldMap Data.Text.Lazy.Builder.fromString
                [ "Configuration error: No webhook secret for "
                , case mode of
                    Stripe.LiveMode -> "live"
                    Stripe.TestMode -> "test"
                , " mode."
                ]

chooseSecret
    :: (Monad m, Scotty.ScottyError e) =>
    Stripe.Mode
    -> Stripe.BothModes (Maybe Stripe.WebhookSecretKey)
    -> Scotty.ActionT e m Stripe.WebhookSecretKey
chooseSecret mode secrets =
    case Stripe.applyMode mode secrets of
        Just x -> return x
        Nothing -> missingKeyAction mode

parseBody
    :: (Monad m, Scotty.ScottyError e) =>
    Data.ByteString.ByteString
    -> Scotty.ActionT e m Data.Aeson.Value
parseBody bs =
    case Data.Aeson.eitherDecode (Data.ByteString.Lazy.fromStrict bs) of
        Left x ->
          do
            Scotty.status Network.HTTP.Types.Status.badRequest400
            Scotty.text (Data.Text.Lazy.pack x)
            Scotty.finish
        Right x ->
            return x

getMode :: (Monad m, Scotty.ScottyError e) =>
    Data.Aeson.Value -> Scotty.ActionT e m Stripe.Mode
getMode val =
    case (aesonAttr "livemode" >=> aesonBool) val of

      Nothing ->
        do
          Scotty.status Network.HTTP.Types.Status.badRequest400
          Scotty.text (Data.Text.Lazy.pack
              "Webhook attribute \"livemode\" is missing.")
          Scotty.finish

      Just livemode ->
          return (Stripe.isLiveMode' livemode)

{- | Determines whether the request contains a valid Stripe signature header. -}

hasValidSig
    :: (Monad m, Scotty.ScottyError e) =>
    Stripe.WebhookSecretKey
    -> Data.ByteString.ByteString
    -> Scotty.ActionT e m Bool
hasValidSig secret body =
  do
    sigMaybe <- getSig
    return $
        case sigMaybe of
            Nothing -> False
            Just sig -> Stripe.isSigValid sig secret body

getSigText :: (Monad m, Scotty.ScottyError e) =>
    Scotty.ActionT e m (Maybe Data.Text.Text)
getSigText =
  do
    x <- Scotty.header (Data.Text.Lazy.pack "Stripe-Signature")
    return (Data.Text.Lazy.toStrict <$> x)

getSig :: (Monad m, Scotty.ScottyError e) =>
    Scotty.ActionT e m (Maybe Stripe.Sig)
getSig =
  do
    x <- getSigText
    return (x >>= Stripe.parseSig)

------------------------------------------------------------

-- Internal Aeson decoding functions

aesonAttr :: String -> Data.Aeson.Value -> Maybe Data.Aeson.Value
aesonAttr x = aesonObject >=> Data.Aeson.KeyMap.lookup (Data.Aeson.Key.fromString x)

aesonObject :: Data.Aeson.Value -> Maybe Data.Aeson.Object
aesonObject (Data.Aeson.Object x) = Just x
aesonObject _ = Nothing

aesonBool :: Data.Aeson.Value -> Maybe Bool
aesonBool (Data.Aeson.Bool x) = Just x
aesonBool _ = Nothing
