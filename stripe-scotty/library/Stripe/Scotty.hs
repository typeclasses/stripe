{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

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

-- base
import Control.Monad (when, (>=>))

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- http-types
import qualified Network.HTTP.Types.Status

-- scotty
import qualified Web.Scotty

-- stripe-concepts
import qualified Stripe.Concepts as Stripe

-- stripe-signature
import qualified Stripe.Signature as Stripe

-- text
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

-- unordered-containers
import qualified Data.HashMap.Strict

{- | Terminates request processing if the request does not contain a valid
Stripe signature header.

This action returns the request body as a strict byte string; it has to fully
evaluate the request body to do the signature check, so we might as well return
this information to you for subsequent use. -}

requireSig
    :: Stripe.WebhookSecretKey
    -> Web.Scotty.ActionM Data.ByteString.ByteString

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
    :: Stripe.BothModes (Maybe Stripe.WebhookSecretKey)
    -> Web.Scotty.ActionM (Stripe.Mode, Data.Aeson.Value)

requireSig_eitherMode secrets =
  do
    body   <- getBody
    value  <- parseBody body
    mode   <- getMode value
    secret <- chooseSecret mode secrets
    okay   <- hasValidSig secret body

    when (not okay) invalidSigAction

    return (mode, value)

getBody :: Web.Scotty.ActionM Data.ByteString.ByteString
getBody = Data.ByteString.Lazy.toStrict <$> Web.Scotty.body

invalidSigAction :: Web.Scotty.ActionM a
invalidSigAction =
  do
    Web.Scotty.status Network.HTTP.Types.Status.forbidden403
    Web.Scotty.text (Data.Text.Lazy.pack "Invalid Stripe signature")
    Web.Scotty.finish

missingKeyAction :: Stripe.Mode -> Web.Scotty.ActionM a
missingKeyAction mode =
  do
    Web.Scotty.status Network.HTTP.Types.Status.internalServerError500
    Web.Scotty.text message
    Web.Scotty.finish
  where
    message = Data.Text.Lazy.Builder.toLazyText $
        t "Configuration error: No webhook secret for " <>
        t (modeString mode) <> t " mode."

    t = Data.Text.Lazy.Builder.fromString

    modeString =
        \case
            Stripe.LiveMode -> "live"
            Stripe.TestMode -> "test"

chooseSecret
    :: Stripe.Mode
    -> Stripe.BothModes (Maybe Stripe.WebhookSecretKey)
    -> Web.Scotty.ActionM Stripe.WebhookSecretKey

chooseSecret mode secrets =
    case Stripe.applyMode mode secrets of
        Just x -> return x
        Nothing -> missingKeyAction mode

parseBody
    :: Data.ByteString.ByteString
    -> Web.Scotty.ActionM Data.Aeson.Value

parseBody bs =
    case Data.Aeson.eitherDecode (Data.ByteString.Lazy.fromStrict bs) of
        Left x ->
          do
            Web.Scotty.status Network.HTTP.Types.Status.badRequest400
            Web.Scotty.text (Data.Text.Lazy.pack x)
            Web.Scotty.finish
        Right x ->
            return x

getMode :: Data.Aeson.Value -> Web.Scotty.ActionM Stripe.Mode
getMode val =
    case (aesonAttr "livemode" >=> aesonBool) val of

      Nothing ->
        do
          Web.Scotty.status Network.HTTP.Types.Status.badRequest400
          Web.Scotty.text (Data.Text.Lazy.pack
              "Webhook attribute \"livemode\" is missing.")
          Web.Scotty.finish

      Just livemode ->
          return (Stripe.isLiveMode' livemode)

{- | Determines whether the request contains a valid Stripe signature header. -}

hasValidSig
    :: Stripe.WebhookSecretKey
    -> Data.ByteString.ByteString
    -> Web.Scotty.ActionM Bool

hasValidSig secret body =
  do
    sigMaybe <- getSig
    return $
        case sigMaybe of
            Nothing -> False
            Just sig -> Stripe.isSigValid sig secret body

getSigText :: Web.Scotty.ActionM (Maybe Data.Text.Text)
getSigText =
  do
    x <- Web.Scotty.header (Data.Text.Lazy.pack "Stripe-Signature")
    return (Data.Text.Lazy.toStrict <$> x)

getSig :: Web.Scotty.ActionM (Maybe Stripe.Sig)
getSig =
  do
    x <- getSigText
    return (x >>= Stripe.parseSig)

------------------------------------------------------------

-- Internal Aeson decoding functions

aesonAttr :: String -> Data.Aeson.Value -> Maybe Data.Aeson.Value
aesonAttr x =
    aesonObject >=>
    Data.HashMap.Strict.lookup (Data.Text.pack x)

aesonObject :: Data.Aeson.Value -> Maybe Data.Aeson.Object
aesonObject =
    \case
        Data.Aeson.Object x -> Just x
        _ -> Nothing

aesonBool :: Data.Aeson.Value -> Maybe Bool
aesonBool =
    \case
        Data.Aeson.Bool x -> Just x
        _ -> Nothing
