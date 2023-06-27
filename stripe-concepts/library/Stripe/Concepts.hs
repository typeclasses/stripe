module Stripe.Concepts
  ( -- * Modes
    Mode (..),
    BothModes (..),
    applyMode,

    -- ** Conversion with Bool
    isLiveMode,
    isTestMode,
    isLiveMode',
    isTestMode',

    -- * Keys
    -- $keys
    PublishableApiKey (..),

    -- ** Secret API key
    ApiSecretKey (..),
    textToApiSecretKey,

    -- ** Webhook secret
    WebhookSecretKey (..),
    textToWebhookSecretKey,

    -- * Identifiers
    TokenId (..),
    CustomerId (..),
    ProductId (..),
    PlanId (..),
    SubscriptionId (..),
    InvoiceId (..),
    CouponId (..),

    -- * API Versioning
    ApiVersion (..),
    RequestApiVersion (..),
  )
where

import Data.ByteString qualified
import Data.Data (Data)
import Data.Text qualified
import Data.Text.Encoding qualified
import GHC.Generics (Generic)

------------------------------------------------------------

-- | "To make the API as explorable as possible, accounts have test mode and
-- live mode API keys. There is no 'switch' for changing between modes, just use
-- the appropriate key to perform a live or test transaction. Requests made with
-- test mode credentials never hit the banking networks and incur no cost." -
-- <https://stripe.com/docs/api Stripe>
--
-- This library provides functions to convert back and forth between 'Mode' and
-- 'Bool':
--
-- - 'isLiveMode' (and its inverse, 'isLiveMode'')
-- - 'isTestMode' (and its inverse, 'isTestMode'')
data Mode = LiveMode | TestMode
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)

-- | LiveMode → True; TestMode → False
isLiveMode :: Mode -> Bool
isLiveMode LiveMode = True
isLiveMode TestMode = False

-- | True → LiveMode; False → TestMode
isLiveMode' :: Bool -> Mode
isLiveMode' True = LiveMode
isLiveMode' False = TestMode

-- | LiveMode → False; TestMode → True
isTestMode :: Mode -> Bool
isTestMode LiveMode = False
isTestMode TestMode = True

-- | True → TestMode; False → LiveMode
isTestMode' :: Bool -> Mode
isTestMode' True = TestMode
isTestMode' False = LiveMode

-- | A pair of values of the same type, one for live mode and one for test mode.
--
-- For example, you may wish to use a value of type @'BothModes'
-- 'PublishableApiKey'@ to represent your publishable API keys for both live mode
-- and test mode.
data BothModes a = BothModes {liveMode :: a, testMode :: a}
  deriving stock (Eq, Show, Data, Generic, Functor)

applyMode :: Mode -> BothModes a -> a
applyMode LiveMode = liveMode
applyMode TestMode = testMode

------------------------------------------------------------

-- $keys
--
-- Each Stripe account has a pair of API keys involved in making requests to the
-- Stripe API:
--
--   * The publishable key ('PublishableApiKey')
--   * The secret key ('ApiSecretKey')
--
-- Each webhook endpoint you set up has a "signing secret" ('WebhookSecretKey')
-- that you use to verify the authenticity of the webhook events you receive *from*
-- Stripe.

-- | API secret keys are used to make requests to Stripe.
--
-- "Authenticate your account when using the API by including your secret API key
-- in the request. You can manage your API keys in the Dashboard. Your API keys
-- carry many privileges, so be sure to keep them secret!" -
-- <https://stripe.com/docs/api#authentication Stripe>
--
-- The key is represented here as a 'Data.ByteString.ByteString', but you are
-- likely have the data as a 'Data.Text.Text' value. You can use
-- 'textToApiSecretKey' to do this conversion.
newtype ApiSecretKey = ApiSecretKey Data.ByteString.ByteString
  deriving stock (Eq, Ord)

-- | Publishable API keys are used in client-side code.
--
-- "Publishable API keys are meant solely to identify your account with Stripe,
-- they aren’t secret. In other words, they can safely be published in places like
-- your Stripe.js JavaScript code, or in an Android or iPhone app. Publishable keys
-- only have the power to create tokens." - <https://stripe.com/docs/keys Stripe>
newtype PublishableApiKey = PublishableApiKey Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | Webhook secrets are used to verify the authenticity of webhook events that
-- you receive from Stripe.
--
-- "Stripe can optionally sign the webhook events it sends to your endpoints. We do
-- so by including a signature in each event’s Stripe-Signature header. This allows
-- you to validate that the events were sent by Stripe, not by a third party. [...]
-- Before you can verify signatures, you need to retrieve your endpoint’s secret
-- from your Dashboard’s Webhooks settings. -
-- <https://stripe.com/docs/webhooks/signatures Stripe>
--
-- The key is represented here as a 'Data.ByteString.ByteString', but you are
-- likely have the data as a 'Data.Text.Text' value. You can use
-- 'textToWebhookSecretKey' to do this conversion.
newtype WebhookSecretKey = WebhookSecretKey Data.ByteString.ByteString
  deriving stock (Eq, Ord)

-- | Convert a 'Data.Text.Text' representation of a Stripe API key (that looks
-- something like @"sk_test_BQokikJOvBiI2HlWgH4olfQ2"@) to an 'ApiSecretKey'.
textToApiSecretKey :: Data.Text.Text -> ApiSecretKey
textToApiSecretKey = ApiSecretKey . Data.Text.Encoding.encodeUtf8

-- | Convert a 'Data.Text.Text' representation of a Stripe webhook secret (that
-- looks something like @"whsec_ojm5cmJMGMTw3w7ngjI7mgkRsFGLRtCt"@) to a
-- 'WebhookSecretKey'.
textToWebhookSecretKey :: Data.Text.Text -> WebhookSecretKey
textToWebhookSecretKey = WebhookSecretKey . Data.Text.Encoding.encodeUtf8

------------------------------------------------------------

-- | Identifier of a Stripe "token", which represents a payment source that was
-- submitted by a user to Stripe.
--
-- "This ensures that no sensitive card data touches your server, and allows your
-- integration to operate in a PCI-compliant way." -
-- <https://stripe.com/docs/api/tokens Stripe>
newtype TokenId = TokenId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | A customer identifier assigned by Stripe.
--
-- "Customer objects allow you to perform recurring charges, and to track multiple
-- charges, that are associated with the same customer." -
-- <https://stripe.com/docs/api/customers Stripe>
newtype CustomerId = CustomerId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | The ID of a Stripe product.
--
-- "Product objects describe items that your customers can subscribe to with a
-- Subscription. An associated Plan determines the product pricing." -
-- <https://stripe.com/docs/api/service_products Stripe>
newtype ProductId = ProductId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | The ID of a Stripe subscription plan.
--
-- "Plans define the base price, currency, and billing cycle for subscriptions. For
-- example, you might have a $5/month plan that provides limited access to your
-- products, and a $15/month plan that allows full access." -
-- <https://stripe.com/docs/api/plans Stripe>
newtype PlanId = PlanId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | Identifier for a customer's subscription to a product.
--
-- "Subscriptions allow you to charge a customer on a recurring basis. A
-- subscription ties a customer to a particular plan you've created." -
-- <https://stripe.com/docs/api/subscriptions Stripe>
newtype SubscriptionId = SubscriptionId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | The ID of a Stripe invoice.
--
-- "Invoices are statements of amounts owed by a customer, and are either generated
-- one-off, or generated periodically from a subscription." -
-- <https://stripe.com/docs/api/invoices Stripe>
newtype InvoiceId = InvoiceId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- | The ID of a Stripe coupon.
--
-- "A coupon contains information about a percent-off or amount-off discount you
-- might want to apply to a customer. Coupons may be applied to invoices or
-- orders." -
-- <https://stripe.com/docs/api/coupons Stripe>
newtype CouponId = CouponId Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

------------------------------------------------------------

-- | When Stripe makes a backwards-incompatible change to the API, they release
-- a new API version. The versions are named by the date of their release (e.g.
-- "2019-09-09").
newtype ApiVersion = ApiVersion Data.Text.Text
  deriving stock (Eq, Ord, Show, Data, Generic)

-- |  Your account API settings specify:
--
--  - Which API version is used by default for requests;
--  - Which API version is used for webhook events.
--
-- However, you can override the API version for specific requests. "To set the API
-- version on a specific request, send a @Stripe-Version@ header." -
-- <https://stripe.com/docs/api/versioning Stripe>
data RequestApiVersion
  = -- | Use the default API version specified by your account settings.
    DefaultApiVersion
  | -- | Use a specific API version for this request. (Please note however
    --           that any webhook events generated as a result of this request will
    --           still use your account's default API version.)
    OverrideApiVersion ApiVersion
  deriving stock (Eq, Ord, Show, Data, Generic)
