To test, run `nix-build`. This will build and test the package with all
supported GHC and dependency versions.

Packages in this repository:

- [stripe-concepts] is a minimal package that just defines a common set of types
  for working with the Stripe API.

- [stripe-signature] is for parsing and verifying the `Stripe-Signature` HTTP
  header that Stripe includes when it sends an event to your webhook. You should
  use this to verify the authenticity of the request to ensure that you are not
  acting upon forged events originating from some source other than Stripe.

- [stripe-scotty] provides support for writing a Stripe webhook server using
  [scotty].
  
- [stripe-wreq] helps you make requests to the Stripe API in conjunction with
  the [wreq] package. This is a minimal library that makes no attempt obscure
  either its underpinnings in Wreq or any of the details of the Stripe API.
  Responses are returned as [aeson] values.

  [aeson]:            https://hackage.haskell.org/package/aeson
  [scotty]:           https://hackage.haskell.org/package/scotty
  [stripe-concepts]:  https://hackage.haskell.org/package/stripe-concepts
  [stripe-scotty]:    https://hackage.haskell.org/package/stripe-scotty
  [stripe-signature]: https://hackage.haskell.org/package/stripe-signature
  [stripe-wreq]:      https://hackage.haskell.org/package/stripe-wreq
  [wreq]:             https://hackage.haskell.org/package/wreq

  