When [Stripe] sends an event to your webhook, it includes an HTTP header named
`Stripe-Signature`. You should use this to verify the authenticity of the
request to ensure that you are not acting upon forged events originating from
some source other than Stripe.

  [Stripe]: https://stripe.com/
