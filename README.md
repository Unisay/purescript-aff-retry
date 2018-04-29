# purescript-aff-retry

(Purescript port of Haskell's [retry](https://github.com/Soostone/retry) package)

[![Build Status](https://travis-ci.org/Unisay/purescript-aff-retry.svg?branch=master)](https://travis-ci.org/Unisay/purescript-aff-retry)

[API docs on Pursuit](https://pursuit.purescript.org/packages/purescript-aff-retry/)

Monadic action combinators that add delayed-retry functionality, potentially with exponential-backoff, to arbitrary actions.

The main purpose of this package is to make it easy to work reliably with `MonadAff` actions that often fail. Common examples are database queries and large file uploads.
