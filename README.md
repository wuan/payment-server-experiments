# payment-server-experiments

Playground to test upcoming features/changes for https://github.com/PrivateStorageio/PaymentServer and to learn Haskell on the way.

## Getting started

Just use https://haskellstack.org to build and run this project:

```shell
% stack build
```

```shell
% stack run
```

## Open topics

* [x] Add optional header to take signature information
* [x] Allow receving `application/json` as raw `ByteString`
* Add JSON parsing
* Add signature verification of request payload
* Add tests