The formal specification of the Superfluid protocol is written in Haskell, namely in Haskell 2010 with GHC extensions
notably MultiParamTypeClasses, FunctionalDependencies, GADTs and TypeFamilies.

This package is managed using [cabal](https://www.haskell.org/cabal/) as the `superfluid-protocol-spec` haskell package,
which exports:

-   `core` - a library where Superfluid protocol core type classes and implemented agreement types are defined,
-   `simple` - a library where Superfluid core type classes are instantiated with simple data types for testing purpose,
-   `superfluid-protocol-validator` - an executable where a protocol simulation environment is provided, and test
    outputs of the production protocol implementations can be validated against the specification.

Development
===========

**Prerequisite**

-   Ubuntu `build-essential` (or equivalents in other OSes)
-   `node` (for `npx` command and TDD environment)
-   `GHC` >= 8.6.0
-   [Cabal-install](https://www.haskell.org/cabal/download.html) >= 3.6.0.0

**TDD Environment**

```bash
$ # TDD with test suite only
$ make dev
$ # TDD with both test suite and a demo run
$ TEST_TARGET="test-suite test-demo" make dev
```

Packages
========

## Core Library

The core library exposes the _Concepts_, _Agreements_ and _System_ modules.

### Concepts Module

These are the core Superfluid protocol concepts expressed in multi-parameter type classes.

#### Superfluid Types

**Timestamp**

`class Timestamp ts` type class describes ordered integral types (`Integral ts, Ord ts`) for the time component in a
_Superfluid system_.

**Liquidty**

`class Liquidity lq` type class describes ordered numeric types (`Ord lq, Num lq`) where a _Superfluid system_ can
attach different meaning to, such as demand deposit, safety deposit, app loan & debt etc.

**Address**

`class Address addr` type class describe types (`Eq addr`) which addresses the accounts in a _Superfluid system_.

**RealtimeBalance**

`class RealtimeBalance rtb lq` type class describes a vector of liquidity... TODO more explanations.

#### Core Concepts

**Agreement**

`class AgreementAccountData aad lq ts rtb` - Type class for agreement account data, which provides realtime balance to
the account.

`class AgreementContractData acd lq ts rtb` - Type class for agreement contract data, which binds together several
`AgreementAccountData` for agreement functions to manipulate with.

`data AnyAgreementAccountData lq ts rtb` - AnyType wrapper of any existing `AgreementAccountData`

### Agreements Modules

These are instances of the agreement concept:

-   **TransferableBalanceAgreement**: the payment modality as we are all familiar with, i.e. instant settlement between
    two parties.
-   **ConstantFlowAgreement**: a streaming payment modality allowing sender to stream money to receiver in constant flow
    rate.
-   **GeneralDistributionAgreement**: a pubsub-like payment modality allowing publisher to pay its subscribers in
    predefined proportions instantly or in streams.

### System Modules

These encapsulate Superfluid concepts and agreements as `SuperfluidToken` monad for building any _Superfluid system_.

## Simple Library

The _Simple Superfluid system_ where the Superfluid core type classes are instantiated with simple types mainly for testing purpose:

-   `Timestamp` is `Integer`.
-   `Liquidity` is `Wad`.
-   `Address` in `String`.
-   `SimpleTokenStateT` is a `SuperfluidToken` monad transformer for building your monad stack with.

## Validator

The executable provides three major modes:

- [WIP] Demo mode - Running some typical test scenario for showcasing the _Simple Superfluid system_.
- [TODO] Simulator - Running a simulator in CLI interacting with the _Simple Superfluid system_ running inside.
- [TODO] Validator - Take test output from a _Superfluid system_ implementation and validate it against the
specification.

## Test Suite

They are written in HUnit.

[TODO] WIP, quickchecks, etc...

FAQ
===
