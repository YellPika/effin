effin: Extensible Effects
=========================

This package implements extensible effects, and alternative to monad transformers.
The original paper can be found at http://okmij.org/ftp/Haskell/extensible/exteff.pdf.
The main differences between this library and the one described in the paper are
that this library does not use the Typeable type class, does not require that
effects implement the Functor type class, and has a simpler API for handling
effects.

For example, the following code implements a handler for exceptions:

    newtype Exception e = Throw e

    runException :: Effect (Exception e :+ es) a -> Effect es (Either e a)
    runException = eliminate
        (\x -> return (Right x))
        (\(Throw e) -> return (Left e))

Compare this to the corresponding code in extensible-effects
(http://hackage.haskell.org/package/extensible-effects):

    runExc :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
    runExc = loop . admin
      where
        loop (Val x) = return (Right x)
        loop (E u)   = handleRelay u loop (\(Exc e) -> return (Left e))

In particular, effect implementors are not required to do any recursion, thereby
making effect handlers more composeable.

Future Work
===========

* Support for GHC 7.6. This will require ~~very~~ extremely heavy abuse of `OverlappingInstances`, but it can be done.
* ~~Encapsulation of effects.~~ Done.
* Improved exceptions. Currently:
    * ~~The `finally` function only works with an exception of a single type.~~ Fixed.
    * IO/Async exceptions aren't yet supported.
* Support for effects that require linearity. In particular, any `Region` effect would be
  unsafe because there's no way to ensure that effects like `Thread` aren't used simultaneously.
  Perhaps this can be achieved with something akin to how the IO monad ensures linearity.
