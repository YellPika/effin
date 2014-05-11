effin: Extensible Effects
=========================

This package implements extensible effects, and alternative to monad transformers.
The original paper can be found at http://okmij.org/ftp/Haskell/extensible/exteff.pdf.
The main differences between this library and the one described in the paper are that
this library does not use the `Typeable` type class, and has a simpler API for handling
effects.

For example, the following code implements a handler for exceptions:

    runException :: Effect (Exception e ': es) a -> Effect es (Either e a)
    runException =
        handle (\x -> return (Right x))
        $ eliminate (\(Exception e) -> return (Left e))
        $ defaultRelay

Compare this to the corresponding code in extensible-effects
(http://hackage.haskell.org/package/extensible-effects):

    runExc :: Typeable e => Eff (Exc e :> r) a -> Eff r (Either e a)
    runExc = loop . admin
      where
        loop (Val x) = return (Right x)
        loop (E u)   = handleRelay u loop (\(Exc e) -> return (Left e))

In particular:

* Effect implementors are not required to do any recursion.
* The functions for writing effect handlers can be easily composed.
