{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Example usage:

> import Control.Break
> import Control.Monad.State
> import Prelude hiding (break)
>
> example :: State Int ()
> example = loop (do
>     n <- lift get                -- Inside a `loop`, wrap commands in `lift`
>     if n < 10
>         then lift (put (n + 1))  -- You keep looping by default
>         else break () )          -- Use `break` to exit from the `loop`

The `loop` command runs the given command repeatedly until the command breaks
from the `loop` using `break`:

>>> execState example 0
10

For some effects (like `Control.Monad.Trans.State`), you can omit `lift`:

> example :: State Int ()
> example = loop (do
>     n <- get
>     if n < 10
>         then put (n + 1)
>         else break () )

    The `loop` will return whatever value you supply to `break`:

> example :: State Int Bool
> example = loop (do
>     n <- get
>     if n < 10
>         then put (n + 1)
>         else break True )

>>> runState example 0
(True,10)

-}

module Control.Break (
    -- * Break
      Break
    , loop
    , break

    -- * Re-exports
    , lift
    ) where

import Control.Applicative (Applicative)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Cont   (MonadCont  )
import Control.Monad.State  (MonadState )
import Control.Monad.Writer (MonadWriter)
import Prelude hiding (break)

{-| For the most common use cases you will:

    * build `Break` commands using `lift` or `break`

    * combine `Break` commands using @do@ notation

    * consume `Break` commands using `loop`

    The meaning of the type parameters:

    * @r@: the argument type of `break` and the return type of the `loop`

    * @m@: the base `Monad` that you are running in a `loop`

    * @a@: the return type of a `Break` command (not the same as the return
      value of the `loop`)
-}
newtype Break r m a = Break { unBreak :: ExceptT r m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadCont
    , MonadState  s
    , MonadWriter w
    )

{-| `break` from a `loop`

    The argument you supply to `break` is the return value of the `loop`
-}
break :: Monad m => r -> Break r m a
break r = Break (throwE r)

{-| @(loop m)@ runs the action @\'m\'@ repeatedly until you `break` from the
   `loop`
-}
loop :: Monad m => Break r m () -> m r
loop m = do
    x <- runExceptT (unBreak (forever m))
    case x of
        Left  r -> return r
        Right r -> return r
