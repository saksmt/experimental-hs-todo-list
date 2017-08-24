module Output.Channel(out, err, (<<)) where

import Output.MonadOutput

data Channel = StdOut | StdErr deriving Eq

out :: Channel
out = StdOut

err :: Channel
err = StdErr

infixr 1 <<
(<<) :: (Monad m, MonadOutput a m) => Channel -> a -> m ()
ch << message = (if ch == StdOut then writeMessage else writeError) message

