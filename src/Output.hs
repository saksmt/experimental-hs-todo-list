module Output (
    OutT,
    Out,
    MonadOutput(..),
    OutputStream(..),

    messageStreamT,
    messageStream,
    errorStreamT,
    errorStream,
    streams,
    streamsT,

    out,
    err,
    (<<)
) where

import Output.Out
import Output.MonadOutput
import Output.Stream
import Output.Channel
