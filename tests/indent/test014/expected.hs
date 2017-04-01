foo :: Monad m
    => Functor m
    => MonadIO m
    -> Int
foo x = x
