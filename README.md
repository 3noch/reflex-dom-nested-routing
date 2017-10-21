# Nested Routing for Reflex-DOM

Example:

```haskell
app = runRouteWithPathInFragment $ do
  switchPromptly never <=< withRoute $ \route -> case fromMaybe "" route of
    ""         -> (["users"] <$) <$> buttonClick "Open users"
    "users"    -> users
    "test"     -> codeToRun >> pure never
    "settings" -> text "Settings" >> pure never
    _          -> redirectLocally []
```

`RouteWriter` can make plumbing easer:


```haskell
app = runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ do
  void $ withRoute $ \route -> case fromMaybe "" route of
    ""         -> tellRouteAs ["users"] <=< buttonClick "Open users"
    "users"    -> users
    "test"     -> codeToRun
    "settings" -> text "Settings"
    _          -> tellRedirectLocally []
```

