# Nested Routing for Reflex-DOM

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)

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
    ""         -> tellRouteAs ["users"] =<< buttonClick "Open users"
    "users"    -> users
    "test"     -> codeToRun
    "settings" -> text "Settings"
    _          -> tellRedirectLocally []
```

