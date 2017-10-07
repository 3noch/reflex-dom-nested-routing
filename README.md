# Nested Routing for Reflex-DOM

Example:

```haskell
app = runRouteWithPathInFragment $ do
  switchPromptly never =<< withRoute (\route -> case fromMaybe "" route of
    ""         -> dashboard user
    "users"    -> users user
    "test"     -> codeToRun >> pure never
    "settings" -> text "Settings" >> pure never
    _          -> redirectLocally []
    )
```
