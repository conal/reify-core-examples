[reification-rules]: https://github.com/conal/reification-rules

## Examples of core reification and Haskell-to-hardware compilation

Reification and circuit generation happen via the GHC option `-fplugin=ReificationRules.Plugin` in the .cabal file and the `go` function from `ReificationRules.Run` in Main.hs.

## Dependencies

See the README for [reification-rules].

## Try it out

```
cabal run
```

If you're on Mac OS (or another OS that supports "open") and if everything is working, you'll see one or more displayed PDFs. The PDF gets saved in out/. Edit Main.hs to enable/disable test examples and add new ones.
