[reification-rules]: https://github.com/conal/reification-rules

## Examples of core reification and Haskell-to-hardware compilation

Reification and circuit generation happen via the GHC option `-fplugin=ReificationRules.Plugin` in the .cabal file and the `go` function from `ReificationRules.Run` in Main.hs.

## Try it out

You'll need [graphviz](http://www.graphviz.org/) for rendering the circuit diagrams. For instance, "brew install graphviz" on Mac OS, if you have [homebrew](http://brew.sh/) installed.

    stack build
    stack exec reify-core-examples

If you're on Mac OS (or another OS that supports "open") and if everything is working, you'll see one or more displayed PDFs. The PDF gets saved in out/.

To enable/disable test examples and add new ones, edit Main.hs.
