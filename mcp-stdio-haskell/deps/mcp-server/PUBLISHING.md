# Publishing to Hackage

First update `mcp-server.cabal` to bump the version:

```
version: <new version>
```

Then ensure the whole package builds:

```
cabal update
cabal clean
cabal build
cabal test
cabal sdist
```

Then upload `dist-newstyle/sdist/<package>.tar.gz` to https://hackage.haskell.org/upload

Then build the documentation and upload it:

```
cabal v2-haddock --builddir="dist-newstyle/docs" --haddock-for-hackage --enable-doc
cabal upload -d --publish dist-newstyle/docs/*-docs.tar.gz
```

