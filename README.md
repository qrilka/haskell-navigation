# Haskell navigation

This repository is an attempt to provide tools for easier Haskell source code
navigation. It uses `haskell-indexer` project as a way to get information about
source code.

## Index a package

To index a package you need to do the following:

1. Build this project with `stack build`, so you'll get `query-tool` and
   `kythe-to-call-graph`

2. Build `ghc\_kythe\_wrapper` with `stack build haskell-indexer-pipeline-ghckythe-wrapper`
   (Stack currently doesn't build Github project packages by default)

3. Index your package with

    ```
    ./index.hs <PATH-TO-YOUR-PROJECT> <CALL-GRAPH-FILE-NAME>
    ```

    where `<PATH-TO-YOUR-PROJECT>` is a path pointing to your package directory
    containing a Stack project (i.e. `stack.yaml`) and `<CALL-GRAPH-FILE-NAME>`
    should point to a file where you'll store the resulting call graph

Please note that package indexing takes some time and requires a bit of space
as currently it rebuilds a package "from scratch" starting from an empty
`$STACK_ROOT` so any of required Stackage dependencies will get indexed.

Having done all of that you should be able to run `query-tool` with the call
graph you've created.
