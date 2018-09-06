# "Normal" graph representation

```
data Node
  edges :: [Edge]
  attrs :: [NodeAttribute]

data Edge
  source :: Node
  target :: Node

data NodeAttribute
  node :: Node
  name :: String
  value :: ByteString
```


# Kythe potobuf representation

* VName is Node
* Entry with target ("edge") is an Edge
  - *probably* doesn't have  name and value attrs
* Entry without target ("fact") is a NodeAttribute


# haskell-indexer types

```
module Language.Kythe.Schema.Typed
  data Fact value where
      AnchorSubkindF :: Fact AnchorSubkind
      NodeKindF      :: Fact NodeKind
      FileTextF      :: Fact ByteString
      LocStartF      :: Fact Int
      LocEndF        :: Fact Int
      CompleteF      :: Fact HowComplete
      SnippetStartF  :: Fact Int
      SnippetEndF :: Fact instead
  data Edge
      = ChildOfE
      | OverridesE
      | OverridesRootE
      | ExtendsE
      | AnchorEdgeE AnchorEdge
```

```
module Raw
  data Fact = Fact VName FactName FactValue
  data Edge = Edge VName EdgeName VName
```

# Explanations

* `data Edge` above is `Raw.Edge` in haskell-indexer
* `data NodeAttribute` above is `Raw.Fact` in haskell-indexer
* `data Node` above isn't explicitly represented, instead implicitly represented by `Fact`s that refer to them (first field `VName`)

* `string edge_kind = 2` from kythe is `data Edge` from haskell-indexer
* `string fact_name = 4` and `bytes fact_value = 5` from kythe is `data Fact` from haskell-indexer
