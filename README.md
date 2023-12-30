# hason

> Yes, a Json parser written in Haskell

# Getting Started

> Note: Make sure that GHC is installed in your environment

1. Compile & Link

```bash
ghc -o parse Main.hs -no-keep-hi-files -no-keep-o-files
```

2. Test w/ any `.json` files! ~~Well, unless they don't touch umimplemented TODOs~~

```bash
./parse any.json
```

# TODOs

- [x] use Data.Map instead of [(String, JsonValue)]
- [] support escape sequences
- [] support floats
- [] improve error messages
