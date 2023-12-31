# hason

> Yes, a Json parser written in Haskell

# Getting Started

> Note: Make sure that GHC is installed in your environment

1. Clone this Repository

```bash
git clone git@github.com:J3m3/hason.git hason
cd hason
```

2. Compile & Link

```bash
ghc -o parse Main.hs -no-keep-hi-files -no-keep-o-files
```

3. Let's test! I already provided `about.json` for test, but feel free to test w/ other `.json` files. ~~Well, unless they don't touch umimplemented TODOs.~~

```bash
./parse about.json any.json
```

# TODOs

- [x] use Data.Map instead of [(String, JsonValue)]
- [ ] support escape sequences
- [ ] support floats
- [ ] improve error messages
