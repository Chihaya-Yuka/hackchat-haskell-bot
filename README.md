# hackchat-hs

`hackchat-hs` is a Haskell library for writing [hack.chat](https://hack.chat) bots.

# Installation

- To use this library, you will need to add `hackchat-hs` to your Haskell project. You can include it in your `stack.yaml` or `cabal` project dependencies.

# Usage

```haskell
import HackChat

main :: IO ()
main = do
    chat <- connectToChannel "FunctionalBot" "programming"
    runDaemon chat
```

# License

This project is under the [MIT License](LICENSE).
