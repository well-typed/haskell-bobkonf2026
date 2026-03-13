
# Setup instructions

1. Unless you're on NixOS, use GHCup to install Haskell, Cabal, and HLS (Haskell Language Server). The recommended
   version by GHCup right now is ghc-9.6.7, which is definitely fine. Other recent versions should also be OK.

2. If you for some reason don't have a preferred editor, then get one. VSCode is probably easiest to get working
   with Haskell Language Server. (You should just need the Haskell extension.) Other editors, in particular neovim
   and emacs are known to in principle work with HLS as well, but you'll probably need to find instructions online.

3. To test the GHC and Cabal installation, you should be able to run `cabal update` and then `cabal run bobserver`
   in the root directory of this repository. This should download and build many packages and eventually report
   `"Hello world"`.

4. To test your editor and HLS installation, you should be able (ideally after building) to open the root directory
   of this repository, then navigate to `src/Tutorial.hs`. You should have syntax highlighting, and finding the line
   saying `main = putStrLn "Hello world"` and hovering with the mouse over the string `"Hello world"` should display
   a small window with something like `_ :: String` in it.

