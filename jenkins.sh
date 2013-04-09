# Jenkins should have "cd Java && bash jenkins.sh"
PATH="$HOME/.cabal/bin:$(eval 'echo $'"$HASKELL_RUNTIME"):$TOOLS:$JDK16:$PATH"

cabal update
cabal install cabal-dev
cabal install alex
cabal install happy
cabal install c2hs

cabal --version
cabal-dev --version
ghc --version
ghc-pkg --version
alex --version
happy --version

mkdir -p ./build
git clone ssh://builder@src.galois.com/abcBridge ./build/abcBridge
git clone ssh://builder@src.galois.com/jvm-parser ./build/jvm-parser
# git clone ssh://builder@src.galois.com/Aiger ./build/Aiger
# cabal-dev add-source ./build/abcBridge
# cabal-dev add-source ./build/Aiger
cabal-dev install . ../Verinf ./build/abcBridge ./build/jvm-parser --constraint=hashable==1.1.2.5 --reinstall --flags='build-tests'
./cabal-dev/bin/Tests
bash stage.sh
