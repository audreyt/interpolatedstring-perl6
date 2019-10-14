import Distribution.Simple
import System.Cmd(system)

main = defaultMainWithHooks $ simpleUserHooks { testHook = runElfTests }

runElfTests _ _ _ _ _ = system "runhaskell -i./src ./tests/Test.hs" >> return ()

