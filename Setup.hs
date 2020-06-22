import           Distribution.Simple
import           Distribution.Simple.Program
import           System.Process              (system)

main :: IO ()
main = do
  putStrLn "Building..."
  defaultMainWithHooks $ simpleUserHooks {
    hookedPrograms = [bnfc],
    preBuild = \args buildFlags -> do
      _ <- system "bnfc --haskell \
                      \ Meta.cf \
                      \ -p Meta.Frontend \
                      \ -o src"
      preBuild simpleUserHooks args buildFlags
  }

bnfc :: Program
bnfc = (simpleProgram "bnfc") {
         programFindVersion = findProgramVersion "--version" id
       }
