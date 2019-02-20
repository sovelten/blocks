import Test.DocTest

main :: IO ()
main = doctest ["-isrc",
                "src/Models/Transaction.hs",
                "src/Models/Block.hs",
                "src/Models/ChainState.hs"]
