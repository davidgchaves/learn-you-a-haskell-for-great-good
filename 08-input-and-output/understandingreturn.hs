main = do
    return ()                   --thrown away because it isn't bound to a name
    return "HAHA"               --thrown away because it isn't bound to a name
    line <- getLine
    return "BLAH BLAH BLAH"     --thrown away because it isn't bound to a name
    return 4                    --thrown away because it isn't bound to a name
    putStrLn line               --the only line that prints something to the console
