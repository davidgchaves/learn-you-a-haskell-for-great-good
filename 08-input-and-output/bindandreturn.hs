main = do
    a <- return "hell"          --binded to a
    b <- return "yeah!"         --binded to b
    putStrLn $ a ++ " " ++ b    --prints to the console "hell yeah!"
