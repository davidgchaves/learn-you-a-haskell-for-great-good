--
-- Reverse getLine using fmap
--

main = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-- runhaskell reverseGetLine.hs
--      moar functors FTW!
--      --> You said !WTF srotcnuf raom backwards!
--      --> Yes, you really said !WTF srotcnuf raom backwards!
