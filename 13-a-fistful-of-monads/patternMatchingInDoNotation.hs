justChar :: String -> Maybe Char
justChar s = do
    (x:xs) <- Just s
    return x
-- justChar "Hello" --> Just 'H'
-- justChar ""      --> Nothing

