solveRPN :: String -> Double
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "*" = (y * x):ys
          foldingFunc (x:y:ys) "+" = (y + x):ys
          foldingFunc (x:y:ys) "-" = (y - x):ys
          foldingFunc (x:y:ys) "/" = (y / x):ys
          foldingFunc (x:y:ys) "^" = (y ** x):ys
          foldingFunc (x:xs) "ln"  = log x:xs
          foldingFunc xs "sum"     = [sum xs]
          foldingFunc xs numberStr = read numberStr:xs
-- solveRPN "10 4 3 + 2 * -"              --> -4.0
-- solveRPN "2 3.5 +"                     --> 5.5
-- solveRPN "90 34 12 33 55 66 + * - +"   --> -3947.0
-- solveRPN "90 34 12 33 55 66 + * - + -" --> 4037.0
-- solveRPN "90 3.8 -"                    --> 86.2
-- solveRPN "2.7 ln"                      --> 0.9932517730102834
-- solveRPN "10 10 10 10 sum 4 /"         --> 10.0
-- solveRPN "10 10 10 10 10 sum 4 /"      --> 12.5
-- solveRPN "10 2 ^"                      --> 100.0
