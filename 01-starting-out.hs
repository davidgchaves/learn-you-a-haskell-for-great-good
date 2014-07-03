-- Baby's first functions

doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

-- if is an expression (must return a value), not an statement
doubleSmallNumber x = if x > 100 then x else x * 2

-- ' usually denotes a strict or slightly modified version
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

