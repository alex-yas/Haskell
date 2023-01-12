matrixMul ::Num a => [[a]] -> [[a]] -> [[a]]
matrixMul [[a, b], [c, d]] [[e, f], [g, h]] = [[a * e + b * g, a * f + b * h], [c * e + d * g, c * g + d * h]]

matrixSquare ::Num a => [[a]] -> [[a]]
matrixSquare [[a, b], [c, d]] = [[a * a + b * c, a * b + b * d], [a * c + c * d, b * c + d * d]]

matrixPower ::Num a => Int -> [[a]] -> [[a]]
matrixPower 0 _ = [[1, 0], [0, 1]]
matrixPower 1 a = a
matrixPower p a = if mod p 2 == 1 then matrixMul a (matrixPower (pred p) a) else matrixPower (quot p 2) (matrixSquare a)

extractFibNumber :: Num a => [[a]] -> a
extractFibNumber [[_, _], [_, x]] = x

getFib :: Int -> Int
getFib ind = extractFibNumber(matrixPower ind [[0, 1], [1, 1]])