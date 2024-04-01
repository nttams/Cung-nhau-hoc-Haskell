
stron r = pi * r * r
main = putStrLn "Diện tích của hình tròn là: " >>
 getLine >>= (\r -> print (stron (read r :: Double)))