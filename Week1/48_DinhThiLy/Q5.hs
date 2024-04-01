cylinderVolume :: Double -> Double -> Double
cylinderVolume r h = pi * r * r * h
    putStrLn "Nhập bán kính của hình trụ:"
    radiusStr <- getLine
    putStrLn "Nhập chiều cao của hình trụ:"
    heightStr <- getLine
    let radius = read radiusStr :: Double
        height = read heightStr :: Double
        volume = cylinderVolume radius height
    putStrLn $ "Thể tích của hình trụ là: " ++ show volume
    if volume >= 42
        then putStrLn "thể tich của hình trụ lớn hơn 42"
        else putStrLn "thể tích của hình trụ nhỏ hơn 42"