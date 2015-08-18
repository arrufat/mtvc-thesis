main :: IO ()
main = do
        let videoTags = ["Width", "Height", "Frame Rate", "Channels", "Bits Per Channel"]
            videoInfo = [1920, 1080, 25, 3, 8] :: [Integer]
            videoPrint = zipWith (\t i -> t ++ ": " ++ show i) videoTags videoInfo
            size = product videoInfo
        mapM_ putStrLn videoPrint
        putStrLn $ "Rate in bits/s: " ++ show size ++ " = " ++ show (fromInteger size :: Double)
        putStrLn $ "Rate in Gbits/s: " ++ show (fromInteger size / (1000 * 1000 * 1000) :: Double)
        let comb = combinations 256 4
        putStrLn $ "Combinations 256 over 5: " ++ show (comb :: Integer) ++ " = " ++ show (fromInteger comb :: Double)

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

combinations :: Integral a => a -> a -> a
combinations n k = factorial n `div` (factorial k * factorial (n - k))
