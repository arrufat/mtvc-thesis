main :: IO ()
main = do
        let videoTags = ["Width", "Height", "Frame Rate", "Channels", "Bits Per Channel"]
            videoInfo = [1920, 1080, 25, 3, 8] :: [Integer]
            videoPrint = zipWith (\t i -> t ++ ": " ++ show i) videoTags videoInfo
            size = fromInteger $ product videoInfo :: Double
        mapM_ putStrLn videoPrint
        putStrLn $ "Rate in bits/s: " ++ show size ++ " = " ++ show size
        putStrLn $ "Rate in Gbits/s: " ++ show (size / (1000 * 1000 * 1000))
