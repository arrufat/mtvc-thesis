main :: IO ()
main = do
        let videoTags = ["Width", "Height", "Frame Rate", "Channels", "Bits Per Channel", "Duration (s)"]
            videoInfo = [1920, 1080, 25, 3, 8, 2 * 3600] :: [Integer]
            videoPrint = zipWith (\t i -> t ++ ": " ++ show i) videoTags videoInfo
            size = fromInteger $ product videoInfo :: Double
        mapM_ putStrLn videoPrint
        putStrLn $ "Size in bits: " ++ show size ++ " = " ++ show size
        putStrLn $ "Size in Gbits: " ++ show (size / (1000 * 1000 * 1000))
        putStrLn $ "Size in GiB: " ++ show (size / 8 / (1024 * 1024 * 1024))
