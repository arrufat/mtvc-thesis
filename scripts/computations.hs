main :: IO ()
main = do
        let videoTags = ["Width", "Height", "Frame Rate", "Channels", "Bits Per Channel", "Duration (s)"]
            videoInfo = [1920, 1080, 25, 3, 8, 2 * 3600] :: [Integer]
            videoPrint = zipWith (\t i -> t ++ ": " ++ show i) videoTags videoInfo
            size = product videoInfo
        mapM_ putStrLn videoPrint
        putStrLn $ "Size in bits: " ++ show size ++ " = " ++ show (fromInteger size :: Double)
        putStrLn $ "Size in Gbits: " ++ show (fromInteger size / (1000 * 1000 * 1000) :: Double)
        putStrLn $ "Size in GiB: " ++ show (fromInteger size / 8 / (1024 * 1024 * 1024) :: Double)
