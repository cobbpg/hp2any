import Text.Printf

main :: IO ()
main = exercise 500000

exercise :: Double -> IO ()
exercise d = do
  printf "Input: %f\n" d
  printf "Result: %f\n" (mean [1..d])
  exercise (d*1.5)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
