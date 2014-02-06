import Data.WAVE
-- (hGetWAVE, waveSamples, hPutWAVE, WAVESample)
import System.IO
import DSP.Basic
import Numeric.Transform.Fourier.FFT
import Data.Array
import Data.Complex
import Data.List

data Hole = Hole

main = do
    wave <- hGetWAVE stdin
    print $ signalysis wave 0

        --hPutWAVE stdout $ wave {waveSamples = map (:[]) $ downsample 3 signal, waveHeader = header {waveFrameRate = div frameRate 3}}

signalysis wave channel = show maxIndex' ++ " of " ++ show (length signal) ++ " gives " ++ show cycpersam ++ " cycles per sample, i.e. " ++ show ((1 - cycpersam) * (fromIntegral $ waveFrameRate (waveHeader wave))) ++ " Hz"
    where signal = map (!! channel) $ waveSamples wave :: [WAVESample]
          maxIndex' = maxIndex signal
          cycpersam = ((fromIntegral maxIndex') / (fromIntegral $ length signal))


spectrum :: [WAVESample] -> Array Int (Complex Double)
spectrum signal = fft $ listArray (0, (length signal) - 1) $ map ((:+ (0 :: Double)) . fromIntegral) signal

maxIndex :: [WAVESample] -> Int
maxIndex signal = findMax . map magnitude $ (elems (spectrum signal) :: [Complex Double])


findMax :: Ord a => [a] -> Int
findMax xs = fst . maximumBy (\x y -> compare (snd x) (snd y)) $ zip [0..length xs] xs