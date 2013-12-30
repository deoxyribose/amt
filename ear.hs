import Data.WAVE
-- (hGetWAVE, waveSamples, hPutWAVE, WAVESample)
import System.IO
import DSP.Basic
import Numeric.Transform.Fourier.FFT
import Data.Array
import Data.Complex
import Data.List

main = do
    wave <- hGetWAVE stdin
    let
        signal = map head $ waveSamples wave :: [WAVESample]
        header = waveHeader wave
        spectrum = fft $ listArray (0,(length signal) - 1) $ map ((:+ (0 :: Double)) . fromIntegral) signal
        frameRate = waveFrameRate header
        maxIndex = findMax . map magnitude $ (elems spectrum :: [Complex Double])
        cycpersam = ((fromIntegral maxIndex) / (fromIntegral $ length signal)) in
        print $ show maxIndex ++ " of " ++ show (length signal) ++ " gives " ++ show cycpersam ++ " cycles per sample, i.e. " ++ show ((1 - cycpersam) * (fromIntegral $ waveFrameRate header))

        --hPutWAVE stdout $ wave {waveSamples = map (:[]) $ downsample 3 signal, waveHeader = header {waveFrameRate = div frameRate 3}}


findMax :: Ord a => [a] -> Int
findMax xs = fst . maximumBy (\x y -> compare (snd x) (snd y)) $ zip [0..length xs] xs