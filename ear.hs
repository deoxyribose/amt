import Data.WAVE
-- (hGetWAVE, waveSamples, hPutWAVE, WAVESample)
import System.IO
import DSP.Basic
import Numeric.Transform.Fourier.FFT
import Data.Array
import Data.Complex

main = do
    wave <- hGetWAVE stdin
    let
        signal = map head $ waveSamples wave :: [WAVESample]
        header = waveHeader wave
        frameRate = waveFrameRate header in
        hPutWAVE stdout $ wave {waveSamples = map (:[]) $ downsample 3 signal, waveHeader = header {waveFrameRate = div frameRate 3}}
