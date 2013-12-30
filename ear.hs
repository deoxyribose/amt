import Data.WAVE (hGetWAVE, waveSamples)
import System.IO
import DSP.Basic

main = (fmap (concat . waveSamples) $ hGetWAVE stdin) >>= print