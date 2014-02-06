import Control.Monad
import Control.Monad.Fix
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

main = do
    SDL.init [SDL.InitAudio]
    result <- openAudio audioRate audioFormat audioChannels audioBuffers
    sound <- Mix.loadWAV "/home/seh/Dropbox/test2.wav"
    ch1 <- Mix.playChannel anyChannel sound 0
    fix $ \loop -> do
        SDL.delay 50
        stillPlaying <- numChannelsPlaying
        when (stillPlaying /= 0) loop
    Mix.closeAudio
    SDL.quit

    where   audioRate       = 44100
            audioFormat     = Mix.AudioS16LSB
            audioChannels   = 2
            audioBuffers    = 4096
            anyChannel      = (-1)
