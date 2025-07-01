getwd()

audio_test2 <- readWave("audio.wav")
plot(audio_test2)

#---(a) Plot audio using  function timer. Briefly explain
timer(audio_test2[1:1000], f=22050, threshold=70)

#---(b) construct spectrogram
spectro(audio_test2)

#---(c) construct left channel audio using fft
auLeft <- fft(audio_test2@left)
plot.frequency.spectrum(auLeft)

#otherway to plot frequency spectrum
plot(Mod(audio_test2[1:5000]), type="l",
     xlab="Frequency (Hz)", ylab="Amplitude",
     main="Frequency Spectrum") 
     