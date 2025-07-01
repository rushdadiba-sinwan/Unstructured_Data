library(tuneR)
library(seewave)
library(rpanel)

#generate sine wave
sr = 8000
t = seq(0,2, 1/sr)
y=(2^15-1)*sin(2*pi*440*t)
#plot waveform
plot(y[1:1000], type="l")
w=Wave(y, samp.rate=sr, bit=16)
play(w)
#compute FFT and frequency spectrum
y_fft <- fft(y)
plot(Mod(y_fft[1:1000]), type="h")

#Create 2 sine waves
y1=(2^15-1)*sin(2*pi*220*t)
y2=(2^15-1)*sin(2*pi*880*t)
#plot them in same figure
par(mfrow=c(2,1))
plot(y1[1:500], type="l")
plot(y2[1:500], type="l")
#----optional
plot(y1[1:500], type="l", col='blue')
lines(y2[1:500], col="red", lty=2)

#create and plot different noise types
white <- noise(kind = "white", duration=100000)
pink <- noise(kind = 'pink', duration=100000)
#plot first 2000 samples
plot(white[1:2000], type="l") #line plots
plot(pink[1:2000])

#generate spectrogram
data(tico)
spectro(tico, wl=1024, osc=T)
#-----optional dynspec() but use it independently

#plot mean spectrum
baby <- readWave("babycry.wav")
meanspec(baby)



##CHALLENGE-LEVEL
#task 1 - own function 
audio_func <-function(y) {
  sr=8000,
  t=(0,2,1/sr),
  y=(2^15-1)*sin(2*pi*1000*t),
  
  y_fft <- fft(y),
  output <- plot(Mod(y_fft[1:1000], type="h")),
  return(output)
}
#jawapan:
plot_freq_spect <- function() {
  sr <- 8000
  t <- seq(0,2,1/sr)
  y <- (2^15-1)*sin(2*pi*1000*t)
  y_fft <- fft(y)
  
  N <- length(y)
  freq <- (0:(N-1)) *sr/N
  amplitude <- Mod(y_fft)/N
  amplitude[2:(N/2)] <- 2*amplitude[2:(N/2)]
  
  plot(freq[1:(N/2)], aplitude[1:(N/2)],type="h",
       xlab="Frequency", ylab = "Amplitude",
       main="Frequency Spectrum")
}
w <- plot_freq_spect()

#task 2 - dominant frequency
sw1 = (2^15-1)*sin(2*pi*440*t)
w1 = Wave(sw1, samp.rate=sr, bit=16)
sw2 = (2^15-1)*sin(2*pi*880*t)
w2 = Wave(sw2, samp.rate=sr, bit=16)

combined.1 <- sw1+sw2
combined_wave = Wave(left=combined.1,samp.rate=44100,
                bit=16)
wave_sum <- normalize(combined_wave,unit=16)
wsum_fft <- fft(wave_sum)
highestAmplitude <- which.max(Mod(wsum_fft))
#-----jawapan:
sw1 = (2^15-1) * sin(2*pi*440*t)
highestAmplitude <- which.max(Mod(wsum))



#Task 3 - create own audio file+annotate time segments
sr <- 8000
duration <-1
freqs <- 500

t <- seq(0,duration, by=1/sr)
audio1_sine <- Wave(sin(2*pi*freqs*t)) #numeric
audio2_pink <- noise(kind = 'pink') #Wave object
audio3_square <- Wave(sign(sin(2*pi*freqs*t)))

combined_audio <- normalize(audio1_sine+audio2_pink+audio3_square,unit = 16)
writeWave(combined_audio, "challenge.wav")
t_1<- timer(combined_audio, f=22050, threshold=1)
t_2 <- timer(combined_audio, f=22050, threshold=2)
t_3 <- timer(combined_audio, f=22050, threshold=3)
plot(t_1)
lines(t_2,abline=2)
lines(t_3,abline=3)

#----jawapan
t <- seq(0,1,by=1/sr)
a1 <- Wave((2^15-1)* sin(2*pi*500*t),samp.rate=sr,bit=16)
a2 <- noise(kind = 'pink', duration=length(t),samp.rate = sr)
a3 <- Wave((2^15-1)* sign(sin(2*pi*200*t)), samp.rate=sr,bit=16)

final_audio <- normalize(bind(a1,a2,a3), unit=16)

#Task 4 - Filter frequency
#---combine signals with known frequencies
t <- seq(0,1,by=1/sr)
yy <- sin(2*pi*440*t) + sin(2*pi*880*t) #440Hz + 880Hz
#----use FFT to transform to frequency domain
Y <- fft(y)
#---Identify and remove specific frequencies
n <- length(Y)
freqs <- (0:(n-1)) *sr/n #frequency bins
index_440 <- which.min(abs(freqs - 440)) #index closest to 440Hz
index_mirror <- n-(index_440) +2 #mirror index for real signal
Y[index_440] <-0
Y[index_mirror] <- 0
#------inverse FFT to go back to time-domain
y_filtered <- Re(fft(Y,inverse = T))/n #divided by n to scale the IFFT output
#-----plot-compare before-after filtering
par(mfrow = c(2, 1))
plot(t[1:1000], y[1:1000], type = "l", main = "Original Signal (440 + 880 Hz)")
plot(t[1:1000], y_filtered[1:1000], type = "l", main = "Filtered Signal (440 Hz Removed)")


#Task 5 - spectrogram comparison
baby <- readWave("babycry.wav")
data(tico)
par(mfrow=c(1,2))
spectro(tico,main = "Tico Bird")
spectro(baby,main="Baby Cry")

























