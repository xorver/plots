#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

options(scipen=5)

Data = read.csv(args[1], header = TRUE, sep = ",")

FileSize = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$FileSizeMB' / (1000)
Chunk = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$ChunkSizeMB'
Throughput = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$ThroughputMB'
ThroughputSD = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=sd)$'Data$ThroughputMB'
CPU = aggregate(Data$CPU ~ Data$BlockSizeMB, FUN=mean)$'Data$CPU'
CPUSD = aggregate(Data$CPU ~ Data$BlockSizeMB, FUN=sd)$'Data$CPU'

UniqueFileSize = unique(FileSize)
UniqueChunk = unique(Chunk) / (1024*1024)
FileNum = length(UniqueFileSize)
ChunkNum = length(UniqueChunk)


pdf(args[2])

# plot 1
TH = Throughput[1:ChunkNum]
SD = ThroughputSD[1:ChunkNum]
plot(UniqueChunk, Throughput[1:ChunkNum], ylim=c(250, 400), xlab="Chunk Size [MB]", col='blue', xaxt='n', log="x", ylab="Throughput [MB / s]")
axis(1, at=UniqueChunk, lab=c(UniqueChunk[1:(ChunkNum-1)], "fixed"))
#arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col='blue', length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="blue", lwd=1, lty='dashed')

# 2nd file
TH = Throughput[(ChunkNum+1):(2*ChunkNum)]
SD = ThroughputSD[(ChunkNum+1):(2*ChunkNum)]
points(UniqueChunk, Throughput[(ChunkNum+1):(2*ChunkNum)], pch=22, lty=2, col="red")
#arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col='red', length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="red", lwd=1, lty='dashed')

# 3rd file
TH = Throughput[(2*ChunkNum+1):(3*ChunkNum)]
SD = ThroughputSD[(2*ChunkNum+1):(3*ChunkNum)]
points(UniqueChunk, Throughput[(2*ChunkNum+1):(3*ChunkNum)], pch=23, lty=2, col="black")
#arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col='black', length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="black", lwd=1, lty='dashed')

# legend
legend(17, 270, c("2GB File", "4GB File", "6GB File"), cex=0.8,
col=c("blue", "red", "black"), pch=21:23, lty=1:3);

dev.off()