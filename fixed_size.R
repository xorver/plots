#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

options(scipen=5)

Data = read.csv(args[1], header = TRUE, sep = ",")

FileSize = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$FileSizeMB' / (1000)
Chunk = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$ChunkSizeMB'
Throughput = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=mean)$'Data$ThroughputMB'
ThroughputSD = aggregate(Data$ThroughputMBs ~ Data$ChunkSizeMB + Data$FileSizeMB, FUN=sd)$'Data$ThroughputMB'
CPU = aggregate(Data$CPU0 ~ Data$BlockSizeMB, FUN=mean)$'Data$CPU0'
CPUSD = aggregate(Data$CPU0 ~ Data$BlockSizeMB, FUN=sd)$'Data$CPU0'

UniqueFileSize = unique(FileSize)
UniqueChunk = unique(Chunk) / (1024*1024)
FileNum = length(UniqueFileSize)
ChunkNum = length(UniqueChunk)


pdf(args[2])

# plot 1
TH = Throughput[1:ChunkNum]
SD = ThroughputSD[1:ChunkNum]
plot(UniqueChunk, TH, ylim=c(420, 550), pch=1, xlab="Chunk Size [MB]", col='black', xaxt='n', log="x", ylab="Throughput [MB / s]")
axis(1, at=UniqueChunk, lab=c(UniqueChunk[1:(ChunkNum-1)], "variable"))
arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col="black", length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="black", lwd=1, lty='dashed')

# 2nd file
TH = Throughput[(ChunkNum+1):(2*ChunkNum)]
SD = ThroughputSD[(ChunkNum+1):(2*ChunkNum)]
points(UniqueChunk, TH, pch=0, lty=2, col="red")
arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col='red', length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="red", lwd=1, lty='dashed')

# 3rd file
TH = Throughput[(2*ChunkNum+1):(3*ChunkNum)]
SD = ThroughputSD[(2*ChunkNum+1):(3*ChunkNum)]
points(UniqueChunk, TH, pch=5, lty=3, col="blue")
arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col="blue", length=0.05, angle=90, code=3)
P <- predict(loess(TH ~ UniqueChunk))
lines(P ~ UniqueChunk, col="blue", lwd=1, lty='dashed')

## 3rd file
#TH = Throughput[(3*ChunkNum+1):(4*ChunkNum)]
#SD = ThroughputSD[(3*ChunkNum+1):(4*ChunkNum)]
#points(UniqueChunk, TH, pch=24, lty=4, col="black")
#arrows(UniqueChunk, TH-SD, UniqueChunk, TH+SD, col='black', length=0.05, angle=90, code=3)
#P <- predict(loess(TH ~ UniqueChunk))
#lines(P ~ UniqueChunk, col="black", lwd=1, lty='dashed')

# legend
legend("bottomright", legend=c("4GB File", "8GB File", "16GB File"), cex=0.8, col=c("black", "red", "blue"), pch=c(1,0,5), lty=c(2,2,2));

dev.off()