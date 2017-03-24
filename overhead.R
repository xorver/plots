#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

options(scipen=5)

DataSync = read.csv(args[1], header = TRUE, sep = ",")
DataNoSync = read.csv(args[2], header = TRUE, sep = ",")

SyncBS = aggregate(DataSync$ThroughputMBs ~ DataSync$BlockSizeMB, FUN=sd)$'DataSync$BlockSizeMB' / (1024*1024)
SyncSD = aggregate(DataSync$ThroughputMBs ~ DataSync$BlockSizeMB, FUN=sd)$'DataSync$ThroughputMBs'
SyncTH = aggregate(DataSync$ThroughputMBs ~ DataSync$BlockSizeMB, FUN=mean)$'DataSync$ThroughputMBs'
SyncCPU = aggregate(DataSync$CPU0 ~ DataSync$BlockSizeMB, FUN=mean)$'DataSync$CPU0'
SyncCPUSD = aggregate(DataSync$CPU0 ~ DataSync$BlockSizeMB, FUN=sd)$'DataSync$CPU0'

NoSyncBS = aggregate(DataNoSync$ThroughputMBs ~ DataNoSync$BlockSizeMB, FUN=sd)$'DataNoSync$BlockSizeMB' / (1024*1024)
NoSyncSD = aggregate(DataNoSync$ThroughputMBs ~ DataNoSync$BlockSizeMB, FUN=sd)$'DataNoSync$ThroughputMBs'
NoSyncTH = aggregate(DataNoSync$ThroughputMBs ~ DataNoSync$BlockSizeMB, FUN=mean)$'DataNoSync$ThroughputMBs'
NoSyncCPU = aggregate(DataNoSync$CPU0 ~ DataNoSync$BlockSizeMB, FUN=mean)$'DataNoSync$CPU0'
NoSyncCPUSD = aggregate(DataNoSync$CPU0 ~ DataNoSync$BlockSizeMB, FUN=sd)$'DataNoSync$CPU0'

pdf(args[3])

#attach(mtcars)
#par(mfrow=c(2,1), heights=c(10,1))
#nf <- layout(matrix(c(1,2),ncol=1), heights=c(2,1), TRUE)

# plot 1
plot(SyncBS, SyncTH, ylim=c(100, 400), pch=1, xlab="Block Size [MB]", col='black', xaxt='n', log="x", ylab="Throughput [MB / s]")
axis(1, at=SyncBS, lab=SyncBS)
# with sync
arrows(SyncBS, SyncTH-SyncSD, SyncBS, SyncTH+SyncSD, col='black', length=0.05, angle=90, code=3)
P <- predict(loess(SyncTH ~ SyncBS))
lines(P ~ SyncBS, col="black", lwd=1, lty='dashed')
# without sync
points(NoSyncBS, NoSyncTH, pch=0, lty=2, col="red")
arrows(NoSyncBS, NoSyncTH-NoSyncSD, NoSyncBS, NoSyncTH+NoSyncSD, col='red', length=0.05, angle=90, code=3)
P <- predict(loess(NoSyncTH ~ NoSyncBS))
lines(P ~ NoSyncBS, col="red", lwd=1, lty='dashed')

# legend
legend("bottomright", legend=c("With sync","Without sync"), cex=0.8, col=c("black","red"), pch=c(1, 0), lty=c(2,2));

## plot2
#plot(SyncBS, SyncCPU, ylim=c(12, 16), col='black', xaxt='n', log="x", xlab="Block Size [MB]", ylab="Cpu utilization [%]")
#axis(1, at=SyncBS, lab=SyncBS)
#arrows(SyncBS, SyncCPU-SyncCPUSD, SyncBS, SyncCPU+SyncCPUSD, col='black', length=0.05, angle=90, code=3)
#P <- predict(loess(SyncCPU ~ SyncBS))
#lines(P ~ SyncBS, col="black", lwd=1, lty='dashed')
#
#points(NoSyncBS, NoSyncCPU, pch=22, lty=2, col="red")
#arrows(NoSyncBS, NoSyncCPU-NoSyncCPUSD, NoSyncBS, NoSyncCPU+NoSyncCPUSD, col='red', length=0.05, angle=90, code=3)
#P <- predict(loess(NoSyncCPU ~ NoSyncBS))
#lines(P ~ NoSyncBS, col="red", lwd=1, lty='dashed')
#
#legend(60, 15.5, c("With sync","Without sync"), cex=0.8,
#col=c("black","red"), pch=21:22, lty=1:2);

dev.off()