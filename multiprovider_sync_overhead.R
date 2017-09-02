#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

options(scipen=5)

SharedRead = read.csv(args[1], header = TRUE, sep = ",")
SeparatedRead = read.csv(args[2], header = TRUE, sep = ",")
SharedWrite = read.csv(args[3], header = TRUE, sep = ",")
SeparatedWrite = read.csv(args[4], header = TRUE, sep = ",")
FirstRead = read.csv(args[5], header = TRUE, sep = ",")

SharedReadProviderId = aggregate(SharedRead$Throughput ~ SharedRead$ProviderId, FUN=sd)$'SharedRead$ProviderId'
SharedReadThroughputSd = aggregate(SharedRead$Throughput ~ SharedRead$ProviderId, FUN=sd)$'SharedRead$Throughput'
SharedReadThroughput = aggregate(SharedRead$Throughput ~ SharedRead$ProviderId, FUN=mean)$'SharedRead$Throughput'

SeparatedReadProviderId = aggregate(SeparatedRead$Throughput ~ SeparatedRead$ProviderId, FUN=sd)$'SeparatedRead$ProviderId'
SeparatedReadThroughputSd = aggregate(SeparatedRead$Throughput ~ SeparatedRead$ProviderId, FUN=sd)$'SeparatedRead$Throughput'
SeparatedReadThroughput = aggregate(SeparatedRead$Throughput ~ SeparatedRead$ProviderId, FUN=mean)$'SeparatedRead$Throughput'

SharedWriteProviderId = aggregate(SharedWrite$Throughput ~ SharedWrite$ProviderId, FUN=sd)$'SharedWrite$ProviderId'
SharedWriteThroughputSd = aggregate(SharedWrite$Throughput ~ SharedWrite$ProviderId, FUN=sd)$'SharedWrite$Throughput'
SharedWriteThroughput = aggregate(SharedWrite$Throughput ~ SharedWrite$ProviderId, FUN=mean)$'SharedWrite$Throughput'

SeparatedWriteProviderId = aggregate(SeparatedWrite$Throughput ~ SeparatedWrite$ProviderId, FUN=sd)$'SeparatedWrite$ProviderId'
SeparatedWriteThroughputSd = aggregate(SeparatedWrite$Throughput ~ SeparatedWrite$ProviderId, FUN=sd)$'SeparatedWrite$Throughput'
SeparatedWriteThroughput = aggregate(SeparatedWrite$Throughput ~ SeparatedWrite$ProviderId, FUN=mean)$'SeparatedWrite$Throughput'

FirstReadProviderId = FirstRead$ProviderId
FirstReadThroughput = FirstRead$Throughput

pdf(args[6])

# plot 1
plot(SharedReadProviderId, SharedReadThroughput, pch=1, ylim=c(0, 1200), xlab="Provider number", col='black', xaxt='n', ylab="Throughput [MB / s]")
axis(1, at=SharedReadProviderId, lab=SharedReadProviderId)

# shared first read
points(FirstReadProviderId, FirstReadThroughput, pch=16, lty=2, col="black")

# shared read
arrows(SharedReadProviderId, SharedReadThroughput-SharedReadThroughputSd, SharedReadProviderId, SharedReadThroughput+SharedReadThroughputSd, col='black', length=0.05, angle=90, code=3)
P <- predict(loess(SharedReadThroughput ~ SharedReadProviderId))
lines(P ~ SharedReadProviderId, col="black", lwd=1, lty='dashed')

# separated read
points(SeparatedReadProviderId, SeparatedReadThroughput, pch=0, lty=2, col="red")
arrows(SeparatedReadProviderId, SeparatedReadThroughput-SeparatedReadThroughputSd, SeparatedReadProviderId, SeparatedReadThroughput+SeparatedReadThroughputSd, col='red', length=0.05, angle=90, code=3)
P <- predict(loess(SeparatedReadThroughput ~ SeparatedReadProviderId))
lines(P ~ SeparatedReadProviderId, col="red", lwd=1, lty='dashed')

# shared write
points(SharedWriteProviderId, SharedWriteThroughput, pch=2, lty=2, col="blue")
arrows(SharedWriteProviderId, SharedWriteThroughput-SharedWriteThroughputSd, SharedWriteProviderId, SharedWriteThroughput+SharedWriteThroughputSd, col='blue', length=0.05, angle=90, code=3)
P <- predict(loess(SharedWriteThroughput ~ SharedWriteProviderId))
lines(P ~ SharedWriteProviderId, col='blue', lwd=1, lty='dashed')

# separated write
points(SeparatedWriteProviderId, SeparatedWriteThroughput, pch=5, lty=2, col="green")
arrows(SeparatedWriteProviderId, SeparatedWriteThroughput-SeparatedWriteThroughputSd, SeparatedWriteProviderId, SeparatedWriteThroughput+SeparatedWriteThroughputSd, col='green', length=0.05, angle=90, code=3)
P <- predict(loess(SeparatedWriteThroughput ~ SeparatedWriteProviderId))
lines(P ~ SeparatedWriteProviderId, col='green', lwd=1, lty='dashed')

# legend
legend("topright", legend=c("Shared space first read", "Shared space read","Local space read", "Shared space write", "Local space write"),
cex=0.8, col=c("black","black","red","blue", "green"), pch=c(16, 1, 0, 2, 5), lty=c(0,2,2,2,2));

dev.off()