#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(ggplot2)
library(ggthemes)


options(scipen=5)

SharedRead = read.csv("~/one_projects/plots/fgcs_data/shared_space_filtered_read.csv", header = TRUE, sep = ",")
SeparatedRead = read.csv("~/one_projects/plots/fgcs_data/separated_space_filtered_read.csv", header = TRUE, sep = ",")
SharedWrite = read.csv("~/one_projects/plots/fgcs_data/shared_space_filtered_write.csv", header = TRUE, sep = ",")
SeparatedWrite = read.csv("~/one_projects/plots/fgcs_data/separated_space_filtered_write.csv", header = TRUE, sep = ",")
FirstRead = read.csv("~/one_projects/plots/fgcs_data/shared_space_first_read.csv", header = TRUE, sep = ",")

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

FirstReadProviderId = c(c(1), FirstRead$ProviderId)
FirstReadThroughput = c(9999, FirstRead$Throughput)


df <- data.frame( 
  SharedReadProviderId = SharedReadProviderId,
  SharedReadThroughput = SharedReadThroughput,
  SharedReadThroughputSd = SharedReadThroughputSd
) 

# plot 1
ggplot(df, aes(SharedReadProviderId)) + 
  
  geom_line(aes(y=SharedReadThroughput, colour = "Shared space read"), size=1.3) + 
  geom_point(aes(y=SharedReadThroughput, colour = "Shared space read"), size = 2) +
  geom_errorbar(width=.1, aes(ymin=SharedReadThroughput-SharedReadThroughputSd, ymax=SharedReadThroughput+SharedReadThroughputSd, colour = "Shared space read")) +
  
  geom_line(aes(y=SeparatedReadThroughput, colour = "Local space read"), size=1.3) +
  geom_point(aes(y=SeparatedReadThroughput, colour = "Local space read"), size = 2) +
  geom_errorbar(width=.1, aes(ymin=SeparatedReadThroughput-SeparatedReadThroughputSd, ymax=SeparatedReadThroughput+SeparatedReadThroughputSd, colour = "Local space read")) +
  
  geom_line(aes(y=SharedWriteThroughput, colour = "Shared space write"), size=1.3) +
  geom_point(aes(y=SharedWriteThroughput, colour = "Shared space write"), size = 2) +
  geom_errorbar(width=.1, aes(ymin=SharedWriteThroughput-SharedWriteThroughputSd, ymax=SharedWriteThroughput+SharedWriteThroughputSd, colour = "Shared space write")) +
  
  geom_line(aes(y=SeparatedWriteThroughput, colour = "Local space write"), size=1.3) +
  geom_point(aes(y=SeparatedWriteThroughput, colour = "Local space write"), size = 2) +
  geom_errorbar(width=.1, aes(ymin=SeparatedWriteThroughput-SeparatedWriteThroughputSd, ymax=SeparatedWriteThroughput+SeparatedWriteThroughputSd, colour = "Local space write")) +
  
  geom_point(aes(x=FirstReadProviderId, y=FirstReadThroughput, colour = "Shared space first read"), size = 2) +
  
  scale_colour_discrete("") +
  theme_hc(base_size = 18) +
  theme(legend.position = "right") +
  ylim(0,1000) + 
  scale_x_continuous(breaks=seq(0, 9, 1)) +
  xlab("Provider number") +
  ylab("Throughput [MB / s]")

ggsave("~/one_projects/plots/results/multiprovider_sync_overhead.pdf", width = 10, height = 6)

