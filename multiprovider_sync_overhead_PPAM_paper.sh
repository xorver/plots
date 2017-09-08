#!/bin/bash

./multiprovider_sync_overhead_PPAM_paper.R ppam_data/shared_space_filtered_read.csv \
  ppam_data/separated_space_filtered_read.csv \
  ppam_data/shared_space_filtered_write.csv \
  ppam_data/separated_space_filtered_write.csv \
  ppam_data/shared_space_first_read.csv \
  results/multiprovider_sync_overhead.pdf && open results/multiprovider_sync_overhead.pdf