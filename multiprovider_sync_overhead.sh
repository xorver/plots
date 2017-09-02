#!/bin/bash

./multiprovider_sync_overhead.R fgcs_data/shared_space_filtered_read.csv \
  fgcs_data/separated_space_filtered_read.csv \
  fgcs_data/shared_space_filtered_write.csv \
  fgcs_data/separated_space_filtered_write.csv \
  fgcs_data/shared_space_first_read.csv \
  results/multiprovider_sync_overhead.pdf && open results/multiprovider_sync_overhead.pdf