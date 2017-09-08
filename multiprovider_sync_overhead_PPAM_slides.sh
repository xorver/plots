#!/bin/bash

./multiprovider_sync_overhead_PPAM_slides.R ppam_data/shared_space_filtered_read.csv \
  ppam_data/separated_space_filtered_read.csv \
  ppam_data/shared_space_filtered_write.csv \
  ppam_data/separated_space_filtered_write.csv \
  ppam_data/shared_space_first_read.csv \
  results/multiprovider_sync_overhead_slides.pdf && open results/multiprovider_sync_overhead_slides.pdf