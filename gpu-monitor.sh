#! /bin/bash
watch -n 0.1 nvidia-smi --query-gpu=pstate,power.draw,utilization.gpu,utilization.memory --format=csv
