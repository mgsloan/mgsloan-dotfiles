# Faster whole-disk encryption

sudo cryptsetup refresh \
  --perf-no_read_workqueue \
  --perf-no_write_workqueue \
  --allow-discards \
  --persistent \
  nvme0n1p7_crypt

sudo cryptsetup luksConvertKey /dev/nvme0n1p7 --pbkdf argon2id --iter-time 500

echo zstd | sudo tee /sys/module/hibernate/parameters/compressor
