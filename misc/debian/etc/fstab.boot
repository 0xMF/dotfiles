# /etc/fstab: static file system information.
#
# Use 'blkid' to print the universally unique identifier for a
# device; this may be used with UUID= as a more robust way to name devices
# that works even if disks are added and removed. See fstab(5).
#
# <file system> <mount point>   <type>  <options>       <dump>  <pass>
/dev/mapper/debian--vg-root /               ext4    errors=remount-ro 0       1
/dev/mapper/debian--vg-home /home           ext4    defaults        0       2
/dev/mapper/debian--vg-swap_1 none            swap    sw              0       0
/dev/sr0        /media/cdrom0   udf,iso9660 user,noauto     0       0
/dev/sda1	/boot ext2	defaults 0 2
