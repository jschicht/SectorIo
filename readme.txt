All credits to http://www.codeproject.com/Articles/28314/Reading-and-Writing-to-Raw-Disk-Sectors

This kernel mode driver is simply reading or writing to sectors on the underlying physical disk.

Did 2 changes to the original driver.

1. Added support for reading/writing of arbitrary number of sectors (was hardcoded to 1).
2. Added SL_FORCE_DIRECT_WRITE to the flag in Irp.

To use this driver access it by opening a handle to \\.\sectorio
Then send the driver IOCTL code 0x8000E000 for reading or 0x8000E004 for writing.

Send this struct with DeviceIoControl:

typedef struct _DISK_LOCATION {
	BOOLEAN						bIsRawDiskObj;
	DWORD						dwDiskOrdinal;
	ULONGLONG					ullSectorNum;
} DISK_LOCATION, * PDISK_LOCATION;

* bIsRawDiskObj must always be 1.
* dwDiskOrdinal is the disk number, like 0 for \\.\PhysicalDrive0.
* ullSectorNum is the target sector number to start reading/writing.

Create a buffer containing DISK_LOCATION struct and some space for the data to read/write. So, for writing 2 sectors to disk, the input buffer to DeviceIoControl would have to be 1037 bytes.

On Windows nt6.x (Vista and later) new security measures was introduced that prevented writing to the physical disk in various scenarios. On nt5.x (XP, Server 2003) and earlier, this was not an issue as you could write to anywhere you would on disk. The SL_FORCE_DIRECT_WRITE flag in a kernel mode driver, bypasses that block. The limitation however, is for 64-bit Windows where driver signing has been enforced, effectively preventing unsigned/untrusted drivers from being loaded. The workaround is to boot the system with TESTSIGNING ON in BCD, unless you cracked PatchGuard. 

The driver is currently used in:
 SetMace
 HideAndProtect
 StegoMft

The other way of the writing to physical disk within volume space on nt6.x is to just dismount the volume by sending FSCTL_DISMOUNT_VOLUME, 0x00090020, and then write the data with WriteFile function. This has limitations though.