/*++
Module Name:

    sector.c

Abstract:

    This driver performs RAW reads/writes to the sectors (RAW disk or partitions)

Environment:

    kernel mode only

Notes:

--*/


#define INITGUID

#include "ntddk.h"
#include "ntdddisk.h"
#include "stdarg.h"
#include "stdio.h"
#include <ntddvol.h>

#include <mountdev.h>
#include "wmistr.h"
#include "wmidata.h"
#include "wmiguid.h"
#include "wmilib.h"
#include "sector.h"



WCHAR				g_szDeviceName[] = L"\\Device\\sectorio";
WCHAR				g_szDosDeviceName[] = L"\\DosDevices\\sectorio";
UNICODE_STRING		szDeviceName;
UNICODE_STRING		szDosDeviceName;

PDEVICE_OBJECT	gp_DevObj = NULL;

#pragma alloc_text(INIT, DriverEntry)

NTSTATUS GetGeometry(PDEVICE_OBJECT pDiskDevObj, PDISK_GEOMETRY pDiskGeo)
/*++

Routine Description:

   Returns the Geometry of Disk

Arguments:

   Target Device Object representing the disk and Pointer to geometry structure

Return Value:

    STATUS

--*/
{
	IO_STATUS_BLOCK		IoStatusBlock;
	KEVENT				Event;
	PIRP				pIrp;
	NTSTATUS			status;

	KeInitializeEvent(&Event, NotificationEvent, FALSE);

	pIrp = IoBuildDeviceIoControlRequest(IOCTL_DISK_GET_DRIVE_GEOMETRY,
								  pDiskDevObj, NULL, 0, pDiskGeo,
								  sizeof(DISK_GEOMETRY), FALSE, &Event,
								  &IoStatusBlock);
	
	if (!pIrp) 
	{
		return STATUS_INSUFFICIENT_RESOURCES;
	}

	status = IoCallDriver(pDiskDevObj, pIrp);

	if (status == STATUS_PENDING) 
	{
			KeWaitForSingleObject(&Event, Executive, KernelMode, FALSE,	NULL);
			status = IoStatusBlock.Status;
	}

	return status;
}


NTSTATUS
GetAllDiskObjects()
/*++

Routine Description:

   Enumeration all disk devices

Arguments:

   None

Return Value:

    None

--*/
{
    NTSTATUS		Status;
    PDRIVER_OBJECT	pDiskObject;
    PDEVICE_OBJECT	pDeviceObjectTemp;
    UNICODE_STRING	DestinationString;
    DWORD			dwDeviceNumber;
    DWORD			dwRetLength;
    POBJECT_NAME_INFORMATION pNameBuffer;
    WCHAR			*pNameTemp;
	PDISK_OBJ		pDisk;
	PDISK_GEOMETRY	pDiskGeometry;
	BOOLEAN			bIsFound = FALSE;
	PDEVICE_EXTENSION	pDevExtn = (PDEVICE_EXTENSION)gp_DevObj->DeviceExtension;

    /* All Disk Objects are created by disk.sys driver*/
	RtlInitUnicodeString(&DestinationString, L"\\Driver\\Disk");
    
	// Not a documented function in DDK, see import definition in sector.h
	if (ObReferenceObjectByName(&DestinationString, 64, 0, 0,
								*IoDriverObjectType, KernelMode, 0,
								&pDiskObject) >= 0) 
	{
        pDeviceObjectTemp = pDiskObject->DeviceObject;
        dwDeviceNumber = 0;
        
		if (pDeviceObjectTemp)
        {
			pDiskGeometry = ExAllocatePool(NonPagedPool, sizeof(DISK_GEOMETRY));
            
			if (!pDiskGeometry) 
			{
                       return STATUS_INSUFFICIENT_RESOURCES;
            }

            do
            {
				//Each time memset the geometry structure to zero
				memset(pDiskGeometry, 0x00, sizeof(DISK_GEOMETRY));

                // DeviceType 7 corresponds to FILE_DISK_DEVICE Type Device Object and
				// It should have name too that's why Flags is check for 0x40 (DO_DEVICE_HAS_NAME )
				//DbgPrint("DeviceType: %d", pDeviceObjectTemp->DeviceType);
				if (pDeviceObjectTemp->DeviceType == 7
                        && (pDeviceObjectTemp->Flags & 0x40))
                {
                   ObQueryNameString(pDeviceObjectTemp, NULL, 0, &dwRetLength);

                   pNameBuffer = (POBJECT_NAME_INFORMATION)
									ExAllocatePoolWithTag(PagedPool, dwRetLength, ' sFI');
                   
				   if (!pNameBuffer)
                   {
					   ExFreePool(pDiskGeometry);
                       return STATUS_INSUFFICIENT_RESOURCES;
                   }
                   
				   if (ObQueryNameString(pDeviceObjectTemp, pNameBuffer,
										 dwRetLength, &dwRetLength) == STATUS_SUCCESS 
										 && pNameBuffer->Name.Buffer)
                   {
					   //DbgPrint("pNameBuffer->Name.Buffer: %ws", pNameBuffer->Name.Buffer);
						pDisk  = ExAllocatePool(PagedPool, sizeof(DISK_OBJ));
						
						if (!pDisk) 
						{
									ExFreePool(pDiskGeometry);
									ExFreePool(pNameBuffer);
									return STATUS_INSUFFICIENT_RESOURCES;
						}
                       
						for (pNameTemp = pNameBuffer->Name.Buffer +
							 wcslen(pNameBuffer->Name.Buffer); 
							 pNameTemp > pNameBuffer->Name.Buffer; pNameTemp--) 
						{
							//DbgPrint("pNameTemp: %ws", pNameTemp);
									if (!_wcsnicmp(pNameTemp, L"\\DR", 3)) 
									{
										pDisk->bIsRawDiskObj = TRUE;
										bIsFound = TRUE;
										break;
									}
									else if (!_wcsnicmp(pNameTemp, L"\\DP(", 4)) 
									{
										pDisk->bIsRawDiskObj = FALSE;
										bIsFound = TRUE;
										break;
									}
						}
						if (bIsFound) 
						{
							pDisk->dwDiskOrdinal = (USHORT)pNameBuffer->
													Name.Buffer[wcslen(pNameBuffer->Name.Buffer)-1] 
													- (USHORT) L'0';
							pDisk->pDiskDevObj	= pDeviceObjectTemp;

							ExInterlockedInsertTailList(&pDevExtn->list_head, &pDisk->list, &pDevExtn->list_lock);
							
							Status = GetGeometry(pDisk->pDiskDevObj, pDiskGeometry);

							if (!NT_SUCCESS(Status)) 
							{
								pDisk->bGeometryFound = FALSE;
							} 
							else 
							{
								pDisk->bGeometryFound = TRUE;
								pDisk->ulSectorSize = pDiskGeometry->BytesPerSector;
							}
			
						} //end of if (bIsFound) 
                   }//end of if (ObQueryNameString ...)
                   ExFreePoolWithTag(pNameBuffer, 0);

                }//end of if (pDeviceObjectTemp->DeviceType == 7 ...)
                pDeviceObjectTemp = pDeviceObjectTemp->NextDevice;
            } while (pDeviceObjectTemp); // end of while
			ExFreePool(pDiskGeometry); //Free pDiskGeometry
        }
    }
	return STATUS_SUCCESS;
}

NTSTATUS DriverIoDeviceDispatchRoutine(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp)
{
    PIO_STACK_LOCATION pIsl;
	PIO_STACK_LOCATION IrpSp;
    NTSTATUS status = STATUS_UNSUCCESSFUL;
    ULONG BuffSize, MajorFunc, IoCtl, BuffSize2;
	ULONG InputBuffLen, OutputBuffLen, InputBuffLen2, OutputBuffLen2;
	PVOID pBuff, pData;
	KIRQL OldIrql;
	LARGE_INTEGER lDiskOffset;
	PIRP  pIrp;
	KEVENT Event;
	IO_STATUS_BLOCK ioStatus;
	PDISK_OBJ pDiskObj = NULL;
	PDISK_LOCATION pDiskLoc;
	PDEVICE_EXTENSION pDevExtn = (PDEVICE_EXTENSION)DeviceObject->DeviceExtension;
	PLIST_ENTRY pList = &pDevExtn->list_head;
   	pList = pList->Flink;

	DbgPrint("SectorIo: DriverIoDeviceDispatchRoutine\n");
	
	pDiskLoc = (PDISK_LOCATION)Irp->AssociatedIrp.SystemBuffer;

	while (pList != &pDevExtn->list_head)
	{
		pDiskObj = (PDISK_OBJ) pList;
		if (pDiskObj->bGeometryFound)
		{
			if (pDiskObj->bIsRawDiskObj == pDiskLoc->bIsRawDiskObj)
			{
				if (pDiskObj->dwDiskOrdinal == pDiskLoc->dwDiskOrdinal)
				{
					break;
				}
			}
		}
		pList = pList->Flink;
	}
		
	if (pList == &pDevExtn->list_head)
	{
			status = STATUS_DEVICE_NOT_CONNECTED;
			goto end;
	}
	//DbgPrint("SectorIo: ulSectorSize 0x%08X\n",pDiskObj->ulSectorSize);

    pIsl = IoGetCurrentIrpStackLocation (Irp);
	//DbgPrint("SectorIo: OutputBufferLength 0x%08X\n",pIsl->Parameters.DeviceIoControl.OutputBufferLength);
	//DbgPrint("SectorIo: InputBufferLength 0x%08X\n",pIsl->Parameters.DeviceIoControl.InputBufferLength);

	BuffSize = pIsl->Parameters.DeviceIoControl.OutputBufferLength; //For obtaining the sector size
	//OutputBuffLen = (pDiskObj->ulSectorSize); // By default output size is sector size
	OutputBuffLen = pIsl->Parameters.DeviceIoControl.OutputBufferLength;
	InputBuffLen = sizeof(DISK_LOCATION);
	InputBuffLen2 = pIsl->Parameters.DeviceIoControl.InputBufferLength - sizeof(DISK_LOCATION);
	OutputBuffLen2 = pIsl->Parameters.DeviceIoControl.OutputBufferLength;
	//DbgPrint("SectorIo: InputBuffLen2 0x%08X\n",InputBuffLen2);
	//DbgPrint("SectorIo: OutputBuffLen2 0x%08X\n",OutputBuffLen2);
	pBuff = Irp->AssociatedIrp.SystemBuffer;


	Irp->IoStatus.Information = 0;

	IoCtl = pIsl->Parameters.DeviceIoControl.IoControlCode; 
	switch(IoCtl)
	{
	case	IOCTL_GET_SECTOR_SIZE:
		DbgPrint("SectorIo: IOCTL_GET_SECTOR_SIZE 0x%08X\n",IOCTL_GET_SECTOR_SIZE);
		if (BuffSize >= sizeof(ULONG))
		{
			*(PULONG) pBuff = pDiskObj->ulSectorSize;
			Irp->IoStatus.Information = sizeof(ULONG);
			status = STATUS_SUCCESS;
		} 
		else 
			status = STATUS_INFO_LENGTH_MISMATCH;

		break;

	case	IOCTL_SECTOR_WRITE:
		DbgPrint("SectorIo: IOCTL_SECTOR_WRITE 0x%08X\n",IOCTL_SECTOR_WRITE);
		
		(ULONG) pBuff = ((ULONG) pBuff) + sizeof(DISK_LOCATION); // To accomodate the very ugly hack of transferring write
															   // information and input buffer into the same buffer
		InputBuffLen = InputBuffLen + (pDiskObj->ulSectorSize); // Inputbufflen shud be DISK_LOCATION + sectorsize as we
																// are recieving data in the same buffer
		OutputBuffLen = 0; //Outputbufflen for write operations shud be zero
		

	case	IOCTL_SECTOR_READ:
		DbgPrint("SectorIo: IOCTL_SECTOR_READ 0x%08X\n",IOCTL_SECTOR_READ);
		if (InputBuffLen > pIsl->Parameters.DeviceIoControl.InputBufferLength)
		{
			status = STATUS_INFO_LENGTH_MISMATCH;
			goto end;
		}
		if (OutputBuffLen > pIsl->Parameters.DeviceIoControl.OutputBufferLength)
		{
			status = STATUS_INFO_LENGTH_MISMATCH;
			goto end;
		}

		MajorFunc  = (IoCtl==IOCTL_SECTOR_READ) ? IRP_MJ_READ : IRP_MJ_WRITE;
		BuffSize2 = (IoCtl==IOCTL_SECTOR_READ) ? OutputBuffLen2 : InputBuffLen2;
		//DbgPrint("SectorIo: BuffSize2 0x%08X\n",BuffSize2);
		
		lDiskOffset.QuadPart = (pDiskObj->ulSectorSize) * (pDiskLoc->ullSectorNum);
		KeInitializeEvent(&Event, NotificationEvent, FALSE);
		pIrp = IoBuildSynchronousFsdRequest(MajorFunc, pDiskObj->pDiskDevObj, pBuff, 
											BuffSize2,/*pDiskObj->ulSectorSize,*/ &lDiskOffset, 
											&Event, &ioStatus);
		
		if (!pIrp) {
			status = STATUS_INSUFFICIENT_RESOURCES;
			goto end;
		}
		DbgPrint("SectorIo: IoBuildSynchronousFsdRequest success\n");
		
        if(IoCtl != IOCTL_SECTOR_READ) {
            IrpSp = IoGetNextIrpStackLocation(pIrp);
            //IrpSp->Flags = IrpSp->Flags | SL_FORCE_DIRECT_WRITE;
			IrpSp->Flags |= SL_FORCE_DIRECT_WRITE;
        }
		
		status = IoCallDriver(pDiskObj->pDiskDevObj, pIrp);
		
		if (status == STATUS_PENDING) 
		{
			KeWaitForSingleObject(&Event, Executive, KernelMode, FALSE,	NULL);
			status = ioStatus.Status;

			if (NT_SUCCESS(status))
			{
				//Irp->IoStatus.Information = pDiskObj->ulSectorSize;
				Irp->IoStatus.Information = BuffSize2;
			}
		}
		DbgPrint("SectorIo: IoCallDriver: 0x%08X\n",status);
		break;
	}   

end:
    Irp->IoStatus.Status = status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return status;
}

NTSTATUS DriverDefaultIrpHandler(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp)
{
    Irp->IoStatus.Information = 0;
    Irp->IoStatus.Status = STATUS_SUCCESS;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return STATUS_SUCCESS;
}

NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT	pDriverObject,
    IN PUNICODE_STRING	pRegistryPath
    )
{

    ULONG               ulIndex;
	unsigned long		i;
    //PDRIVER_DISPATCH  * dispatch;
	//UNICODE_STRING		diskdevice;
	//PFILE_OBJECT		pFileObj = NULL;
	//PDEVICE_OBJECT		pDevObj  = NULL;
	NTSTATUS			status;
	PDEVICE_EXTENSION	pDevExtn = NULL;
	//CHAR				*sBuf;
	//LARGE_INTEGER		lDiskOffset;
	//KEVENT				Event;
	//IO_STATUS_BLOCK		ioStatus;
	//PIRP				pIrp = NULL;
	//SIZE_T				size = 512;
	DbgPrint("SectorIo: DriverEntry\r\n");
    //RtlInitUnicodeString(&diskdevice, L"\\Device\\Harddisk0\\DR0");
	RtlInitUnicodeString(&szDeviceName, g_szDeviceName);
	RtlInitUnicodeString(&szDosDeviceName, g_szDosDeviceName);
	//status = IoGetDeviceObjectPointer(&diskdevice, FILE_ALL_ACCESS, &pFileObj, &pDevObj);
	
	status = IoCreateDevice(pDriverObject, sizeof(DEVICE_EXTENSION), &szDeviceName, FILE_DEVICE_UNKNOWN, 0, /*TRUE*/FALSE, &gp_DevObj);
	
	if (!NT_SUCCESS(status)) 
		return status;

	status = IoCreateSymbolicLink(&szDosDeviceName, &szDeviceName);

	if (!NT_SUCCESS(status)) 
	{
		IoDeleteDevice(gp_DevObj);
		return status;
	}
	
	pDevExtn = (PDEVICE_EXTENSION)gp_DevObj->DeviceExtension;

	InitializeListHead(&pDevExtn->list_head);
    KeInitializeSpinLock(&pDevExtn->list_lock);

	status = GetAllDiskObjects();

	if (!NT_SUCCESS(status)) 
	{
		IoDeleteDevice(gp_DevObj);
		IoDeleteSymbolicLink(&szDosDeviceName);
		return status;
	}

	for (i= 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++)
    {
        pDriverObject->MajorFunction[i] = DriverDefaultIrpHandler;
    }
	
	pDriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DriverIoDeviceDispatchRoutine;
	
	pDriverObject->DriverUnload = DriverUnload;

	/*if (!NT_SUCCESS(status)) {
		DbgPrint("IoGetDeviceObjectPointer Failed\n");
	} else {
		DbgPrint("IoGetDeviceObjectPointer Succceded");
		lDiskOffset.QuadPart = (1169944+63)*512;
		sBuf = ExAllocatePool(NonPagedPool, size);
		
		if (!sBuf) {
			ObDereferenceObject(pFileObj);
			return STATUS_INSUFFICIENT_RESOURCES;
		}
		KeInitializeEvent(&Event, NotificationEvent, FALSE);
		memset(sBuf, 'C', size);
		pIrp = IoBuildSynchronousFsdRequest(IRP_MJ_WRITE, pDevObj, sBuf, size, &lDiskOffset, &Event, &ioStatus);
		
		
		if (!pIrp) {
			ObDereferenceObject(pFileObj);
			ExFreePool(sBuf);
			return STATUS_INSUFFICIENT_RESOURCES;
		}
		
		status = IoCallDriver(pDevObj, pIrp);

		if (status == STATUS_PENDING) {
			KeWaitForSingleObject(&Event, Executive, KernelMode, FALSE,	NULL);
			status = ioStatus.Status;
		}
		ExFreePool(sBuf);
		ObDereferenceObject(pFileObj);
	}*/
	
    return(status);

} // end DriverEntry()

VOID
DriverUnload(
    IN PDRIVER_OBJECT pDriverObject
    )
{
	PDEVICE_EXTENSION pDevExtn = (PDEVICE_EXTENSION)gp_DevObj->DeviceExtension;
	PLIST_ENTRY pList = &pDevExtn->list_head;
	PVOID		pObj;

	pList = pList->Flink;
	DbgPrint("SectorIo: DriverUnload\n");

	while(pList != &pDevExtn->list_head) 
	{
		pObj = (PVOID) pList;
		pList = pList->Flink;
		ExFreePool(pObj);
	}

	IoDeleteSymbolicLink(&szDosDeviceName);
    IoDeleteDevice(gp_DevObj);
	
	return ;
}
