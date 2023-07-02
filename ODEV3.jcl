//JVSAMEX2 JOB 1,NOTIFY=Z95637
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=Z95637.CBL(VSAMEX02),DISP=SHR
//LKED.SYSLMOD DD DSN=Z95637.LOAD(VSAMEX02),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//DELETPRT  EXEC PGM=IEFBR14
//FILE01    DD DSN=Z95637.QSAM.PRT,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//RUN       EXEC PGM=VSAMEX02
//STEPLIB   DD DSN=Z95637.LOAD,DISP=SHR
//IDXFILE   DD DSN=Z95637.VSAM.AA,DISP=SHR
//INPFILE  DD DSN=Z95637.QSAM.CC,DISP=SHR
//OUTFILE   DD DSN=Z95637.QSAM.PRT,DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=61,BLKSIZE=0),UNIT=3390
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
