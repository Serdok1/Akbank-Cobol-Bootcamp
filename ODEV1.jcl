//ODEV1J JOB 1,NOTIFY=Z95637
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=Z95637.CBL(CBL0001),DISP=SHR
//LKED.SYSLMOD DD DSN=Z95637.LOAD(CBL0001),DISP=SHR
// IF RC = 0 THEN
//DELET100 EXEC PGM=IEFBR14
//THEFILE   DD DSN=Z95637.DATA.PRT,
// DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//RUN     EXEC PGM=CBL0001
//STEPLIB   DD DSN=Z95637.LOAD,DISP=SHR
//ACCTREC   DD DSN=Z95637.DATA,DISP=SHR
//PRTLINE   DD DSN=Z95637.DATA.PRT,DISP=(NEW,CATLG,DELETE),
//          SPACE=(TRK,(150,10))
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
// ELSE
// ENDIF