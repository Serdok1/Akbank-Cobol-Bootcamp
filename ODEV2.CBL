       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ODEV2
       AUTHOR.        SINA EREN OZBAYRAM
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN TO PRTLINE
                       FILE STATUS IS STATUS-OUTFILE.
           SELECT USERS-DATA   ASSIGN TO USERDATA
                       FILE STATUS IS STATUS-USERDATA.
       DATA DIVISION.

       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05    ORDER-O                 PIC 9(4).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    FIRST-NAME-O            PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LAST-NAME-O             PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    BIRTH-DATE-O            PIC 9(8).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    CURRENT-DATE-O          PIC 9(8).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LIVED-DAYS-O            PIC 9(5).
       FD  USERS-DATA RECORDING MODE F.
       01  USER-FIELDS.
           05    ORDER-I                 PIC 9(4).
           05    FIRST-NAME-I            PIC X(15).
           05    LAST-NAME-I             PIC X(15).
           05    BIRTH-DATE-I            PIC X(8).
           05    CURRENT-DATE-I          PIC X(8).
       WORKING-STORAGE SECTION.
       01  HEADER.
           05 FILLER                     PIC X(4)  VALUE "ORD.".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE "FIRST NAME".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE "LAST NAME".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "BIRTHDAY".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "TODAY".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(5)  VALUE "LIVED".
       01  ERR.
           05 FILLER                     PIC X(4)  VALUE "----".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(31)
                             VALUE "ERROR. INVALID INPUT CHK SYSOUT".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "--------".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "--------".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(5)  VALUE "-----".
       01  WS-PARSED-YEARS.
           05  WS-CURRENT-DATE           PIC 9(8).
           05  WS-E-CURRENT-DATE.
               07 WS-CURRENT-YEAR        PIC 9(4).
               07 WS-CURRENT-MTH         PIC 9(2).
               07 WS-CURRENT-DAY         PIC 9(2).
           05  WS-BIRTH-DATE             PIC 9(8).
           05  WS-EBIRTH-DATE.
               07 WS-BIRTH-YEAR          PIC 9(4).
               07 WS-CURRENT-MTH         PIC 9(2).
               07 WS-CURRENT-DAY         PIC 9(2).
           05 WS-PARAMS.
               06 WS-PARAM-DATE          PIC X(8).
               06 WS-E-PARAM-DATE.
                   07 WS-PARAM-YEAR      PIC 9(4).
                   07 WS-PARAM-MTH       PIC 9(2).
                       88 THURTY-ONE              VALUE 1 3 5 7 8 10 12.
                       88 FEB                     VALUE 2.
                       88 THURTY                  VALUE 4 6 9 11.
                   07 WS-PARAM-DAY       PIC 9(2).
       01  WS-DAYS-LIVED                 PIC 9(5).
       01  WS-LEAP-YEAR.
           05  WS-RULE1                  PIC 9(1).
           05  WS-RULE2                  PIC 9(2).
           05  WS-RULE3                  PIC 9(3).
       01  WS-TMP                        PIC 9(8).
       01  WS-FLAGS.
           05 STATUS-OUTFILE             PIC 99.
              88 OUTFILE-SUCC            VALUE 00 97.
           05 STATUS-USERDATA            PIC 99.
              88 USERDATA-SUCC           VALUE 00 97.
              88 ST-EOF                  VALUE 10.
           05 WS-VALID-ORDER             PIC 9(1) VALUE 1.
       01  READ-CNT                      PIC 9(11) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0110-FILE-CONTROL.
           PERFORM 0200-PROCESS-FILE.
           PERFORM 0800-CLOSE-FILES.
           PERFORM 0999-EXIT.
       0100-OPEN-FILES.
           OPEN INPUT  USERS-DATA.
           OPEN OUTPUT OUT-FILE.
       0110-FILE-CONTROL.
           IF NOT USERDATA-SUCC
              DISPLAY "Userdata file not found. Exiting..."
              STOP RUN
           END-IF.
           IF NOT OUTFILE-SUCC
              DISPLAY "Output file not found. Exiting..."
              STOP RUN
           END-IF.
       0200-PROCESS-FILE.
           PERFORM 0210-HEADER-PRINT.
           PERFORM 0250-READ-RECORD
           PERFORM UNTIL ST-EOF
               IF WS-VALID-ORDER = 1
                   PERFORM 0300-CALCULATE-DAYS
               END-IF
                   PERFORM 0400-WRITE-RECORD
               PERFORM 0250-READ-RECORD
           END-PERFORM.
       0210-HEADER-PRINT.
           MOVE HEADER TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
       0250-READ-RECORD.
           READ USERS-DATA
           MOVE BIRTH-DATE-I TO WS-PARAM-DATE.
           ADD 1 TO READ-CNT
           PERFORM 0260-INPUT-VALIDATOR.
           IF WS-VALID-ORDER = 1
               MOVE CURRENT-DATE-I TO WS-PARAM-DATE
               PERFORM 0260-INPUT-VALIDATOR
           END-IF.
       0260-INPUT-VALIDATOR.
           IF WS-PARAM-DATE  NOT NUMERIC
               DISPLAY WS-PARAM-DATE ": Invalid date for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           ELSE
               MOVE WS-PARAM-DATE TO WS-E-PARAM-DATE
               PERFORM 0270-YEAR-VALIDATOR
               IF WS-VALID-ORDER = 1
                   PERFORM 0280-MONTH-VALIDATOR
               END-IF
               IF WS-VALID-ORDER = 1
                   PERFORM 0290-DAY-VALIDATOR
               END-IF
           END-IF.
       0270-YEAR-VALIDATOR.
           IF (WS-PARAM-YEAR < 1601)
               DISPLAY WS-PARAM-YEAR ": Invalid year for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
       0280-MONTH-VALIDATOR.
           IF (WS-PARAM-MTH < 1 OR WS-PARAM-MTH > 12)
               DISPLAY WS-PARAM-MTH ": Invalid month for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
       0290-DAY-VALIDATOR.
           IF THURTY-ONE
                IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 31)
                     DISPLAY WS-PARAM-DAY ": Invalid day for order "
                             ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                END-IF
           ELSE IF FEB
               DIVIDE 4 INTO WS-PARAM-YEAR   GIVING WS-TMP
                    REMAINDER WS-RULE1
               DIVIDE 100 INTO WS-PARAM-YEAR GIVING WS-TMP
                    REMAINDER WS-RULE2
               DIVIDE 400 INTO WS-PARAM-YEAR GIVING WS-TMP
                    REMAINDER WS-RULE3
               IF ((WS-RULE1 = 0 AND WS-RULE2 NOT = 0) OR WS-RULE3 = 0)
                   IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 29)
                       DISPLAY WS-PARAM-DAY ": Invalid day for order "
                               ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                   END-IF
               ELSE
                   IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 28)
                       DISPLAY WS-PARAM-DAY ": Invalid day for order "
                               ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                   END-IF
               END-IF
           ELSE IF THURTY
               IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 30)
                   DISPLAY WS-PARAM-DAY ": Invalid day for order "
                           ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
               END-IF
           END-IF.
       0300-CALCULATE-DAYS.
           MOVE BIRTH-DATE-I TO WS-BIRTH-DATE.
           COMPUTE WS-BIRTH-DATE =
           FUNCTION INTEGER-OF-DATE(WS-BIRTH-DATE).
           MOVE CURRENT-DATE-I TO WS-CURRENT-DATE.
           COMPUTE WS-CURRENT-DATE  =
           FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE).
           MOVE WS-CURRENT-DATE TO WS-E-CURRENT-DATE.
           MOVE WS-BIRTH-DATE TO WS-EBIRTH-DATE.
           IF (WS-CURRENT-YEAR - WS-BIRTH-YEAR > 270)
               DISPLAY WS-CURRENT-YEAR " " WS-BIRTH-YEAR
                       ": Invalid age for order " ORDER-I
                       " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
           IF (WS-EBIRTH-DATE > WS-E-CURRENT-DATE)
               DISPLAY WS-DAYS-LIVED ": Invalid dates for order "
                       ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           ELSE
              COMPUTE WS-DAYS-LIVED = WS-CURRENT-DATE - WS-BIRTH-DATE
           END-IF.
       0400-WRITE-RECORD.
           INITIALIZE OUT-REC
           IF WS-VALID-ORDER = 1
               MOVE SPACES  TO OUT-REC
               MOVE ORDER-I TO ORDER-O
               MOVE FIRST-NAME-I TO FIRST-NAME-O
               MOVE LAST-NAME-I TO LAST-NAME-O
               MOVE BIRTH-DATE-I TO BIRTH-DATE-O
               MOVE CURRENT-DATE-I TO CURRENT-DATE-O
               MOVE WS-DAYS-LIVED  TO LIVED-DAYS-O
           ELSE
               MOVE ERR TO OUT-REC
               MOVE 1 TO WS-VALID-ORDER
           END-IF.
           WRITE OUT-REC.
       0800-CLOSE-FILES.
           CLOSE USERS-DATA.
           CLOSE OUT-FILE.
       0999-EXIT.
           STOP RUN.
