       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTPOST.
      *----------------------------------------------------------
      * LOADS SAMPLE ACCOUNTS FROM A TABLE, CALLS CALCINT AND
      * SHOWS BALANCE BEFORE AND AFTER POSTING INTEREST.
      *----------------------------------------------------------
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY ACCTCPY.

       01  WS-SAMPLE-DATA.
           05  FILLER PIC X(45) VALUE
               '10000001RAMESH KUMAR             S00001250000'.
           05  FILLER PIC X(45) VALUE
               '10000002PRIYA SHARMA             C00000480050'.
           05  FILLER PIC X(45) VALUE
               '10000003ANITA DESAI              S00000000000'.
           05  FILLER PIC X(45) VALUE
               '10000004JOHN MATHEW              X00000100000'.
       01  WS-SAMPLE-TABLE REDEFINES WS-SAMPLE-DATA.
           05  WS-ROW              PIC X(45) OCCURS 4 TIMES.

       01  WS-IDX                  PIC 9(02).
       01  WS-INTEREST             PIC S9(09)V99.
       01  WS-RC                   PIC 9(02).
       01  WS-OLD-BAL              PIC S9(09)V99.
       01  WS-TOTAL-INTEREST       PIC S9(11)V99 VALUE ZERO.
       01  WS-EDIT-AMT             PIC ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-NEW             PIC ZZZ,ZZZ,ZZ9.99-.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               MOVE WS-ROW(WS-IDX) TO ACCOUNT-RECORD
               MOVE ACCT-BALANCE   TO WS-OLD-BAL

               CALL 'CALCINT' USING ACCOUNT-RECORD
                                    WS-INTEREST
                                    WS-RC

               EVALUATE WS-RC
                   WHEN 00
                       ADD WS-INTEREST TO ACCT-BALANCE
                       ADD WS-INTEREST TO WS-TOTAL-INTEREST
                       MOVE WS-OLD-BAL   TO WS-EDIT-AMT
                       MOVE ACCT-BALANCE TO WS-EDIT-NEW
                       DISPLAY ACCT-NO ' ' ACCT-NAME
                               ' OLD=' WS-EDIT-AMT
                               ' NEW=' WS-EDIT-NEW
                   WHEN 04
                       DISPLAY ACCT-NO ' ' ACCT-NAME
                               ' NO INTEREST (ZERO BALANCE)'
                   WHEN OTHER
                       DISPLAY ACCT-NO ' ' ACCT-NAME
                               ' INVALID ACCOUNT TYPE'
               END-EVALUATE
           END-PERFORM

           MOVE WS-TOTAL-INTEREST TO WS-EDIT-AMT
           DISPLAY 'TOTAL INTEREST POSTED: ' WS-EDIT-AMT
           GOBACK.
