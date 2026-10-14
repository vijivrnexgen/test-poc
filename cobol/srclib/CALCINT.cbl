       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.
      *----------------------------------------------------------
      * CALCULATES ONE MONTH'S INTEREST FOR AN ACCOUNT
      * RETURN CODES: 00 = OK
      *               04 = ZERO/NEGATIVE BALANCE, NO INTEREST
      *               08 = INVALID ACCOUNT TYPE
      *----------------------------------------------------------
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RATES.
           05  WS-SAVINGS-RATE     PIC V9(04) VALUE .0400.
           05  WS-CURRENT-RATE     PIC V9(04) VALUE .0050.

       LINKAGE SECTION.
           COPY ACCTCPY.
       01  LS-INTEREST             PIC S9(09)V99.
       01  LS-RETURN-CODE          PIC 9(02).

       PROCEDURE DIVISION USING ACCOUNT-RECORD
                                LS-INTEREST
                                LS-RETURN-CODE.
       0000-MAIN.
           MOVE ZERO TO LS-INTEREST
           MOVE ZERO TO LS-RETURN-CODE

           EVALUATE TRUE
               WHEN NOT SAVINGS-ACCT AND NOT CURRENT-ACCT
                   MOVE 08 TO LS-RETURN-CODE
               WHEN ACCT-BALANCE NOT > ZERO
                   MOVE 04 TO LS-RETURN-CODE
               WHEN SAVINGS-ACCT
                   COMPUTE LS-INTEREST ROUNDED =
                           ACCT-BALANCE * WS-SAVINGS-RATE / 12
               WHEN CURRENT-ACCT
                   COMPUTE LS-INTEREST ROUNDED =
                           ACCT-BALANCE * WS-CURRENT-RATE / 12
           END-EVALUATE

           GOBACK.
