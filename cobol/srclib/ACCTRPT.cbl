       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTRPT.
      *----------------------------------------------------------
      * DISPLAYS A PROJECTED-INTEREST REPORT FOR SAMPLE ACCOUNTS.
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
       01  WS-TOTAL-BAL            PIC S9(11)V99 VALUE ZERO.
       01  WS-TOTAL-INT            PIC S9(11)V99 VALUE ZERO.

       01  DTL-LINE.
           05  DL-ACCT-NO          PIC 9(08).
           05  FILLER              PIC X(01) VALUE SPACE.
           05  DL-NAME             PIC X(25).
           05  FILLER              PIC X(01) VALUE SPACE.
           05  DL-TYPE             PIC X(01).
           05  FILLER              PIC X(01) VALUE SPACE.
           05  DL-BALANCE          PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER              PIC X(01) VALUE SPACE.
           05  DL-INTEREST         PIC ZZZ,ZZ9.99-.
           05  FILLER              PIC X(01) VALUE SPACE.
           05  DL-MSG              PIC X(10).

       01  TOT-LINE.
           05  FILLER              PIC X(36) VALUE 'TOTALS'.
           05  TL-BALANCE          PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER              PIC X(01) VALUE SPACE.
           05  TL-INTEREST         PIC ZZ,ZZZ,ZZ9.99-.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'MONTHLY INTEREST PROJECTION'
           DISPLAY 'ACCT NO  NAME                      T'
                   '      BALANCE     INTEREST'
           DISPLAY ALL '-'  UPON CONSOLE

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               MOVE WS-ROW(WS-IDX) TO ACCOUNT-RECORD

               CALL 'CALCINT' USING ACCOUNT-RECORD
                                    WS-INTEREST
                                    WS-RC

               MOVE ACCT-NO      TO DL-ACCT-NO
               MOVE ACCT-NAME    TO DL-NAME
               MOVE ACCT-TYPE    TO DL-TYPE
               MOVE ACCT-BALANCE TO DL-BALANCE
               MOVE WS-INTEREST  TO DL-INTEREST

               EVALUATE WS-RC
                   WHEN 00
                       MOVE SPACES TO DL-MSG
                       ADD ACCT-BALANCE TO WS-TOTAL-BAL
                       ADD WS-INTEREST  TO WS-TOTAL-INT
                   WHEN 04
                       MOVE 'NO INT'   TO DL-MSG
                   WHEN OTHER
                       MOVE 'BAD TYPE' TO DL-MSG
               END-EVALUATE

               DISPLAY DTL-LINE
           END-PERFORM

           MOVE WS-TOTAL-BAL TO TL-BALANCE
           MOVE WS-TOTAL-INT TO TL-INTEREST
           DISPLAY TOT-LINE
           GOBACK.
