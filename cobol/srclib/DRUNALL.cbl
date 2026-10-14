       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRUNALL.
      *----------------------------------------------------------
      * WRAPPER: DYNAMICALLY CALLS THE POSTING AND REPORT PROGRAMS
      *----------------------------------------------------------
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PGM-NAMES.
           05  WS-POST-PGM         PIC X(08) VALUE 'INTPOST'.
           05  WS-RPT-PGM          PIC X(08) VALUE 'ACCTRPT'.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY '=== STEP 1: INTEREST POSTING ==='
           CALL WS-POST-PGM
               ON EXCEPTION
                   DISPLAY 'RUNALL: CANNOT LOAD ' WS-POST-PGM
                   MOVE 16 TO RETURN-CODE
                   STOP RUN
           END-CALL
           CANCEL WS-POST-PGM

           DISPLAY ' '
           DISPLAY '=== STEP 2: INTEREST REPORT ==='
           CALL WS-RPT-PGM
               ON EXCEPTION
                   DISPLAY 'RUNALL: CANNOT LOAD ' WS-RPT-PGM
                   MOVE 16 TO RETURN-CODE
                   STOP RUN
           END-CALL
           CANCEL WS-RPT-PGM

           DISPLAY ' '
           DISPLAY '=== ALL PROGRAMS COMPLETED ==='
           STOP RUN.
