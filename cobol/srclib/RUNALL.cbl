       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNALL.
      *----------------------------------------------------------
      * WRAPPER: RUNS THE INTEREST POSTING, THEN THE REPORT
      *----------------------------------------------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY '=== STEP 1: INTEREST POSTING ==='
           CALL 'INTPOST'

           DISPLAY ' '
           DISPLAY '=== STEP 2: INTEREST REPORT ==='
           CALL 'ACCTRPT'

           DISPLAY ' '
           DISPLAY '=== ALL PROGRAMS COMPLETED ==='
           STOP RUN.
