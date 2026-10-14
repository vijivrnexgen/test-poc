       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CUSTPGM.
       AUTHOR.        VRNEXGEN1.
      *================================================================*
      * PROGRAM   : CUSTPGM                                            *
      * TRANSID   : CUST                                               *
      * MAPSET    : CUSTMAP        MAP : CUSTM01                       *
      * FILE      : CUSTFILE  ->  VRX.CICS.CUSTOMER.KSDS (KEY CUSID)   *
      *                                                                *
      * FUNCTION  : CUSTOMER MAINTENANCE - PSEUDO-CONVERSATIONAL CRUD  *
      *             ENTER = VALIDATE INPUT (NO FILE I/O)               *
      *             F4    = INSERT  (WRITE)                            *
      *             F5    = READ    (READ)                             *
      *             F6    = UPDATE  (READ UPDATE + REWRITE)            *
      *             F7    = DELETE  (READ, CONFIRM, READ UPDATE +      *
      *                              DELETE)                           *
      *             F3    = EXIT    CLEAR = RESET SCREEN               *
      *                                                                *
      * COMMAREA  : CARRIES THE LAST KEY READ/WRITTEN, AN IMAGE OF THE *
      *             RECORD AS DISPLAYED (OPTIMISTIC LOCK CHECK ON      *
      *             UPDATE/DELETE) AND THE DELETE-CONFIRM FLAG.        *
      *                                                                *
      * ERRORS    : EVERY FILE/BMS COMMAND USES RESP/RESP2. EXPECTED   *
      *             CONDITIONS (NOTFND, DUPREC, MAPFAIL) ARE HANDLED   *
      *             IN LINE; ENVIRONMENT CONDITIONS (NOTOPEN,          *
      *             DISABLED, FILENOTFOUND, IOERR, LENGERR, INVREQ,    *
      *             ILLOGIC, NOSPACE, NOTAUTH) IN 8000-FILE-ERROR;     *
      *             ANYTHING ELSE IN 8900-UNEXPECTED-ERROR, WHICH      *
      *             SHOWS RESP/RESP2 AND LOGS TO TD QUEUE CSSL.        *
      *                                                                *
      * MAINTENANCE LOG                                                *
      * DATE       BY        DESCRIPTION                               *
      * ---------- --------- ----------------------------------------- *
      * 2026-09-25 VRNEXGEN1 INITIAL VERSION                           *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS VALID-KEY-CHAR   IS 'A' THRU 'I' 'J' THRU 'R'
                                     'S' THRU 'Z' '0' THRU '9'
           CLASS VALID-EMAIL-CHAR IS 'A' THRU 'I' 'J' THRU 'R'
                                     'S' THRU 'Z' 'a' THRU 'i'
                                     'j' THRU 'r' 's' THRU 'z'
                                     '0' THRU '9' '.' '_' '-'
                                     '@' '+'.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * CONSTANTS                                                      *
      *----------------------------------------------------------------*
       01  WS-CONSTANTS.
           05  WS-PGM-NAME             PIC X(08) VALUE 'CUSTPGM'.
           05  WS-END-MESSAGE          PIC X(40) VALUE
               'CUSTOMER MAINTENANCE SESSION ENDED.'.

      *----------------------------------------------------------------*
      * CICS RESPONSE FIELDS                                           *
      *----------------------------------------------------------------*
       01  WS-CICS-RESPONSE.
           05  WS-RESP                 PIC S9(08) COMP VALUE ZERO.
           05  WS-RESP2                PIC S9(08) COMP VALUE ZERO.
           05  WS-AUX-RESP             PIC S9(08) COMP VALUE ZERO.
           05  WS-RESP-DISP            PIC 9(04)       VALUE ZERO.
           05  WS-RESP2-DISP           PIC 9(04)       VALUE ZERO.
           05  WS-FILE-OP              PIC X(07)       VALUE SPACES.
           05  WS-ERR-TEXT             PIC X(45)       VALUE SPACES.

      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-VALID-SW             PIC X(01) VALUE 'Y'.
               88  INPUT-VALID                   VALUE 'Y'.
               88  INPUT-INVALID                 VALUE 'N'.
           05  WS-RECEIVE-SW           PIC X(01) VALUE 'Y'.
               88  RECEIVE-OK                    VALUE 'Y'.
               88  RECEIVE-FAILED                VALUE 'N'.
           05  WS-PREV-DEL-PENDING     PIC X(01) VALUE 'N'.
               88  DELETE-WAS-PENDING            VALUE 'Y'.

      *----------------------------------------------------------------*
      * SCREEN WORK AREA - SAME LAYOUT AS CUSTREC (175 BYTES)          *
      *----------------------------------------------------------------*
       01  WS-SCREEN-DATA.
           05  WS-SCR-CUSID            PIC X(10).
           05  WS-SCR-DETAILS.
               10  WS-SCR-CUSNAME      PIC X(30).
               10  WS-SCR-CUSADDR      PIC X(50).
               10  WS-SCR-LOC          PIC X(20).
               10  WS-SCR-EMAILID      PIC X(50).
               10  WS-SCR-PHONE        PIC X(15).

       01  WS-MESSAGE                  PIC X(77) VALUE SPACES.
       01  WS-CURSOR-FIELD             PIC X(08) VALUE 'CUSID'.
       01  WS-RID-KEY                  PIC X(10) VALUE SPACES.

      *----------------------------------------------------------------*
      * VALIDATION WORK FIELDS                                         *
      *----------------------------------------------------------------*
       01  WS-VALIDATION-WORK.
           05  WS-IDX                  PIC S9(04) COMP VALUE ZERO.
           05  WS-LEN                  PIC S9(04) COMP VALUE ZERO.
           05  WS-AT-COUNT             PIC S9(04) COMP VALUE ZERO.
           05  WS-AT-POS               PIC S9(04) COMP VALUE ZERO.
           05  WS-DOT-POS              PIC S9(04) COMP VALUE ZERO.
           05  WS-DDOT-COUNT           PIC S9(04) COMP VALUE ZERO.
           05  WS-DIGIT-COUNT          PIC S9(04) COMP VALUE ZERO.
           05  WS-TRIM-MAXLEN          PIC S9(04) COMP VALUE ZERO.
           05  WS-CHAR                 PIC X(01)       VALUE SPACE.
           05  WS-TRIM-FIELD           PIC X(50)       VALUE SPACES.

      *----------------------------------------------------------------*
      * ERROR LOG RECORD (TD QUEUE CSSL)                               *
      *----------------------------------------------------------------*
       01  WS-LOG-RECORD.
           05  FILLER                  PIC X(08) VALUE 'CUSTPGM '.
           05  WS-LOG-TRANID           PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  WS-LOG-TERMID           PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  WS-LOG-TEXT             PIC X(77) VALUE SPACES.

      *----------------------------------------------------------------*
      * COMMAREA - PASSED BETWEEN PSEUDO-CONVERSATIONAL TASKS          *
      *----------------------------------------------------------------*
       01  WS-COMMAREA.
           05  CA-PROGRAM-ID           PIC X(08).
           05  CA-LAST-KEY             PIC X(10).
           05  CA-DELETE-PENDING       PIC X(01).
               88  CA-DELETE-IS-PENDING          VALUE 'Y'.
           05  CA-REC-IMAGE            PIC X(175).

      *----------------------------------------------------------------*
      * RECORD LAYOUT, SYMBOLIC MAP, CICS COPYBOOKS                    *
      *----------------------------------------------------------------*
           COPY CUSTREC.
           COPY CUSTMAP.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(194).

       PROCEDURE DIVISION.
      *================================================================*
      * 0000-MAINLINE                                                  *
      * FIRST ENTRY (NO/INVALID COMMAREA) -> SEND EMPTY MAP.           *
      * SUBSEQUENT ENTRY -> PROCESS THE AID KEY THE USER PRESSED.      *
      * EVERY PATH EXCEPT F3 ENDS WITH RETURN TRANSID('CUST').         *
      *================================================================*
       0000-MAINLINE.
           IF EIBCALEN = LENGTH OF WS-COMMAREA
               MOVE DFHCOMMAREA TO WS-COMMAREA
           END-IF

           IF EIBCALEN NOT = LENGTH OF WS-COMMAREA
              OR CA-PROGRAM-ID NOT = WS-PGM-NAME
               PERFORM 1000-FIRST-TIME
           ELSE
               PERFORM 2000-PROCESS-INPUT
           END-IF

           PERFORM 9000-RETURN-TO-CICS
           GOBACK.

      *================================================================*
      * 1000-FIRST-TIME - INITIALISE COMMAREA AND SEND AN EMPTY MAP    *
      *================================================================*
       1000-FIRST-TIME.
           INITIALIZE WS-COMMAREA
           MOVE WS-PGM-NAME            TO CA-PROGRAM-ID
           MOVE 'N'                    TO CA-DELETE-PENDING
           MOVE SPACES                 TO WS-SCREEN-DATA
           MOVE 'ENTER A CUSTOMER ID, THEN PRESS F4-F7 OR ENTER'
                                       TO WS-MESSAGE
           MOVE 'CUSID'                TO WS-CURSOR-FIELD
           PERFORM 7000-SEND-MAP.

      *================================================================*
      * 2000-PROCESS-INPUT - DISPATCH ON EIBAID                        *
      *================================================================*
       2000-PROCESS-INPUT.
      *    DELETE CONFIRMATION IS VALID FOR ONE INTERACTION ONLY
           MOVE CA-DELETE-PENDING      TO WS-PREV-DEL-PENDING
           MOVE 'N'                    TO CA-DELETE-PENDING

           EVALUATE EIBAID
               WHEN DFHPF3
               WHEN DFHPF15
                   PERFORM 2100-END-SESSION
               WHEN DFHCLEAR
                   PERFORM 2200-CLEAR-SCREEN
               WHEN DFHPA1
               WHEN DFHPA2
               WHEN DFHPA3
                   MOVE 'PA KEYS ARE NOT SUPPORTED - USE ENTER OR F3-F7'
                                       TO WS-MESSAGE
                   PERFORM 7100-SEND-MESSAGE-ONLY
               WHEN OTHER
                   PERFORM 3000-RECEIVE-MAP
                   IF RECEIVE-OK
                       PERFORM 2300-DISPATCH-FUNCTION
                   END-IF
                   PERFORM 7000-SEND-MAP
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2100-END-SESSION - F3: CLEAR SCREEN AND END THE CONVERSATION   *
      *----------------------------------------------------------------*
       2100-END-SESSION.
           EXEC CICS SEND TEXT
                FROM(WS-END-MESSAGE)
                LENGTH(LENGTH OF WS-END-MESSAGE)
                ERASE
                FREEKB
                RESP(WS-RESP)
           END-EXEC

      *    RETURN WITHOUT TRANSID - PSEUDO-CONVERSATION ENDS HERE
           EXEC CICS RETURN
           END-EXEC.

      *----------------------------------------------------------------*
      * 2200-CLEAR-SCREEN - CLEAR KEY: RESET SCREEN AND STATE          *
      *----------------------------------------------------------------*
       2200-CLEAR-SCREEN.
           PERFORM 6100-CLEAR-RECORD-IMAGE
           MOVE SPACES                 TO WS-SCREEN-DATA
           MOVE 'SCREEN CLEARED - ENTER A CUSTOMER ID'
                                       TO WS-MESSAGE
           MOVE 'CUSID'                TO WS-CURSOR-FIELD
           PERFORM 7000-SEND-MAP.

      *----------------------------------------------------------------*
      * 2300-DISPATCH-FUNCTION - MAP RECEIVED, ROUTE BY AID KEY        *
      *----------------------------------------------------------------*
       2300-DISPATCH-FUNCTION.
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 4000-VALIDATE-ONLY
               WHEN DFHPF4
                   PERFORM 4100-INSERT-CUSTOMER
               WHEN DFHPF5
                   PERFORM 4200-READ-CUSTOMER
               WHEN DFHPF6
                   PERFORM 4300-UPDATE-CUSTOMER
               WHEN DFHPF7
                   PERFORM 4400-DELETE-CUSTOMER
               WHEN OTHER
                   MOVE 'INVALID KEY - USE ENTER, F3, F4, F5, F6 OR F7'
                                       TO WS-MESSAGE
           END-EVALUATE.

      *================================================================*
      * 3000-RECEIVE-MAP                                               *
      *================================================================*
       3000-RECEIVE-MAP.
           SET RECEIVE-OK              TO TRUE
           MOVE 'RECEIVE'              TO WS-FILE-OP

           EXEC CICS RECEIVE
                MAP('CUSTM01')
                MAPSET('CUSTMAP')
                INTO(CUSTM01I)
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(EOC)
                   PERFORM 3100-EXTRACT-SCREEN-FIELDS
               WHEN DFHRESP(MAPFAIL)
      *            NO MODIFIED FIELDS WERE TRANSMITTED
                   SET RECEIVE-FAILED  TO TRUE
                   MOVE SPACES         TO WS-SCREEN-DATA
                   MOVE 'NO DATA RECEIVED - ENTER A CUSTOMER ID'
                                       TO WS-MESSAGE
                   MOVE 'CUSID'        TO WS-CURSOR-FIELD
               WHEN DFHRESP(INVMPSZ)
                   SET RECEIVE-FAILED  TO TRUE
                   MOVE 'TERMINAL TOO SMALL - MAP CUSTM01 NEEDS 24X80'
                                       TO WS-MESSAGE
               WHEN OTHER
                   SET RECEIVE-FAILED  TO TRUE
                   MOVE SPACES         TO WS-SCREEN-DATA
                   PERFORM 8900-UNEXPECTED-ERROR
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 3100-EXTRACT-SCREEN-FIELDS                                     *
      * L = 0 MEANS NOTHING WAS RECEIVED (OR ERASE-EOF WAS USED).      *
      * LOW-VALUES (NULLS) FROM THE TERMINAL ARE CONVERTED TO SPACES.  *
      *----------------------------------------------------------------*
       3100-EXTRACT-SCREEN-FIELDS.
           MOVE SPACES                 TO WS-SCREEN-DATA
           IF CUSIDL   > ZERO
               MOVE CUSIDI             TO WS-SCR-CUSID
           END-IF
           IF CUSNAMEL > ZERO
               MOVE CUSNAMEI           TO WS-SCR-CUSNAME
           END-IF
           IF CUSADDRL > ZERO
               MOVE CUSADDRI           TO WS-SCR-CUSADDR
           END-IF
           IF LOCL     > ZERO
               MOVE LOCI               TO WS-SCR-LOC
           END-IF
           IF EMAILIDL > ZERO
               MOVE EMAILIDI           TO WS-SCR-EMAILID
           END-IF
           IF PHONEL   > ZERO
               MOVE PHONEI             TO WS-SCR-PHONE
           END-IF

           INSPECT WS-SCREEN-DATA REPLACING ALL LOW-VALUES BY SPACES
           MOVE FUNCTION UPPER-CASE(WS-SCR-CUSID)
                                       TO WS-SCR-CUSID.

      *================================================================*
      * 4000-VALIDATE-ONLY - ENTER: EDIT CHECKS, NO FILE ACCESS        *
      *================================================================*
       4000-VALIDATE-ONLY.
           PERFORM 5000-VALIDATE-KEY
           IF INPUT-VALID
               IF WS-SCR-DETAILS = SPACES
                   MOVE 'CUSTOMER ID OK - PRESS F5 TO READ OR F4 TO ADD'
                                       TO WS-MESSAGE
               ELSE
                   PERFORM 5100-VALIDATE-DETAILS
                   IF INPUT-VALID
                       MOVE 'INPUT IS VALID - PRESS F4, F5, F6 OR F7'
                                       TO WS-MESSAGE
                   END-IF
               END-IF
           END-IF.

      *================================================================*
      * 4100-INSERT-CUSTOMER - F4                                      *
      *================================================================*
       4100-INSERT-CUSTOMER.
           PERFORM 5000-VALIDATE-KEY
           IF INPUT-VALID
               PERFORM 5100-VALIDATE-DETAILS
           END-IF

           IF INPUT-VALID
               MOVE WS-SCREEN-DATA     TO CUSTOMER-RECORD
               MOVE 'WRITE'            TO WS-FILE-OP

               EXEC CICS WRITE
                    FILE('CUSTFILE')
                    FROM(CUSTOMER-RECORD)
                    RIDFLD(CR-CUSID)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       MOVE 'CUSTOMER SUCCESSFULLY CREATED'
                                       TO WS-MESSAGE
                       PERFORM 6000-SAVE-RECORD-IMAGE
                   WHEN DFHRESP(DUPREC)
                       MOVE 'CUSTOMER ID ALREADY EXISTS'
                                       TO WS-MESSAGE
                       MOVE 'CUSID'    TO WS-CURSOR-FIELD
                   WHEN OTHER
                       PERFORM 8000-FILE-ERROR
               END-EVALUATE
           END-IF.

      *================================================================*
      * 4200-READ-CUSTOMER - F5                                        *
      *================================================================*
       4200-READ-CUSTOMER.
           PERFORM 5000-VALIDATE-KEY
           IF INPUT-VALID
               MOVE WS-SCR-CUSID       TO WS-RID-KEY
               MOVE 'READ'             TO WS-FILE-OP

               EXEC CICS READ
                    FILE('CUSTFILE')
                    INTO(CUSTOMER-RECORD)
                    RIDFLD(WS-RID-KEY)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       MOVE CUSTOMER-RECORD TO WS-SCREEN-DATA
                       PERFORM 6000-SAVE-RECORD-IMAGE
                       MOVE 'CUSTOMER RECORD FOUND'
                                       TO WS-MESSAGE
                       MOVE 'CUSNAME'  TO WS-CURSOR-FIELD
                   WHEN DFHRESP(NOTFND)
                       PERFORM 6200-CUSTOMER-NOT-FOUND
                   WHEN OTHER
                       PERFORM 8000-FILE-ERROR
               END-EVALUATE
           END-IF.

      *================================================================*
      * 4300-UPDATE-CUSTOMER - F6                                      *
      * READ ... UPDATE TAKES THE VSAM RECORD LOCK, REWRITE RELEASES   *
      * IT. IF THE RECORD WAS READ IN THIS CONVERSATION AND HAS SINCE  *
      * BEEN CHANGED BY ANOTHER USER, THE UPDATE IS REFUSED.           *
      *================================================================*
       4300-UPDATE-CUSTOMER.
           PERFORM 5000-VALIDATE-KEY
           IF INPUT-VALID
               PERFORM 5100-VALIDATE-DETAILS
           END-IF

           IF INPUT-VALID
               MOVE WS-SCR-CUSID       TO WS-RID-KEY
               MOVE 'READUPD'          TO WS-FILE-OP

               EXEC CICS READ
                    FILE('CUSTFILE')
                    INTO(CUSTOMER-RECORD)
                    RIDFLD(WS-RID-KEY)
                    UPDATE
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       PERFORM 4310-APPLY-UPDATE
                   WHEN DFHRESP(NOTFND)
                       PERFORM 6200-CUSTOMER-NOT-FOUND
                   WHEN OTHER
                       PERFORM 8000-FILE-ERROR
               END-EVALUATE
           END-IF.

       4310-APPLY-UPDATE.
           EVALUATE TRUE
               WHEN CA-LAST-KEY = CR-CUSID
                AND CA-REC-IMAGE NOT = CUSTOMER-RECORD
                   PERFORM 4390-UNLOCK-RECORD
                   MOVE
                     'RECORD CHANGED BY ANOTHER USER - F5 TO RELOAD'
                                       TO WS-MESSAGE
               WHEN CUSTOMER-RECORD = WS-SCREEN-DATA
                   PERFORM 4390-UNLOCK-RECORD
                   MOVE 'NO CHANGES ENTERED - RECORD NOT UPDATED'
                                       TO WS-MESSAGE
               WHEN OTHER
      *            KEY (CR-CUSID) IS NEVER CHANGED ON A REWRITE
                   MOVE WS-SCR-CUSNAME TO CR-CUSNAME
                   MOVE WS-SCR-CUSADDR TO CR-CUSADDR
                   MOVE WS-SCR-LOC     TO CR-LOC
                   MOVE WS-SCR-EMAILID TO CR-EMAILID
                   MOVE WS-SCR-PHONE   TO CR-PHONE
                   MOVE 'REWRITE'      TO WS-FILE-OP

                   EXEC CICS REWRITE
                        FILE('CUSTFILE')
                        FROM(CUSTOMER-RECORD)
                        RESP(WS-RESP)
                        RESP2(WS-RESP2)
                   END-EXEC

                   EVALUATE WS-RESP
                       WHEN DFHRESP(NORMAL)
                           MOVE 'CUSTOMER SUCCESSFULLY UPDATED'
                                       TO WS-MESSAGE
                           PERFORM 6000-SAVE-RECORD-IMAGE
                       WHEN OTHER
                           PERFORM 8000-FILE-ERROR
                   END-EVALUATE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 4390-UNLOCK-RECORD - RELEASE A READ-UPDATE LOCK NOT USED.      *
      * THE RESPONSE IS DELIBERATELY NOT REPORTED: THE LOCK IS ALSO    *
      * FREED AT TASK END (RETURN), SO A FAILURE HERE IS HARMLESS.     *
      *----------------------------------------------------------------*
       4390-UNLOCK-RECORD.
           EXEC CICS UNLOCK
                FILE('CUSTFILE')
                RESP(WS-AUX-RESP)
           END-EXEC.

      *================================================================*
      * 4400-DELETE-CUSTOMER - F7                                      *
      * FIRST F7 : READ AND DISPLAY THE RECORD, ASK FOR CONFIRMATION.  *
      * SECOND F7 (SAME KEY, NEXT INTERACTION): READ UPDATE, CHECK THE *
      * RECORD IS UNCHANGED, THEN DELETE THE RECORD HELD FOR UPDATE.   *
      *================================================================*
       4400-DELETE-CUSTOMER.
           PERFORM 5000-VALIDATE-KEY
           IF INPUT-VALID
               IF DELETE-WAS-PENDING
                  AND CA-LAST-KEY = WS-SCR-CUSID
                   PERFORM 4420-CONFIRMED-DELETE
               ELSE
                   PERFORM 4410-REQUEST-CONFIRMATION
               END-IF
           END-IF.

       4410-REQUEST-CONFIRMATION.
           PERFORM 4200-READ-CUSTOMER
           IF INPUT-VALID
              AND WS-RESP = DFHRESP(NORMAL)
               MOVE 'Y'                TO CA-DELETE-PENDING
               MOVE 'PRESS F7 AGAIN TO CONFIRM DELETE OF THIS CUSTOMER'
                                       TO WS-MESSAGE
               MOVE 'CUSID'            TO WS-CURSOR-FIELD
           END-IF.

       4420-CONFIRMED-DELETE.
           MOVE WS-SCR-CUSID           TO WS-RID-KEY
           MOVE 'READUPD'              TO WS-FILE-OP

           EXEC CICS READ
                FILE('CUSTFILE')
                INTO(CUSTOMER-RECORD)
                RIDFLD(WS-RID-KEY)
                UPDATE
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   IF CUSTOMER-RECORD NOT = CA-REC-IMAGE
      *                CHANGED SINCE IT WAS SHOWN - SHOW NEW VERSION
                       PERFORM 4390-UNLOCK-RECORD
                       MOVE CUSTOMER-RECORD TO WS-SCREEN-DATA
                       PERFORM 6000-SAVE-RECORD-IMAGE
                       MOVE 'Y'        TO CA-DELETE-PENDING
                       MOVE
                         'RECORD CHANGED BY ANOTHER USER - F7 TO DELETE'
                                       TO WS-MESSAGE
                   ELSE
                       PERFORM 4430-DELETE-HELD-RECORD
                   END-IF
               WHEN DFHRESP(NOTFND)
                   PERFORM 6200-CUSTOMER-NOT-FOUND
               WHEN OTHER
                   PERFORM 8000-FILE-ERROR
           END-EVALUATE.

      *    DELETE WITHOUT RIDFLD DELETES THE RECORD READ FOR UPDATE
       4430-DELETE-HELD-RECORD.
           MOVE 'DELETE'               TO WS-FILE-OP

           EXEC CICS DELETE
                FILE('CUSTFILE')
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE SPACES         TO WS-SCR-DETAILS
                   PERFORM 6100-CLEAR-RECORD-IMAGE
                   MOVE 'CUSTOMER SUCCESSFULLY DELETED'
                                       TO WS-MESSAGE
                   MOVE 'CUSID'        TO WS-CURSOR-FIELD
               WHEN DFHRESP(NOTFND)
                   PERFORM 6200-CUSTOMER-NOT-FOUND
               WHEN OTHER
                   PERFORM 8000-FILE-ERROR
           END-EVALUATE.

      *================================================================*
      * 5000-VALIDATE-KEY                                              *
      * CUSID: MANDATORY, NOT SPACES, A-Z/0-9 ONLY, NO EMBEDDED OR     *
      * LEADING SPACES. MAX 10 IS ENFORCED BY THE MAP FIELD LENGTH.    *
      *================================================================*
       5000-VALIDATE-KEY.
           SET INPUT-VALID             TO TRUE
           IF WS-SCR-CUSID = SPACES
               SET INPUT-INVALID       TO TRUE
               MOVE 'CUSTOMER ID IS REQUIRED'
                                       TO WS-MESSAGE
               MOVE 'CUSID'            TO WS-CURSOR-FIELD
           ELSE
               MOVE WS-SCR-CUSID       TO WS-TRIM-FIELD
               MOVE 10                 TO WS-TRIM-MAXLEN
               PERFORM 8500-GET-TRIMMED-LENGTH
               IF WS-SCR-CUSID(1:WS-LEN) IS NOT VALID-KEY-CHAR
                   SET INPUT-INVALID   TO TRUE
                   MOVE 'CUSTOMER ID: USE A-Z AND 0-9 ONLY, NO SPACES'
                                       TO WS-MESSAGE
                   MOVE 'CUSID'        TO WS-CURSOR-FIELD
               END-IF
           END-IF.

      *================================================================*
      * 5100-VALIDATE-DETAILS  (INSERT / UPDATE / ENTER)               *
      * CUSNAME MANDATORY. EMAILID AND PHONE ARE OPTIONAL, BUT WHEN    *
      * ENTERED THEY MUST PASS THE FORMAT CHECKS.                      *
      *================================================================*
       5100-VALIDATE-DETAILS.
           IF WS-SCR-CUSNAME = SPACES
               SET INPUT-INVALID       TO TRUE
               MOVE 'CUSTOMER NAME IS REQUIRED'
                                       TO WS-MESSAGE
               MOVE 'CUSNAME'          TO WS-CURSOR-FIELD
           END-IF

           IF INPUT-VALID
              AND WS-SCR-EMAILID NOT = SPACES
               PERFORM 5200-VALIDATE-EMAIL
           END-IF

           IF INPUT-VALID
              AND WS-SCR-PHONE NOT = SPACES
               PERFORM 5300-VALIDATE-PHONE
           END-IF.

      *----------------------------------------------------------------*
      * 5200-VALIDATE-EMAIL - BASIC STRUCTURE CHECK:                   *
      *   VALID CHARACTERS ONLY, NO EMBEDDED SPACES, EXACTLY ONE '@',  *
      *   '@' NOT FIRST, NO '..', NOT STARTING WITH '.', NO '.@' OR    *
      *   '@.', AND A '.' IN THE DOMAIN THAT IS NOT THE LAST CHARACTER.*
      *----------------------------------------------------------------*
       5200-VALIDATE-EMAIL.
           MOVE WS-SCR-EMAILID         TO WS-TRIM-FIELD
           MOVE 50                     TO WS-TRIM-MAXLEN
           PERFORM 8500-GET-TRIMMED-LENGTH
           MOVE ZERO                   TO WS-AT-COUNT WS-AT-POS
                                          WS-DOT-POS  WS-DDOT-COUNT

           IF WS-SCR-EMAILID(1:WS-LEN) IS NOT VALID-EMAIL-CHAR
               PERFORM 5290-EMAIL-ERROR
           ELSE
               INSPECT WS-SCR-EMAILID(1:WS-LEN)
                   TALLYING WS-AT-COUNT   FOR ALL '@'
                            WS-DDOT-COUNT FOR ALL '..'
               PERFORM VARYING WS-IDX FROM 1 BY 1
                         UNTIL WS-IDX > WS-LEN
                   EVALUATE WS-SCR-EMAILID(WS-IDX:1)
                       WHEN '@'
                           MOVE WS-IDX TO WS-AT-POS
                       WHEN '.'
                           MOVE WS-IDX TO WS-DOT-POS
                   END-EVALUATE
               END-PERFORM
      *        WS-DOT-POS NOW HOLDS THE POSITION OF THE LAST '.'
               EVALUATE TRUE
                   WHEN WS-AT-COUNT NOT = 1
                   WHEN WS-AT-POS = 1
                   WHEN WS-DDOT-COUNT > ZERO
                   WHEN WS-SCR-EMAILID(1:1) = '.'
                   WHEN WS-DOT-POS <= WS-AT-POS + 1
                   WHEN WS-DOT-POS = WS-LEN
                   WHEN WS-SCR-EMAILID(WS-AT-POS - 1:1) = '.'
                   WHEN WS-SCR-EMAILID(WS-AT-POS + 1:1) = '.'
                       PERFORM 5290-EMAIL-ERROR
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       5290-EMAIL-ERROR.
           SET INPUT-INVALID           TO TRUE
           MOVE 'EMAIL ID IS INVALID - EXPECTED NAME@DOMAIN.COM'
                                       TO WS-MESSAGE
           MOVE 'EMAILID'              TO WS-CURSOR-FIELD.

      *----------------------------------------------------------------*
      * 5300-VALIDATE-PHONE                                            *
      *   DIGITS, OPTIONAL LEADING '+', '-' AS INTERNAL SEPARATOR ONLY,*
      *   NO SPACES, AT LEAST 7 DIGITS.  E.G. +91-9840012345           *
      *----------------------------------------------------------------*
       5300-VALIDATE-PHONE.
           MOVE WS-SCR-PHONE           TO WS-TRIM-FIELD
           MOVE 15                     TO WS-TRIM-MAXLEN
           PERFORM 8500-GET-TRIMMED-LENGTH
           MOVE ZERO                   TO WS-DIGIT-COUNT

           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > WS-LEN
                        OR INPUT-INVALID
               MOVE WS-SCR-PHONE(WS-IDX:1) TO WS-CHAR
               EVALUATE TRUE
                   WHEN WS-CHAR IS NUMERIC
                       ADD 1           TO WS-DIGIT-COUNT
                   WHEN WS-CHAR = '+' AND WS-IDX = 1
                       CONTINUE
                   WHEN WS-CHAR = '-' AND WS-IDX > 1
                                      AND WS-IDX < WS-LEN
                       CONTINUE
                   WHEN OTHER
                       SET INPUT-INVALID TO TRUE
                       MOVE
                         'PHONE: DIGITS AND - ONLY (OPTIONAL LEADING +)'
                                       TO WS-MESSAGE
               END-EVALUATE
           END-PERFORM

           IF INPUT-VALID
              AND WS-DIGIT-COUNT < 7
               SET INPUT-INVALID       TO TRUE
               MOVE 'PHONE NUMBER MUST CONTAIN AT LEAST 7 DIGITS'
                                       TO WS-MESSAGE
           END-IF

           IF INPUT-INVALID
               MOVE 'PHONE'            TO WS-CURSOR-FIELD
           END-IF.

      *================================================================*
      * 6000-6200 COMMAREA STATE HELPERS                               *
      *================================================================*
       6000-SAVE-RECORD-IMAGE.
           MOVE CR-CUSID               TO CA-LAST-KEY
           MOVE CUSTOMER-RECORD        TO CA-REC-IMAGE.

       6100-CLEAR-RECORD-IMAGE.
           MOVE SPACES                 TO CA-LAST-KEY
                                          CA-REC-IMAGE
           MOVE 'N'                    TO CA-DELETE-PENDING.

       6200-CUSTOMER-NOT-FOUND.
           MOVE SPACES                 TO WS-SCR-DETAILS
           PERFORM 6100-CLEAR-RECORD-IMAGE
           MOVE 'CUSTOMER ID NOT FOUND' TO WS-MESSAGE
           MOVE 'CUSID'                TO WS-CURSOR-FIELD.

      *================================================================*
      * 7000-SEND-MAP - BUILD SYMBOLIC OUTPUT MAP AND SEND IT          *
      * BLANK FIELDS ARE LEFT AS LOW-VALUES SO THE TERMINAL SHOWS      *
      * NULLS (INSERT MODE WORKS). CURSOR IS SET BY L = -1 (SYMBOLIC   *
      * CURSOR POSITIONING) ON THE FIELD NAMED IN WS-CURSOR-FIELD.     *
      *================================================================*
       7000-SEND-MAP.
           MOVE LOW-VALUES             TO CUSTM01O
           IF WS-SCR-CUSID   NOT = SPACES
               MOVE WS-SCR-CUSID       TO CUSIDO
           END-IF
           IF WS-SCR-CUSNAME NOT = SPACES
               MOVE WS-SCR-CUSNAME     TO CUSNAMEO
           END-IF
           IF WS-SCR-CUSADDR NOT = SPACES
               MOVE WS-SCR-CUSADDR     TO CUSADDRO
           END-IF
           IF WS-SCR-LOC     NOT = SPACES
               MOVE WS-SCR-LOC         TO LOCO
           END-IF
           IF WS-SCR-EMAILID NOT = SPACES
               MOVE WS-SCR-EMAILID     TO EMAILIDO
           END-IF
           IF WS-SCR-PHONE   NOT = SPACES
               MOVE WS-SCR-PHONE       TO PHONEO
           END-IF
           MOVE WS-MESSAGE             TO MSGO

           EVALUATE WS-CURSOR-FIELD
               WHEN 'CUSNAME'
                   MOVE -1             TO CUSNAMEL
               WHEN 'CUSADDR'
                   MOVE -1             TO CUSADDRL
               WHEN 'LOC'
                   MOVE -1             TO LOCL
               WHEN 'EMAILID'
                   MOVE -1             TO EMAILIDL
               WHEN 'PHONE'
                   MOVE -1             TO PHONEL
               WHEN OTHER
                   MOVE -1             TO CUSIDL
           END-EVALUATE

           EXEC CICS SEND
                MAP('CUSTM01')
                MAPSET('CUSTMAP')
                FROM(CUSTM01O)
                ERASE
                CURSOR
                FREEKB
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9900-ABEND-SEND-FAILED
           END-IF.

      *----------------------------------------------------------------*
      * 7100-SEND-MESSAGE-ONLY - DATAONLY: UPDATE MESSAGE LINE ONLY,   *
      * LEAVING WHATEVER THE USER HAS TYPED ON THE SCREEN INTACT.      *
      *----------------------------------------------------------------*
       7100-SEND-MESSAGE-ONLY.
           MOVE LOW-VALUES             TO CUSTM01O
           MOVE WS-MESSAGE             TO MSGO

           EXEC CICS SEND
                MAP('CUSTM01')
                MAPSET('CUSTMAP')
                FROM(CUSTM01O)
                DATAONLY
                FREEKB
                RESP(WS-RESP)
                RESP2(WS-RESP2)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9900-ABEND-SEND-FAILED
           END-IF.

      *================================================================*
      * 8000-FILE-ERROR - ENVIRONMENT/FILE CONDITIONS COMMON TO THE    *
      * READ, WRITE, REWRITE AND DELETE COMMANDS ON CUSTFILE.          *
      *================================================================*
       8000-FILE-ERROR.
           MOVE WS-RESP                TO WS-RESP-DISP
           MOVE WS-RESP2               TO WS-RESP2-DISP
           MOVE SPACES                 TO WS-ERR-TEXT

           EVALUATE WS-RESP
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'CUSTFILE IS CLOSED - OPEN IT WITH CEMT'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(DISABLED)
                   MOVE 'CUSTFILE IS DISABLED - ENABLE IT WITH CEMT'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(FILENOTFOUND)
                   MOVE 'CUSTFILE NOT DEFINED/INSTALLED IN CICS'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(IOERR)
                   MOVE 'VSAM I/O ERROR ON CUSTFILE'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(LENGERR)
                   MOVE 'RECORD LENGTH MISMATCH - CHECK RECORDSIZE'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(INVREQ)
                   MOVE 'INVALID REQUEST - CHECK FILE ADD/UPD/DEL'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(ILLOGIC)
                   MOVE 'VSAM LOGIC ERROR ON CUSTFILE'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(NOSPACE)
                   MOVE 'NO SPACE LEFT IN THE CUSTOMER DATASET'
                                       TO WS-ERR-TEXT
               WHEN DFHRESP(NOTAUTH)
                   MOVE 'NOT AUTHORIZED TO ACCESS CUSTFILE'
                                       TO WS-ERR-TEXT
               WHEN OTHER
                   PERFORM 8900-UNEXPECTED-ERROR
           END-EVALUATE

           IF WS-ERR-TEXT NOT = SPACES
               MOVE SPACES             TO WS-MESSAGE
               STRING WS-FILE-OP       DELIMITED BY SPACE
                      ': '             DELIMITED BY SIZE
                      WS-ERR-TEXT      DELIMITED BY '  '
                      ' RESP2='        DELIMITED BY SIZE
                      WS-RESP2-DISP    DELIMITED BY SIZE
                 INTO WS-MESSAGE
               END-STRING
               PERFORM 8950-LOG-ERROR
           END-IF.

      *----------------------------------------------------------------*
      * 8500-GET-TRIMMED-LENGTH                                        *
      * IN : WS-TRIM-FIELD, WS-TRIM-MAXLEN (FIELD MUST NOT BE SPACES)  *
      * OUT: WS-LEN = POSITION OF LAST NON-BLANK CHARACTER             *
      *----------------------------------------------------------------*
       8500-GET-TRIMMED-LENGTH.
           MOVE WS-TRIM-MAXLEN         TO WS-LEN
           PERFORM UNTIL WS-LEN < 2
                      OR WS-TRIM-FIELD(WS-LEN:1) NOT = SPACE
               SUBTRACT 1              FROM WS-LEN
           END-PERFORM.

      *----------------------------------------------------------------*
      * 8900-UNEXPECTED-ERROR - SHOW RESP/RESP2 INSTEAD OF ABENDING    *
      *----------------------------------------------------------------*
       8900-UNEXPECTED-ERROR.
           MOVE WS-RESP                TO WS-RESP-DISP
           MOVE WS-RESP2               TO WS-RESP2-DISP
           MOVE SPACES                 TO WS-MESSAGE
           STRING 'UNEXPECTED CICS ERROR - RESP='
                                       DELIMITED BY SIZE
                  WS-RESP-DISP         DELIMITED BY SIZE
                  ' RESP2='            DELIMITED BY SIZE
                  WS-RESP2-DISP        DELIMITED BY SIZE
                  ' ('                 DELIMITED BY SIZE
                  WS-FILE-OP           DELIMITED BY SPACE
                  ')'                  DELIMITED BY SIZE
             INTO WS-MESSAGE
           END-STRING
           PERFORM 8950-LOG-ERROR.

      *----------------------------------------------------------------*
      * 8950-LOG-ERROR - WRITE DIAGNOSTIC TO TD QUEUE CSSL (MSGUSR).   *
      * A LOGGING FAILURE MUST NOT HIDE THE ORIGINAL ERROR, SO ITS     *
      * RESPONSE GOES TO WS-AUX-RESP AND IS IGNORED.                   *
      *----------------------------------------------------------------*
       8950-LOG-ERROR.
           MOVE EIBTRNID               TO WS-LOG-TRANID
           MOVE EIBTRMID               TO WS-LOG-TERMID
           MOVE WS-MESSAGE             TO WS-LOG-TEXT

           EXEC CICS WRITEQ TD
                QUEUE('CSSL')
                FROM(WS-LOG-RECORD)
                LENGTH(LENGTH OF WS-LOG-RECORD)
                RESP(WS-AUX-RESP)
           END-EXEC.

      *================================================================*
      * 9000-RETURN-TO-CICS - END THIS TASK, KEEP THE CONVERSATION.    *
      * CICS STARTS TRANSID CUST WHEN THE USER NEXT PRESSES AN AID KEY *
      * AND PASSES WS-COMMAREA BACK AS DFHCOMMAREA.                    *
      *================================================================*
       9000-RETURN-TO-CICS.
           EXEC CICS RETURN
                TRANSID('CUST')
                COMMAREA(WS-COMMAREA)
                LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      * 9900-ABEND-SEND-FAILED - THE SCREEN CANNOT BE WRITTEN, SO NO   *
      * MESSAGE CAN BE SHOWN: LOG IT AND ABEND WITH A DUMP (CUSM).     *
      *----------------------------------------------------------------*
       9900-ABEND-SEND-FAILED.
           MOVE 'SEND'                 TO WS-FILE-OP
           PERFORM 8900-UNEXPECTED-ERROR
           EXEC CICS ABEND
                ABCODE('CUSM')
           END-EXEC.
