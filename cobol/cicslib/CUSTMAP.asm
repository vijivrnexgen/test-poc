*=====================================================================*
* MAPSET   : CUSTMAP                                                  *
* MAP      : CUSTM01   (24 X 80 3270)                                 *
* SYSTEM   : CUSTOMER MAINTENANCE SYSTEM  (TRANSACTION CUST)          *
* PROGRAM  : CUSTPGM                                                  *
*                                                                     *
* ASSEMBLE TWICE:  SYSPARM(MAP)   -> PHYSICAL MAP  (LOAD MODULE)      *
*                  SYSPARM(DSECT) -> SYMBOLIC MAP  (COBOL COPYBOOK)   *
*                                                                     *
* NOTES    : DATA FIELDS CARRY FSET SO THEY ARE ALWAYS RETURNED ON    *
*            RECEIVE MAP (NEEDED FOR PARTIAL EDITS BEFORE F6=UPDATE). *
*            MAPATTS WITHOUT DSATTS = COLOUR/HILIGHT IN PHYSICAL MAP  *
*            ONLY, SO THE SYMBOLIC MAP HAS STANDARD L/F/A/I/O FIELDS. *
*            EACH INPUT FIELD IS FOLLOWED BY A 1-BYTE ASKIP STOPPER.  *
*=====================================================================*
         PRINT NOGEN
CUSTMAP  DFHMSD TYPE=&SYSPARM,                                         X
               MODE=INOUT,                                             X
               LANG=COBOL,                                             X
               STORAGE=AUTO,                                           X
               TIOAPFX=YES,                                            X
               CTRL=(FREEKB),                                          X
               MAPATTS=(COLOR,HILIGHT)
*
CUSTM01  DFHMDI SIZE=(24,80),                                          X
               LINE=1,                                                 X
               COLUMN=1
*
         DFHMDF POS=(1,1),                                             X
               LENGTH=79,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=BLUE,                                             X
               INITIAL='-----------------------------------------------X
               --------------------------------'
*
         DFHMDF POS=(2,2),                                             X
               LENGTH=10,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='TRAN: CUST'
*
         DFHMDF POS=(2,26),                                            X
               LENGTH=27,                                              X
               ATTRB=(ASKIP,BRT),                                      X
               COLOR=WHITE,                                            X
               INITIAL='CUSTOMER MAINTENANCE SYSTEM'
*
         DFHMDF POS=(3,1),                                             X
               LENGTH=79,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=BLUE,                                             X
               INITIAL='-----------------------------------------------X
               --------------------------------'
*
         DFHMDF POS=(5,2),                                             X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Customer ID      :'
*
CUSID    DFHMDF POS=(5,21),                                            X
               LENGTH=10,                                              X
               ATTRB=(UNPROT,NORM,IC,FSET),                            X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(5,32),                                            X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(7,2),                                             X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Customer Name    :'
*
CUSNAME  DFHMDF POS=(7,21),                                            X
               LENGTH=30,                                              X
               ATTRB=(UNPROT,NORM,FSET),                               X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(7,52),                                            X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(9,2),                                             X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Address          :'
*
CUSADDR  DFHMDF POS=(9,21),                                            X
               LENGTH=50,                                              X
               ATTRB=(UNPROT,NORM,FSET),                               X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(9,72),                                            X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(11,2),                                            X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Location         :'
*
LOC      DFHMDF POS=(11,21),                                           X
               LENGTH=20,                                              X
               ATTRB=(UNPROT,NORM,FSET),                               X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(11,42),                                           X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(13,2),                                            X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Email ID         :'
*
EMAILID  DFHMDF POS=(13,21),                                           X
               LENGTH=50,                                              X
               ATTRB=(UNPROT,NORM,FSET),                               X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(13,72),                                           X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(15,2),                                            X
               LENGTH=18,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Phone Number     :'
*
PHONE    DFHMDF POS=(15,21),                                           X
               LENGTH=15,                                              X
               ATTRB=(UNPROT,NORM,FSET),                               X
               COLOR=GREEN,                                            X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(15,37),                                           X
               LENGTH=1,                                               X
               ATTRB=(ASKIP)
*
         DFHMDF POS=(18,2),                                            X
               LENGTH=8,                                               X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=TURQUOISE,                                        X
               INITIAL='Message:'
*
MSG      DFHMDF POS=(19,2),                                            X
               LENGTH=77,                                              X
               ATTRB=(ASKIP,BRT),                                      X
               COLOR=YELLOW
*
         DFHMDF POS=(22,2),                                            X
               LENGTH=70,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=BLUE,                                             X
               INITIAL='F4=INSERT   F5=READ   F6=UPDATE   F7=DELETE   FX
               3=EXIT   ENTER=VALIDATE'
*
         DFHMDF POS=(23,1),                                            X
               LENGTH=79,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               COLOR=BLUE,                                             X
               INITIAL='-----------------------------------------------X
               --------------------------------'
*
         DFHMSD TYPE=FINAL
         END
