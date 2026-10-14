      * ACCTCPY - ACCOUNT MASTER RECORD (LRECL 80)
       01  ACCOUNT-RECORD.
           05  ACCT-NO             PIC 9(08).
           05  ACCT-NAME           PIC X(25).
           05  ACCT-TYPE           PIC X(01).
               88  SAVINGS-ACCT              VALUE 'S'.
               88  CURRENT-ACCT              VALUE 'C'.
           05  ACCT-BALANCE        PIC S9(09)V99.
           05  FILLER              PIC X(35).
