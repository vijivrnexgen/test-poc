       01  CUSTM01I.
           02  FILLER PIC X(12).
           02  CUSIDL    COMP  PIC  S9(4).
           02  CUSIDF    PICTURE X.
           02  FILLER REDEFINES CUSIDF.
             03 CUSIDA    PICTURE X.
           02  CUSIDI  PIC X(10).
           02  CUSNAMEL    COMP  PIC  S9(4).
           02  CUSNAMEF    PICTURE X.
           02  FILLER REDEFINES CUSNAMEF.
             03 CUSNAMEA    PICTURE X.
           02  CUSNAMEI  PIC X(30).
           02  CUSADDRL    COMP  PIC  S9(4).
           02  CUSADDRF    PICTURE X.
           02  FILLER REDEFINES CUSADDRF.
             03 CUSADDRA    PICTURE X.
           02  CUSADDRI  PIC X(50).
           02  LOCL    COMP  PIC  S9(4).
           02  LOCF    PICTURE X.
           02  FILLER REDEFINES LOCF.
             03 LOCA    PICTURE X.
           02  LOCI  PIC X(20).
           02  EMAILIDL    COMP  PIC  S9(4).
           02  EMAILIDF    PICTURE X.
           02  FILLER REDEFINES EMAILIDF.
             03 EMAILIDA    PICTURE X.
           02  EMAILIDI  PIC X(50).
           02  PHONEL    COMP  PIC  S9(4).
           02  PHONEF    PICTURE X.
           02  FILLER REDEFINES PHONEF.
             03 PHONEA    PICTURE X.
           02  PHONEI  PIC X(15).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  MSGI  PIC X(77).
       01  CUSTM01O REDEFINES CUSTM01I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CUSIDO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CUSNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  CUSADDRO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  LOCO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  EMAILIDO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  PHONEO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  MSGO  PIC X(77).
