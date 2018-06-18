
      $set ans85 mf noosvs defaultbyte"00"
      ****************************************************************
      *
      *  Program:
      *      KWI_NSM2.CBL
      *  Desription:
      *      "Gets" Named Shared Memory (KWI_NSM.MEM) created in 
      *      KWI_NSM1.CBL. Clears semaphore (KWI_NSM.SEM) created
      *      in KWI_NSM1.CBL to signal that NSM was retrieved
      *      successfully.
      *
      ****************************************************************

      ****************************************************************
      *
      *     Enable the PASCAL calling convention (number 3)
      *     and call it APIENTRY because it is used for APIENTRY
      *     functions.  (We will use it for COBOL to COBOL calls
      *     as well.)
      *
      ****************************************************************
        special-names.
            call-convention 3 is APIENTRY.

        working-storage section.

        01  RC                   PIC 9(4) COMP-5 VALUE 0.
 
      ****************************************************************
      *  Fields used to access system semaphore
      ****************************************************************
        01  Semaphore-fields.
            05 sem-handle       USAGE IS POINTER.
            05 sem-name         PIC X(30) 
                                VALUE "\SEM\KWI_NSM.SEM" & X'00'.
 
      ****************************************************************
      *  Fields used to access Named Shared Memory
      ****************************************************************
        01  seg-fields.
            05  seg-name        PIC X(30)
                                VALUE "\SHAREMEM\KWI_NSM.MEM"& X'00'.
            05  seg-ptr.
                10  seg-zero        PIC 9(4) COMP-5 VALUE ZERO.
 
                10  seg-selector    PIC 9(4) COMP-5.
            05  seg-pt              REDEFINES seg-ptr.
                10 seg-pointer      USAGE IS POINTER.

        01  seg-selector-fields.
            05  seg-selector-ptr.
                10  seg-pointers-zero     PIC 9(4) COMP-5 VALUE ZERO.
                10  seg-pointers-selector PIC 9(4) COMP-5.
            05  seg-selector-pt           REDEFINES seg-selector-ptr.
                10 seg-selectors-pointer  USAGE IS POINTER.

        procedure division APIENTRY.
        main section.

      *****************************************************************
      *  Obrain selector for Named Shared Memory (NSM) allocated in
      *  parent program
      *****************************************************************
        CALL APIENTRY "DosGetShrSeg" USING
                BY REFERENCE seg-name
                BY REFERENCE seg-selector
           RETURNING RC

           IF RC NOT = 0
               EXIT PROGRAM RETURNING RC
           End-If.

      *****************************************************************
      *  Optain handle for system semaphore created in parent program
      *****************************************************************
        CALL APIENTRY "DosOpenSem" USING
                BY REFERENCE sem-handle
                BY REFERENCE sem-name
           RETURNING RC

           IF RC NOT = 0
               EXIT PROGRAM RETURNING RC
           End-If.

      *****************************************************************
      *  Clear semaphore to signal parent program that NSM was 
      *  received successfully
      *****************************************************************
        CALL APIENTRY "DosSemClear" USING
                BY VALUE sem-handle
                RETURNING RC

           IF RC NOT = 0
               EXIT PROGRAM RETURNING RC
           End-If.

      *****************************************************************
      *  Close semaphore since it is not used further.
      *****************************************************************
        CALL APIENTRY "DosCloseSem" USING
                        BY VALUE sem-handle
                RETURNING RC
                IF RC NOT = 0
                    EXIT PROGRAM RETURNING RC
                End-If.

        GOBACK.

