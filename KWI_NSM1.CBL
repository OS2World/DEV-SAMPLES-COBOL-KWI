
      $set ans85 mf noosvs defaultbyte"00"
      ****************************************************************
      *
      *  Program:
      *      KWI_NSM1.CBL
      *  Desription:
      *      Allocates 32K of Named Shared Memory (NSM).  Creates a
      *      system semaphore so child process can signal receipt of 
      *      NSM.  Launches child process and waits for semaphore 
      *      to clear.
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
      ****************************************************************
      *  Constants used to create and access system semaphore
      ****************************************************************
           78 SEM-EXCLUSIVE             VALUE 0.
           78 SEM-NOT-EXCLUSIVE         VALUE 1.
           78 SEM-WAIT                  VALUE -1.
           78 SEM-RETURN                VALUE 0.

      ****************************************************************
      *  Constants used to launch child process
      ****************************************************************
           78 FILE-NAME-SIZE            VALUE 30.
           78 EXEC-SYNC                 VALUE 0.
           78 EXEC-ASYNC                VALUE 1.
           78 EXEC-ASYNCRESULT          VALUE 2.
           78 EXEC-TRACE                VALUE 3.
           78 EXEC-BACKGROUND           VALUE 4.
           78 EXEC-LOAD                 VALUE 5.


        01  RC          PIC 9(4) COMP-5 VALUE 0.
 
      ****************************************************************
      *  Fields used to allocate and access Named Shared Memory
      ****************************************************************
        01  seg-fields.
            05  seg-name        PIC X(30)
                                VALUE '\SHAREMEM\KWI_NSM.MEM'& X'00'.
            05  seg-ptr.
                10  seg-zero            PIC 9(4) COMP-5 VALUE ZERO.
 
                10  seg-selector        PIC 9(4) COMP-5.
            05  seg-pt                  REDEFINES seg-ptr.
                10 seg-pointer          USAGE IS POINTER.
        01  seg-selector-fields.
            05  seg-selector-ptr.
                10  seg-pointers-zero      PIC 9(4) COMP-5 VALUE ZERO.
                10  seg-pointers-selector  PIC 9(4) COMP-5.
            05  seg-selector-pt            REDEFINES seg-selector-ptr.
                10 seg-selectors-pointer   USAGE IS POINTER.

      ****************************************************************
      *  Fields used to create and access system semaphore
      ****************************************************************
        01 Semaphore-fields.
           05  sem-handle         USAGE IS POINTER.
           05  sem-name           PIC X(30)
                                  VALUE '\SEM\KWI_NSM.SEM' & X'00'.

      ****************************************************************
      *  Fields used to start child process.
      ****************************************************************
        01  START-FIELDS.
            05  START-FAIL-NAME         PIC X(30) VALUE x'00'.
            05  START-ARGS              PIC X(13) VALUE x'00'.
            05  START-ENV               PIC X(15) VALUE x'00'.
            05  START-PGM               PIC X(15) 
                                        VALUE 'KWI_NSM2.EXE' & x'00'.
            05  RESULTCODES.
                10 RESULTCODES-codeTerminate    PIC 9(4) COMP-5.
                10 RESULTCODES-codeResult       PIC 9(4) COMP-5.

        procedure division APIENTRY.
        main section.

      ****************************************************************
      *  Allocate Named Shared Memory
      ****************************************************************
        CALL APIENTRY "DosAllocShrSeg" USING
                        BY value 32768 SIZE 2
                        BY REFERENCE seg-name
                        BY REFERENCE seg-selector
                RETURNING RC
                IF RC NOT = 0
                    EXIT PROGRAM RETURNING RC
                End-If.

      ****************************************************************
      *  Create System Semaphore
      ****************************************************************
        CALL APIENTRY "DosCreateSem" USING
                        BY VALUE SEM-NOT-EXCLUSIVE SIZE 2
                        BY REFERENCE sem-handle
                        BY REFERENCE sem-name
                RETURNING RC
                IF RC NOT = 0
                   EXIT PROGRAM RETURNING RC
                End-If.

      ****************************************************************
      *  Set Semaphore so it can be cleared by child process
      ****************************************************************
        CALL APIENTRY "DosSemSet" USING
                        BY VALUE sem-handle
                RETURNING RC
                IF RC NOT = 0
                   EXIT PROGRAM RETURNING RC
                End-If.

      ****************************************************************
      *  Launch child process
      ****************************************************************
        CALL APIENTRY "DosExecPgm" USING
                        BY REFERENCE START-FAIL-NAME
                        BY VALUE     FILE-NAME-SIZE SIZE 2
                        BY VALUE     EXEC-ASYNC     SIZE 2
                        BY REFERENCE START-ARGS
                        BY REFERENCE START-ENV
                        BY REFERENCE RESULTCODES
                        BY REFERENCE START-PGM
                RETURNING RC
                IF RC NOT = 0
                    EXIT PROGRAM RETURNING RC
                End-If.

      ****************************************************************
      *  Wait for semaphore to be cleared
      ****************************************************************
        CALL APIENTRY "DosSemWait" USING
                        BY VALUE sem-handle
                        BY VALUE SEM-WAIT
                RETURNING RC

                IF RC NOT = 0
                    EXIT PROGRAM RETURNING RC
                End-If.

       GOBACK.

