       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-DATA.
       author. Graham Hanson.
       installation. My Laptop.
       date-written. 26/04/2025.
       date-compiled.
       security. None.


       ENVIRONMENT DIVISION.
       configuration section.
       source-computer. GrahamLap-01 with debugging mode.
       object-computer. GrahamLap-01.
       special-names.
       decimal-point is comma.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT UPDATES
           ASSIGN TO './Updates.dat'
           ORGANISATION IS LINE SEQUENTIAL
           access mode is sequential.   *> default
       SELECT EMPLOYEE
           ASSIGN TO './Employee.dat'
           ORGANISATION IS INDEXED
           access mode is random
           RECORD KEY IS EMPLOYEE-ID    *> primary key
           file status is WS-FILE-STATUS.    

       DATA DIVISION.
       FILE SECTION.
      *    Bring in the file definition
       copy updates-rec.
       copy employee-rec.

       WORKING-STORAGE SECTION.

           01 filler.
      *      indicates whether we have read the mandatory header record
             10 filler pic x value 'n'.
                88  hdr-found value 'Y'.
                88  hdr-not-found value 'n'.
      *      indicates when we get to End of File
             10 filler PIC X(1) VALUE 'N'.
                88 WS-EOF VALUE 'T'.
             10 filler pic x(1) value spaces.
                88 RT-BLANK value ' '.
                88 RT-HDR value 'H'.
                88 RT-INSERT value 'I'.
                88 RT-UNKNOWN value 'X'.

           77 WS-REC-COUNT pic 9(4) comp sync value 0.
           77 WS-REC-COUNT-D pic Z(3)9 .
           77 WS-BLANK-COUNT pic 9(3)  comp sync value 0.

           77 WS-FILE-STATUS pic x(6) value spaces.
           77 WS-BLANK-COUNT-D pic z(3)9 .

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-procedure
       MAIN-PROCEDURE.
      D    display 'Starting...'

           OPEN i-o EMPLOYEE
      D    display WS-FILE-STATUS.
           if WS-FILE-STATUS = '000035' then
               OPEN OUTPUT EMPLOYEE
               if WS-FILE-STATUS not = '000000' then
                   display 'FATAL: Unable to open EMPLOYEE file. Status=' WS-FILE-STATUS
                   STOP RUN
               end-if
               display "Created EMPLOYEE file."
           else if WS-FILE-STATUS not = '000000' then
               display 'FATAL: Unable to open EMPLOYEE file. Status=' WS-FILE-STATUS
               STOP RUN
           else
      D        display 'Opened EMPLOYEE file.'
           end-if
    
           OPEN INPUT UPDATES
           PERFORM READ-PROCEDURE UNTIL WS-EOF
           CLOSE UPDATES

           CLOSE EMPLOYEE

           perform RUN-STATS-PROCEDURE

      D    display 'Finished.'

           STOP RUN.

       READ-PROCEDURE.
           add 1 to WS-REC-COUNT.
      *    display 'Reading Record - 'WS-REC-COUNT
           READ UPDATES
               AT END set WS-EOF to true
               NOT AT END PERFORM PROCESS-UPDATE
           END-READ.

       GET-RECORD-TYPE.
           if HDR = space or hdr = low-value then
      *        display 'empty record...'
              add 1 to WS-BLANK-COUNT
              set RT-BLANK to true
              exit paragraph
           else
           if HDR-CHK = 'EMPLOYEE-ID' then
               if hdr-found then
                 set WS-EOF to true
                 display "FATAL: DUPLICATE HEADER RECORD."
                 exit paragraph
              else
                 set hdr-found to true
                 set RT-HDR to true
                 exit paragraph
              end-if
           else if UPD-OPERATION = 'I' then
               set RT-INSERT to true
               exit paragraph
           else
               set RT-UNKNOWN to true
               exit paragraph
           end-if.

       PROCESS-UPDATE.
           PERFORM GET-RECORD-TYPE.
           if WS-EOF or RT-HDR or RT-BLANK then
               exit paragraph
           end-if.
                    
      * process the update record
           if RT-INSERT then
               PERFORM PROCESS-INSERT
           else 
               display 'FATAL: UNKNOWN UPDATE OPERATION ' UPD-OPERATION 
               display '       on record ' WS-REC-COUNT
               display REC   
               set WS-EOF to true
           end-if.
   .

       PROCESS-INSERT.
           MOVE UPD-EMPLOYEE-ID TO EMPLOYEE-ID
           MOVE UPD-EMPLOYEE-NAME TO EMPLOYEE-NAME
           MOVE UPD-EMPLOYEE-AGE TO EMPLOYEE-AGE
           MOVE UPD-EMPLOYEE-GRADE TO EMPLOYEE-GRADE
           MOVE UPD-EMPLOYEE-SALARY TO EMPLOYEE-SALARY

           WRITE EMPLOYEE-RECORD
               INVALID KEY
                   DISPLAY 'FATAL: DUPLICATE EMPLOYEE ID ' UPD-EMPLOYEE-ID
                   set WS-EOF to true
               NOT INVALID KEY
                   DISPLAY 'Inserted Employee ID ' UPD-EMPLOYEE-ID
           END-WRITE.

       DISPLAY-PROCEDURE.
      *     display 'display rec - 'WS-REC-COUNT.

           if HDR-CHK = 'EMPLOYEE-ID' then
      *        display 'Header Record...'
              if hdr-found then
                 set WS-EOF to true
                 display "FATAL: DUPLICATE HEADER RECORD."
                 exit paragraph
              end-if
              set hdr-found to true
              exit paragraph
           else if HDR = space or hdr = low-value then
      *        display 'empty record...'
              add 1 to WS-BLANK-COUNT
              exit paragraph
           else
              if hdr-not-found then
                set WS-EOF to true
                display 'FATAL: Header Record Missing.'
                exit section
              end-if

               DISPLAY 'EMPLOYEE ID IS     : ' UPD-EMPLOYEE-ID
               DISPLAY 'EMPLOYEE NAME IS   : ' UPD-EMPLOYEE-NAME
               DISPLAY 'EMPLOYEE AGE is    : ' UPD-EMPLOYEE-AGE
               DISPLAY 'EMPLOYEE GRADE is  : ' UPD-EMPLOYEE-GRADE
               DISPLAY 'EMPLOYEE SALARY IS : £' UPD-EMPLOYEE-SALARY

               DISPLAY '-------------------------------------'
           end-if.

       RUN-STATS-PROCEDURE.
           move WS-REC-COUNT to WS-REC-COUNT-D
           DISPLAY 'Processed ' WS-REC-COUNT-D ' records'
           if WS-BLANK-COUNT greater than 0 then
               move WS-BLANK-COUNT to WS-BLANK-COUNT-D
               display '  including ' WS-BLANK-COUNT-D ' blank records'
           end-if