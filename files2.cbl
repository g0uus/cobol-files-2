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
       select Report-File
           assign to print
           organization is line sequential.

       DATA DIVISION.
       FILE SECTION.
      *    Bring in the file definition
       copy updates-rec.
       copy employee-rec.

       FD Report-File
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
           01 Report-Record PIC X(80).

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
           01  Report-Header.
                05  FILLER         PIC X(20) VALUE 'EMPLOYEE DATA REPORT'.
                05  FILLER         PIC X(20) VALUE ' GENERATED ON: '.
                05  REPORT-DATE    PIC X(20).
                05  FILLER         PIC X(20) VALUE SPACES.
           
           01  Report-Trailer.
                05 EOR.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'END OF REPORT'.
                   10  FILLER         PIC X(20) VALUE SPACES.
               05 Blank_record-Count.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'TOTAL BLANK RECORDS: '.
                   10  TOTAL-BLANKS   PIC Z(5)9 .
                   10  FILLER         PIC X(20) VALUE SPACES.
               05 Inserted-Record-Count.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'TOTAL RECORDS INSERTED: '.
                   10  TOTAL-INSERTS  PIC Z(5)9 .
                   10  FILLER         PIC X(20) VALUE SPACES.
               05 Updated-Record-Count.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'TOTAL RECORDS UPDATED: '.
                   10  TOTAL-UPDATES  PIC Z(5)9.
                   10  FILLER         PIC X(20) VALUE SPACES.
               05 Deleted-Record-Count.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'TOTAL RECORDS DELETED: '.
                   10  TOTAL-DELETES  PIC Z(5)9.
                   10  FILLER         PIC X(20) VALUE SPACES.
               05 Final-Counts.
                   10  FILLER         PIC X(20) VALUE SPACES.
                   10  FILLER         PIC X(24) VALUE 'TOTAL RECORDS PROCESSED: '.
                   10  TOTAL-RECS     PIC Z(5)9 .
                   10  FILLER         PIC X(20) VALUE SPACES.

           01 WS-TEMP-DT.   
               05 WS-TEMP-DATE-TIME.            
                   10 WS-TEMP-DATE.              
                      15 WS-TEMP-YEAR  PIC  9(4). 
                      15 WS-TEMP-MONTH PIC  9(2).
                      15 WS-TEMP-DAY   PIC  9(2).
                   10 WS-TEMP-TIME.              
                      15 WS-TEMP-HOUR  PIC  9(2).
                      15 WS-TEMP-MIN   PIC  9(2).
                      15 WS-TEMP-SEC   PIC  9(2).
                      15 WS-TEMP-MS    PIC  9(2).
                   10 WS-DIFF-GMT      PIC S9(4).
             
           01 WS-FORMATTED-DT.   
               05 WS-FORMATTED-DATE-TIME.                       
                   15 WS-FORMATTED-YEAR  PIC  9(4). 
                   15 FILLER             PIC X VALUE '-'.
                   15 WS-FORMATTED-MONTH PIC  9(2).
                   15 FILLER             PIC X VALUE '-'.
                   15 WS-FORMATTED-DAY   PIC  9(2).  
                   15 FILLER             PIC X VALUE ' '.           
                   15 WS-FORMATTED-HOUR  PIC  9(2).
                   15 FILLER             PIC X VALUE ':'.
                   15 WS-FORMATTED-MIN   PIC  9(2).
                   15 FILLER             PIC X VALUE ':'.
                   15 WS-FORMATTED-SEC   PIC  9(2).
       >>IF USE_MILLISECONDS is defined
                   15 FILLER             PIC X VALUE ':'.
                   15 WS-FORMATTED-MS    PIC  9(2).
       >>END-IF
             

           77 WS-REC-COUNT pic 9(4) comp sync value 0.
           77 WS-REC-COUNT-D pic Z(3)9 .
           77 WS-INSERT-COUNT pic 9(4) comp sync value 0.
           77 WS-UPDATE-COUNT pic 9(4) comp sync value 0.
           77 WS-DELETE-COUNT pic 9(4) comp sync value 0.
           77 WS-BLANK-COUNT pic 9(3)  comp sync value 0.

           77 WS-FILE-STATUS pic x(6) value spaces.
           77 WS-BLANK-COUNT-D pic z(3)9 .

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-procedure
       MAIN-PROCEDURE.
      D    display 'Starting...'

           OPEN output Report-File
           PERFORM Write-report-header.
           
           OPEN i-o EMPLOYEE
      D    display WS-FILE-STATUS.
           if WS-FILE-STATUS = '000035' then
               OPEN OUTPUT EMPLOYEE
               if WS-FILE-STATUS not = '000000' then
                   display 'FATAL: Unable to create new EMPLOYEE file. Status=' WS-FILE-STATUS
                   STOP RUN
               end-if
               display "Created new EMPLOYEE file."
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

           PERFORM Write-Report-Trailer.
           CLOSE Report-File.
      D    display 'Finished.'

           STOP RUN.

       Write-report-header.
           MOVE FUNCTION CURRENT-DATE TO WS-TEMP-DATE-TIME
           MOVE WS-TEMP-YEAR  TO WS-FORMATTED-YEAR
           MOVE WS-TEMP-MONTH TO WS-FORMATTED-MONTH
           MOVE WS-TEMP-DAY   TO WS-FORMATTED-DAY
           MOVE WS-TEMP-HOUR  TO WS-FORMATTED-HOUR
           MOVE WS-TEMP-MIN   TO WS-FORMATTED-MIN
           MOVE WS-TEMP-SEC   TO WS-FORMATTED-SEC
       >>IF USE_MILLISECONDS is defined
                  MOVE WS-TEMP-MS    TO WS-FORMATTED-MS
       >> end-if
           move WS-FORMATTED-DATE-TIME to REPORT-DATE

           WRITE Report-Record FROM Report-Header
           AFTER ADVANCING 2 LINES.

       Write-Report-Trailer.
           MOVE WS-BLANK-COUNT TO TOTAL-BLANKS
           MOVE WS-INSERT-COUNT TO TOTAL-INSERTS
           MOVE WS-UPDATE-COUNT TO TOTAL-UPDATES
           MOVE WS-DELETE-COUNT TO TOTAL-DELETES
           ADD WS-INSERT-COUNT WS-UPDATE-COUNT WS-DELETE-COUNT 
               GIVING TOTAL-RECS
           Write Report-Record FROM Blank_record-Count  
               AFTER ADVANCING 2 LINES.
           Write Report-Record FROM Inserted-Record-Count
               AFTER ADVANCING 1 LINE.
           Write Report-Record FROM Updated-Record-Count
               AFTER ADVANCING 1 LINE.
           Write Report-Record FROM Deleted-Record-Count
               AFTER ADVANCING 1 LINE.
           Write Report-Record FROM Final-Counts
               AFTER ADVANCING 1 LINE.
           WRITE Report-Record FROM Report-Trailer
               AFTER ADVANCING 1 LINE. 
       READ-PROCEDURE.
           add 1 to WS-REC-COUNT.
      *    display 'Reading Record - 'WS-REC-COUNT
           READ UPDATES
               AT END set WS-EOF to true
               NOT AT END PERFORM PROCESS-UPDATE-REC
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

       PROCESS-UPDATE-REC.
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
                   exit paragraph
               NOT INVALID KEY
                   DISPLAY 'Inserted Employee ID ' UPD-EMPLOYEE-ID
                   exit paragraph
           END-WRITE.
           ADD 1 TO WS-INSERT-COUNT.

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