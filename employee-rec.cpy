       FD EMPLOYEE
      *    these are all defaults or ignored
           is external
      *    record contains 65 characters
           block contains 1 records
           label records are standard.
      *    Recording mode is not supported by gcobol
      *    RECORDING MODE IS V.
           01 EMPLOYEE-RECORD. 
               05 EMPLOYEE-ID PIC 9(3).
               05 EMPLOYEE-NAME PIC X(6).
               05 EMPLOYEE-AGE PIC 9(2).
               05 EMPLOYEE-GRADE PIC X(1).
               05 EMPLOYEE-SALARY PIC 9(5).