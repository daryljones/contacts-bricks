      *> PERSV -- record-id validator for the PERS contacts facility,
      *> invoked via EXEC CICS LINK PROGRAM('PERSV') COMMAREA(...) from
      *> PERS. Reads the candidate id from DFHCOMMAREA; if it parses as
      *> a positive integer, rewrites it as the canonical 8-digit
      *> zero-padded form ("1" -> "00000001") so the downstream READ
      *> sees the same key the list screen displays. Non-numeric input
      *> is left alone; the caller's READ will then fail NOTFND.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERSV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KEY-IN PIC X(8).
       01 NUM    PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       MAIN.
           MOVE DFHCOMMAREA TO KEY-IN.
           IF KEY-IN NOT = SPACES THEN
      *> NUMVAL strips leading / trailing spaces. Non-numeric input
      *> yields 0 in standard COBOL; the IF NUM > 0 guard then leaves
      *> KEY-IN untouched so garbage passes through and surfaces as
      *> NOTFND at the caller's READ rather than as "00000000".
               COMPUTE NUM = FUNCTION NUMVAL(KEY-IN)
               IF NUM > 0 THEN
                   MOVE NUM TO KEY-IN
               END-IF
           END-IF.
           DISPLAY 'PERSV: validated record id = ' KEY-IN.
           MOVE KEY-IN TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.
           STOP RUN.
