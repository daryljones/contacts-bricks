      *> PERS -- person contacts CRUD facility (BRICKS COBOL).
      *>
      *> Action menu (PERS1):
      *>   A=Add        auto-assign an 8-digit record id, prompt PERS2
      *>   Q=Query      read record by id, display PERS2 in read-only
      *>   U=Update     read with UPDATE, edit on PERS2, REWRITE
      *>   D=Delete     read for confirmation, ENTER deletes, PF3 cancels
      *>   L=List       paged list via LINK PROGRAM('PERL')
      *>   S=Search     filtered scan via LINK PROGRAM('PERL') with the
      *>                operator's filter in DFHCOMMAREA
      *>
      *> Record format on disk (pipe-separated, 15 fields):
      *>   name|sex|race|dob|addr1|addr2|city|state|zip|email|ph1|ph2|
      *>     note|date-added|date-updated
      *>
      *> Date fields are YYYY-MM-DD; PERS uses EXEC CICS ASSIGN
      *> TODAYYR / TODAYMO / TODAYDY to stamp them.
      *>
      *> Record IDs are 8-digit zero-padded numerics ("00000001"). On
      *> Add, PERS finds the current max id via STARTBR / READNEXT and
      *> writes max+1; on Q/U/D it LINKs PERV first so "1" canonicalises
      *> to "00000001" before the READ.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DFHAID.
       COPY DFHRESP.
       01 USR PIC X(8).
       01 TRM PIC X(4).

      *> SCR is the PERS1 menu IO group. RECID is the operator-typed
      *> record id (validated by PERV for Q/U/D); SEARCH carries the
      *> optional filter for the S action.
       01 SCR.
          05 INFOLINE PIC X(78).
          05 ACTION   PIC X(1).
          05 RECID    PIC X(8).
          05 SEARCH   PIC X(30).
          05 MSG      PIC X(60).

      *> DET is the PERS2 detail IO group. Each field renames its map
      *> slot with a D prefix because DataByName is global across the
      *> program (see the comment at the top of GUST.cob).
       01 DET.
          05 DINFOLINE PIC X(78).
          05 DRECID    PIC X(8).
          05 DADDED    PIC X(10).
          05 DUPDATED  PIC X(10).
          05 DNAME     PIC X(40).
          05 DSEX      PIC X(1).
          05 DRACE     PIC X(20).
          05 DDOB      PIC X(10).
          05 DADDR1    PIC X(40).
          05 DADDR2    PIC X(40).
          05 DCITY     PIC X(25).
          05 DSTATE    PIC X(2).
          05 DZIP      PIC X(10).
          05 DEMAIL    PIC X(40).
          05 DPH1      PIC X(16).
          05 DPH2      PIC X(16).
          05 DNOTE     PIC X(60).
          05 DMODE     PIC X(76).
          05 DMSG      PIC X(76).

       01 KEY-IN  PIC X(8).
       01 SRCH    PIC X(30).
       01 SAVED-SEARCH PIC X(30).
       01 REC     PIC X(400).

      *> Scratch UNSTRING targets, one per record field. Names start
      *> with Q to stay unique against the D-prefixed map group above.
       01 QNAME   PIC X(40).
       01 QSEX    PIC X(1).
       01 QRACE   PIC X(20).
       01 QDOB    PIC X(10).
       01 QADDR1  PIC X(40).
       01 QADDR2  PIC X(40).
       01 QCITY   PIC X(25).
       01 QSTATE  PIC X(2).
       01 QZIP    PIC X(10).
       01 QEMAIL  PIC X(40).
       01 QPH1    PIC X(16).
       01 QPH2    PIC X(16).
       01 QNOTE   PIC X(60).
       01 QADDED  PIC X(10).
       01 QUPDATED PIC X(10).

      *> Today's date components glued into TODAY-STR as YYYY-MM-DD.
       01 TY        PIC 9(4).
       01 TM        PIC 9(2).
       01 TD        PIC 9(2).
       01 TODAY-STR PIC X(10).

       01 EXIT-FLAG    PIC X(1) VALUE 'N'.
       01 VALID-ACTION PIC X(1) VALUE 'N'.
       01 CMSG         PIC X(60).
       01 RESP-STR     PIC X(8).

      *> Auto-id allocation. FIND-MAX-ID walks every key with
      *> STARTBR / READNEXT and remembers the highest; ACT-ADD then
      *> writes MAX-ID + 1. On concurrent Adds the loser gets a
      *> RESP-DUPREC back from WRITE and can retry.
       01 MAX-ID    PIC 9(8) VALUE 0.
       01 CUR-ID    PIC 9(8) VALUE 0.
       01 NEW-ID    PIC 9(8) VALUE 0.
       01 SCAN-DONE PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN.
           EXEC CICS ASSIGN USERID(USR) TERMID(TRM) END-EXEC.

      *> Stamp today's date once; reused for DADDED (on Add) and
      *> DUPDATED (on Add + Update).
           EXEC CICS ASSIGN TODAYYR(TY) TODAYMO(TM) TODAYDY(TD) END-EXEC.
           MOVE SPACES TO TODAY-STR.
           STRING TY DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TM DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  TD DELIMITED BY SIZE
               INTO TODAY-STR
           END-STRING.

           MOVE SPACES TO SCR.
           STRING 'User: ' DELIMITED BY SIZE
                  USR DELIMITED BY SIZE
                  '                            Term: ' DELIMITED BY SIZE
                  TRM DELIMITED BY SIZE
               INTO INFOLINE
           END-STRING.

           PERFORM MENU-LOOP UNTIL EXIT-FLAG = 'Y'.

           EXEC CICS RETURN END-EXEC.
           STOP RUN.

       MENU-LOOP.
           EXEC CICS SEND MAP('PERS1') FROM(SCR) ERASE END-EXEC.
           EXEC CICS RECEIVE MAP('PERS1') INTO(SCR) END-EXEC.

      *> PF12 from the menu exits the transaction (matches GUST).
           IF EIBAID = PF12 THEN
               MOVE 'Y' TO EXIT-FLAG
           END-IF.

           IF EXIT-FLAG NOT = 'Y' THEN
               MOVE FUNCTION UPPER-CASE(ACTION) TO ACTION
               MOVE FUNCTION UPPER-CASE(RECID) TO KEY-IN
               MOVE SEARCH TO SRCH
               PERFORM DISPATCH
           END-IF.

       DISPATCH.
      *> Clear MSG at dispatch entry. See GUST.cob's note on why
      *> "IF MSG = SPACES" can't be the dispatch guard.
           MOVE SPACES TO MSG.
           MOVE 'Y' TO VALID-ACTION.

           IF ACTION = SPACES THEN
               MOVE 'Action required (A/Q/U/D/L/S, F12=exit).' TO MSG
               MOVE 'N' TO VALID-ACTION
           END-IF.
           IF VALID-ACTION = 'Y' THEN
               IF ACTION NOT = 'A' AND ACTION NOT = 'Q'
                  AND ACTION NOT = 'U' AND ACTION NOT = 'D'
                  AND ACTION NOT = 'L' AND ACTION NOT = 'S' THEN
                   STRING 'Unknown action: ' DELIMITED BY SIZE
                          ACTION DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
                   MOVE 'N' TO VALID-ACTION
               END-IF
           END-IF.
           IF VALID-ACTION = 'Y' THEN
               IF ACTION = 'Q' OR ACTION = 'U' OR ACTION = 'D' THEN
                   IF KEY-IN = SPACES THEN
                       MOVE 'Record ID required for Q/U/D.' TO MSG
                       MOVE 'N' TO VALID-ACTION
                   END-IF
               END-IF
           END-IF.

           IF VALID-ACTION = 'Y' THEN
      *> Canonicalise the key for actions that need one. A auto-
      *> assigns, L/S do not need a key, so skip PERV for those.
               IF ACTION = 'Q' OR ACTION = 'U' OR ACTION = 'D' THEN
                   EXEC CICS LINK PROGRAM('PERV')
                                  COMMAREA(KEY-IN) END-EXEC
               END-IF
               EVALUATE ACTION
                   WHEN 'A'
                       PERFORM ACT-ADD
                   WHEN 'Q'
                       PERFORM ACT-QUERY
                   WHEN 'U'
                       PERFORM ACT-UPDATE
                   WHEN 'D'
                       PERFORM ACT-DELETE
                   WHEN 'L'
                       PERFORM ACT-LIST
                   WHEN 'S'
                       PERFORM ACT-SEARCH
                   WHEN OTHER
                       MOVE 'Internal error: action fell through.' TO MSG
               END-EVALUATE
           END-IF.

       ACT-ADD.
      *> Allocate the next id, then prompt PERS2 with that id pre-set
      *> and DADDED / DUPDATED both stamped today.
           PERFORM FIND-MAX-ID.
           COMPUTE NEW-ID = MAX-ID + 1.
           MOVE NEW-ID TO KEY-IN.

           MOVE SPACES    TO DET.
           MOVE KEY-IN    TO DRECID.
           MOVE TODAY-STR TO DADDED.
           MOVE TODAY-STR TO DUPDATED.
           MOVE 'Mode: ADD -- fill in fields and press ENTER (PF3=cancel).'
                TO DMODE.

           EXEC CICS SEND MAP('PERS2') FROM(DET) ERASE END-EXEC.
           EXEC CICS RECEIVE MAP('PERS2') INTO(DET) END-EXEC.

           IF EIBAID = PF03 THEN
               MOVE 'Add cancelled.' TO MSG
           ELSE
               PERFORM BUILD-REC
               EXEC CICS WRITE FILE('contacts') FROM(REC)
                               RIDFLD(KEY-IN) END-EXEC
               IF EIBRESP = RESP-NORMAL THEN
                   STRING 'Contact ' DELIMITED BY SIZE
                          KEY-IN DELIMITED BY SIZE
                          ' added.' DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
               IF EIBRESP = RESP-DUPREC THEN
                   STRING 'Contact ' DELIMITED BY SIZE
                          KEY-IN DELIMITED BY SIZE
                          ' already exists -- retry.' DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
               IF EIBRESP NOT = RESP-NORMAL
                  AND EIBRESP NOT = RESP-DUPREC THEN
                   MOVE EIBRESP TO RESP-STR
                   STRING 'WRITE failed RESP=' DELIMITED BY SIZE
                          RESP-STR DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
           END-IF.

       ACT-QUERY.
           EXEC CICS READ FILE('contacts') INTO(REC)
                          RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP = RESP-NOTFND THEN
               STRING 'Contact ' DELIMITED BY SIZE
                      KEY-IN DELIMITED BY SIZE
                      ' not found.' DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP NOT = RESP-NORMAL
              AND EIBRESP NOT = RESP-NOTFND THEN
               MOVE EIBRESP TO RESP-STR
               STRING 'READ failed RESP=' DELIMITED BY SIZE
                      RESP-STR DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP = RESP-NORMAL THEN
               PERFORM SHOW-DETAIL-FROM-REC
               MOVE 'Mode: QUERY -- press ENTER to return to the menu.'
                    TO DMODE
               EXEC CICS SEND MAP('PERS2') FROM(DET) ERASE END-EXEC
               EXEC CICS RECEIVE MAP('PERS2') INTO(DET) END-EXEC
               STRING 'Query of ' DELIMITED BY SIZE
                      KEY-IN DELIMITED BY SIZE
                      ' complete.' DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.

       ACT-UPDATE.
           EXEC CICS READ FILE('contacts') INTO(REC)
                          RIDFLD(KEY-IN) UPDATE END-EXEC.
           IF EIBRESP = RESP-NOTFND THEN
               STRING 'Contact ' DELIMITED BY SIZE
                      KEY-IN DELIMITED BY SIZE
                      ' not found.' DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP NOT = RESP-NORMAL
              AND EIBRESP NOT = RESP-NOTFND THEN
               MOVE EIBRESP TO RESP-STR
               STRING 'READ UPDATE failed RESP=' DELIMITED BY SIZE
                      RESP-STR DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP = RESP-NORMAL THEN
               PERFORM SHOW-DETAIL-FROM-REC
               MOVE 'Mode: UPDATE -- edit fields and press ENTER (PF3=cancel).'
                    TO DMODE
               EXEC CICS SEND MAP('PERS2') FROM(DET) ERASE END-EXEC
               EXEC CICS RECEIVE MAP('PERS2') INTO(DET) END-EXEC
               IF EIBAID = PF03 THEN
                   MOVE 'Update cancelled.' TO MSG
               ELSE
      *> Stamp the updated date and rebuild. DADDED stays as-is
      *> (the map slot is PROT, so the operator can't edit it).
                   MOVE TODAY-STR TO DUPDATED
                   PERFORM BUILD-REC
                   EXEC CICS REWRITE FILE('contacts') FROM(REC) END-EXEC
                   IF EIBRESP = RESP-NORMAL THEN
                       STRING 'Contact ' DELIMITED BY SIZE
                              KEY-IN DELIMITED BY SIZE
                              ' updated.' DELIMITED BY SIZE
                           INTO MSG
                       END-STRING
                   END-IF
                   IF EIBRESP NOT = RESP-NORMAL THEN
                       MOVE EIBRESP TO RESP-STR
                       STRING 'REWRITE failed RESP=' DELIMITED BY SIZE
                              RESP-STR DELIMITED BY SIZE
                           INTO MSG
                       END-STRING
                   END-IF
               END-IF
           END-IF.

       ACT-DELETE.
      *> Two-step delete: render the record first so the operator
      *> confirms with ENTER, or backs out with PF3.
           EXEC CICS READ FILE('contacts') INTO(REC)
                          RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP = RESP-NOTFND THEN
               STRING 'Contact ' DELIMITED BY SIZE
                      KEY-IN DELIMITED BY SIZE
                      ' not found.' DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP NOT = RESP-NORMAL
              AND EIBRESP NOT = RESP-NOTFND THEN
               MOVE EIBRESP TO RESP-STR
               STRING 'READ failed RESP=' DELIMITED BY SIZE
                      RESP-STR DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.
           IF EIBRESP = RESP-NORMAL THEN
               PERFORM SHOW-DETAIL-FROM-REC
               MOVE 'Mode: DELETE -- ENTER confirms, PF3 cancels.'
                    TO DMODE
               EXEC CICS SEND MAP('PERS2') FROM(DET) ERASE END-EXEC
               EXEC CICS RECEIVE MAP('PERS2') INTO(DET) END-EXEC
               IF EIBAID = PF03 THEN
                   MOVE 'Delete cancelled.' TO MSG
               ELSE
                   EXEC CICS DELETE FILE('contacts')
                                    RIDFLD(KEY-IN) END-EXEC
                   IF EIBRESP = RESP-NORMAL THEN
                       STRING 'Contact ' DELIMITED BY SIZE
                              KEY-IN DELIMITED BY SIZE
                              ' deleted.' DELIMITED BY SIZE
                           INTO MSG
                       END-STRING
                   END-IF
                   IF EIBRESP NOT = RESP-NORMAL THEN
                       MOVE EIBRESP TO RESP-STR
                       STRING 'DELETE failed RESP=' DELIMITED BY SIZE
                              RESP-STR DELIMITED BY SIZE
                           INTO MSG
                       END-STRING
                   END-IF
               END-IF
           END-IF.

       ACT-LIST.
           MOVE SPACES TO CMSG.
           EXEC CICS LINK PROGRAM('PERL') COMMAREA(CMSG) END-EXEC.
           IF CMSG = '0' THEN
               MOVE 'No contacts on file.' TO MSG
           END-IF.
           IF CMSG NOT = '0' THEN
               STRING 'List complete (' DELIMITED BY SIZE
                      CMSG DELIMITED BY SIZE
                      ' records).' DELIMITED BY SIZE
                   INTO MSG
               END-STRING
           END-IF.

       ACT-SEARCH.
      *> Blank search term degrades to ACT-LIST (matches the cust.rexx
      *> twin). A non-empty SRCH triggers PERL's filtered path; the
      *> COMMAREA carries the filter in and the match count out.
           IF SRCH = SPACES THEN
               MOVE SPACES TO CMSG
               EXEC CICS LINK PROGRAM('PERL') COMMAREA(CMSG) END-EXEC
               IF CMSG = '0' THEN
                   MOVE 'No contacts on file.' TO MSG
               END-IF
               IF CMSG NOT = '0' THEN
                   STRING 'List complete (' DELIMITED BY SIZE
                          CMSG DELIMITED BY SIZE
                          ' records).' DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
           END-IF.
           IF SRCH NOT = SPACES THEN
               MOVE SRCH TO SAVED-SEARCH
               EXEC CICS LINK PROGRAM('PERL') COMMAREA(SRCH) END-EXEC
               IF FUNCTION NUMVAL(SRCH) = 0 THEN
                   STRING 'No contacts match "' DELIMITED BY SIZE
                          SAVED-SEARCH DELIMITED BY SIZE
                          '".' DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
               IF FUNCTION NUMVAL(SRCH) NOT = 0 THEN
                   STRING 'Search "' DELIMITED BY SIZE
                          SAVED-SEARCH DELIMITED BY SIZE
                          '" matched ' DELIMITED BY SIZE
                          SRCH DELIMITED BY SIZE
                          ' record(s).' DELIMITED BY SIZE
                       INTO MSG
                   END-STRING
               END-IF
           END-IF.

       FIND-MAX-ID.
           MOVE 0 TO MAX-ID.
           MOVE 'N' TO SCAN-DONE.
           EXEC CICS STARTBR FILE('contacts') END-EXEC.
           PERFORM SCAN-MAX UNTIL SCAN-DONE = 'Y'.
           EXEC CICS ENDBR FILE('contacts') END-EXEC.

       SCAN-MAX.
           MOVE SPACES TO REC.
           MOVE SPACES TO KEY-IN.
           EXEC CICS READNEXT FILE('contacts') INTO(REC)
                              RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP NOT = RESP-NORMAL THEN
               MOVE 'Y' TO SCAN-DONE
           ELSE
      *> NUMVAL of a non-numeric key returns 0, which never beats
      *> MAX-ID -- so a stray manual entry harmlessly contributes 0
      *> and the loop keeps scanning real ids.
               COMPUTE CUR-ID = FUNCTION NUMVAL(KEY-IN)
               IF CUR-ID > MAX-ID THEN
                   MOVE CUR-ID TO MAX-ID
               END-IF
           END-IF.

       BUILD-REC.
      *> Glue the 15 detail fields into the pipe-delimited record.
      *> STRING DELIMITED BY SIZE rstrips each segment (bricks
      *> pragmatic concat rule), matching GUST's customer-file layout.
           MOVE SPACES TO REC.
           STRING DNAME    DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DSEX     DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DRACE    DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DDOB     DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DADDR1   DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DADDR2   DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DCITY    DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DSTATE   DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DZIP     DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DEMAIL   DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DPH1     DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DPH2     DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DNOTE    DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DADDED   DELIMITED BY SIZE
                  '|'      DELIMITED BY SIZE
                  DUPDATED DELIMITED BY SIZE
               INTO REC
           END-STRING.

       SHOW-DETAIL-FROM-REC.
      *> UNSTRING REC on '|' into the 15 scratch slots, then move into
      *> the D-prefixed DET children expected by SEND MAP('PERS2').
           MOVE SPACES TO QNAME.    MOVE SPACES TO QSEX.
           MOVE SPACES TO QRACE.    MOVE SPACES TO QDOB.
           MOVE SPACES TO QADDR1.   MOVE SPACES TO QADDR2.
           MOVE SPACES TO QCITY.    MOVE SPACES TO QSTATE.
           MOVE SPACES TO QZIP.     MOVE SPACES TO QEMAIL.
           MOVE SPACES TO QPH1.     MOVE SPACES TO QPH2.
           MOVE SPACES TO QNOTE.    MOVE SPACES TO QADDED.
           MOVE SPACES TO QUPDATED.
           UNSTRING REC DELIMITED BY '|'
               INTO QNAME QSEX QRACE QDOB QADDR1 QADDR2
                    QCITY QSTATE QZIP QEMAIL QPH1 QPH2
                    QNOTE QADDED QUPDATED
           END-UNSTRING.
           MOVE SPACES   TO DET.
           MOVE KEY-IN   TO DRECID.
           MOVE QADDED   TO DADDED.
           MOVE QUPDATED TO DUPDATED.
           MOVE QNAME    TO DNAME.
           MOVE QSEX     TO DSEX.
           MOVE QRACE    TO DRACE.
           MOVE QDOB     TO DDOB.
           MOVE QADDR1   TO DADDR1.
           MOVE QADDR2   TO DADDR2.
           MOVE QCITY    TO DCITY.
           MOVE QSTATE   TO DSTATE.
           MOVE QZIP     TO DZIP.
           MOVE QEMAIL   TO DEMAIL.
           MOVE QPH1     TO DPH1.
           MOVE QPH2     TO DPH2.
           MOVE QNOTE    TO DNOTE.
