      *> PERL -- COBOL twin of GUSL, adapted for the PERS contacts file.
      *> Paged list with PF7/PF8 (DFHCOMMAREA blank) and one-shot
      *> filtered search (DFHCOMMAREA carries the operator's filter).
      *> Returns the record count back through DFHCOMMAREA so PERS can
      *> render "List complete (N records)." or "matched N record(s)."
      *>
      *> Contact records are pipe-delimited with 15 fields; only NAME,
      *> CITY and PH1 are surfaced on the list row -- the rest live on
      *> PERS2 for detailed query. The full record participates in the
      *> filter haystack so a search for "Alice", "boston", or even a
      *> partial email matches the row regardless of which column the
      *> needle ended up in.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DFHAID.
       COPY DFHRESP.

      *> SCR is the PERSL map IO group. Each ROW group concatenates
      *> ID || ' ' || NAME(30) || ' ' || CITY(20) || ' ' || PHONE(16)
      *> = 77 chars, which fits the LEN 76 ROW slots after rstrip.
       01 SCR.
          05 INFOLINE PIC X(78).
          05 ROW1.
             10 K1   PIC X(8).
             10 SP1A PIC X(1).
             10 N1   PIC X(30).
             10 SP1B PIC X(1).
             10 C1   PIC X(20).
             10 SP1C PIC X(1).
             10 P1   PIC X(16).
          05 ROW2.
             10 K2   PIC X(8).
             10 SP2A PIC X(1).
             10 N2   PIC X(30).
             10 SP2B PIC X(1).
             10 C2   PIC X(20).
             10 SP2C PIC X(1).
             10 P2   PIC X(16).
          05 ROW3.
             10 K3   PIC X(8).
             10 SP3A PIC X(1).
             10 N3   PIC X(30).
             10 SP3B PIC X(1).
             10 C3   PIC X(20).
             10 SP3C PIC X(1).
             10 P3   PIC X(16).
          05 ROW4.
             10 K4   PIC X(8).
             10 SP4A PIC X(1).
             10 N4   PIC X(30).
             10 SP4B PIC X(1).
             10 C4   PIC X(20).
             10 SP4C PIC X(1).
             10 P4   PIC X(16).
          05 ROW5.
             10 K5   PIC X(8).
             10 SP5A PIC X(1).
             10 N5   PIC X(30).
             10 SP5B PIC X(1).
             10 C5   PIC X(20).
             10 SP5C PIC X(1).
             10 P5   PIC X(16).
          05 ROW6.
             10 K6   PIC X(8).
             10 SP6A PIC X(1).
             10 N6   PIC X(30).
             10 SP6B PIC X(1).
             10 C6   PIC X(20).
             10 SP6C PIC X(1).
             10 P6   PIC X(16).
          05 ROW7.
             10 K7   PIC X(8).
             10 SP7A PIC X(1).
             10 N7   PIC X(30).
             10 SP7B PIC X(1).
             10 C7   PIC X(20).
             10 SP7C PIC X(1).
             10 P7   PIC X(16).
          05 ROW8.
             10 K8   PIC X(8).
             10 SP8A PIC X(1).
             10 N8   PIC X(30).
             10 SP8B PIC X(1).
             10 C8   PIC X(20).
             10 SP8C PIC X(1).
             10 P8   PIC X(16).
          05 ROW9.
             10 K9   PIC X(8).
             10 SP9A PIC X(1).
             10 N9   PIC X(30).
             10 SP9B PIC X(1).
             10 C9   PIC X(20).
             10 SP9C PIC X(1).
             10 P9   PIC X(16).
          05 ROW10.
             10 K10   PIC X(8).
             10 SP10A PIC X(1).
             10 N10   PIC X(30).
             10 SP10B PIC X(1).
             10 C10   PIC X(20).
             10 SP10C PIC X(1).
             10 P10   PIC X(16).
          05 ROW11.
             10 K11   PIC X(8).
             10 SP11A PIC X(1).
             10 N11   PIC X(30).
             10 SP11B PIC X(1).
             10 C11   PIC X(20).
             10 SP11C PIC X(1).
             10 P11   PIC X(16).
          05 ROW12.
             10 K12   PIC X(8).
             10 SP12A PIC X(1).
             10 N12   PIC X(30).
             10 SP12B PIC X(1).
             10 C12   PIC X(20).
             10 SP12C PIC X(1).
             10 P12   PIC X(16).
          05 ROW13.
             10 K13   PIC X(8).
             10 SP13A PIC X(1).
             10 N13   PIC X(30).
             10 SP13B PIC X(1).
             10 C13   PIC X(20).
             10 SP13C PIC X(1).
             10 P13   PIC X(16).
          05 ROW14.
             10 K14   PIC X(8).
             10 SP14A PIC X(1).
             10 N14   PIC X(30).
             10 SP14B PIC X(1).
             10 C14   PIC X(20).
             10 SP14C PIC X(1).
             10 P14   PIC X(16).
          05 ROW15.
             10 K15   PIC X(8).
             10 SP15A PIC X(1).
             10 N15   PIC X(30).
             10 SP15B PIC X(1).
             10 C15   PIC X(20).
             10 SP15C PIC X(1).
             10 P15   PIC X(16).

      *> Saved per-page keys. The row groups themselves carry the keys
      *> in K<n>, but rows are unprotected so the operator could in
      *> principle overtype them. SAVE-KEYS snapshots the keys after
      *> the page is populated so cursor-selection always points at the
      *> record we originally rendered.
       01 SK1  PIC X(8).
       01 SK2  PIC X(8).
       01 SK3  PIC X(8).
       01 SK4  PIC X(8).
       01 SK5  PIC X(8).
       01 SK6  PIC X(8).
       01 SK7  PIC X(8).
       01 SK8  PIC X(8).
       01 SK9  PIC X(8).
       01 SK10 PIC X(8).
       01 SK11 PIC X(8).
       01 SK12 PIC X(8).
       01 SK13 PIC X(8).
       01 SK14 PIC X(8).
       01 SK15 PIC X(8).

      *> DET is the PERS2 detail IO group, reused for the "view from
      *> list" flow. Same shape and naming as PERS.cob's DET so the
      *> SEND MAP('PERS2') FROM(DET) lands fields in the right slots.
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

      *> Cursor-selection scratch. EIBCPOSN is 1-based; SEL-FROM-CURSOR
      *> reduces it to a map row, then to a 1..15 row slot (or 0 when
      *> the cursor wasn't on a list row). SEL-KEY is the saved key
      *> for that slot, fed back into KEY-IN for the SHOW-DETAIL READ.
       01 CPOSN     PIC 9(4) VALUE 0.
       01 CRBASE    PIC 9(4) VALUE 0.
       01 CROW      PIC 9(4) VALUE 0.
       01 SEL-SLOT  PIC 9(2) VALUE 0.
       01 SEL-KEY   PIC X(8).

       01 KEY-IN PIC X(8).
       01 REC    PIC X(400).
       01 NCOUNT PIC 9(4) VALUE 0.

       01 PAGE-NO   PIC 9(4) VALUE 1.
       01 SKIPCNT   PIC 9(8).
       01 SKIPGOAL  PIC 9(8).
       01 EXIT-FLAG PIC X(1) VALUE 'N'.

      *> Search-filter state (mirrors GUSL).
       01 FILTER-IN PIC X(2000).
       01 FILTER    PIC X(2000).
       01 FILTERED  PIC X(1) VALUE 'N'.
       01 HAYRAW    PIC X(450).
       01 HAY       PIC X(450).
       01 MATCHIDX  PIC 9(8).
       01 SLOT      PIC 9(4) VALUE 0.
       01 SCAN-DONE PIC X(1) VALUE 'N'.

      *> Scratch UNSTRING targets -- one per record field. NM / CY /
      *> PH are the columns surfaced on the list row; the rest are
      *> consumed by UNSTRING but not displayed (they still feed the
      *> haystack via the unmodified REC).
       01 SNAME    PIC X(40).
       01 SSEX     PIC X(1).
       01 SRACE    PIC X(20).
       01 SDOB     PIC X(10).
       01 SADDR1   PIC X(40).
       01 SADDR2   PIC X(40).
       01 SCITY    PIC X(25).
       01 SSTATE   PIC X(2).
       01 SZIP     PIC X(10).
       01 SEMAIL   PIC X(40).
       01 SPH1     PIC X(16).
       01 SPH2     PIC X(16).
       01 SNOTE    PIC X(60).
       01 SADDED   PIC X(10).
       01 SUPDATED PIC X(10).

       PROCEDURE DIVISION.
       MAIN.
      *> DFHCOMMAREA optionally carries a filter from PERS's S action.
      *> Upper-case + trim once so each record's POS check is cheap.
      *> Blank filter -> the legacy paginated all-records flow.
           MOVE DFHCOMMAREA TO FILTER-IN.
           MOVE FUNCTION UPPER-CASE(FILTER-IN) TO FILTER.
           IF FUNCTION TRIM(FILTER) NOT = SPACES THEN
               MOVE 'Y' TO FILTERED
           END-IF.

           IF FILTERED = 'Y' THEN
               PERFORM FILTERED-LIST
           ELSE
               PERFORM PAGE-LOOP UNTIL EXIT-FLAG = 'Y'
           END-IF.

           MOVE NCOUNT TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.
           STOP RUN.

       FILTERED-LIST.
           PERFORM CLEAR-ROWS.
           MOVE 0 TO NCOUNT.
           MOVE 0 TO SLOT.
           MOVE 'N' TO SCAN-DONE.
           MOVE SPACES TO INFOLINE.
           EXEC CICS STARTBR FILE('contacts') END-EXEC.
           PERFORM SCAN-ONE UNTIL SCAN-DONE = 'Y'.
           EXEC CICS ENDBR FILE('contacts') END-EXEC.

           STRING 'PERL -- search: ' DELIMITED BY SIZE
                  FILTER DELIMITED BY SIZE
                  '  matched: ' DELIMITED BY SIZE
                  NCOUNT DELIMITED BY SIZE
               INTO INFOLINE
           END-STRING.

      *> Snapshot the keys for cursor-selection, then loop on
      *> SEND / RECEIVE so ENTER on a row opens detail and PF3 exits.
      *> Other keys exit too -- nothing useful to do on the same screen.
           PERFORM SAVE-KEYS.
           MOVE 'N' TO EXIT-FLAG.
           PERFORM SHOW-FILTERED-ONCE UNTIL EXIT-FLAG = 'Y'.

       SHOW-FILTERED-ONCE.
           EXEC CICS SEND MAP('PERSL') FROM(SCR) ERASE END-EXEC.
           EXEC CICS RECEIVE MAP('PERSL') END-EXEC.
           EVALUATE EIBAID
               WHEN ENTER
                   PERFORM SEL-FROM-CURSOR
                   IF SEL-SLOT > 0 THEN
                       PERFORM PICK-KEY
                       IF SEL-KEY NOT = SPACES THEN
                           MOVE SEL-KEY TO KEY-IN
                           PERFORM SHOW-DETAIL
                       END-IF
                   END-IF
               WHEN PF03
                   MOVE 'Y' TO EXIT-FLAG
               WHEN PF12
                   MOVE 'Y' TO EXIT-FLAG
               WHEN OTHER
                   MOVE 'Y' TO EXIT-FLAG
           END-EVALUATE.

       SCAN-ONE.
           MOVE SPACES TO REC.
           MOVE SPACES TO KEY-IN.
           EXEC CICS READNEXT FILE('contacts') INTO(REC)
                              RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP NOT = RESP-NORMAL THEN
               MOVE 'Y' TO SCAN-DONE
           END-IF.
           IF EIBRESP = RESP-NORMAL THEN
               MOVE SPACES TO HAYRAW
               STRING KEY-IN DELIMITED BY SIZE
                      ' ' DELIMITED BY SIZE
                      REC DELIMITED BY SIZE
                   INTO HAYRAW
               END-STRING
               MOVE FUNCTION UPPER-CASE(HAYRAW) TO HAY
               COMPUTE MATCHIDX = FUNCTION POS(
                   FUNCTION TRIM(FILTER), HAY)
               IF MATCHIDX > 0 THEN
                   COMPUTE NCOUNT = NCOUNT + 1
                   IF SLOT < 15 THEN
                       COMPUTE SLOT = SLOT + 1
                       PERFORM SPLIT
                       PERFORM PLACE-ROW
                   END-IF
               END-IF
           END-IF.

       PLACE-ROW.
      *> Fan the current match into the ROW<SLOT> slot. EVALUATE
      *> unrolls the dispatch -- bricks COBOL has no OCCURS yet.
           EVALUATE SLOT
               WHEN 1
                   MOVE KEY-IN TO K1  MOVE SNAME TO N1
                   MOVE SCITY TO C1   MOVE SPH1 TO P1
               WHEN 2
                   MOVE KEY-IN TO K2  MOVE SNAME TO N2
                   MOVE SCITY TO C2   MOVE SPH1 TO P2
               WHEN 3
                   MOVE KEY-IN TO K3  MOVE SNAME TO N3
                   MOVE SCITY TO C3   MOVE SPH1 TO P3
               WHEN 4
                   MOVE KEY-IN TO K4  MOVE SNAME TO N4
                   MOVE SCITY TO C4   MOVE SPH1 TO P4
               WHEN 5
                   MOVE KEY-IN TO K5  MOVE SNAME TO N5
                   MOVE SCITY TO C5   MOVE SPH1 TO P5
               WHEN 6
                   MOVE KEY-IN TO K6  MOVE SNAME TO N6
                   MOVE SCITY TO C6   MOVE SPH1 TO P6
               WHEN 7
                   MOVE KEY-IN TO K7  MOVE SNAME TO N7
                   MOVE SCITY TO C7   MOVE SPH1 TO P7
               WHEN 8
                   MOVE KEY-IN TO K8  MOVE SNAME TO N8
                   MOVE SCITY TO C8   MOVE SPH1 TO P8
               WHEN 9
                   MOVE KEY-IN TO K9  MOVE SNAME TO N9
                   MOVE SCITY TO C9   MOVE SPH1 TO P9
               WHEN 10
                   MOVE KEY-IN TO K10 MOVE SNAME TO N10
                   MOVE SCITY TO C10  MOVE SPH1 TO P10
               WHEN 11
                   MOVE KEY-IN TO K11 MOVE SNAME TO N11
                   MOVE SCITY TO C11  MOVE SPH1 TO P11
               WHEN 12
                   MOVE KEY-IN TO K12 MOVE SNAME TO N12
                   MOVE SCITY TO C12  MOVE SPH1 TO P12
               WHEN 13
                   MOVE KEY-IN TO K13 MOVE SNAME TO N13
                   MOVE SCITY TO C13  MOVE SPH1 TO P13
               WHEN 14
                   MOVE KEY-IN TO K14 MOVE SNAME TO N14
                   MOVE SCITY TO C14  MOVE SPH1 TO P14
               WHEN 15
                   MOVE KEY-IN TO K15 MOVE SNAME TO N15
                   MOVE SCITY TO C15  MOVE SPH1 TO P15
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       PAGE-LOOP.
      *> Reset per-page state, restart the browse from the top, skip
      *> (PAGE-1)*15 records, then read 15 for display. See GUSL's
      *> commentary on why we re-browse per page instead of buffering.
           PERFORM CLEAR-ROWS.
           MOVE 0 TO NCOUNT.
           MOVE SPACES TO INFOLINE.
           MOVE 'PERL -- contacts (PF7=Prev PF8=Next PF3=Exit).'
                TO INFOLINE.

           EXEC CICS STARTBR FILE('contacts') END-EXEC.

           COMPUTE SKIPGOAL = (PAGE-NO - 1) * 15.
           MOVE 0 TO SKIPCNT.
           PERFORM SKIP-ONE UNTIL SKIPCNT >= SKIPGOAL.

           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K1 MOVE SNAME TO N1
              MOVE SCITY TO C1 MOVE SPH1 TO P1 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K2 MOVE SNAME TO N2
              MOVE SCITY TO C2 MOVE SPH1 TO P2 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K3 MOVE SNAME TO N3
              MOVE SCITY TO C3 MOVE SPH1 TO P3 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K4 MOVE SNAME TO N4
              MOVE SCITY TO C4 MOVE SPH1 TO P4 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K5 MOVE SNAME TO N5
              MOVE SCITY TO C5 MOVE SPH1 TO P5 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K6 MOVE SNAME TO N6
              MOVE SCITY TO C6 MOVE SPH1 TO P6 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K7 MOVE SNAME TO N7
              MOVE SCITY TO C7 MOVE SPH1 TO P7 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K8 MOVE SNAME TO N8
              MOVE SCITY TO C8 MOVE SPH1 TO P8 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K9 MOVE SNAME TO N9
              MOVE SCITY TO C9 MOVE SPH1 TO P9 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K10 MOVE SNAME TO N10
              MOVE SCITY TO C10 MOVE SPH1 TO P10 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K11 MOVE SNAME TO N11
              MOVE SCITY TO C11 MOVE SPH1 TO P11 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K12 MOVE SNAME TO N12
              MOVE SCITY TO C12 MOVE SPH1 TO P12 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K13 MOVE SNAME TO N13
              MOVE SCITY TO C13 MOVE SPH1 TO P13 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K14 MOVE SNAME TO N14
              MOVE SCITY TO C14 MOVE SPH1 TO P14 END-IF.
           PERFORM ONE-ROW.
           IF EIBRESP = RESP-NORMAL THEN PERFORM SPLIT
              MOVE KEY-IN TO K15 MOVE SNAME TO N15
              MOVE SCITY TO C15 MOVE SPH1 TO P15 END-IF.

           EXEC CICS ENDBR FILE('contacts') END-EXEC.

      *> Snapshot the keys for cursor-selection before the page goes
      *> out -- rows are unprotected so the K<n> fields could come
      *> back overtyped on RECEIVE.
           PERFORM SAVE-KEYS.

           EXEC CICS SEND MAP('PERSL') FROM(SCR) ERASE END-EXEC.
           EXEC CICS RECEIVE MAP('PERSL') END-EXEC.

      *> AID dispatch: PF3/PF12 exit; PF7/PF8 page; ENTER on a row
      *> opens the PERS2 detail for that record then re-renders the
      *> page on the next iteration. Cursor not on a row = no-op so
      *> the operator can press ENTER harmlessly elsewhere.
           EVALUATE EIBAID
               WHEN PF03
                   MOVE 'Y' TO EXIT-FLAG
               WHEN PF12
                   MOVE 'Y' TO EXIT-FLAG
               WHEN PF07
                   IF PAGE-NO > 1 THEN
                       COMPUTE PAGE-NO = PAGE-NO - 1
                   END-IF
               WHEN PF08
                   COMPUTE PAGE-NO = PAGE-NO + 1
               WHEN ENTER
                   PERFORM SEL-FROM-CURSOR
                   IF SEL-SLOT > 0 THEN
                       PERFORM PICK-KEY
                       IF SEL-KEY NOT = SPACES THEN
                           MOVE SEL-KEY TO KEY-IN
                           PERFORM SHOW-DETAIL
                       END-IF
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       SKIP-ONE.
           MOVE SPACES TO REC.
           MOVE SPACES TO KEY-IN.
           EXEC CICS READNEXT FILE('contacts') INTO(REC)
                              RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP = RESP-NORMAL THEN
               COMPUTE SKIPCNT = SKIPCNT + 1
           END-IF.
           IF EIBRESP NOT = RESP-NORMAL THEN
               MOVE SKIPGOAL TO SKIPCNT
           END-IF.

       ONE-ROW.
           MOVE SPACES TO REC.
           MOVE SPACES TO KEY-IN.
           EXEC CICS READNEXT FILE('contacts') INTO(REC)
                              RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP = RESP-NORMAL THEN
               COMPUTE NCOUNT = NCOUNT + 1
           END-IF.

       SPLIT.
           MOVE SPACES TO SNAME.    MOVE SPACES TO SSEX.
           MOVE SPACES TO SRACE.    MOVE SPACES TO SDOB.
           MOVE SPACES TO SADDR1.   MOVE SPACES TO SADDR2.
           MOVE SPACES TO SCITY.    MOVE SPACES TO SSTATE.
           MOVE SPACES TO SZIP.     MOVE SPACES TO SEMAIL.
           MOVE SPACES TO SPH1.     MOVE SPACES TO SPH2.
           MOVE SPACES TO SNOTE.    MOVE SPACES TO SADDED.
           MOVE SPACES TO SUPDATED.
           UNSTRING REC DELIMITED BY '|'
               INTO SNAME SSEX SRACE SDOB SADDR1 SADDR2
                    SCITY SSTATE SZIP SEMAIL SPH1 SPH2
                    SNOTE SADDED SUPDATED
           END-UNSTRING.

       CLEAR-ROWS.
           MOVE SPACES TO ROW1.
           MOVE SPACES TO ROW2.
           MOVE SPACES TO ROW3.
           MOVE SPACES TO ROW4.
           MOVE SPACES TO ROW5.
           MOVE SPACES TO ROW6.
           MOVE SPACES TO ROW7.
           MOVE SPACES TO ROW8.
           MOVE SPACES TO ROW9.
           MOVE SPACES TO ROW10.
           MOVE SPACES TO ROW11.
           MOVE SPACES TO ROW12.
           MOVE SPACES TO ROW13.
           MOVE SPACES TO ROW14.
           MOVE SPACES TO ROW15.
           MOVE SPACES TO SK1.   MOVE SPACES TO SK2.
           MOVE SPACES TO SK3.   MOVE SPACES TO SK4.
           MOVE SPACES TO SK5.   MOVE SPACES TO SK6.
           MOVE SPACES TO SK7.   MOVE SPACES TO SK8.
           MOVE SPACES TO SK9.   MOVE SPACES TO SK10.
           MOVE SPACES TO SK11.  MOVE SPACES TO SK12.
           MOVE SPACES TO SK13.  MOVE SPACES TO SK14.
           MOVE SPACES TO SK15.

       SAVE-KEYS.
      *> Snapshot whatever's in the row K<n> slots into SK<n> so the
      *> ENTER-on-row selection survives an operator overtyping a row.
           MOVE K1  TO SK1.   MOVE K2  TO SK2.
           MOVE K3  TO SK3.   MOVE K4  TO SK4.
           MOVE K5  TO SK5.   MOVE K6  TO SK6.
           MOVE K7  TO SK7.   MOVE K8  TO SK8.
           MOVE K9  TO SK9.   MOVE K10 TO SK10.
           MOVE K11 TO SK11.  MOVE K12 TO SK12.
           MOVE K13 TO SK13.  MOVE K14 TO SK14.
           MOVE K15 TO SK15.

       SEL-FROM-CURSOR.
      *> EIBCPOSN is the 1-based cursor position on the most recent
      *> map response. Reduce it to a 0-based map row (offset / 80);
      *> ROW1 is at map row 5 and ROW15 at row 19, so SLOT = row - 4.
      *> SEL-SLOT = 0 means the cursor wasn't on a list row.
           MOVE EIBCPOSN TO CPOSN.
           MOVE 0 TO SEL-SLOT.
           IF CPOSN > 0 THEN
               COMPUTE CRBASE = CPOSN - 1
               COMPUTE CROW = CRBASE / 80
               IF CROW >= 5 AND CROW <= 19 THEN
                   COMPUTE SEL-SLOT = CROW - 4
               END-IF
           END-IF.

       PICK-KEY.
      *> EVALUATE unrolls the SK<SEL-SLOT> fetch -- bricks COBOL has
      *> no OCCURS yet, so each slot gets its own WHEN.
           MOVE SPACES TO SEL-KEY.
           EVALUATE SEL-SLOT
               WHEN 1  MOVE SK1 TO SEL-KEY
               WHEN 2  MOVE SK2 TO SEL-KEY
               WHEN 3  MOVE SK3 TO SEL-KEY
               WHEN 4  MOVE SK4 TO SEL-KEY
               WHEN 5  MOVE SK5 TO SEL-KEY
               WHEN 6  MOVE SK6 TO SEL-KEY
               WHEN 7  MOVE SK7 TO SEL-KEY
               WHEN 8  MOVE SK8 TO SEL-KEY
               WHEN 9  MOVE SK9 TO SEL-KEY
               WHEN 10 MOVE SK10 TO SEL-KEY
               WHEN 11 MOVE SK11 TO SEL-KEY
               WHEN 12 MOVE SK12 TO SEL-KEY
               WHEN 13 MOVE SK13 TO SEL-KEY
               WHEN 14 MOVE SK14 TO SEL-KEY
               WHEN 15 MOVE SK15 TO SEL-KEY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       SHOW-DETAIL.
      *> READ the chosen record, parse, and SEND the PERS2 detail map.
      *> ENTER or PF3 from the detail screen returns to the list loop;
      *> the next iteration re-paints the list page so any inadvertent
      *> row overtype is overwritten.
           EXEC CICS READ FILE('contacts') INTO(REC)
                          RIDFLD(KEY-IN) END-EXEC.
           IF EIBRESP = RESP-NORMAL THEN
               PERFORM SPLIT
               MOVE SPACES   TO DET
               MOVE KEY-IN   TO DRECID
               MOVE SADDED   TO DADDED
               MOVE SUPDATED TO DUPDATED
               MOVE SNAME    TO DNAME
               MOVE SSEX     TO DSEX
               MOVE SRACE    TO DRACE
               MOVE SDOB     TO DDOB
               MOVE SADDR1   TO DADDR1
               MOVE SADDR2   TO DADDR2
               MOVE SCITY    TO DCITY
               MOVE SSTATE   TO DSTATE
               MOVE SZIP     TO DZIP
               MOVE SEMAIL   TO DEMAIL
               MOVE SPH1     TO DPH1
               MOVE SPH2     TO DPH2
               MOVE SNOTE    TO DNOTE
               MOVE 'View from list -- ENTER or PF3 returns to the list.'
                    TO DMODE
               EXEC CICS SEND MAP('PERS2') FROM(DET) ERASE END-EXEC
               EXEC CICS RECEIVE MAP('PERS2') INTO(DET) END-EXEC
           END-IF.
