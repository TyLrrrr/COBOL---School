       IDENTIFICATION DIVISION.
      *************************

       PROGRAM-ID. VSEX9-3.
       AUTHOR. TYLER SATHER.
       INSTALLATION. M STATE.
       DATE-WRITTEN. APRIL 25 2020.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CUSTOMER-SALES-FILE
           ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTSALE.DAT".

           SELECT SORT-FILE
           ASSIGN TO "SORTWORK".

           SELECT SORTED-SALES-FILE
           ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch9B\SORTCUST.DAT".

           SELECT CUSTOMER-ADDRESS-FILE
           ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTADDR.DAT".

           SELECT REPORT-FILE
           ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch9B\VSEX9-3B.RPT".

       DATA DIVISION.

       FILE SECTION.

       FD  CUSTOMER-SALES-FILE.

       01 CUSTOMER-SALES-REC PIC X(76).

       SD  SORT-FILE.

       01 SORT-RECORD.
         02 PIC X(41).
         02 SR-AUTO-MAKE PIC X(20).
         02 PIC X(15).

       FD  SORTED-SALES-FILE.

       01 SORTED-SALES-REC PIC X(76).

       FD  CUSTOMER-ADDRESS-FILE.

       01 CUSTOMER-ADDRESS-RECORD.
         02 CAR-NUMBER PIC X(4).
         02 CAR-ADDRESS PIC X(20).

       FD  REPORT-FILE.

       01 REPORT-LINE-OUT PIC X(80).

       WORKING-STORAGE SECTION.

       01 SWITCHES.

         02 SW-END-OF-FILE PIC X.
           88 END-OF-FILE VALUE "Y".

       01 ACCUMULATORS.

         02 AC-LINE-COUNT PIC 999.
         02 AC-PAGE-COUNT PIC 999.
         02 AC-RECORD-COUNT PIC 9(5).
         02 AC-TOTAL-PURCHASES PIC 9(6)V99.

       01 WORK-AREA.

         02 WA-TODAYS-DATE-TIME.
           03 WA-TODAYS-DATE.
             04 WA-TODAYS-YEAR PIC 9(4).
             04 WA-TODAYS-MONTH PIC 99.
             04 WA-TODAYS-DAY PIC 99.
           03 WA-TODAYS-TIME.
             04 WA-TODAYS-HOUR PIC 99.
             04 WA-TODAYS-MINUTES PIC 99.
           03 PIC X(9).

         02 WA-DATE.
           03 WA-MONTH PIC 99.
           03 WA-DAY PIC 99.
           03 WA-YEAR PIC 9(4).

         02 WA-RUN-DATE REDEFINES WA-DATE PIC 9(8).

         02 WA-AM-PM PIC XX.
         02 WA-ADDRESS PIC X(20).
         02 WA-SATISFACTION-RATING PIC X(4).

       01 SATISFACTION-DATA.
         02 PIC X(5) VALUE "0DISS".
         02 PIC X(5) VALUE "1UND".
         02 PIC X(5) VALUE "2SAT".

       01 SATISFACTION-TABLE REDEFINES SATISFACTION-DATA.
         02 ST-ENTRY OCCURS 3 TIMES INDEXED BY ST-INDEX.
           03 ST-CODE PIC X.
           03 ST-RATING PIC X(4).

       01 CUSTOMER-ADDRESS-TABLE.
         02 CAT-ENTRY OCCURS 73 TIMES ASCENDING KEY CAT-NUMBER
                                   INDEXED BY ADDR-INDEX.
           03 CAT-NUMBER PIC X(4).
           03 CAT-ADDRESS PIC X(20).

       01 CUSTOMER-SALES-RECORD.
         02 CSR-ZIP-CODE PIC X(5).
         02 CSR-ZIP-PLUS-4 PIC X(4).
         02 CSR-CUSTOMER-NUMBER PIC 9(4).
         02 CSR-CUSTOMER-NAME PIC X(20).
         02 CSR-PURCHASE-DATE PIC 9(8).
         02 CSR-AUTO-MAKE PIC X(20).
         02 CSR-PURCHASE-PRICE PIC 9(5)V99.
         02 CSR-AUTO-YEAR PIC X(4).
         02 PIC XXX.
         02 CSR-SATISFACTION-CODE PIC X.
           88 DISSATISFIED VALUE "0".
           88 UNDECIDED VALUE "1".
           88 SATISFIED VALUE "2".

       01 REPORT-HEADINGS.
         02 RH-LINE-1.
           03 PIC X(6) VALUE "DATE: ".
           03 RH-RUN-DATE PIC Z9/99/9999.
           03 PIC X(15) VALUE SPACES.
           03 PIC X(13) VALUE "EZ AUTO SALES".
           03 PIC X(26) VALUE SPACES.
           03 PIC X(5) VALUE "PAGE ".
           03 RH-PAGE PIC ZZ9.
         02 RH-LINE-2.
           03 PIC X(6) VALUE "TIME: ".
           03 RH-HOUR PIC Z9.
           03 PIC X VALUE ':'.
           03 RH-MINUTES PIC 99.
           03 RH-AM-PM PIC XX.
           03 PIC X(13) VALUE SPACES.
           03 PIC X(23) VALUE "CUSTOMER ADDRESS REPORT".
         02 RH-LINE-3.
           03 PIC X(8) VALUE "CUSTOMER".
           03 PIC X(13) VALUE SPACES.
           03 PIC X(6) VALUE "STREET".
           03 PIC X(15) VALUE SPACES.
           03 PIC X(4) VALUE "AUTO".
           03 PIC X(17) VALUE SPACES.
           03 PIC X(8) VALUE "PURCHASE".
         02 RH-LINE-4.
           03 PIC X(4) VALUE "NAME".
           03 PIC X(17) VALUE SPACES.
           03 PIC X(7) VALUE "ADDRESS".
           03 PIC X(14) VALUE SPACES.
           03 PIC X(4) VALUE "MAKE".
           03 PIC X(17) VALUE SPACES.
           03 PIC X(5) VALUE "PRICE".
           03 PIC X(6) VALUE SPACES.
           03 PIC X(4) VALUE "SAT.".
         02 RH-LINE-5.
           03 PIC X(20) VALUE ALL '-'.
           03 PIC X VALUE SPACES.
           03 PIC X(20) VALUE ALL '-'.
           03 PIC X VALUE SPACES.
           03 PIC X(20) VALUE ALL '-'.
           03 PIC X VALUE SPACES.
           03 PIC X(9) VALUE ALL '-'.
           03 PIC XX VALUE SPACES.
           03 PIC X(4) VALUE ALL '-'.

       01 DETAIL-LINE.
         02 DL-CUSTOMER-NAME PIC X(20).
         02 PIC X VALUE SPACES.
         02 DL-ADDRESS PIC X(20).
         02 PIC X VALUE SPACES.
         02 DL-AUTO-MAKE PIC X(20).
         02 PIC X VALUE SPACES.
         02 DL-PURCHASE-PRICE PIC ZZ,ZZZ.99.
         02 PIC XX VALUE SPACES.
         02 DL-SATISFACTION-RATING PIC X(4).

       01 SUMMARY-LINES.
         02 SL-LINE-1.
           03 PIC X(21) VALUE SPACES.
           03 PIC X(26) VALUE "TOTAL CUSTOMERS LISTED   =".
           03 PIC X(9) VALUE SPACES.
           03 SL-RECORD-COUNT PIC ZZ9.
         02 SL-LINE-2.
           03 PIC X(21) VALUE SPACES.
           03 PIC X(26) VALUE "TOTAL CUSTOMER PURCHASES =".
           03 PIC X VALUE SPACES.
           03 SL-TOTAL-PURCHASES PIC $ZZZ,ZZ9.99.
         02 SL-LINE-3.
           03 PIC X(33) VALUE SPACES.
           03 PIC X(13) VALUE "END OF REPORT".

       PROCEDURE DIVISION.

       MAIN-PROGRAM.

           PERFORM A-100-INITIALIZATION.
           PERFORM B-100-LOAD-ADDRESS-TABLE.
           PERFORM C-100-PROCESS-FILE.
           PERFORM D-100-WRAP-UP.
           STOP RUN.

       A-100-INITIALIZATION.

           INITIALIZE ACCUMULATORS.

           OPEN OUTPUT REPORT-FILE.

           MOVE FUNCTION CURRENT-DATE TO WA-TODAYS-DATE-TIME.
           MOVE WA-TODAYS-MONTH TO WA-MONTH.
           MOVE WA-TODAYS-DAY TO WA-DAY.
           MOVE WA-TODAYS-YEAR TO WA-YEAR.

           EVALUATE TRUE
               WHEN WA-TODAYS-HOUR = 00
                   MOVE "AM" TO WA-AM-PM
                   MOVE 12 TO WA-TODAYS-HOUR
               WHEN WA-TODAYS-HOUR < 12
                   MOVE "AM" TO WA-AM-PM
               WHEN WA-TODAYS-HOUR = 12
                   MOVE "PM" TO WA-AM-PM
               WHEN WA-TODAYS-HOUR > 12
                   MOVE "PM" TO WA-AM-PM
                   SUBTRACT 12 FROM WA-TODAYS-HOUR.

           SORT SORT-FILE
           ON ASCENDING KEY SR-AUTO-MAKE
           USING CUSTOMER-SALES-FILE
           GIVING SORTED-SALES-FILE.

       B-100-LOAD-ADDRESS-TABLE.
           OPEN INPUT CUSTOMER-ADDRESS-FILE.
           MOVE "N" TO SW-END-OF-FILE.
           READ CUSTOMER-ADDRESS-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
           PERFORM B-200-LOAD VARYING ADDR-INDEX
           FROM 1 BY 1
           UNTIL END-OF-FILE OR ADDR-INDEX > 73.

           CLOSE CUSTOMER-ADDRESS-FILE.

       B-200-LOAD.

           MOVE CAR-NUMBER TO CAT-NUMBER(ADDR-INDEX).
           MOVE CAR-ADDRESS TO CAT-ADDRESS(ADDR-INDEX).

           READ CUSTOMER-ADDRESS-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

       C-100-PROCESS-FILE.

           OPEN INPUT SORTED-SALES-FILE

           MOVE "N" TO SW-END-OF-FILE.
           READ SORTED-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
           PERFORM C-200-PROCESS-RECORD UNTIL END-OF-FILE.

           MOVE AC-RECORD-COUNT TO SL-RECORD-COUNT.
           MOVE AC-TOTAL-PURCHASES TO SL-TOTAL-PURCHASES.

           WRITE REPORT-LINE-OUT FROM SL-LINE-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-LINE-OUT FROM SL-LINE-2 AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM SL-LINE-3 AFTER ADVANCING 2 LINES.

           CLOSE SORTED-SALES-FILE.

       C-200-PROCESS-RECORD.

           IF AC-LINE-COUNT = 0
               PERFORM C-300-WRITE-HEADINGS.

           SEARCH ALL CAT-ENTRY
               AT END
                   MOVE "NOT FOUND" TO WA-ADDRESS
               WHEN CAT-NUMBER(ADDR-INDEX) = CSR-CUSTOMER-NUMBER
                   MOVE CAT-ADDRESS(ADDR-INDEX) TO WA-ADDRESS.

           SET ST-INDEX TO 1.
           SEARCH ST-ENTRY
               AT END
                   MOVE "NOT FOUND" TO WA-SATISFACTION-RATING
               WHEN ST-CODE(ST-INDEX) = CSR-SATISFACTION-CODE
                   MOVE ST-RATING(ST-INDEX) TO WA-SATISFACTION-RATING.

           MOVE CSR-CUSTOMER-NAME TO DL-CUSTOMER-NAME.
           MOVE WA-ADDRESS TO DL-ADDRESS.
           MOVE CSR-AUTO-MAKE TO DL-AUTO-MAKE.
           MOVE CSR-PURCHASE-PRICE TO DL-PURCHASE-PRICE.
           MOVE WA-SATISFACTION-RATING TO DL-SATISFACTION-RATING.

           WRITE REPORT-LINE-OUT FROM DETAIL-LINE AFTER ADVANCING 1 LINE.
           ADD 1 TO AC-LINE-COUNT.
           ADD 1 TO AC-RECORD-COUNT.
           ADD CSR-PURCHASE-PRICE TO AC-TOTAL-PURCHASES.

           IF AC-LINE-COUNT > 55

               PERFORM C-300-WRITE-HEADINGS.
           READ SORTED-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

       C-300-WRITE-HEADINGS.

           ADD 1 TO AC-PAGE-COUNT
           MOVE 0 TO AC-LINE-COUNT.

           MOVE WA-RUN-DATE TO RH-RUN-DATE.
           MOVE AC-PAGE-COUNT TO RH-PAGE.
           MOVE WA-TODAYS-HOUR TO RH-HOUR.
           MOVE WA-TODAYS-MINUTES TO RH-MINUTES.
           MOVE WA-AM-PM TO RH-AM-PM.

           WRITE REPORT-LINE-OUT FROM RH-LINE-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-LINE-OUT FROM RH-LINE-2 AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM RH-LINE-3 AFTER ADVANCING 2 LINES.
           WRITE REPORT-LINE-OUT FROM RH-LINE-4 AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM RH-LINE-5 AFTER ADVANCING 1 LINE.
           ADD 7 TO AC-LINE-COUNT.

       D-100-WRAP-UP.

           CLOSE REPORT-FILE.
           DISPLAY "CUSTOMER ADDRESS REPORT PROGRAM HAS TERMINATED".