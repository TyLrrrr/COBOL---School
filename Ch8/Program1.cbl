       IDENTIFICATION DIVISION.
      ************************

       PROGRAM-ID. VSEX8-3.
       AUTHOR. TYLER SATHER.
       INSTALLATION. M STATE.
       DATE-WRITTEN. ARP, 22 2020
       DATE-COMPILED.
      ******************************************************************
      *                      PROGRAM NARRATIVE                         *
      *                                                                *
      *  THIS PROGRAM PRODUCES THE CUSTOMER SALES SUBTOTAL REPORT      *
      *  LISTING ALL SALES BY AUTO MAKE AND PROVIDING SUMMARY          *
      *  INFORMATION FOR EACH MAKE OF AUTO. THE CUSTOMER SALES FILE    *
      *  IS SORTED BY AUTOMAKE PRIOR TO PRODUCING THE REPORT.          *
      *  TWO TABLES ARE UTILIZED, ONE FOR CUSTOMER ADDRESSES AND ONE   *
      *  SATISFACTION RATING DESCRIPTION                               *
      *                                                                *
      *  INPUT:         CUSTSALE.DAT  -  CUSTOMER SALES FILE           *
      *                 CUSTADDR.DAT  -  CUSTOMER ADDRESS FILE         *
      *  OUTPUT:        EX8-3.RPT     -  CUSTOMER SALES SUBTOTAL REPORT*
      *                                                                *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.
      **********************

       INPUT-OUTPUT SECTION.
      **********************

       FILE-CONTROL.

           SELECT CUSTOMER-SALES-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTSALE.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN TO "SORTWORK".

           SELECT SORTED-SALES-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch8\SORTCUST.DAT"
                      ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CUSTOMER-ADDRESS-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTADDR.DAT"
                    ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SALES-SUBTOTAL-REPORT
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch8\EX8-3.RPT".
      /
       DATA DIVISION.
      ***************

       FILE SECTION.
      **************

      ******************************************************************
      *                                                                *
      *               INPUT FILE - CUSTOMER SALES FILE                 *
      *                                                                *
      ******************************************************************

       FD  CUSTOMER-SALES-FILE.

       01 CUSTOMER-SALES-REC PIC X(76).


      ******************************************************************
      *                                                                *
      *               SORT FILE - SORT SALES FILE BY AUTO MAKE         *
      *                                                                *
      ******************************************************************

       SD  SORT-FILE.

       01 SORT-RECORD.
         02                        PIC X(41).
         02 SR-AUTO-MAKE           PIC X(20).
         02                        PIC X(15).
      /
      ******************************************************************
      *                                                                *
      *              SORTED FILE - SORTED SALES FILE                   *
      *                                                                *
      ******************************************************************

       FD  SORTED-SALES-FILE.

       01 SORTED-SALES-REC PIC X(76).

      ******************************************************************
      *                                                                *
      *    ADDRESS FILE - CUSTOMER ADDRESS FILE FOR NON EMBEDDED TABLE *
      *                                                                *
      ******************************************************************

       FD  CUSTOMER-ADDRESS-FILE.

       01 CUSTOMER-ADDRESS-RECORD.
         02 CAR-NUMBER                 PIC X(4).
         02 CAR-ADDRESS                PIC X(20).

      ******************************************************************
      *                                                                *
      *             REPORT FILE - CUSTOMER SALES SUBTOTAL REPORT       *
      *                                                                *
      ******************************************************************

       FD  SALES-SUBTOTAL-REPORT.

       01 SALES-LINE-OUT PIC X(100).
      /
       WORKING-STORAGE SECTION.
      ************************

      ******************************************************************
      *                                                                *
      *                        SWITCHES                                *
      *                                                                *
      ******************************************************************

       01 SWITCHES.

         02 SW-END-OF-FILE PIC X.
           88 END-OF-FILE VALUE "Y".

      ******************************************************************
      *                                                                *
      *                      ACCUMULATORS                              *
      *                                                                *
      ******************************************************************

       01 ACCUMULATORS.

         02 AC-LINE-COUNT PIC 999.
         02 AC-PAGE-COUNT PIC 999.
         02 AC-RECORD-COUNT PIC 999.
         02 AC-AUTO-COUNT PIC 999.
         02 AC-TOTAL-SALES PIC 9(6)V99.
         02 AC-AUTO-SALES PIC 9(6)V99.
      /
      *****************************************************************
      *                                                               *
      *                      WORK AREA                                *
      *                                                               *
      *****************************************************************

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
         02 WA-RUN-DATE REDEFINES
            WA-DATE PIC 9(8).

         02 WA-HOLD-AUTO PIC X(20).
         02 WA-DEL-1 PIC X.
         02 WA-AUTO-FIELDS.
           03 WA-AUTO-NAME-1.
             04 PIC X.
             04 WA-AUTO-REST-1 PIC X(14).
           03 WA-AUTO-NAME-2.
             04 PIC X.
             04 WA-AUTO-REST-2 PIC X(14).

         02 WA-CUSTOMER-NAME-FIELDS.
           03 WA-NAME-1.
             04 PIC X.
             04 WA-REST-1 PIC X(14).
           03 WA-NAME-2.
             04 PIC X.
             04 WA-REST-2 PIC X(14).
           03 WA-NAME-3.
             04 PIC X.
             04 WA-REST-3 PIC X(14).

      /


      ******************************************************************
      *                                                                *
      *                   EMBEDDED SATISFACTION CODE TABLE             *
      *                                                                *
      ******************************************************************

       01 SATISFACTION-DATA.
         02 PIC X(10) VALUE "0DISSAT.".
         02 PIC X(10) VALUE "1UNDECIDED".
         02 PIC X(10) VALUE "2SATISFIED".


       01 SATISFACTION-TABLE REDEFINES SATISFACTION-DATA.
         02 ST-ENTRY OCCURS 3 TIMES
                     INDEXED BY ST-INDEX.
           03 ST-CODE PIC X.
           03 ST-RATING PIC X(9).

      /
      ******************************************************************
      *                                                                *
      *                NON-EMBEDDED CUSTOMER ADDRESS TABLE             *
      *                                                                *
      ******************************************************************

       01 CUSTOMER-ADDRESS-TABLE.
         02 CAT-ENTRY OCCURS 73 TIMES
                       INDEXED BY ADDR-INDEX.
           03 CAT-NUMBER PIC X(4).
           03 CAT-ADDRESS PIC X(20).

       COPY"C:\Users\sathe\Desktop\Mstate\COBOL\Data\SALESREC.CBL".


      ******************************************************************
      *                                                                *
      *          REPORT HEADINGS FOR CUSTOMER SALES SUBTOTAL REPORT    *
      *                                                                *
      ******************************************************************

       01 REPORT-HEADINGS.

         02 RH-LINE-1.
           03 PIC X(6) VALUE "DATE: ".
           03 RH-DATE PIC Z9/99/9999.
           03 PIC X(25) VALUE SPACES.
           03 PIC X(13) VALUE "EZ AUTO SALES".
           03 PIC X(27) VALUE SPACES.
           03 PIC X(5) VALUE "PAGE ".
           03 RH-PAGE-COUNT PIC Z9.

         02 RH-LINE-2.
           03 PIC X(6) VALUE "TIME: ".
           03 RH-HOUR PIC Z9.
           03 PIC X VALUE ":".
           03 RH-MINUTES PIC 99.
           03 PIC X VALUE SPACE.
           03 RH-AM-PM PIC XX.
           03 PIC X(18) VALUE SPACES.
           03 PIC X(30) VALUE "CUSTOMER SALES SUBTOTAL REPORT".

         02 RH-LINE-2A.
           03 PIC X(12) VALUE "TYLER SATHER".

         02 RH-LINE-3.
           03 PIC X(9) VALUE "MAKE -->".
           03 RH-AUTO-MAKE PIC X(20).

         02 RH-LINE-4.
           03 PIC X(8) VALUE "CUSTOMER".
           03 PIC X(46) VALUE SPACES.
           03 PIC X(8) VALUE "PURCHASE".
           03 PIC X(4) VALUE SPACES.
           03 PIC X(8) VALUE "PURCHASE".
           03 PIC X(4) VALUE SPACES.
           03 PIC X(6) VALUE "SATIS.".

         02 RH-LINE-5.
           03 PIC X(6) VALUE "NUMBER".
           03 PIC XXX VALUE SPACES.
           03 PIC X(13) VALUE "CUSTOMER NAME".
           03 PIC X(9) VALUE SPACES.
           03 PIC X(14) VALUE "STREET ADDRESS".
           03 PIC X(9) VALUE SPACES.
           03 PIC X(4) VALUE "DATE".
           03 PIC X(8) VALUE SPACES.
           03 PIC X(5) VALUE "PRICE".
           03 PIC X(7) VALUE SPACES.
           03 PIC X(6) VALUE "RATING".

         02 RH-LINE-6.
           03 PIC X(8) VALUE ALL "-".
           03 PIC X VALUE SPACE.
           03 PIC X(20) VALUE ALL "-".
           03 PIC XX VALUE SPACES.
           03 PIC X(20) VALUE ALL "-".
           03 PIC XXX VALUE SPACES.
           03 PIC X(10) VALUE ALL "-".
           03 PIC XX VALUE SPACES.
           03 PIC X(9) VALUE ALL "-".
           03 PIC XXX VALUE SPACES.
           03 PIC X(8) VALUE ALL "-".

      /
      ******************************************************************
      *                                                                *
      *         DETAIL LINE FOR CUSTOMER SALES SUBTOTAL REPORT         *
      *                                                                *
      ******************************************************************

       01 DETAIL-LINE.
         02 PIC XX VALUE SPACES.
         02 DL-CUSTOMER-NUMBER PIC X(4).
         02 PIC XXX VALUE SPACES.
         02 DL-CUSTOMER-NAME PIC X(20).
         02 PIC XX VALUE SPACES.
         02 DL-CUSTOMER-ADDRESS PIC X(20).
         02 PIC XXX VALUE SPACES.
         02 DL-PURCHASE-DATE PIC Z9/99/9999.
         02 PIC XX VALUE SPACES.
         02 DL-PURCHASE-PRICE PIC $ZZ,ZZZ.99.
         02 PIC XX VALUE SPACES.
         02 DL-SATISFACTION-RATING PIC X(9).

      /
      ******************************************************************
      *                                                                *
      *           SUMMARY LINES FOR CUSTOMER SALES SUBTOTAL REPORT     *
      *                                                                *
      ******************************************************************

       01 SUMMARY-LINES.

         02 SL-LINE-1.
           03 PIC X(66) VALUE SPACES.
           03 PIC X(10) VALUE ALL "-".

         02 SL-LINE-2.
           03 PIC X(12) VALUE "*  TOTAL OF ".
           03 SL-MAKE-COUNT PIC ZZ9.
           03 PIC X(14) VALUE " CARS OF MAKE ".
           03 SL-AUTO-MAKE PIC X(21).
           03 PIC X(15) VALUE " SOLD FOR ".
           03 SL-AUTO-TOTAL PIC $ZZZ,ZZZ.99.

         02 SL-LINE-3.
           03 PIC X(24) VALUE SPACES.
           03 PIC X(18) VALUE "TOTAL CARS SOLD = ".
           03 SL-TOTAL-SOLD PIC ZZ9.

         02 SL-LINE-4.
           03 PIC X(24) VALUE SPACES.
           03 PIC X(18) VALUE "TOTAL PURCHASES = ".
           03 SL-TOTAL-SALES PIC $ZZZ,ZZZ.99.

         02 SL-LINE-5.
           03 PIC X(30) VALUE SPACES.
           03 PIC X(13) VALUE "END OF REPORT".
      /
       PROCEDURE DIVISION.
      *******************
      ******************************************************************
      *                                                                *
      *  MAIN-PROGRAM.  THIS IS THE MAIN PARAGRAPH OF THIS PROGRAM     *
      *                                                                *
      ******************************************************************

       MAIN-PROGRAM.

           PERFORM A-100-INITIALIZATION.
           PERFORM B-100-LOAD-ADDRESS-TABLE.
           PERFORM C-100-PROCESS-FILE.
           PERFORM D-100-WRAP-UP.
           STOP RUN.

      ******************************************************************
      *                                                                *
      *              THE INITIALIZATION PARAGRAPH FOLLOWS              *
      *                                                                *
      ******************************************************************

       A-100-INITIALIZATION.

           INITIALIZE ACCUMULATORS.

           MOVE FUNCTION CURRENT-DATE TO WA-TODAYS-DATE-TIME.
           MOVE WA-TODAYS-MONTH TO WA-MONTH.
           MOVE WA-TODAYS-DAY TO WA-DAY.
           MOVE WA-TODAYS-YEAR TO WA-YEAR.
           MOVE WA-RUN-DATE TO RH-DATE.


           EVALUATE TRUE
               WHEN WA-TODAYS-HOUR = 00
                   MOVE "AM" TO RH-AM-PM
                   MOVE 12 TO WA-TODAYS-HOUR
               WHEN WA-TODAYS-HOUR < 12
                   MOVE "AM" TO RH-AM-PM
               WHEN WA-TODAYS-HOUR = 12
                   MOVE "PM" TO RH-AM-PM
               WHEN WA-TODAYS-HOUR > 12
                   MOVE "PM" TO RH-AM-PM
                   SUBTRACT 12 FROM WA-TODAYS-HOUR.
           MOVE WA-TODAYS-HOUR TO RH-HOUR.
           MOVE WA-TODAYS-MINUTES TO RH-MINUTES.

           SORT SORT-FILE
           ON ASCENDING KEY SR-AUTO-MAKE
             USING CUSTOMER-SALES-FILE
             GIVING SORTED-SALES-FILE.
      /
      ******************************************************************
      *                                                                *
      *      LOAD CUSTOMER ADDRESS TABLE PARAGRAPH                     *
      *                                                                *
      ******************************************************************

       B-100-LOAD-ADDRESS-TABLE.

           OPEN INPUT CUSTOMER-ADDRESS-FILE.
           MOVE "N" TO SW-END-OF-FILE.

           READ CUSTOMER-ADDRESS-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

           PERFORM B-200-LOAD
             VARYING ADDR-INDEX FROM 1 BY 1
             UNTIL END-OF-FILE OR ADDR-INDEX > 73.

           CLOSE CUSTOMER-ADDRESS-FILE.

      ******************************************************************
      *                                                                *
      *      MOVE CUSTOMER ADDRESS FILE DATA TO ADDRESS TABLE          *
      *                                                                *
      ******************************************************************

       B-200-LOAD.

           MOVE CAR-NUMBER TO CAT-NUMBER(ADDR-INDEX).
           MOVE CAR-ADDRESS TO CAT-ADDRESS(ADDR-INDEX).

           READ CUSTOMER-ADDRESS-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
      /
      ******************************************************************
      *                                                                *
      *                   REPORT PROCESSING PARAGRAPH                  *
      *                                                                *
      ******************************************************************

       C-100-PROCESS-FILE.

           OPEN INPUT SORTED-SALES-FILE
             OUTPUT SALES-SUBTOTAL-REPORT.

           MOVE "N" TO SW-END-OF-FILE.

           READ SORTED-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

           MOVE CSR-AUTO-MAKE TO WA-HOLD-AUTO.

           PERFORM C-300-PRINT-HEADINGS.
           PERFORM C-310-AUTO-HEADINGS.

           PERFORM C-200-PROCESS-RECORDS
             UNTIL END-OF-FILE.

           PERFORM C-320-AUTO-BREAK.
           MOVE AC-TOTAL-SALES TO SL-TOTAL-SALES.
           MOVE AC-RECORD-COUNT TO SL-TOTAL-SOLD.
           WRITE SALES-LINE-OUT FROM SL-LINE-3
               AFTER ADVANCING 3 LINES.
           WRITE SALES-LINE-OUT FROM SL-LINE-4
               AFTER ADVANCING 1 LINE.
           WRITE SALES-LINE-OUT FROM SL-LINE-5
               AFTER ADVANCING 2 LINES.

      /
      ******************************************************************
      *                                                                *
      *                 RECORD PROCESSING PARAGRAPH                    *
      *                                                                *
      ******************************************************************

       C-200-PROCESS-RECORDS.

           IF CSR-AUTO-MAKE NOT = WA-HOLD-AUTO
               PERFORM C-320-AUTO-BREAK
               PERFORM C-310-AUTO-HEADINGS.

           MOVE CSR-CUSTOMER-NUMBER TO DL-CUSTOMER-NUMBER.
           PERFORM C-330-CUSTOMER-NAME.
           SET ADDR-INDEX TO 1.
           SEARCH CAT-ENTRY
               AT END
                   MOVE "NOT FOUND" TO DL-CUSTOMER-ADDRESS
               WHEN CAT-NUMBER(ADDR-INDEX) = CSR-CUSTOMER-NUMBER
                   MOVE CAT-ADDRESS(ADDR-INDEX) TO DL-CUSTOMER-ADDRESS.

           MOVE CSR-PURCHASE-DATE TO DL-PURCHASE-DATE.
           MOVE CSR-PURCHASE-PRICE TO DL-PURCHASE-PRICE.

           SET ST-INDEX TO 1.
           SEARCH ST-ENTRY
               AT END
                   MOVE "NOT FOUND" TO DL-SATISFACTION-RATING
               WHEN ST-CODE (ST-INDEX) = CSR-SATISFACTION-CODE
                   MOVE ST-RATING (ST-INDEX) TO DL-SATISFACTION-RATING.

           WRITE SALES-LINE-OUT FROM DETAIL-LINE
             AFTER ADVANCING 1 LINE.

           ADD 1 TO AC-LINE-COUNT.
           ADD 1 TO AC-AUTO-COUNT.
           ADD CSR-PURCHASE-PRICE TO AC-AUTO-SALES.

           IF AC-LINE-COUNT > 55
               MOVE ZERO TO AC-LINE-COUNT
               PERFORM C-300-PRINT-HEADINGS
               PERFORM C-310-AUTO-HEADINGS.

           READ SORTED-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
      /
      ******************************************************************
      *                                                                *
      *                    PAGE HEADING PARAGRAPH                      *
      *                                                                *
      ******************************************************************

       C-300-PRINT-HEADINGS.

           ADD 1 TO AC-PAGE-COUNT.
           MOVE AC-PAGE-COUNT TO RH-PAGE-COUNT.
           WRITE SALES-LINE-OUT FROM RH-LINE-1
             AFTER ADVANCING PAGE.
           WRITE SALES-LINE-OUT FROM RH-LINE-2
             AFTER ADVANCING 1 LINE.
           WRITE SALES-LINE-OUT FROM RH-LINE-2A
             AFTER ADVANCING 1 LINE.

           MOVE 3 TO AC-LINE-COUNT.

      ******************************************************************
      *                                                                *
      *                AUTO HEADING PARAGRAPH                          *
      *                                                                *
      ******************************************************************

       C-310-AUTO-HEADINGS.

           MOVE SPACES TO WA-AUTO-FIELDS.
           UNSTRING CSR-AUTO-MAKE DELIMITED BY "-" OR ALL SPACES
             INTO WA-AUTO-NAME-1 DELIMITER IN WA-DEL-1
                  WA-AUTO-NAME-2.
           MOVE FUNCTION LOWER-CASE (WA-AUTO-REST-1) TO WA-AUTO-REST-1.
           MOVE FUNCTION LOWER-CASE (WA-AUTO-REST-2) TO WA-AUTO-REST-2.

           MOVE SPACES TO RH-AUTO-MAKE.
           STRING WA-AUTO-NAME-1 DELIMITED BY SPACES
             WA-DEL-1 DELIMITED BY SIZE
             WA-AUTO-NAME-2 DELIMITED BY SPACES
             INTO RH-AUTO-MAKE.


           WRITE SALES-LINE-OUT FROM RH-LINE-3
             AFTER ADVANCING 2 LINES.
           WRITE SALES-LINE-OUT FROM RH-LINE-4
             AFTER ADVANCING 2 LINES.
           WRITE SALES-LINE-OUT FROM RH-LINE-5
             AFTER ADVANCING 1 LINE.
           WRITE SALES-LINE-OUT FROM RH-LINE-6
             AFTER ADVANCING 1 LINE.
           MOVE SPACES TO SALES-LINE-OUT.
           WRITE SALES-LINE-OUT
             AFTER ADVANCING 1 LINE.
           ADD 6 TO AC-LINE-COUNT.
           
      /
      ******************************************************************
      *                                                                *
      *              WRITE AUT CONTROL BREAK TOTAL                     *
      *                                                                *
      ******************************************************************

       C-320-AUTO-BREAK.

           MOVE AC-AUTO-COUNT TO SL-MAKE-COUNT.
           MOVE RH-AUTO-MAKE TO SL-AUTO-MAKE.
           MOVE AC-AUTO-SALES TO SL-AUTO-TOTAL.

           WRITE SALES-LINE-OUT FROM SL-LINE-1
             AFTER ADVANCING 1 LINE.
           WRITE SALES-LINE-OUT FROM SL-LINE-2
             AFTER ADVANCING 1 LINE.

           ADD AC-AUTO-COUNT TO AC-RECORD-COUNT.
           ADD AC-AUTO-SALES TO AC-TOTAL-SALES.

           ADD 2 TO AC-LINE-COUNT.

           MOVE CSR-AUTO-MAKE TO WA-HOLD-AUTO.
           MOVE ZERO TO AC-AUTO-COUNT
                        AC-AUTO-SALES.
      /
      ******************************************************************
      *                                                                *
      *             CALCULATE VENDOR CODE PARAGRAPH                    *
      *                                                                *
      ******************************************************************

       C-330-CUSTOMER-NAME.

           MOVE SPACES TO WA-CUSTOMER-NAME-FIELDS.
           UNSTRING CSR-CUSTOMER-NAME DELIMITED BY "-" OR ALL SPACES
             INTO WA-NAME-1
             WA-NAME-2
             WA-NAME-3.

           MOVE FUNCTION LOWER-CASE (WA-REST-1) TO WA-REST-1.
           MOVE FUNCTION LOWER-CASE (WA-REST-2) TO WA-REST-2.
           MOVE FUNCTION LOWER-CASE (WA-REST-3) TO WA-REST-3.

           MOVE SPACES TO DL-CUSTOMER-NAME.
           IF WA-NAME-3 = SPACES
               STRING WA-NAME-2 DELIMITED BY SPACES
                 " " DELIMITED BY SIZE
                 WA-NAME-1 DELIMITED BY ","
                 INTO DL-CUSTOMER-NAME
           ELSE
               STRING WA-NAME-2 DELIMITED BY SPACES
                 " " DELIMITED BY SIZE
                 WA-NAME-3 DELIMITED BY SPACES
                 " " DELIMITED BY SIZE
                 WA-NAME-1 DELIMITED BY ","
                 INTO DL-CUSTOMER-NAME.

      ******************************************************************
      *                                                                *
      *                 END OF JOB PARAGRAPH                           *
      *                                                                *
      ******************************************************************

       D-100-WRAP-UP.

           CLOSE SORTED-SALES-FILE
             SALES-SUBTOTAL-REPORT.

           DISPLAY "THE SALES SUBTOTAL REPORT PROGRAM HAS ENDED".


      ******************************************************************
      *                       END OF PROGRAM                           *
      ******************************************************************
      /
