       IDENTIFICATION DIVISION.
      ************************

       PROGRAM-ID. VSEX9-3.
       AUTHOR. TYLER SATHER.
       INSTALLATION. MSTATE.
       DATE-WRITTEN. APR. 25, 2020.
       DATE-COMPILED.
      ******************************************************************
      *                      PROGRAM NARRATIVE                         *
      *                                                                *
      *    THIS PROGRAM VALIDATES AND SORTS RECORDS IN A TRANSACTION   *
      *    FILE THAT IS THEN USED TO UPDATE THE RECORDS IN THE CUSTOMER*
      *    SALES FILE. THE RECORDS IN THE SALES FILE ARE ALREADI IN    *
      *    SEQUENCE BY CUSTOMER NUMBER. THE SALES FILE IS OPEN FOR I-O *
      *    AND UPDATED IN PLACE USING THE REWRITE STATEMENT. A REPORT  *
      *    IS CREATED LISTING THE DISPOSITION OF EVERY TRANSACTION.    *
      *    DURING PROCESSING. AUTO MAKE, AUTO YEAR, PURCHASE DATE,     *
      *    PURCHASE PRICE, AND SATISFACTION RATING ARE ALL UPDATED     *
      *                                                                *
      *                                                                *
      *                                                                *
      *        INPUT FILE:    AUTOTRAN.DAT -    TRANSACTION FILE       *
      *                                                                *
      *        INPUT/OUTPUT:  CUSTSALE.DAT -    CUSTOMER SALES FILE    *
      *                       STRANFL.DAT  -    SORTED TRANSACTION FILE*
      *                                                                *
      *        OUTPUT:        VSEX9-3A.RPT -    TRANSACTION REPORT     *
      *                                                                *
      *        SORT FILE:     SORTWORK     - SORT FILE FOR TRANSACTIONS*
      *                                                                *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.
      *********************

       INPUT-OUTPUT SECTION.
      *********************

       FILE-CONTROL.

           SELECT TRANSACTION-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\AUTOTRAN.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CUSTOMER-SALES-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTSALE.DAT".

           SELECT SORTED-TRANSACTION-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch9\STRANFL.DAT".

           SELECT TRANSACTION-REPORT
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Ch9\VSEX9-3A.RPT".

           SELECT SORT-FILE
               ASSIGN TO "SORTWORK".
      /
       DATA DIVISION.
      ***************

       FILE SECTION.
      *************
      ******************************************************************
      *                                                                *
      *        TRANSACTION FILE - VALIDATED AND SORTED IN THE          *
      *        PROGRAM.                                                *
      ******************************************************************

       FD  TRANSACTION-FILE.

       01 TRANSACTION-RECORD.
         02 TR-CUSTOMER-NUMBER PIC X(4).
         02 TR-PURCHASE-DATE-ALPHA PIC X(8).
         02 TR-PURCHASE-DATE REDEFINES
            TR-PURCHASE-DATE-ALPHA PIC 9(8).
         02 TR-AUTO-MAKE PIC X(20).
         02 TR-PURCHASE-PRICE-ALPHA PIC X(7).
         02 TR-PURCHASE-PRICE REDEFINES
            TR-PURCHASE-PRICE-ALPHA PIC 9(5)V99.
         02 TR-AUTO-YEAR PIC X(4).
         02 TR-SATISFACTION-CODE PIC X.

      ******************************************************************
      *   INPUT-FILE - CUSTOMER SALES FILE                             *
      *   THE RECORD LAYOUT, CUSTOMER-RECORD, IS DEFINED IN THE        *
      *   WORKING-STORAGE SECTION                                      *
      ******************************************************************

       FD  CUSTOMER-SALES-FILE.

       01 CUST-RECORD PIC X(76).
      /
      ******************************************************************
      *                                                                *
      *      SORTED TRANSACTION FILE - CONTAINS SORTED TRANSACTIONS    *
      *                                                                *
      ******************************************************************

       FD  SORTED-TRANSACTION-FILE.

       01 SORTED-TRAN-RECORD.
         02 STR-CUSTOMER-NUMBER PIC X(4).
         02 STR-PURCHASE-DATE PIC 9(8).
         02 STR-AUTO-MAKE PIC X(20).
         02 STR-PURCHASE-PRICE PIC 9(5)V99.
         02 STR-AUTO-YEAR PIC X(4).
         02 STR-SATISFACTION-CODE PIC X.

      ******************************************************************
      *                                                                *
      *    TRANSACTION LOG   -  REPORT ON EVERY TRANSACTION PROCESSED  *
      *                                                                *
      ******************************************************************

       FD  TRANSACTION-REPORT.

       01 TRAN-REPORT-LINE-OUT PIC X(80).

      ******************************************************************
      *                                                                *
      *      SORT FILE  -  FOR SORTING THE TRANSACTION FILE BY         *
      *                    CUSTOMER NUMBER.                            *
      *                                                                *
      ******************************************************************

       SD  SORT-FILE.

       01 SORT-RECORD.
         02 SR-CUSTOMER-NUMBER PIC X(4).
         02 PIC X(40).

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

         02 SW-VALID-TRANSACTION PIC X.
           88 VALID-TRANSACTION VALUE "Y".

      ******************************************************************
      *                                                                *
      *                      ACCUMULATORS                              *
      *                                                                *
      ******************************************************************

       01 ACCUMULATORS.

         02 AC-LINE-COUNT PIC 999.
         02 AC-PAGE-COUNT PIC 999.
         02 AC-BAD-TRANS-COUNT PIC 999.
         02 AC-POSTED-TRANS-COUNT PIC 999.
         02 AC-UNMATCHED-TRANS-COUNT PIC 999.
         02 AC-DUPLICATE-TRANS-COUNT PIC 999.
      /
      ******************************************************************
      *                                                                *
      *                     WORK AREA FIELDS                           *
      *                                                                *
      ******************************************************************

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

         02 WA-RUN-DATE
               REDEFINES WA-DATE PIC 9(8).

         02 WA-TEST-DATE PIC 9(8) VALUE 20011128.
         02 WA-PURCHASE-DATE PIC 9(8).
         02 WA-PREV-CUSTOMER-NUMBER PIC X(4).

      ******************************************************************
      *                                                                *
      *        RECORD LAYOUT - CUSTOMER SALES FILE                     *
      *                                                                *
      ******************************************************************

       01 CUSTOMER-SALES-RECORD.

         02 CSR-ZIP-CODE PIC X(5).
         02 CSR-ZIP-PLUS-4 PIC X(4).
         02 CSR-CUSTOMER-NUMBER PIC X(4).
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
      
      /
      ******************************************************************
      *                                                                *
      *         REPORT HEADINGS FOR TRANSACTION LOG REPORT             *
      *                                                                *
      ******************************************************************

       01 TRANSACTION-REPORT-HEADINGS.

         02 TRH-LINE-1.
           03 PIC X(6) VALUE "DATE: ".
           03 TRH-DATE PIC Z9/99/9999.
           03 PIC X(17) VALUE SPACES.
           03 PIC X(13) VALUE "EZ AUTO SALES".
           03 PIC X(26) VALUE SPACES.
           03 PIC X(5) VALUE "PAGE:".
           03 TRH-PAGE-COUNT PIC ZZ9.

         02 TRH-LINE-2.
           03 PIC X(6) VALUE "TIME: ".
           03 TRH-HOUR PIC Z9.
           03 PIC X VALUE ":".
           03 TRH-MINUTES PIC 99.
           03 TRH-AM-PM PIC XX.
           03 PIC X(10) VALUE SPACES.
           03 PIC X(33) VALUE
               "CUSTOMER SALES TRANSACTION REPORT".

         02 TRH-LINE-2A.
           03 PIC X(12) VALUE "TYLER SATHER".

         02 TRH-LINE-3.
           03 PIC X(18) VALUE "TRANSACTION RECORD".
           03 PIC X(31) VALUE SPACES.
           03 PIC X(7) VALUE "MESSAGE".

         02 TRH-LINE-4.
           03 PIC X(44) VALUE ALL "-".
           03 PIC X(5) VALUE SPACES.
           03 PIC X(30) VALUE ALL "-".
      /
      ******************************************************************
      *                                                                *
      *            DETAIL LINE FOR THE TRANSACTION REPORT              *
      *                                                                *
      ******************************************************************

       01 TRANSACTION-DETAIL-LINE.

         02 TDL-TRANSACTION-RECORD PIC X(44).
         02 PIC X(5) VALUE SPACES.
         02 TDL-MESSAGE PIC X(30).

      ******************************************************************
      *                                                                *
      *            SUMMARY LINES FOR THE TRANSACTION LOG REPORT        *
      *                                                                *
      ******************************************************************

       01 TRANSACTION-SUMMARY-LINES.

         02 TSL-BAD-TOTAL.
           03 PIC X(23) VALUE SPACES.
           03 PIC X(32) VALUE "TOTAL BAD TRANSACTIONS       =  ".
           03 TSL-BAD-TRANS-COUNT PIC ZZ9.

         02 TSL-POSTED-TOTAL.
           03 PIC X(23) VALUE SPACES.
           03 PIC X(32) VALUE "TOTAL POSTED TRANSACTIONS    =  ".
           03 TSL-POSTED-COUNT PIC ZZ9.

         02 TSL-UNMATCHED-TRAN-TOTAL.
           03 PIC X(23) VALUE SPACES.
           03 PIC X(32) VALUE "TOTAL UNMATCHED TRANSACTIONS =  ".
           03 TSL-UNMATCHED-TRANS-COUNT PIC ZZ9.

         02 TSL-DUPLICATE-TRAN-TOTAL.
           03 PIC X(23) VALUE SPACES.
           03 PIC X(32) VALUE "TOTAL DUPLICATE TRANSACTIONS =  ".
           03 TSL-DUPLICATE-TRANS-COUNT PIC ZZ9.

         02 TSL-END-OF-REPORT.
           03 PIC X(34) VALUE SPACES.
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
           PERFORM B-100-TRANSACTION-VALIDATION.
           PERFORM C-100-UPDATE-FILE.
           PERFORM D-100-WRAP-UP.
           STOP RUN.

      ******************************************************************
      *                                                                *
      *                   HOUSEKEEPING PARAGRAPH FOLLOWS               *
      *                                                                *
      ******************************************************************

       A-100-INITIALIZATION.

           INITIALIZE ACCUMULATORS.

           MOVE FUNCTION CURRENT-DATE TO WA-TODAYS-DATE-TIME.
           MOVE WA-TODAYS-MONTH TO WA-MONTH.
           MOVE WA-TODAYS-DAY TO WA-DAY.
           MOVE WA-TODAYS-YEAR TO WA-YEAR.
           MOVE WA-RUN-DATE TO TRH-DATE.

           EVALUATE TRUE
               WHEN WA-TODAYS-HOUR = 00
                   MOVE "AM" TO TRH-AM-PM
                   MOVE 12 TO WA-TODAYS-HOUR
               WHEN WA-TODAYS-HOUR < 12
                   MOVE "AM" TO TRH-AM-PM
               WHEN WA-TODAYS-HOUR = 12
                   MOVE "PM" TO TRH-AM-PM
               WHEN WA-TODAYS-HOUR > 12
                   MOVE "PM" TO TRH-AM-PM
                   SUBTRACT 12 FROM WA-TODAYS-HOUR.

           MOVE WA-TODAYS-HOUR TO TRH-HOUR.
           MOVE WA-TODAYS-MINUTES TO TRH-MINUTES.

           OPEN OUTPUT TRANSACTION-REPORT.
      /
      ******************************************************************
      *                                                                *
      *       TRANSACTION VALIDATION -SORT BY CUSTOMER NUMBER          *
      *                                                                *
      ******************************************************************

       B-100-TRANSACTION-VALIDATION.

           SORT SORT-FILE
           ON ASCENDING KEY SR-CUSTOMER-NUMBER
             INPUT PROCEDURE B-200-VALIDATE-TRANS-FILE
             GIVING SORTED-TRANSACTION-FILE.

      ******************************************************************
      *                                                                *
      *           INPUT PROCEDURE FOR TRANSACTION VALIDATION           *
      *                                                                *
      ******************************************************************

       B-200-VALIDATE-TRANS-FILE.

           OPEN INPUT TRANSACTION-FILE.
           MOVE "N" TO SW-END-OF-FILE.

           READ TRANSACTION-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

           PERFORM B-300-RELEASE-TRANSACTION
             UNTIL END-OF-FILE.

           CLOSE TRANSACTION-FILE.
      /
      *****************************************************************
      *                                                               *
      *                VALIDATE AND RELEASE TRANSACTIONS              *
      *                                                               *
      *****************************************************************

       B-300-RELEASE-TRANSACTION.

           MOVE TRANSACTION-RECORD TO TDL-TRANSACTION-RECORD
           MOVE "Y" TO SW-VALID-TRANSACTION.

           IF TR-CUSTOMER-NUMBER NOT NUMERIC
               MOVE "CUSTOMER NUMBER NOT NUMERIC" TO TDL-MESSAGE
               PERFORM B-400-WRITE-LOG.

           IF TR-PURCHASE-DATE-ALPHA NOT NUMERIC
               MOVE "PURCAHSE DATE NOT NUMERIC" TO TDL-MESSAGE
               PERFORM B-400-WRITE-LOG
           ELSE
               MOVE TR-PURCHASE-DATE(1:2) TO WA-PURCHASE-DATE(5:2)
               MOVE TR-PURCHASE-DATE(3:2) TO WA-PURCHASE-DATE(7:2)
               MOVE TR-PURCHASE-DATE(5:4) TO WA-PURCHASE-DATE(1:4)
               IF WA-PURCHASE-DATE > WA-TEST-DATE
                   MOVE "DATE OUT OF RANGE" TO TDL-MESSAGE
                   PERFORM B-400-WRITE-LOG.

           IF TR-PURCHASE-PRICE-ALPHA NOT NUMERIC
               MOVE "PRICE NOT NUMERIC" TO TDL-MESSAGE
               PERFORM B-400-WRITE-LOG
           ELSE
               IF TR-PURCHASE-PRICE <= 500
                   MOVE "PRICE TOO BELOW MINIMUM" TO TDL-MESSAGE
                   PERFORM B-400-WRITE-LOG.

           IF TR-AUTO-YEAR NOT NUMERIC
               MOVE "AUTO YEAR NOT NUMERIC" TO TDL-MESSAGE
               PERFORM B-400-WRITE-LOG
           ELSE
           IF TR-AUTO-YEAR > WA-TODAYS-YEAR
               MOVE "AUTO YEAR OUT OF RANGE" TO TDL-MESSAGE.

           IF TR-SATISFACTION-CODE NOT = "0" AND "1" AND "2"
               MOVE "SATISFACTION CODE OUT OF RANGE" TO TDL-MESSAGE
               PERFORM B-400-WRITE-LOG.

           IF VALID-TRANSACTION
               RELEASE SORT-RECORD FROM TRANSACTION-RECORD.

           READ TRANSACTION-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
      /
      *****************************************************************
      *                                                               *
      *                   WRITE TO TRANSACTION LOG                    *
      *                                                               *
      *****************************************************************

       B-400-WRITE-LOG.

           IF AC-LINE-COUNT = 0
               PERFORM M-500-LOG-HEADINGS.

           IF VALID-TRANSACTION
               WRITE TRAN-REPORT-LINE-OUT FROM TRANSACTION-DETAIL-LINE
                 AFTER ADVANCING 2 LINES
               ADD 2 TO AC-LINE-COUNT
               ADD 1 TO AC-BAD-TRANS-COUNT
               MOVE SPACES TO TDL-TRANSACTION-RECORD
               MOVE "N" TO SW-VALID-TRANSACTION
           ELSE
               WRITE TRAN-REPORT-LINE-OUT FROM TRANSACTION-DETAIL-LINE
                 AFTER ADVANCING 1 LINE
               ADD 1 TO AC-LINE-COUNT.

           IF AC-LINE-COUNT > 55
               MOVE ZERO TO AC-LINE-COUNT.
      /
      *****************************************************************
      *                                                               *
      *              CUSTOMER SALES FILE UPDATE PROCESSING            *
      *                                                               *
      *****************************************************************

       C-100-UPDATE-FILE.

           OPEN I-O CUSTOMER-SALES-FILE
             INPUT SORTED-TRANSACTION-FILE.

           READ CUSTOMER-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE HIGH-VALUES TO CSR-CUSTOMER-NUMBER.

           READ SORTED-TRANSACTION-FILE
               AT END
                   MOVE HIGH-VALUES TO STR-CUSTOMER-NUMBER.

           PERFORM C-200-UPDATE-RECORDS
             UNTIL CSR-CUSTOMER-NUMBER = HIGH-VALUES AND
             STR-CUSTOMER-NUMBER = HIGH-VALUES.

           MOVE AC-BAD-TRANS-COUNT TO TSL-BAD-TRANS-COUNT.
           MOVE AC-POSTED-TRANS-COUNT TO TSL-POSTED-COUNT.
           MOVE AC-UNMATCHED-TRANS-COUNT TO TSL-UNMATCHED-TRANS-COUNT.
           MOVE AC-DUPLICATE-TRANS-COUNT TO TSL-DUPLICATE-TRANS-COUNT.

           WRITE TRAN-REPORT-LINE-OUT FROM TSL-BAD-TOTAL
             AFTER ADVANCING 3 LINES.
           WRITE TRAN-REPORT-LINE-OUT FROM TSL-POSTED-TOTAL
             AFTER ADVANCING 1 LINE.
           WRITE TRAN-REPORT-LINE-OUT FROM TSL-UNMATCHED-TRAN-TOTAL
             AFTER ADVANCING 1 LINE.
           WRITE TRAN-REPORT-LINE-OUT FROM TSL-DUPLICATE-TRAN-TOTAL
             AFTER ADVANCING 1 LINE.
           WRITE TRAN-REPORT-LINE-OUT FROM TSL-END-OF-REPORT
             AFTER ADVANCING 2 LINES.
      /
      *****************************************************************
      *                                                               *
      *                 UPDATE INVENTORY RECORD PARAGRAPH             *
      *                                                               *
      *****************************************************************

       C-200-UPDATE-RECORDS.

           IF AC-LINE-COUNT = 0
               PERFORM M-500-LOG-HEADINGS.

           EVALUATE TRUE
               WHEN STR-CUSTOMER-NUMBER = CSR-CUSTOMER-NUMBER
                   PERFORM C-300-POST-TRANSACTION
               WHEN STR-CUSTOMER-NUMBER > CSR-CUSTOMER-NUMBER
                   PERFORM C-310-UNMATCHED-MASTER
               WHEN STR-CUSTOMER-NUMBER < CSR-CUSTOMER-NUMBER
                   PERFORM C-320-UNMATCHED-TRANSACTION.

           IF AC-LINE-COUNT > 55
               MOVE ZERO TO AC-LINE-COUNT.
      /
      *****************************************************************
      *                                                               *
      *                  POSTED TRANSACTION PARAGRAPH                 *
      *                                                               *
      *****************************************************************

       C-300-POST-TRANSACTION.

           MOVE STR-PURCHASE-DATE TO CSR-PURCHASE-DATE.
           MOVE STR-PURCHASE-PRICE TO CSR-PURCHASE-PRICE.
           MOVE STR-AUTO-MAKE TO CSR-AUTO-MAKE.
           MOVE STR-AUTO-YEAR TO CSR-AUTO-YEAR.
           MOVE STR-SATISFACTION-CODE TO CSR-SATISFACTION-CODE.

           REWRITE CUST-RECORD FROM CUSTOMER-SALES-RECORD.

           ADD 1 TO AC-POSTED-TRANS-COUNT.
           MOVE SORTED-TRAN-RECORD TO TDL-TRANSACTION-RECORD.
           MOVE "TRANSACTION POSTED" TO TDL-MESSAGE.

           WRITE TRAN-REPORT-LINE-OUT FROM TRANSACTION-DETAIL-LINE
             AFTER ADVANCING 2 LINES.

           ADD 2 TO AC-LINE-COUNT.
           MOVE STR-CUSTOMER-NUMBER TO WA-PREV-CUSTOMER-NUMBER.

           READ CUSTOMER-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE HIGH-VALUES TO CSR-CUSTOMER-NUMBER.

           READ SORTED-TRANSACTION-FILE
               AT END
                   MOVE HIGH-VALUES TO STR-CUSTOMER-NUMBER.
      /
      *****************************************************************
      *                                                               *
      *             UNMATCHED MASTER RECORD PARAGRAPH                 *
      *                                                               *
      *****************************************************************

       C-310-UNMATCHED-MASTER.

           READ CUSTOMER-SALES-FILE INTO CUSTOMER-SALES-RECORD
               AT END
                   MOVE HIGH-VALUES TO CSR-CUSTOMER-NUMBER.

      *****************************************************************
      *                                                               *
      *               UNMATCHED TRANSACTION PARAGRAPH                 *
      *                                                               *
      *****************************************************************

       C-320-UNMATCHED-TRANSACTION.

           IF STR-CUSTOMER-NUMBER = WA-PREV-CUSTOMER-NUMBER
               MOVE "DUPLICATE TRANSACTION" TO TDL-MESSAGE
               ADD 1 TO AC-DUPLICATE-TRANS-COUNT
           ELSE
               MOVE "UNMATCHED TRANSACTION" TO TDL-MESSAGE
               ADD 1 TO AC-UNMATCHED-TRANS-COUNT.

           MOVE SORTED-TRAN-RECORD TO TDL-TRANSACTION-RECORD.

           WRITE TRAN-REPORT-LINE-OUT FROM TRANSACTION-DETAIL-LINE
             AFTER ADVANCING 2 LINES.

           ADD 2 TO AC-LINE-COUNT.
           MOVE STR-CUSTOMER-NUMBER TO WA-PREV-CUSTOMER-NUMBER.

           READ SORTED-TRANSACTION-FILE
               AT END
                   MOVE HIGH-VALUES TO STR-CUSTOMER-NUMBER.
      /
      *****************************************************************
      *                                                               *
      *            TRANSACTION LOG HEADING PARAGRAPH                  *
      *            PERFORMED IN MULTIPLE STREAMS                      *
      *                                                               *
      *****************************************************************

       M-500-LOG-HEADINGS.

           ADD 1 TO AC-PAGE-COUNT.
           MOVE AC-PAGE-COUNT TO TRH-PAGE-COUNT.

           WRITE TRAN-REPORT-LINE-OUT FROM TRH-LINE-1
             AFTER ADVANCING PAGE.
           WRITE TRAN-REPORT-LINE-OUT FROM TRH-LINE-2
             AFTER ADVANCING 1 LINE.
           WRITE TRAN-REPORT-LINE-OUT FROM TRH-LINE-2A
             AFTER ADVANCING 1 LINE.
           WRITE TRAN-REPORT-LINE-OUT FROM TRH-LINE-3
             AFTER ADVANCING 2 LINES.
           WRITE TRAN-REPORT-LINE-OUT FROM TRH-LINE-4
             AFTER ADVANCING 1 LINE.

           MOVE 6 TO AC-LINE-COUNT.

      *****************************************************************
      *                                                               *
      *                    END OF JOB PARAGRAPH                       *
      *                                                               *
      *****************************************************************
       D-100-WRAP-UP.

           CLOSE CUSTOMER-SALES-FILE
             SORTED-TRANSACTION-FILE
             TRANSACTION-REPORT.

           DISPLAY " ".
           DISPLAY "CUSTOMER SALES UPDATE PROGRAM HAS TERMINATED".
           DISPLAY " ".

      ******************************************************************
      *                       END OF PROGRAM                           *
      ******************************************************************
      /
