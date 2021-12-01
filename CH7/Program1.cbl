       IDENTIFICATION DIVISION.
      ************************

       PROGRAM-ID. VSEX7-2.
       AUTHOR. TYLER SATHER.
       INSTALLATION. M STATE.
       DATE-WRITTEN. APRIL 8TH 2020.
       DATE-COMPILED.
      ******************************************************************
      *                      PROGRAM NARRATIVE                         *
      *                                                                *
      *    THIS PROGRAM READS ALL RECORDS IN THE CUSTOMER FILE         *
      *    FOR THE ABC DEPARTMENT STORE AND DISPLAYS A DETAIL          *
      *    CONTAINING A DATE OF PURCHASE, AN ACCOUNT NUMBER,           *
      *    CUSTOMER NAME, CLERK NAME, AND PURCHASE AMOUNT. A           *
      *    A RECORD COUNT IS DISPLAYE AT THE END OF THE REPORT.        *
      *                                                                *
      *                                                                *
      *                                                                *
      *        INPUT:     CUSTOMER.DAT - CUSTOMER INPUT FILE           *
      *                   CLRKNAME.DAT - CLERK FILE FOR LOADING TABLE  *
      *                                  WITH CLERK NAMES              *
      *        OUTPUT:    VSEX7-2.RPT  - CLERK NAME REPORT             *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.
      **********************

       INPUT-OUTPUT SECTION.
      **********************

       FILE-CONTROL.

           SELECT CUSTOMER-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CUSTOMER.DAT".

           SELECT CLERK-NAME-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\Data\CLRKNAME.DAT".

           SELECT REPORT-FILE
               ASSIGN TO "C:\Users\sathe\Desktop\Mstate\COBOL\CH7\VSEX7-2.RPT".

      /
       DATA DIVISION.
      ***************

       FILE SECTION.
      **************

      ******************************************************************
      *                                                                *
      *    INPUT-FILE -    CUSTOMER FILE                               *
      *                                                                *
      ******************************************************************

       FD  CUSTOMER-FILE.

       01 CUSTOMER-REC PIC X(74).

      ******************************************************************
      *                                                                *
      *    INPUT-FILE - SALES CLERKNAME FILE FOR LOADING TABLE         *
      *                                                                *
      ******************************************************************

       FD  CLERK-NAME-FILE.

       01 CLERK-NAME-RECORD.
         02 CNR-ID                         PIC XX.
         02 CNR-NAME                       PIC X(20). 

     

      ******************************************************************
      *                                                                *
      *    REPORT-FILE                                                 *
      *                                                                *
      ******************************************************************

       FD  REPORT-FILE.

       01 REPORT-LINE-OUT                   PIC X(80).

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
         02 AC-TOTAL-PURCHASES PIC 9(5)V99.
      /
      ******************************************************************
      *                                                                *
      *                       WORK AREA FIELDS                         *
      *                                                                *
      ******************************************************************

       01 WORK-AREA.

         02 WA-TODAYS-DATE-TIME.
           03 WA-TODAYS-DATE.
             04 WA-TODAYS-YEAR     PIC 9(4).
             04 WA-TODAYS-MONTH    PIC 99.
             04 WA-TODAYS-DAY      PIC 99.
           03 WA-TODAYS-TIME.
             04 WA-TODAYS-HOUR     PIC 99.
             04 WA-TODAYS-MINUTES  PIC 99.
           03                      PIC X(9).

         02 WA-DATE.
           03 WA-MONTH             PIC 99.
           03 WA-DAY               PIC 99.
           03 WA-YEAR              PIC 9(4).

         02 WA-RUN-DATE REDEFINES
            WA-DATE                PIC 9(8).

         02 WA-AM-PM               PIC XX.
         02 WA-SUB                 PIC 99.

         02 WA-PURCHASE-DATE.
           03 WA-PURCHASE-DAY      PIC XX.
           03                      PIC X VALUE "-".
           03 WA-PURCHASE-MONTH    PIC XXX.
           03                      PIC X VALUE "-".
           03 WA-PURCHASE-YEAR     PIC X(4).

         02 WA-SCREEN-HOLD         PIC X.
         02 WA-HYPHENS             PIC X(20) VALUE ALL "-".

      /
      ******************************************************************
      *                                                                *
      *                   EMBEDDED MONTH NAME TABLE                    *
      *                                                                *
      ******************************************************************

       01 MONTH-TABLE-DATA.
         02                        PIC XXX VALUE "JAN".
         02                        PIC XXX VALUE "FEB".
         02                        PIC XXX VALUE "MAR".
         02                        PIC XXX VALUE "APR".
         02                        PIC XXX VALUE "MAY".
         02                        PIC XXX VALUE "JUN".
         02                        PIC XXX VALUE "JUL".
         02                        PIC XXX VALUE "AUG".
         02                        PIC XXX VALUE "SEP".
         02                        PIC XXX VALUE "OCT".
         02                        PIC XXX VALUE "NOV".
         02                        PIC XXX VALUE "DEC".

       01 MONTH-TABLE REDEFINES MONTH-TABLE-DATA.
         02 MNT-NAME OCCURS 12 TIMES PIC XXX.

      ******************************************************************
      *                                                                *
      *                NON-EMBEDDED WAREHOUSE TABLE                    *
      *                                                                *
      ******************************************************************

       01 CLERK-NAME-TABLE.
         02 CNT-ENTRY OCCURS 11 TIMES
                       ASCENDING KEY CNT-ID
                       INDEXED BY CNT-INDEX.

           03 CNT-ID PIC XX.
           03 CNT-NAME PIC X(20).

      /
      ******************************************************************
      *   CUSTOMER FILE RECORD LAYOUT                                  *
      *   PURCHASE DATE - MMDDYYYY FORMAT                              *
      ******************************************************************
       01 CUSTOMER-RECORD.
         02 CR-PURCHASE-DATE                   PIC 9(8).
         02 CR-ACCOUNT-NUMBER                  PIC X(6).
         02 CR-CUSTOMER-NAME                   PIC X(20).
         02 CR-ITEM-PURCHASED                  PIC X(20).
         02 CR-QUANTITY                        PIC 9.
         02 CR-BALANCE                         PIC S9(4)V99.
         02 CR-PURCHASE-AMOUNT                 PIC 9(4)V99.
         02                                    PIC X(5).
         02 CR-CLERK-ID                        PIC XX.

      /


      ******************************************************************
      *                                                                *
      *       REPORT HEADINGS FOR THE CLERK NAME REPORT                *
      *                                                                *
      ******************************************************************

       01 REPORT-HEADINGS.

         02 RH-LINE-1.
           03                              PIC X(6) VALUE "DATE: ".
           03 RH-RUN-DATE                  PIC Z9/99/9999.
           03                              PIC X(11) VALUE SPACES.
           03                              PIC X(20) VALUE
              "ABC DEPARTMENT STORE".
           03                              PIC X(17) VALUE SPACES.
           03                              PIC X(5) VALUE
              "PAGE".
           03 RH-PAGE                      PIC ZZ9.

         02 RH-LINE-2.
           03                              PIC X(6) VALUE
              "TIME: ".
           03 RH-HOUR                      PIC X9.
           03                              PIC X VALUE ":".
           03 RH-MINUTES                   PIC 99.
           03 RH-AM-PM                     PIC XX.
           03                              PIC X(15) VALUE SPACES.
           03                              PIC X(17) VALUE
               "CLERK NAME REPORT".

         02 RH-LINE-2A.
           03                              PIC X(12) VALUE
               "TYLER SATHER".

         02 RH-LINE-3.
           03                              PIC X(7) VALUE
               "DATE OF".
           03                              PIC X(6) VALUE SPACES.
           03                              PIC X(7) VALUE "ACCOUNT".
           03                              PIC X(43) VALUE SPACES.
           03                              PIC X(8) VALUE "PURCHASE".

         02 RH-LINE-4.
           03                              PIC X(8) VALUE "PURCHASE".
           03                              PIC X(5) VALUE SPACES.
           03                              PIC X(6) VALUE "NUMBER".
           03                              PIC XX VALUE SPACES.
           03                              PIC X(13) VALUE
               "CUSTOMER NAME".
           03                              PIC X(8) VALUE SPACES.
           03                              PIC X(10) VALUE
               "CLERK NAME".
           03                              PIC X(11) VALUE SPACES.
           03                              PIC X(6) VALUE "AMOUNT".

         02 RH-LINE-5.
           03                              PIC X(11) VALUE ALL "-".
           03                              PIC XX VALUE SPACES.
           03                              PIC X(7) VALUE ALL "-".
           03                              PIC X VALUE SPACES.
           03                              PIC X(20) VALUE ALL "-".
           03                              PIC X VALUE SPACES.
           03                              PIC X(20) VALUE ALL "-".
           03                              PIC X VALUE SPACES.
           03                              PIC X(9) VALUE ALL "-".


      /
      ******************************************************************
      *                                                                *
      *          DETAIL LINE FOR THE CLERK NAME REPORT                 *
      *                                                                *
      ******************************************************************

       01 DETAIL-LINE.
         02 DL-PURCHASE-DATE               PIC X(11).
         02                                PIC XX VALUE SPACES.
         02 DL-ACCOUNT-NUMBER              PIC X(6).
         02                                PIC XX VALUE SPACES.
         02 DL-CUSTOMER-NAME               PIC X(20).
         02                                PIC X VALUE SPACES.
         02 DL-CLERK-NAME                  PIC X(20).
         02                                PIC X VALUE SPACES.
         02 DL-PURCHASE-AMOUNT             PIC $Z,ZZZ.99.

      ******************************************************************
      *                                                                *
      *           SUMMARY LINE FOR THE CLERK NAME REPORT               *
      *                                                                *
      ******************************************************************

       01 SUMMARY-LINES.

         02 SL-LINE-1.
           03                              PIC X(18) VALUE SPACES.
           03                              PIC X(26) VALUE
               "TOTAL RECORDS PROCESSED =".
           03                              PIC X VALUE SPACES.
           03 SL-RECORD-COUNT              PIC ZZ9.

         02 SL-LINE-2.
           03                              PIC X(18) VALUE SPACES.
           03                              PIC X(18) VALUE
               "TOTAL PURCHASES =".
           03                              PIC XX VALUE SPACES.
           03 SL-TOTAL-PURCHASES           PIC $ZZ,ZZZ.99.

         02 SL-LINE-3.
           03                              PIC X(24) VALUE SPACES.
           03                              PIC X(13) VALUE
               "END OF REPORT".

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
           PERFORM B-100-LOAD-CLERK-NAME-TABLE.
           PERFORM C-100-PROCESS-FILE.
           PERFORM D-100-WRAP-UP.
           STOP RUN.

      ******************************************************************
      *                                                                *
      *                   HOUSEKEEPING PARAGRAPH FOLLOWS               *
      *                                                                *
      ******************************************************************

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
      /
      ******************************************************************
      *                                                                *
      *                    TABLE LOADING PARAGRAPH                     *
      *                                                                *
      ******************************************************************

       B-100-LOAD-CLERK-NAME-TABLE.

           OPEN INPUT CLERK-NAME-FILE.

           MOVE "N" TO SW-END-OF-FILE.


           READ CLERK-NAME-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.

           PERFORM B-200-LOAD
             VARYING CNT-INDEX FROM 1 BY 1
             UNTIL END-OF-FILE OR CNT-INDEX > 11.

           CLOSE CLERK-NAME-FILE.

      ******************************************************************
      *                                                                *
      *           MOVE CLERK NAME TO DATA TO NAME TABLE                *
      *                                                                *
      ******************************************************************

       B-200-LOAD.

           MOVE CNR-ID TO CNT-ID(CNT-INDEX).
           MOVE CNR-NAME TO CNT-NAME(CNT-INDEX).


           READ CLERK-NAME-FILE
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
      /
      ******************************************************************
      *                                                                *
      *             FILE PROCESSING CONTROL PARAGRAPH                  *
      *                                                                *
      ******************************************************************

       C-100-PROCESS-FILE.

           OPEN INPUT CUSTOMER-FILE.
           MOVE "N" TO SW-END-OF-FILE.
           READ CUSTOMER-FILE INTO CUSTOMER-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
           PERFORM C-200-PROCESS-RECORD
             UNTIL END-OF-FILE.

           MOVE AC-RECORD-COUNT TO SL-RECORD-COUNT.
           MOVE AC-TOTAL-PURCHASES TO SL-TOTAL-PURCHASES.

           ADD 1 TO AC-LINE-COUNT.
           WRITE REPORT-LINE-OUT FROM SL-LINE-1
             AFTER ADVANCING 2 LINES.
           ADD 1 TO AC-LINE-COUNT.
           WRITE REPORT-LINE-OUT FROM SL-LINE-2
             AFTER ADVANCING 1 LINE.
           ADD 2 TO AC-LINE-COUNT.
           WRITE REPORT-LINE-OUT FROM SL-LINE-3
             AFTER ADVANCING 1 LINE.

      /
      ******************************************************************
      *                                                                *
      *           SEARCH TABLE AND DISPLAY DETAIL LINE                 *
      *                                                                *
      ******************************************************************

       C-200-PROCESS-RECORD.

           IF AC-LINE-COUNT = 0
               PERFORM C-400-WRITE-HEADINGS.

           MOVE CR-PURCHASE-DATE(3:2) TO WA-PURCHASE-DAY.
           MOVE CR-PURCHASE-DATE(1:2) TO WA-SUB.
           MOVE MNT-NAME(WA-SUB) TO WA-PURCHASE-MONTH.
           MOVE CR-PURCHASE-DATE(5:4) TO WA-PURCHASE-YEAR.

           SEARCH ALL CNT-ENTRY
               AT END
                   MOVE "NOT FOUND" TO CNR-NAME
               WHEN CNT-ID(CNT-INDEX) = CR-CLERK-ID
                   MOVE CNT-NAME(CNT-INDEX) TO CNR-NAME.

           MOVE WA-PURCHASE-DATE TO DL-PURCHASE-DATE.
           MOVE CR-ACCOUNT-NUMBER TO DL-ACCOUNT-NUMBER.
           MOVE CR-CUSTOMER-NAME TO DL-CUSTOMER-NAME.
           MOVE CNR-NAME TO DL-CLERK-NAME.
           MOVE CR-PURCHASE-AMOUNT TO DL-PURCHASE-AMOUNT,

           WRITE REPORT-LINE-OUT FROM DETAIL-LINE
             AFTER ADVANCING 1 LINE.

           ADD 1 TO AC-LINE-COUNT.
           ADD 1 TO AC-RECORD-COUNT.
           ADD CR-PURCHASE-AMOUNT TO AC-TOTAL-PURCHASES.

           IF AC-LINE-COUNT > 55
               PERFORM C-400-WRITE-HEADINGS.

           READ CUSTOMER-FILE INTO CUSTOMER-RECORD
               AT END
                   MOVE "Y" TO SW-END-OF-FILE.
      /

      ******************************************************************
      *                                                                *
      *           HEADING PARAGRAPH                                    *
      *                                                                *
      ******************************************************************
       C-400-WRITE-HEADINGS.

           ADD 1 TO AC-PAGE-COUNT.
           MOVE 0 TO AC-LINE-COUNT.

           MOVE WA-RUN-DATE TO RH-RUN-DATE.
           MOVE AC-PAGE-COUNT TO RH-PAGE.
           MOVE WA-TODAYS-HOUR TO RH-HOUR.
           MOVE WA-TODAYS-MINUTES TO RH-MINUTES.
           MOVE WA-AM-PM TO RH-AM-PM.

           WRITE REPORT-LINE-OUT FROM RH-LINE-1
             AFTER ADVANCING 2 LINES.
           WRITE REPORT-LINE-OUT FROM RH-LINE-2
             AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM RH-LINE-2A
             AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM RH-LINE-3
             AFTER ADVANCING 2 LINES.
           WRITE REPORT-LINE-OUT FROM RH-LINE-4
             AFTER ADVANCING 1 LINE.
           WRITE REPORT-LINE-OUT FROM RH-LINE-5
             AFTER ADVANCING 1 LINE.
           ADD 8 TO AC-LINE-COUNT.

      /    
      ******************************************************************
      *                                                                *
      *                      END OF JOB PARAGRAPH                      *
      *                                                                *
      ******************************************************************

       D-100-WRAP-UP.

           CLOSE CUSTOMER-FILE
                 REPORT-FILE.
           DISPLAY " ".
           DISPLAY "CLERK NAME REPORT PROGRAM HAS TERMINATED".
           DISPLAY " ".
           ACCEPT WA-SCREEN-HOLD.

      ******************************************************************
      *                         END OF PROGRAM                         *
      ******************************************************************
      /
