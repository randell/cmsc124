      ****************************************************************
       IDENTIFICATION DIVISION.
      ****************************************************************
       PROGRAM-ID. CMSC 124 Exercise 1.
       AUTHOR. Randell Benavidez.
       DATE-WRITTEN. July 28, 2004.
       DATE-COMPILED. July 28, 2004.
      ****************************************************************
      * This program does the following:                             *
      *                                                              *
      *  -asks for students' information until 'n' (which means NO)  *
      *   is enter when asked if another info should be entered      *
      *                                                              *
      *  -given the course code, searches for the corresponding      *
      *   course title and units from data.txt                       *
      *                                                              *
      *  -writes each record into grades.txt                         *
      *                                                              *
      *  -allows the user to choose to view the record by student or *
      *   by course                                                  *
      *                                                              *
      *  -if record is viewed by student, asks for the student number*
      *   then displays the course code, title, units, grade and     *
      *   average                                                    *
      *                                                              *
      *  -if record is viewed by course, asks for the course code    *
      *   then displays the student numbers and grades               *
      ****************************************************************

      ****************************************************************
       ENVIRONMENT DIVISION.
      ****************************************************************

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT grades ASSIGN TO DISK.
           SELECT datum ASSIGN TO DISK.

      ****************************************************************
       DATA DIVISION.
      ****************************************************************

       FILE SECTION.
       FD grades LABEL RECORDS ARE STANDARD
               VALUE OF FILE-ID is 'grades.txt'.

       01 std-info.
           02 stdnum PIC X(9).
           02 code PIC X(8).
           02 title PIC X(50).
           02 units PIC 9v9.
           02 grade PIC 9v99.

       FD datum LABEL RECORDS ARE STANDARD
               VALUE OF FILE-ID is 'data.txt'.

       01 course-info.
           02 coursecode PIC X(8).
           02 coursetitle PIC X(50).
           02 courseunits PIC 9v9.

       WORKING-STORAGE SECTION.
       77 ans PIC X VALUE SPACES.
       77 eof PIC 9 VALUE 0.
       77 aorb PIC X VALUE SPACES.
       77 studnum PIC X(9) VALUE SPACES.
       77 d-units PIC 9.9 VALUE ZERO.
       77 d-grade PIC 9.99 VALUE ZERO.
       77 crs PIC X(8) VALUE SPACES.
       77 total-units PIC 99v9 VALUE ZERO.
       77 ave PIC 9v99 VALUE ZERO.
       77 total-grade PIC 99v99 VALUE ZERO.
       77 earned-grade PIC 99v99 VALUE ZERO.

      ****************************************************************
       PROCEDURE DIVISION.
      ****************************************************************

       Main-program.
           DISPLAY(1, 1) ERASE.

           OPEN OUTPUT grades.
           PERFORM ASK-INFO UNTIL ans = 'n'.
           CLOSE grades.

           DISPLAY(1, 1) ERASE.
           DISPLAY(4, 6) 'a. view by student'.
           DISPLAY(5, 6) 'b. view by course'.
           DISPLAY(7, 6) 'Choose a letter: '.
           ACCEPT (7, 23) aorb.

           IF aorb = 'a'
             PERFORM VIEW-BY-STD.
           IF aorb = 'b'
             PERFORM VIEW-BY-CRS.

           STOP RUN.

       ASK-INFO.
           DISPLAY(1, 1) ERASE.

           DISPLAY(4, 6) 'Enter stdnum: '.           
           DISPLAY(5, 12) 'course code: '.
           DISPLAY(6, 12) 'grade: '.           

           ACCEPT (4, 25) stdnum.
           ACCEPT (5, 25) code.
           ACCEPT (6, 25) grade.

           DISPLAY(8, 6) 'Enter another? '.
           ACCEPT (8, 25) ans.

           OPEN INPUT datum.
           READ datum AT END MOVE 1 TO eof.
           PERFORM READ-COURSE-DATA UNTIL eof = 1.
           CLOSE datum.
           MOVE 0 TO eof.

           WRITE std-info.

           EXIT.

       READ-COURSE-DATA.
           IF code = coursecode
             MOVE coursetitle TO title
             MOVE courseunits TO units.
           READ datum AT END MOVE 1 TO eof.

           EXIT.

       VIEW-BY-STD.
           DISPLAY(1, 1) ERASE.

           DISPLAY(4, 6) 'Enter a stud num: '.
           ACCEPT (4, 24) studnum.

           DISPLAY(6, 1) 'Course'.
           DISPLAY(6, 10) 'Title'.
           DISPLAY(6, 61) 'Units'.
           DISPLAY(6, 70) 'Grade.'
           DISPLAY '    '.

           OPEN INPUT grades.
           READ grades AT END MOVE 1 TO eof.
           PERFORM READ-STD-DATA UNTIL eof = 1.
           CLOSE grades.
           MOVE 0 TO eof.

           DISPLAY ' '.

           COMPUTE ave = total-grade / total-units.
           DISPLAY 'Average: ' ave.

           EXIT.

       READ-STD-DATA.
           IF studnum = stdnum
             PERFORM DISPLAY-STD-DATA
             COMPUTE earned-grade = units * grade.
             COMPUTE total-grade = total-grade + earned-grade.
             COMPUTE total-units = total-units + units.
           READ grades AT END MOVE 1 To eof.

           EXIT.

       DISPLAY-STD-DATA.           
           MOVE units TO d-units.
           MOVE grade TO d-grade.
           DISPLAY code ' ' title ' ' d-units '        ' d-grade.

           EXIT.

       VIEW-BY-CRS.
           DISPLAY(1, 1) ERASE.

           DISPLAY(4, 6) 'Enter a course: '.
           ACCEPT (4, 22) crs.

           DISPLAY(6, 1) 'stdnum'.
           DISPLAY(6, 11) 'grade'.
           DISPLAY ' '.

           OPEN INPUT grades.
           READ grades AT END MOVE 1 TO eof.
           PERFORM READ-CRS-DATA UNTIL eof = 1.
           CLOSE grades.
           MOVE 0 TO eof.

           EXIT.

       READ-CRS-DATA.
           IF crs = code
             PERFORM DISPLAY-CRS-DATA.
           READ grades AT END MOVE 1 TO eof.

           EXIT.

       DISPLAY-CRS-DATA.
           MOVE grade TO d-grade.
           DISPLAY stdnum ' ' d-grade.

           EXIT.
