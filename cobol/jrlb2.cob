      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
       PROGRAM-ID. CMSC 124 Exercise 2.
       AUTHOR. Randell Benavidez.
       DATE-WRITTEN. July 21, 2004
       DATE-COMPILED. July 21, 2004.
      *****************************************************************
      * This program does the following:
      *  - gets a number n
      *  - gets info for n entries
      *  - writes data to grades.txt
      *  - allows the user to get a stdnum
      *  - prints all courses/grades for that student
      *  - allows to view a student after another
      *  - checks if the student exists
      *****************************************************************

      *****************************************************************
       ENVIRONMENT DIVISION.
      *****************************************************************

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT grades ASSIGN TO DISK.

      *****************************************************************
       DATA DIVISION.
      *****************************************************************

       FILE SECTION.
       FD grades LABEL RECORDS ARE STANDARD
               VALUE OF FILE-ID is 'grades.txt'.

       01 std-info.
           02 stdnum PIC X(9).
           02 subject PIC X(10).
           02 grade PIC 9v99.
       

       WORKING-STORAGE SECTION.
       77 n PIC 99 VALUE 0.
       77 cnt PIC 99 VALUE 0.
       77 d-cnt PIC ZZ.
       77 eof PIC 9 VALUE 0.
       77 stud PIC X(9) VALUE SPACES.
       77 a-grade PIC 9.99 VALUE ZERO.
       77 ans PIC X VALUE 'y'.
       77 cont PIC X VALUE SPACES.
       77 cor PIC 9 VALUE 0.
       77 found PIC 9 VALUE 0.

      *****************************************************************
       PROCEDURE DIVISION.
      *****************************************************************

       Main-Program.
           DISPLAY(1, 1) ERASE.

           DISPLAY(4, 6) 'Enter n: '.
           ACCEPT (4, 16) n.

           OPEN OUTPUT grades.
           PERFORM WRITE-INPUT UNTIL cnt = n.
           CLOSE grades.

           PERFORM VIEW-REC UNTIL ans = 'n'.

           STOP RUN.
           
       WRITE-INPUT.
           DISPLAY(1, 1) ERASE.

           COMPUTE cnt = cnt + 1.
           MOVE cnt TO d-cnt.
           DISPLAY(4, 6) 'Entry ' d-cnt.

           DISPLAY(4, 15) 'stdnum: '.
           ACCEPT (4, 23) stdnum.
           DISPLAY(5, 15) 'course: '.
           ACCEPT (5, 23) subject.
           DISPLAY(6, 15) 'grade: '.
           ACCEPT (6, 23) grade.

           WRITE std-info.           

           EXIT.

       VIEW-REC.
           DISPLAY(1, 1) ERASE.
           DISPLAY(4, 6) 'View subjects for: '.
           ACCEPT (4, 25) stud.                      

           OPEN INPUT grades.
           READ grades AT END MOVE 1 TO eof.
           PERFORM READ-INPUT UNTIL eof = 1.
           CLOSE grades.
           MOVE 0 to eof.

           IF found = 1
             DISPLAY(6, 6) 'Subjects of ' stud.
           IF found = 0
             DISPLAY(9, 6) 'Student number not found'.

           DISPLAY(8, 5) ' '.
           DISPLAY ' '.
           DISPLAY 'Press any key to continue...'
           ACCEPT cont.

           DISPLAY(1, 1) ERASE.
           DISPLAY(4, 6) 'View another student (Y/N)? '.
           ACCEPT ans.           

           EXIT.
                      
       READ-INPUT.
           MOVE 0 TO found.
           IF stud = stdnum
             MOVE 1 TO found
             PERFORM DISPLAY-FILE-DATA.
           READ grades AT END MOVE 1 to eof.

           EXIT.

       DISPLAY-FILE-DATA.
           MOVE grade TO a-grade.
           DISPLAY subject ' ........ ' a-grade.

           EXIT.
