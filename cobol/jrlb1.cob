      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
       PROGRAM-ID. CMSC 124 Exercise 1.
       AUTHOR. Randell Benavidez.
       DATE-WRITTEN. July 7, 2004.
       DATE-COMPILED. July 7, 2004.
      *****************************************************************
      * This program does the following:                              *
      *  -gets a salesman's info, price per unit                      *
      *  -gets a number n                                             *
      *  -gets number of units sold for n months                      *
      *  -computes total sales, average per month,                    *
      *   highest in a month, lowest in  a month                      *
      *  -uses zero suppression                                       *
      *****************************************************************

      *****************************************************************
       ENVIRONMENT DIVISION.
      *****************************************************************

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

      *****************************************************************
       DATA DIVISION.
      *****************************************************************

       WORKING-STORAGE SECTION.
       01 salesman-info.
           02 name PIC X(15) VALUE SPACES.
           02 product PIC X(10) .
           02 unit-price PIC 99 VALUE ZERO.
       01 sales-info.
           02 months PIC 99 VALUE ZERO.
           02 month-units PIC 99 VALUE ZERO.
           02 total-units PIC 9999 VALUE ZERO.
           02 total-price PIC 999999 VALUE ZERO.
           02 ave-units PIC 99v99 VALUE ZERO.
           02 ave-price PIC 9999v99 VALUE ZERO.
           02 highest-units PIC 99 VALUE ZERO.
           02 lowest-units PIC 99 VALUE ZERO.
           02 highest-price PIC 9999 VALUE ZERO.
           02 lowest-price PIC 9999 VALUE ZERO.
       01 counters.
           02 n PIC 99.
       01 display-chars.
           02 d-n PIC ZZ.
           02 d-total-units PIC ZZZZ.
           02 d-total-price PIC ZZZZZZ.
           02 d-ave-units PIC ZZ.ZZ.
           02 d-ave-price PIC ZZZZ.ZZ.                     
           02 d-highest-units PIC ZZ.        
           02 d-lowest-units PIC ZZ.
           02 d-highest-price PIC ZZZZ.
           02 d-lowest-price PIC ZZZZ.

      *****************************************************************
       PROCEDURE DIVISION.
      *****************************************************************
      
      *****************************************************************
      * This is the main paragraph of the program.  It serves as the  *
      *   as the controlling point of the whole program.              *
      *****************************************************************
       Main-Program.
           DISPLAY(1, 1) ERASE.           
           PERFORM Display-Border.
           
           DISPLAY(4, 6)  'Enter Name:'.
           DISPLAY(5, 6)  'Product Selling:'.           
           DISPLAY(6, 6) 'Price/Unit:'.           
           DISPLAY(8, 6) 'How many months?'.
           
           ACCEPT (4, 23) name.
           ACCEPT (5, 23) product.
           ACCEPT (6, 23) unit-price.
           ACCEPT (8, 23) months.

           DISPLAY(1, 1) ERASE.
           MOVE 1 TO n.
           PERFORM Monthly-Sales UNTIL n > months.
                      					 
           COMPUTE lowest-price = lowest-units * unit-price. 
           COMPUTE highest-price = highest-units * unit-price.
           
           MOVE total-units TO d-total-units.
           MOVE total-price TO d-total-price.
           MOVE ave-units TO d-ave-units.
           MOVE ave-price TO d-ave-price.
           MOVE highest-units TO d-highest-units.
       		 MOVE lowest-units TO d-lowest-units.       		           
           MOVE highest-price TO d-highest-price.
           MOVE lowest-price TO d-lowest-price.
           
           DISPLAY(1, 1) ERASE.           
           PERFORM Display-Border.
           
           DISPLAY(4, 6) 'Sales Report For: ', name.
           DISPLAY(6, 6) 'UNITS SOLD'.
           DISPLAY(7, 6) 'Total Units Sold: ' d-total-units ' ' product.
           DISPLAY(8, 6) 'Gross Sales: ', d-total-price.
           DISPLAY(10, 6) 'Units Per Month: ' d-ave-units ' ' product.
           DISPLAY(11, 6) 'Gross Sales Per Month: ', d-ave-price.           
           DISPLAY(13, 6) 'Highest: ' d-highest-units ' ' product.
           DISPLAY(14, 6) 'Highest Gross Sales: ', d-highest-price.
           DISPLAY(16, 6) 'Lowest: ' d-lowest-units ' ' product.
           DISPLAY(17, 6) 'Lowest Gross Sales: ', d-lowest-price.

           STOP RUN.
           
       Monthly-Sales.
           DISPLAY(1, 1) ERASE.
           PERFORM Display-Border.
       
           MOVE n TO d-n.
           DISPLAY(4, 6) 'Units sold in month ', d-n, ':'           
           ACCEPT (4, 31) month-units.
           DISPLAY ' '.
           
           COMPUTE total-units = total-units + month-units.
           COMPUTE total-price = total-units * unit-price.
           COMPUTE ave-units = total-units / months.
           COMPUTE ave-price = total-price / months.           
                      
           IF (n = 1)
             MOVE month-units TO highest-units
           	 MOVE month-units TO lowest-units.           
           IF (month-units > highest-units)
             MOVE month-units TO highest-units.             
           IF (month-units < lowest-units)
             MOVE month-units TO lowest-units.
           
           COMPUTE n = n + 1.  
           
           EXIT.
           
       Display-Border.
           DISPLAY(2, 2) '******************************************'.
           DISPLAY(2, 42) '*'.
           DISPLAY(19, 2) '******************************************'.
           DISPLAY(3, 2) '*'.
           DISPLAY(3, 43) '*'.
           DISPLAY(4, 2) '*'.
           DISPLAY(4, 43) '*'.
           DISPLAY(5, 2) '*'.
           DISPLAY(5, 43) '*'.
           DISPLAY(6, 2) '*'.
           DISPLAY(6, 43) '*'.
           DISPLAY(7, 2) '*'.
           DISPLAY(7, 43) '*'.
           DISPLAY(8, 2) '*'.
           DISPLAY(8, 43) '*'.
           DISPLAY(9, 2) '*'.
           DISPLAY(9, 43) '*'.
           DISPLAY(10, 2) '*'.
           DISPLAY(10, 43) '*'.
           DISPLAY(11, 2) '*'.
           DISPLAY(11, 43) '*'.
           DISPLAY(12, 2) '*'.
           DISPLAY(12, 43) '*'.
           DISPLAY(13, 2) '*'.
           DISPLAY(13, 43) '*'.
           DISPLAY(14, 2) '*'.
           DISPLAY(14, 43) '*'.
           DISPLAY(15, 2) '*'.
           DISPLAY(15, 43) '*'.
           DISPLAY(16, 2) '*'.           
           DISPLAY(16, 43) '*'.
           DISPLAY(17, 2) '*'.
           DISPLAY(17, 43) '*'.
           DISPLAY(18, 2) '*'.
           DISPLAY(18, 43) '*'.
           DISPLAY(19, 2) '*'.
           DISPLAY(19, 43) '*'.
           
           EXIT.
