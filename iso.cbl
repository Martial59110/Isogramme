       IDENTIFICATION DIVISION.
        PROGRAM-ID. iso.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        
        01  WS-ALPHABET.
           05 WS-A PIC X VALUE 'A'.
           05 WS-B PIC X VALUE 'B'.
           05 WS-C PIC X VALUE 'C'.
           05 WS-D PIC X VALUE 'D'.
           05 WS-E PIC X VALUE 'E'.
           05 WS-F PIC X VALUE 'F'.
           05 WS-G PIC X VALUE 'G'.
           05 WS-H PIC X VALUE 'H'.
           05 WS-I PIC X VALUE 'I'.
           05 WS-J PIC X VALUE 'J'.
           05 WS-K PIC X VALUE 'K'.
           05 WS-L PIC X VALUE 'L'.
           05 WS-M PIC X VALUE 'M'.
           05 WS-N PIC X VALUE 'N'.
           05 WS-O PIC X VALUE 'O'.
           05 WS-P PIC X VALUE 'P'.
           05 WS-Q PIC X VALUE 'Q'.
           05 WS-R PIC X VALUE 'R'.
           05 WS-S PIC X VALUE 'S'.
           05 WS-T PIC X VALUE 'T'.
           05 WS-U PIC X VALUE 'U'.
           05 WS-V PIC X VALUE 'V'.
           05 WS-W PIC X VALUE 'W'.
           05 WS-X PIC X VALUE 'X'.
           05 WS-Y PIC X VALUE 'Y'.
           05 WS-Z PIC X VALUE 'Z'.
        
        01  WS-WORD-LENGTH PIC 99.
        01  WS-WORD PIC X(50).
        01  WS-WORD1 PIC X(50).
        01 WS-COUNTER-IDX PIC 99.
        01  WS-COUNTERS.
           05 WS-CNT-A PIC 99 VALUE ZEROES.
           05 WS-CNT-B PIC 99 VALUE ZEROES.
           05 WS-CNT-C PIC 99 VALUE ZEROES.
           05 WS-CNT-D PIC 99 VALUE ZEROES.
           05 WS-CNT-E PIC 99 VALUE ZEROES.
           05 WS-CNT-F PIC 99 VALUE ZEROES.
           05 WS-CNT-G PIC 99 VALUE ZEROES.
           05 WS-CNT-H PIC 99 VALUE ZEROES.
           05 WS-CNT-I PIC 99 VALUE ZEROES.
           05 WS-CNT-J PIC 99 VALUE ZEROES.
           05 WS-CNT-K PIC 99 VALUE ZEROES.
           05 WS-CNT-L PIC 99 VALUE ZEROES.
           05 WS-CNT-M PIC 99 VALUE ZEROES.
           05 WS-CNT-N PIC 99 VALUE ZEROES.
           05 WS-CNT-O PIC 99 VALUE ZEROES.
           05 WS-CNT-P PIC 99 VALUE ZEROES.
           05 WS-CNT-Q PIC 99 VALUE ZEROES.
           05 WS-CNT-R PIC 99 VALUE ZEROES.
           05 WS-CNT-S PIC 99 VALUE ZEROES.
           05 WS-CNT-T PIC 99 VALUE ZEROES.
           05 WS-CNT-U PIC 99 VALUE ZEROES.
           05 WS-CNT-V PIC 99 VALUE ZEROES.
           05 WS-CNT-W PIC 99 VALUE ZEROES.
           05 WS-CNT-X PIC 99 VALUE ZEROES.
           05 WS-CNT-Y PIC 99 VALUE ZEROES.
           05 WS-CNT-Z PIC 99 VALUE ZEROES.
        
        01  WS-FLAG PIC X VALUE 'N'.
        
        01  WS-INPUT-MSG.
           05 WS-INPUT-MSG-LINE PIC X(50).
        
        01  WS-OUTPUT-MSG.
           05 WS-OUTPUT-MSG-LINE PIC X(50).
         
         PROCEDURE DIVISION.
         
         100-MAIN.
             DISPLAY "Enter a word: ".
             ACCEPT WS-WORD1.
             MOVE FUNCTION UPPER-CASE (WS-WORD1) TO WS-WORD.
            
           INSPECT WS-WORD TALLYING WS-CNT-A FOR ALL WS-A
           INSPECT WS-WORD TALLYING WS-CNT-B FOR ALL WS-B
           INSPECT WS-WORD TALLYING WS-CNT-C FOR ALL WS-C
           INSPECT WS-WORD TALLYING WS-CNT-D FOR ALL WS-D
           INSPECT WS-WORD TALLYING WS-CNT-E FOR ALL WS-E
           INSPECT WS-WORD TALLYING WS-CNT-F FOR ALL WS-F
           INSPECT WS-WORD TALLYING WS-CNT-G FOR ALL WS-G
           INSPECT WS-WORD TALLYING WS-CNT-H FOR ALL WS-H
           INSPECT WS-WORD TALLYING WS-CNT-I FOR ALL WS-I
           INSPECT WS-WORD TALLYING WS-CNT-J FOR ALL WS-J
           INSPECT WS-WORD TALLYING WS-CNT-K FOR ALL WS-K
           INSPECT WS-WORD TALLYING WS-CNT-L FOR ALL WS-L
           INSPECT WS-WORD TALLYING WS-CNT-M FOR ALL WS-M
           INSPECT WS-WORD TALLYING WS-CNT-N FOR ALL WS-N
           INSPECT WS-WORD TALLYING WS-CNT-O FOR ALL WS-O
           INSPECT WS-WORD TALLYING WS-CNT-P FOR ALL WS-P
           INSPECT WS-WORD TALLYING WS-CNT-Q FOR ALL WS-Q
           INSPECT WS-WORD TALLYING WS-CNT-R FOR ALL WS-R
           INSPECT WS-WORD TALLYING WS-CNT-S FOR ALL WS-S
           INSPECT WS-WORD TALLYING WS-CNT-T FOR ALL WS-T
           INSPECT WS-WORD TALLYING WS-CNT-U FOR ALL WS-U
           INSPECT WS-WORD TALLYING WS-CNT-V FOR ALL WS-V
           INSPECT WS-WORD TALLYING WS-CNT-W FOR ALL WS-W
           INSPECT WS-WORD TALLYING WS-CNT-X FOR ALL WS-X
           INSPECT WS-WORD TALLYING WS-CNT-Y FOR ALL WS-Y
           INSPECT WS-WORD TALLYING WS-CNT-Z FOR ALL WS-Z
           

            IF WS-CNT-A > 1 OR WS-CNT-B > 1 OR WS-CNT-C > 1 
            OR WS-CNT-D > 1 OR
               WS-CNT-E > 1 OR WS-CNT-F > 1 OR WS-CNT-G > 1 
               OR WS-CNT-H > 1 OR
               WS-CNT-I > 1 OR WS-CNT-J > 1 OR WS-CNT-K > 1 
               OR WS-CNT-L > 1 OR
               WS-CNT-M > 1 OR WS-CNT-N > 1 OR WS-CNT-O > 1 
               OR WS-CNT-P > 1 OR
               WS-CNT-Q > 1 OR WS-CNT-R > 1 OR WS-CNT-S > 1 
               OR WS-CNT-T > 1 OR
               WS-CNT-U > 1 OR WS-CNT-V > 1 OR WS-CNT-W > 1 
               OR WS-CNT-X > 1 OR
               WS-CNT-Y > 1 OR WS-CNT-Z > 1
                MOVE 'Y' TO WS-FLAG
            ELSE
                MOVE 'N' TO WS-FLAG
            END-IF

            IF WS-FLAG = 'Y'
                DISPLAY "The word is an isogram."
            ELSE
                DISPLAY "The word is not an isogram."
            END-IF

            STOP RUN.
