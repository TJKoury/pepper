C$Procedure      MPRINT ( Matrix Print )
 
      SUBROUTINE MPRINT ( MAT, ROWS, COLS, FMT )
 
C$ Abstract
C
C     Format the contents of a d.p. matrix for printing.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C      UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               ROWS
      INTEGER               COLS
      DOUBLE PRECISION      MAT ( ROWS, COLS )
 
      CHARACTER*(*)         FMT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MAT        I   A d.p. matrix.
C     ROWS       I   The number of rows in the matrix.
C     COLS       I   The number of columns in the matrix.
C     FMT        I   A picture of the format for each in the matrix.
C     LINES      O   An arrary of text lines
C
C$ Detailed_Input
C
C     MAT        A matrix of ROWS rows and COLS columns that should
C                be converted into a suitable text format.
C
C     ROWS       The number of rows in MAT
C
C     COLS       The number of columns in MAT.
C
C     FMT        The format to use when creating entries.
C
C$ Detailed_Output
C
C     LINES      Array of string which when printed will present the
C                matrix in commonly used format for matrices.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine is a utility for those times when you simply want
C     to print a matrix.
C
C     Suppose MATRIX contains the inverses of the integers 1
C     through 9.  To print this matrix make the following call
C
C     CALL MPRINT ( MATRIX,  3, 3, '-x.xxxx' )
C
C     You'll get the following output.
C     (you need to view this with a fixed pitch font or things
C      may not look so great).
C
C        1.0000     0.5000     0.3333
C        0.2500     0.2000     0.1667
C        0.1429     0.1250     0.1111
C
C     If the (3,2) entry gets corrupted with a value like
C     -1282.291 you'll get the following.
C
C        1.0000     0.5000     0.3333
C        0.2500     0.2000     0.1667
C        0.1429    -1.282E+03  0.1111
C
C     Notice how the "out of range" guy sticks out calling
C     attention to himself.  To print a 6x6 matrix just change
C     the 3 to 6 in the calling sequence.
C
C     CALL MPRINT ( MATRIX,  6, 6, '-x.xxxx' )
C
C
C        1.0000     0.5000     0.3333     0.2500     0.2000     0.1667
C        0.1429     0.1250     0.1111     0.1000     0.0909     0.0833
C        0.0769    -1.282E+03  0.0667     0.0625     0.0588     0.0556
C        0.0526     0.0500     0.0476     0.0455     0.0435     0.0417
C        0.0400     0.0385     0.0370     0.0357     0.0345     0.0333
C        0.0323     0.0313     0.0303     0.0294     0.0286     0.0278
C
C     The routine assumes you've got room for 132 characters per
C     line on output.  If the matrix runs over that, the output is
C     truncated on the right and and star '*' placed in the 132nd
C     column.
C
C$ Examples
C
C     Suppose you would like to print the contents of a matrix.
C     The following block of code will do this for you.
C
C     ROWS = 3
C     COLS = 3
C
C     CALL PMATRIX ( MAT, ROWS, COLS, '-##.######' )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    Testutil Version 1.1.0, 9-JAN-1997 (WLT)
C
C        Replaced calls to DPFMT_1 with DPFMT
C
C-    SPICELIB Version 1.0.0, 1-MAY-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Print a matrix
C
C-&
C
C     Spicelib functions
C
      INTEGER               NBLEN
      INTEGER               RTRIM
 
C
C     Local Variables.
C
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )
 
      CHARACTER*(LNSIZE)    TEMP
      CHARACTER*(LNSIZE)    LINE
 
      INTEGER               SMSIZE
      PARAMETER           ( SMSIZE = 8 )
 
      CHARACTER*(SMSIZE)    TOOBIG
 
      INTEGER               WIDTH
      INTEGER               C
      INTEGER               I
      INTEGER               J
      INTEGER               SPACE
      INTEGER               SIGDIG
 
      LOGICAL               STARS
 
      WIDTH  = MAX( 1, NBLEN(FMT) )
      SPACE  = MIN( 4, MAX(1, (((80 - WIDTH)/COLS) - WIDTH ) ) )
      SIGDIG = MAX( 1, WIDTH - 6 + SPACE - 1  )
 
 
 
      IF ( WIDTH - 6 + SPACE - 1 .LT. 1 ) THEN
         STARS  = .TRUE.
         TOOBIG = '********'
      ELSE
         STARS  = .FALSE.
      END IF
 
      DO I = 1, ROWS
 
         C = 1
 
         DO J = 1, COLS
 
            CALL DPFMT ( MAT(I,J), FMT, TEMP )
 
            IF ( RTRIM ( TEMP ) .GT. WIDTH ) THEN
 
               IF (STARS) THEN
                  TEMP          = ' '
                  TEMP(1:WIDTH) = TOOBIG
               ELSE
                  CALL DPSTRF ( MAT(I,J), SIGDIG, 'E', TEMP )
               END IF
 
            END IF
 
            IF ( C .LT. LNSIZE ) THEN
 
               LINE(C:) = TEMP
 
               IF ( C + WIDTH .GT. LNSIZE ) THEN
                  LINE(LNSIZE:LNSIZE) = '*'
               END IF
 
            ELSE IF ( C .EQ. LNSIZE ) THEN
 
               LINE(C:C) = '*'
 
            END IF
 
            C = C + WIDTH + SPACE
 
         END DO
 
         CALL TOSTDO ( LINE(1:RTRIM(LINE)) )
 
      END DO
 
      RETURN
      END
 
 
 
