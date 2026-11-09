!$Procedure      MPRINT ( Matrix Print )

      SUBROUTINE MPRINT ( MAT, ROWS, COLS, FMT )

! $ Abstract
!
!     Format the contents of a d.p. matrix for printing.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               ROWS
      INTEGER               COLS
      DOUBLE PRECISION      MAT ( ROWS, COLS )

      CHARACTER*(*)         FMT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAT        I   A d.p. matrix.
!     ROWS       I   The number of rows in the matrix.
!     COLS       I   The number of columns in the matrix.
!     FMT        I   A picture of the format for each in the matrix.
!     LINES      O   An arrary of text lines
!
! $ Detailed_Input
!
!     MAT        A matrix of ROWS rows and COLS columns that should
!                be converted into a suitable text format.
!
!     ROWS       The number of rows in MAT
!
!     COLS       The number of columns in MAT.
!
!     FMT        The format to use when creating entries.
!
! $ Detailed_Output
!
!     LINES      Array of string which when printed will present the
!                matrix in commonly used format for matrices.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine is a utility for those times when you simply want
!     to print a matrix.
!
!     Suppose MATRIX contains the inverses of the integers 1
!     through 9.  To print this matrix make the following call
!
!     CALL MPRINT ( MATRIX,  3, 3, '-x.xxxx' )
!
!     You'll get the following output.
!     (you need to view this with a fixed pitch font or things
!      may not look so great).
!
!        1.0000     0.5000     0.3333
!        0.2500     0.2000     0.1667
!        0.1429     0.1250     0.1111
!
!     If the (3,2) entry gets corrupted with a value like
!     -1282.291 you'll get the following.
!
!        1.0000     0.5000     0.3333
!        0.2500     0.2000     0.1667
!        0.1429    -1.282E+03  0.1111
!
!     Notice how the "out of range" guy sticks out calling
!     attention to himself.  To print a 6x6 matrix just change
!     the 3 to 6 in the calling sequence.
!
!     CALL MPRINT ( MATRIX,  6, 6, '-x.xxxx' )
!
!
!        1.0000     0.5000     0.3333     0.2500     0.2000     0.1667
!        0.1429     0.1250     0.1111     0.1000     0.0909     0.0833
!        0.0769    -1.282E+03  0.0667     0.0625     0.0588     0.0556
!        0.0526     0.0500     0.0476     0.0455     0.0435     0.0417
!        0.0400     0.0385     0.0370     0.0357     0.0345     0.0333
!        0.0323     0.0313     0.0303     0.0294     0.0286     0.0278
!
!     The routine assumes you've got room for 132 characters per
!     line on output.  If the matrix runs over that, the output is
!     truncated on the right and and star '*' placed in the 132nd
!     column.
!
! $ Examples
!
!     Suppose you would like to print the contents of a matrix.
!     The following block of code will do this for you.
!
!     ROWS = 3
!     COLS = 3
!
!     CALL PMATRIX ( MAT, ROWS, COLS, '-##.######' )
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Literature_References
!
!       None.
!
! $ Version
!
!-    Testutil Version 1.1.0, 9-JAN-1997 (WLT)
!
!        Replaced calls to DPFMT_1 with DPFMT
!
!-    SPICELIB Version 1.0.0, 1-MAY-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Print a matrix
!
!-&
!
!     Spicelib functions
!
      INTEGER               NBLEN
      INTEGER               RTRIM

!
!     Local Variables.
!

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



