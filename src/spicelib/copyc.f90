!$Procedure COPYC ( Copy a character cell )

      SUBROUTINE COPYC ( CELL, COPY )

! $ Abstract
!
!     Copy the contents of a character cell to another cell.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)      CELL     ( LBCELL:* )
      CHARACTER*(*)      COPY     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CELL       I   Cell to be copied.
!     COPY       O   New cell.
!
! $ Detailed_Input
!
!     CELL     is a cell.
!
! $ Detailed_Output
!
!     COPY     is a cell which contains the same elements as the
!              input cell, in the same order. If the size (maximum
!              cardinality) of the output cell is smaller than
!              the cardinality of the input cell, then only as many
!              items as will fit in the output cell are copied,
!              and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output cell in not large enough to hold the elements
!         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
!
!     2)  If length of the elements of the output cell is less than the
!         length of the elements of the input cell, the error
!         SPICE(ELEMENTSTOOSHORT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The copy routines (COPYC, COPYD, and COPYI) are used primarily
!     to manipulate working cells, since many routines that use cells
!     (binary set routines, for instance) do not allow cells to be
!     combined or manipulated in place.
!
! $ Examples
!
!     In the following example, COPYC is used to copy the result
!     of the union of two sets (ordered cells) from a temporary
!     working set back into the one of the original set.
!
!           CALL UNIONC ( BODIES, PLANETS, TEMP )
!           CALL COPYC  ( TEMP,   BODIES       )
!
!     If the size of the temporary cell is greater than the size
!     of the original set, the function FAILED should be checked to be
!     sure that no overflow occurred. If BODIES is at least as
!     large as TEMP, no such check is necessary.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     copy a character cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 09-JAN-1989 (NJB)
!
!        Error signaled if output set elements are not long enough.
!        Length must be at least max of lengths of input elements.
!        Also, calling protocol for EXCESS has been changed. And,
!        elements LBCELL through -2 of control area are now copied to
!        the output cell.
!
!-&


!
!     SPICELIB functions
!
      INTEGER          SIZEC
      INTEGER          CARDC
      LOGICAL          RETURN
      INTEGER          LASTPC
!
!     Local variables
!
      INTEGER          SIZE
      INTEGER          CARD
      INTEGER          MOVED
      INTEGER          I
      LOGICAL          TRUNC
      INTEGER          REQLEN


!
!     Set up the error processing.
!
      IF ( RETURN() ) RETURN
      CALL CHKIN ( 'COPYC' )

!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      CARD = CARDC ( CELL )
      SIZE = SIZEC ( COPY )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.  Copy the control area too, except for the
!     the size and cardinality values.  Truncation indicator
!     starts at .FALSE.
!
      TRUNC  = .FALSE.
      REQLEN = 0

      MOVED  =  MIN ( CARD, SIZE )

      DO I = 1, MOVED
         COPY(I) = CELL(I)
!
!        Test for truncation:
!
         IF ( COPY(I) .NE. CELL(I) ) THEN
            TRUNC   = .TRUE.
            REQLEN  = MAX ( REQLEN, LASTPC( CELL(I) ) )
         END IF

      END DO

      DO I = LBCELL, -2
         COPY(I) = CELL(I)
!
!        Test for truncation:
!
         IF ( COPY(I) .NE. CELL(I) ) THEN
            TRUNC   = .TRUE.
            REQLEN  = MAX ( REQLEN, LASTPC( CELL(I) ) )
         END IF

      END DO

!
!     Set the cardinality of the output cell.
!
      CALL SCARDC ( MOVED, COPY )

!
!     We've got an error if the output cell was too small.
!
      IF ( SIZE .LT. CARD ) THEN
         CALL EXCESS (  CARD - SIZE,  'cell' )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )
         CALL CHKOUT ( 'COPYC' )
         RETURN
      END IF

!
!     We also have an error if the output set elements are not long
!     enough.
!
      IF (  TRUNC  ) THEN

         CALL SETMSG ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         CALL ERRINT ( '#', LEN( COPY(LBCELL) )  )
         CALL ERRINT ( '#', REQLEN  )
         CALL SIGERR ( 'SPICE(ELEMENTSTOOSHORT)' )

         CALL CHKOUT ( 'COPYC' )
         RETURN

      END IF
      CALL CHKOUT ( 'COPYC' )
      RETURN
      END
