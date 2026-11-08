!$Procedure WNCOND ( Contract the intervals of a DP window )

      SUBROUTINE WNCOND ( LEFT, RIGHT, WINDOW )

! $ Abstract
!
!     Contract each of the intervals of a double precision window.
!
! $ Required_Reading
!
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WINDOW  ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LEFT       I   Amount added to each left endpoint.
!     RIGHT      I   Amount subtracted from each right endpoint.
!     WINDOW    I-O  Window to be contracted.
!
! $ Detailed_Input
!
!     LEFT     is the amount to be added to the left endpoint of each
!              interval in the input window. The amount LEFT is signed.
!
!     RIGHT    is the amount to be subtracted from the right endpoint of
!              each interval in the window. The amount RIGHT is signed.
!
!     WINDOW   on input, is a window containing zero or more
!              intervals.
!
! $ Detailed_Output
!
!     WINDOW   on output, is the original window with each of its
!              intervals contracted by LEFT units on the left and
!              RIGHT units on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The cardinality of the input WINDOW must be even. Left
!         endpoints of stored intervals must be strictly greater than
!         preceding right endpoints. Right endpoints must be greater
!         than or equal to corresponding left endpoints. Invalid window
!         data are not diagnosed by this routine and may lead to
!         unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine contracts (shortens) each of the intervals in
!     the input window. The adjustments are not necessarily symmetric.
!     That is, LEFT units are added to the left endpoint of each
!     interval, and RIGHT units are subtracted from the right endpoint
!     of each interval, where LEFT and RIGHT may be different.
!
!     Intervals are dropped when they are contracted by amounts
!     greater than their measures.
!
! $ Examples
!
!     Let WINDOW contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
!
!     Then the following series of calls
!
!           CALL WNCOND (  2,  1, WINDOW )              (1)
!           CALL WNCOND ( -2,  2, WINDOW )              (2)
!           CALL WNCOND ( -2, -1, WINDOW )              (3)
!
!     produces the following series of windows
!
!           [ 9, 10 ]  [ 25, 26 ]                       (1)
!           [ 7,  8 ]  [ 23, 24 ]                       (2)
!           [ 5,  9 ]  [ 21, 25 ]                       (3)
!
!     Note that intervals may be "contracted" by negative amounts.
!     In the example above, the second call shifts each interval to
!     the left, while the third call undoes the effect of the first
!     call (without restoring the destroyed intervals).
!
!     Note also that the third call is exactly equivalent to the
!     call
!
!           CALL WNEXPD ( 2, 1, WINDOW )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added entry #1 in $Exceptions section. Extended LEFT and RIGHT
!        description in $Detailed_Input.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     contract the intervals of a d.p. window
!
!-&


!
!     Spicelib functions
!
      LOGICAL               RETURN



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNCOND' )
      END IF


!
!     This is just negative expansion.
!
      CALL WNEXPD ( -LEFT, -RIGHT, WINDOW )


      CALL CHKOUT ( 'WNCOND' )
      RETURN
      END
