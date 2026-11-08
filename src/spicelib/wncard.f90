!$Procedure WNCARD ( Cardinality of a double precision window )

      integer function wncard ( window )

! $ Abstract
!
!     Return the cardinality (number of intervals) of a double
!     precision window.
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

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      double precision      window  ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WINDOW     I   Input window.
!
!     The function returns the cardinality of the input window.
!
! $ Detailed_Input
!
!     WINDOW   is a window containing zero or more intervals.
!
! $ Detailed_Output
!
!     The function returns the cardinality of (number of intervals in)
!     the input window.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of elements in WINDOW is not even,
!         the error SPICE(INVALIDSIZE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The window cardinality (WNCARD) function simply wraps a CARD call
!     then divides the result by 2. A common error when using the SPICE
!     windows function is to use the CARDD value as the number of
!     window intervals rather than the CARDD/2 value.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) The following code example demonstrates how to insert an
!        interval into an existing double precision SPICE window, and
!        how to loop over all its intervals to extract their left and
!        right points.
!
!
!        Example code begins here.
!
!
!              PROGRAM WNCARD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               WNCARD
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               WNSIZE
!              PARAMETER           ( WNSIZE = 10 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
!              DOUBLE PRECISION      LEFT
!              DOUBLE PRECISION      RIGHT
!
!              INTEGER               I
!
!        C
!        C     Validate the window with size WNSIZE and zero elements.
!        C
!              CALL WNVALD( WNSIZE, 0, WINDOW )
!
!        C
!        C     Insert the intervals
!        C
!        C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!        C
!        C     into WINDOW.
!        C
!              CALL WNINSD(  1.D0,  3.D0, WINDOW )
!              CALL WNINSD(  7.D0, 11.D0, WINDOW )
!              CALL WNINSD( 23.D0, 27.D0, WINDOW )
!
!        C
!        C     Loop over the number of intervals in WINDOW, output
!        C     the LEFT and RIGHT endpoints for each interval.
!        C
!              DO I=1, WNCARD(WINDOW)
!
!                 CALL WNFETD( WINDOW, I, LEFT, RIGHT )
!
!                 WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
!             .                  ' [', LEFT,',',  RIGHT, ']'
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Interval 1 [   1.000,   3.000]
!        Interval 2 [   7.000,  11.000]
!        Interval 3 [  23.000,  27.000]
!
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
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Updated to remove unnecessary lines of code in the
!        Standard SPICE error handling CHKIN statements.
!
!        Edited the header to comply to NAIF standard. Created complete
!        code example from code fragment and added example's problem
!        statement.
!
!-    SPICELIB Version 1.0.1, 24-APR-2010 (EDW)
!
!        Minor edit to code comments eliminating typo.
!
!-    SPICELIB Version 1.0.0, 10-AUG-2007 (EDW)
!
!-&


! $ Index_Entries
!
!     cardinality of a d.p. window
!
!-&


!
!     SPICELIB functions
!
      logical               return
      logical               even

      integer               cardd

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         wncard = 0
         return
      end if

      call chkin ( 'WNCARD' )


      wncard = cardd(window)

!
!     Confirm the cardinality as an even integer.
!
      if( .not. even(wncard) ) then

         call setmsg ( 'Invalid window size, a window should ' &
              // 'have an even number of elements. The size was #.')
         call errint ( '#', wncard )
         call sigerr ( 'SPICE(INVALIDSIZE)' )
         call chkout ( 'WNCARD' )
         wncard = 0
         return

      end if

!
!     Set return value. Cardinality in a SPICE window sense
!     means the number of intervals, half the cell
!     cardinality value.
!
      wncard = wncard/2

      call chkout ( 'WNCARD' )
      return
      end
