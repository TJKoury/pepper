!$Procedure SIZED ( Size of a double precision cell )

      integer(ip) function sized ( cell )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the size (maximum cardinality) of a double precision
!     cell.
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

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp)              cell   ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CELL       I   Input cell.
!
!     The function returns the size of the input cell.
!
! $ Detailed_Input
!
!     CELL     is a cell.
!
! $ Detailed_Output
!
!     The function returns the size of (maximum number of elements in)
!     the input cell.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input array has invalid cardinality, the error
!         SPICE(INVALIDCARDINALITY) is signaled. SIZEI returns
!         an unspecified value in this case.
!
!     2)  If the input array has invalid size, the error
!         SPICE(INVALIDSIZE) is signaled. SIZEI returns
!         an unspecified value in this case.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The size (SIZE) functions are typically used in conjunction
!     with the cardinality functions to predict (and subsequently
!     avoid) overflows when manipulating cells. In the following
!     example, SIZEI is used to determine whether the integer cell
!     ORIGINAL can be safely copied into the integer cell SAVE before
!     actually attempting the operation. (If ORIGINAL contains more
!     elements than SAVE is capable of holding, then the operation
!     will fail.)
!
!           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
!              CALL COPYI ( ORIGINAL, SAVE, ERROR )
!
!           ELSE
!            .
!            .
!           END DO
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of the
!        function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     size of a d.p. cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
!
!        Check for valid input cell added. The input cell must
!        have valid size and cardinality values.
!
!-&


!
!     SPICELIB functions
!
      logical               return



      if ( return() ) then
         sized = 0
         return
      else
         call chkin ( 'SIZED' )
      end if

!
!     Set return value, regardless of validity.
!
      sized = int ( cell(-1) )

!
!     Squeal if something is awry.
!

      if (  int( cell(-1) )  <  0  )  then

         call setmsg ( 'Invalid cell size.  The size was #.')
         call errint ( '#', int( cell(-1) )  )
         call sigerr ( 'SPICE(INVALIDSIZE)'  )
         call chkout ( 'SIZED' )
         return

      else if ( int( cell(0) )  <  0  ) then

         call setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call errint ( '#', int ( cell(0) )  )
         call sigerr ( 'SPICE(INVALIDCARDINALITY)'  )
         call chkout ( 'SIZED' )
         return

      else if ( int(cell(0)) > int(cell(-1))  ) then

         call setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call errint ( '#', int(cell( 0))  )
         call errint ( '#', int(cell(-1))  )
         call sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         call chkout ( 'SIZED' )
         return

      end if

      call chkout ( 'SIZED' )
      return
      end
