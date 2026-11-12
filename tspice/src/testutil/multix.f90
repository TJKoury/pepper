!$Procedure MULTIX ( Multiple dimensional index )

      SUBROUTINE MULTIX ( BASIDX, N, DIMS, OFFSET, COORDS )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Map an offset into a one-dimensional array to the corresponding
!     coordinates of a specified n-dimensional array. The
!     lowest-indexed coordinate varies fastest.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     UTILITY
!
! $ Declarations

      INTEGER               BASIDX
      INTEGER               N
      INTEGER               DIMS   ( N )
      INTEGER               OFFSET
      INTEGER               COORDS ( N )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     BASIDX     I   Start index of array.
!     N          I   Number of array dimensions.
!     DIMS       I   Array dimensions.
!     OFFSET     I   Offset from array base.
!     COORDS     O   N-tuple of coordinates corresponding to offset.
!
! $ Detailed_Input
!
!     BASIDX         is the base value used for array indexing.
!                    OFFSET should be in the range
!
!                       BASIDX : SIZE-1
!
!                   where size is the product of the elements of
!                   DIMS.
!
!                   For C-style indexing, BASIDX should be set to 0.
!                   For default Fortran-style indexing, BASIDX should
!                   be set to 1.
!
!     N             is the dimension of the array DIMS.
!
!     DIMS          is an array whose components define the shape and
!                   size of a multi-dimensional array; call this array
!                   ARR.
!
!     OFFSET        is the index in the array ARR defined by DIMS of an
!                   element of interest, when the array is viewed as a
!                   one-dimensional array:  in this view, the
!                   multidimensional array ARR is considered to have
!                   column-major organization:
!
!                      The first DIMS(1) elements of the array are
!
!                        ARR( BASIDX,           BASIDX, ... , BASIDX )
!                               .
!                               .
!                               .
!                        ARR( BASIDX+DIMS(1)-1, BASIDX, ... , BASIDX )
!
!                      The next DIMS(1) elements of the array are
!
!                        ARR( BASIDX,           BASIDX+1, ... , BASIDX )
!                               .
!                               .
!                               .
!                        ARR( BASIDX+DIMS(1)-1, BASIDX+1, ... , BASIDX )
!
!                      and so on.
! $ Detailed_Output
!
!     COORDS        is an N-tuple containing the coordinates of the
!                   array element located at OFFSET positions from
!                   BASIDX, when the column-major array ARR defined by
!                   DIMS is viewed as a one-dimensional array.
!
!                   The Ith component of COORDS ranges from BASIDX to
!                   DIMS(I)-1.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!    1) If N is less than 1, this routine will fail in a system-
!        dependent manner.
!
!    2) If OFFSET is less than BASIDX or greater than BASIDX+P-1
!       where
!
!          P = DIMS(1) * DIMS(2) * ... DIMS(N),
!
!       the error SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine enables traversal of a many-dimensional cartesian
!     product using a single loop.  Suppose the cartesian product C of
!     the sets S1, ..., SN is defined by:
!
!        C = S1  x  S2  x ... x SN
!
!     and that each set SI has cardinality DIMS(I).
!
!     Then if we let P be the product
!
!        P = DIMS(1) * DIMS(2) * ... * DIMS(N)
!
!     and we associate with each N-tuple of integers ( x1, ..., xN ),
!     the element of C
!
!        ( S1(x1), S2(x2), ..., SN(xn) )
!
!     where x1, ... xN are in the range 1:N, then this routine
!     makes it simple to map the integers 1, ..., P onto C.
!
!     In Fortran, we achieve this with the loop
!
!        DO I = 1, P
!
!           CALL MULTIX ( 1, N, DIMS, I, COORDS )
!
!           [  Do something with COORDS.
!
!              The Ith element of C is
!
!              ( S1(COORDS(1)), S2(COORDS(2)), ..., SN(COORDS(N)) )
!           ]
!
!        END DO
!
!
!     See the TSPICE test family F_DYN01 for an example of this
!     technique.
!
! $ Examples
!
!     1) Let
!
!           BASIDX = 1
!           N      = 2
!           DIMS   = ( 4, 2 )
!
!        The following array shows the mapping of OFFSET to elements
!        of ARR:
!
!              1 5
!              2 6
!              3 7
!              4 8
!
!        That is to say,
!
!          OFFSET = 1   yields   COORDS = ( 1, 1 )
!          OFFSET = 2   yields   COORDS = ( 2, 1 )
!                         .
!                         .
!                         .
!          OFFSET = 8   yields   COORDS = ( 4, 2 )
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 21-DEC-2004 (NJB)
!
!-&

! $ Index_Entries
!
!       Compute multidimensional array index from offset
!
!-&


!
!     Local variables
!
      INTEGER               I
      INTEGER               M
      INTEGER               MAXIDX
      INTEGER               Q
      INTEGER               REM


!
!     Use discovery check-in.
!
      IF ( N .LT. 1 ) THEN
!
!        If N is less than 1, this routine may crash before
!        reaching these lines.  In case it doesn't....
!
         CALL CHKIN  ( 'MULTIX'                       )
         CALL SETMSG ( 'N must be at least 1;  N = #' )
         CALL ERRINT ( '#', N                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'MULTIX'                       )
         RETURN

      END IF

      IF ( OFFSET .LT. BASIDX ) THEN

         CALL CHKIN  ( 'MULTIX'                                      )
         CALL SETMSG ( 'OFFSET must not be less than BASIDX, which ' // &
                       'is #;  OFFSET = #'                           )
         CALL ERRINT ( '#', BASIDX                                   )
         CALL ERRINT ( '#', OFFSET                                   )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                      )
         CALL CHKOUT ( 'MULTIX'                                      )
         RETURN

      END IF

!
!     We're going to use the output array COORDS as work space.
!     Later, we'll fill in the output values.
!
!     Compute the products of the first I dimensions, I = 1 to N-1.
!     The Ith product will go into element I+1 of COORDS.
!
      COORDS(1) = 1

      DO I = 2, N

         COORDS(I) = DIMS(I-1) * COORDS(I-1)

      END DO

!
!     Convert to 0-relative indexing.  We assume OFFSET ranges from
!     BASIDX to MAXIDX, where MAXIDX is
!
!        <the array size>   -  1  +  BASIDX.
!
      M = OFFSET - BASIDX

!
!     At this point we can check to make sure OFFSET is not too large.
!
      MAXIDX =  ( COORDS(N) * DIMS(N) )  -  1  +  BASIDX

      IF ( OFFSET .GT. MAXIDX ) THEN

         CALL CHKIN  ( 'MULTIX'                                )
         CALL SETMSG ( 'OFFSET must not exceed #;  OFFSET = #' )
         CALL ERRINT ( '#', MAXIDX                             )
         CALL ERRINT ( '#', OFFSET                             )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'MULTIX'                                )
         RETURN

      END IF

!
!     Pick off the coordinates, starting with the highest-indexed.
!
      DO I = N, 2, -1
!
!        After the RMAINI call, we no longer need the work space
!        value of COORDS(I).  At this point, we overwrite that
!        value with the output value of COORDS(I).
!
         CALL RMAINI ( M, COORDS(I), Q, REM )

         M         = REM
         COORDS(I) = Q

      END DO

      COORDS(1) = REM

!
!     If the index base is non-zero, map each coordinate back to
!     the original representation.
!
      IF ( BASIDX .NE. 0 ) THEN

         DO I = 1, N
            COORDS(I) = COORDS(I) + BASIDX
         END DO

      END IF

      END


