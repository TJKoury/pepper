!$Procedure SGCTDF ( Generic Segments Create Test DAF )

      SUBROUTINE SGCTDF ( FILE, ND, NI, VALUE, CASES, NCASE, HANDLE )

! $ Abstract
!
!    Create a simple generic segments DAF for testing purposes.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FILE
      INTEGER               ND
      INTEGER               NI
      DOUBLE PRECISION      VALUE
      INTEGER               CASES ( * )
      INTEGER               NCASE
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   name of DAF to create.
!     ND         I   number of double precision components in descr.
!     NI         I   number of integer components in descriptor.
!     VALUE      I   double precision value to fill segments with.
!     CASES      I   array that describes the segment sizes to create.
!     NCASE      I   size of CASES array.
!     HANDLE     O   integer DAF handle.
!
! $ Detailed_Input
!
!     FILE       is the name of the DAF to create for testing.
!
!     ND,        are the number of double precision and integer
!     NI         components in each segment descriptor respectively.
!
!     VALUE      is the double precision value that is to fill each
!                of the segment arrays, except the last value.
!
!     CASES      is an array of integers that prescribe the size of
!                each segment that is to be constructed.
!
!     NCASE      is the number of entries in the CASES array.
!
! $ Detailed_Output
!
!     HANDLE     is the DAF handle associated with the file that was
!                just created.
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
!     Any exceptions signaled will be generated and trapped by
!     routines in the call tree of SGCTDF.
!
! $ Particulars
!
!     This routine creates a DAF file suitable for generic segments
!     testing.  The DAF consists of multiple segments where each segment
!     has a single value encoded in the array, except the final value.
!     The final value indicates the number of elements in the segment
!     array.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    F_SGMETA Version 1.0.0, 16-JUN-1999 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Variables
!
      DOUBLE PRECISION      DESCR ( 125 )
      INTEGER               I
      INTEGER               J

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SGCTDF' )
      END IF

!
!     Initialize DESCR to 0.
!
      DO I = 1, 125

         DESCR ( I ) = 0.0D0

      END DO

!
!     Create the new DAF. First check to make certain no file of name
!     FILE exists. If it does remove it.
!
      CALL KILFIL ( FILE )
      CALL DAFONW ( FILE, &
                    'DAF/TST', &
                    ND, &
                    NI, &
                    'Test DAF F_SGMETA', &
                    0, &
                    HANDLE               )

!
!     Create each of the segments.  Note this is terribly inefficient,
!     but will work just fine for test cases.
!
      DO I = 1, NCASE

!
!        Start a new segment.  Note the contents of the descriptor are
!        irrelevant, since we are only trying to test the functionality
!        of SGMETA.
!
         CALL DAFBNA ( HANDLE, DESCR, 'Test Segment' )

!
!        Now load up the array.  This is where the serious inefficieny
!        comes in.
!
         DO J = 1, ( CASES(I) - 1 )

            CALL DAFADA ( VALUE, 1 )

         END DO

!
!        Place the number of elements into the last entry in the array
!        before ending it's creation.
!
         CALL DAFADA ( DBLE ( CASES(I) ), 1 )

         CALL DAFENA ( )

      END DO

      CALL CHKOUT( 'SGCTDF' )
      RETURN

      END
