!$Procedure F_ZZPLAT ( Test Fortran Intrinsics )

      SUBROUTINE F_ZZPLAT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Test family to verify that ZZPLATFM returns proper values.
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

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine checks to make certain that values returned from
!     ZZPLATFM are correct.  Not all values are checked.
!
! $ Examples
!
!     None.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 29-NOV-2021 (BVS)
!
!        Added TOUCH* calls for *EQUIV variables to make sure that
!        they don't get out-optimized by compilers like IFORT on
!        Linux.
!
!-    TSPICE Version 1.0.0, 18-DEC-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!



!
!     Local Variables
!
      CHARACTER*(8)         BFFID

      DOUBLE PRECISION      DEQUIV

      INTEGER               IEQUIV ( 2 )

!
!     EQUIVALENCE statements
!
      EQUIVALENCE ( DEQUIV, IEQUIV )

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_ZZPLAT' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Binary File Format Consistency Check' )

!
!     Fetch the binary file format ID string.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', BFFID )

!
!     "Touch" IEQUIV to make sure it does not get optimized out,
!     as is the case with IFORT on Linux.
!
      IEQUIV(1) = TOUCHI ( IEQUIV(1) )
      IEQUIV(2) = TOUCHI ( IEQUIV(2) )

!
!     Set the test value. "Touch" it as well.
!
      DEQUIV = 1.0D0

      DEQUIV = TOUCHD ( DEQUIV )

!
!     Branch based on the value returned; check equivalenced
!     integers for the appropriate values.
!
      IF ( EQSTR ( BFFID, 'BIG-IEEE' ) ) THEN

         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 1072693248, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,          0, OK )

      ELSE IF ( EQSTR ( BFFID, 'LTL-IEEE' ) ) THEN

         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 0,          0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 1072693248, 0, OK )

      ELSE IF ( EQSTR ( BFFID, 'VAX-GFLT' ) ) THEN

         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 16400, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,     0, OK )

      ELSE IF ( EQSTR ( BFFID, 'VAX-DFLT' ) ) THEN

         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 16512, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,     0, OK )
      ELSE
         CALL TSTMSG ( '#', 'This test does not support this format.' )
         CALL CHCKSI ( 'ERROR', 0, '=', 1, 0, OK )
      END IF


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
