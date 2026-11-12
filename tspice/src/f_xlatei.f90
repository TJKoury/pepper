!$Procedure F_XLATEI ( ZZXLATEI Test Family )

      SUBROUTINE F_XLATEI ( OK )
      use pepper_legacy
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZXLATEI
!     routine.
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
!     This routine exercises ZZXLATEI's logic.
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
!-    TSPICE Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    TSPICE Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    TSPICE Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    TSPICE Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    TSPICE Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    TSPICE Version 1.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!

!
!     Local Parameters
!
      INTEGER               NUMOUT
      PARAMETER           ( NUMOUT = 256 )

!
!     Local Variables
!
      CHARACTER*(1024)      INPUT

      INTEGER               COMPAR ( NUMOUT )
      INTEGER               I
      INTEGER               J
      INTEGER               INBFF
      INTEGER               OUTPUT ( NUMOUT )
      INTEGER               SPACE

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_XLATEI' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INBFF out of range error' )

!
!     Setup the inputs and outputs for checking the lower bound.
!
      INBFF = 0
      SPACE = 10

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

!
!     Invoke the module.
!
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Check to see that OUTPUT was unmodified.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
!     Setup the inputs and outputs for checking the upper bound.
!
      INBFF = NUMBFF + 1
      SPACE = 10

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

!
!     Invoke the module.
!
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Check to see that OUTPUT was unmodified.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- Bad Byte Count BIG-IEEE INPUT' )

!
!     Setup the inputs and outputs.
!
      INBFF = BIGI3E

      SPACE = 10

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

!
!     Invoke the module. Restrict INPUT to 1:13 range, since
!     BIG-IEEE integers come in 4-byte packages.
!
      CALL ZZXLATEI ( INBFF, INPUT(1:13), SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Check to see that OUTPUT was unmodified.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- Not enough SPACE to store OUTPUT' )

!
!     Setup the inputs and outputs.
!
      INBFF = BIGI3E

      SPACE = 10

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

!
!     Invoke the module. Restrict INPUT to 1:13 range, since
!     BIG-IEEE integers come in 4-byte packages.
!
      CALL ZZXLATEI ( INBFF, INPUT(1:80), SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Check to see that OUTPUT was unmodified.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- Unsupported INBFF' )

!
!     Setup the inputs and outputs.
!
      INBFF = VAXDFL
      SPACE = 10

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

!
!     Invoke the module. Restrict INPUT to 1:13 range, since
!     BIG-IEEE integers come in 4-byte packages.
!
      CALL ZZXLATEI ( INBFF, INPUT(1:40), SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )

!
!     Check to see that OUTPUT was unmodified.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Extreme Integer Values' )

!
!     Setup the inputs and outputs.
!
      INBFF = BIGI3E

      SPACE = 2

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

      COMPAR(1) = INTMAX()
      COMPAR(2) = INTMIN()

!
!     Get the smallest possible integer bit pattern in memory,
!     even if INTMIN does not cooperate.
!
      IF ( COMPAR(2) .EQ. -2147483647 ) THEN
         COMPAR(2) = COMPAR(2) - 1
      END IF

!
!     Prepare the INPUT buffer.
!
      CALL T_XLTFWI ( COMPAR, 2, INBFF, INPUT(1:8) )

!
!     Invoke the module.
!
      CALL ZZXLATEI ( INBFF, INPUT(1:8), SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check to see that OUTPUT is reasonable.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Integral Sequence' )

!
!     Setup the inputs and outputs.
!
      INBFF = BIGI3E

      SPACE = NUMOUT

      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO

      DO I = 1, 31
         COMPAR(I) = 2**(I-1)
      END DO

      DO I = 32, 62
         COMPAR(I) = -2**(I-32)
      END DO

      DO I = 62, NUMOUT
         COMPAR(I) = 1000*I
      END DO

!
!     Prepare the INPUT buffer.
!
      CALL T_XLTFWI ( COMPAR, NUMOUT, INBFF, INPUT )

!
!     Invoke the module.
!
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )

!
!     Check for an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check to see that OUTPUT is as expected.
!
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Byte Value Cycle' )

!
!     Setup the inputs and outputs for the LSB byte case.
!
      INBFF = BIGI3E

      SPACE = 256

      DO J = 1, 4

         DO I = 1, SPACE
            OUTPUT(I) = 0
            COMPAR(I) = 0
         END DO

         DO I = 1, SPACE
            COMPAR(I) = ISHFT ( (I-1), 8*(J-1) )
         END DO

!
!        Prepare the INPUT buffer.
!
         CALL T_XLTFWI ( COMPAR, SPACE, INBFF, INPUT(1:4*SPACE) )

!
!        Invoke the module.
!
         CALL ZZXLATEI ( INBFF, INPUT(1:4*SPACE), SPACE, OUTPUT )

!
!        Check for an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check to see that OUTPUT was unmodified.
!
         CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, SPACE, OK )

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
