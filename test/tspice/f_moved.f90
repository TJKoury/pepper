!$Procedure  F_MOVED ( Family of tests for MOVED )

      SUBROUTINE F_MOVED ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests MOVED.
!
!
! $ Required_Reading
!
!     TBD.
!
! $ Keywords
!
!     TBD.
!
! $ Declarations
!
!     TBD.
!
! $ Brief_I/O
!
!     TBD.
!
! $ Detailed_Input
!
!     TBD.
!
! $ Detailed_Output
!
!     TBD.
!
! $ Parameters
!
!     TBD.
!
! $ Exceptions
!
!     TBD.
!
! $ Files
!
!     TBD.
!
! $ Particulars
!
!     This routine has already been used to detect a bug (NDIM = -1
!     resulted in a segmentation fault for the PC-LINUX and NeXT
!     platforms, on which memmove is used to effect the copy).
!
!     So don't laugh.
!
! $ Examples
!
!     TBD.
!
! $ Restrictions
!
!     TBD.
!
! $ Literature_References
!
!     TBD.
!
! $ Author_and_Institution
!
!     TBD.
!
! $ Version
!
!-     Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-     Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-     Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-     Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-     Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-     Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-     Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-     Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-     Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-     Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-     Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-     Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-     Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-     Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-     Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-     Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-     Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-     Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-     Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-     Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-     Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-     Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-     Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-     Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-     Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-     Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    Version 1.0.1 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    Version 1.0.0 12-NOV-2001 (NJB)
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               INTMAX

!
!     Local parameters
!
      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 17 )

      INTEGER               E1
      PARAMETER           ( E1     =  10 )

      INTEGER               E2
      PARAMETER           ( E2     = 100 )

      INTEGER               E3
      PARAMETER           ( E3     = 300 )

!
!     Local variables
!
      DOUBLE PRECISION      BUFF   ( NITEMS )
      DOUBLE PRECISION      BUFF2  ( NITEMS )
      DOUBLE PRECISION      XBUFF  ( NITEMS )

      INTEGER               H
      INTEGER               I
      INTEGER               IC     ( 2 )
      INTEGER               IC2    ( 2 )
      INTEGER               J
      INTEGER               K
      INTEGER               MAXP
      INTEGER               XIC    ( 2 )

!
!     EQUIVALENCE statements
!
      EQUIVALENCE           ( BUFF (1), IC  )
      EQUIVALENCE           ( BUFF2(1), IC2 )

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_MOVED' )

!
!     Case 1.
!
      CALL TCASE ( 'Test MOVED for a selection of standard d.p. '// &
                   'numbers.' )


      BUFF(1)   =    1.D0
      BUFF(2)   =   -1.D0
      BUFF(3)   =    0.D0
      BUFF(4)   =    10.D0**( E1)
      BUFF(5)   =    10.D0**(-E1)
      BUFF(6)   = -( 10.D0**( E1) )
      BUFF(7)   = -( 10.D0**(-E1) )
      BUFF(8)   =    10.D0**( E2)
      BUFF(9)   =    10.D0**(-E2)
      BUFF(10)  = -( 10.D0**( E2) )
      BUFF(11)  = -( 10.D0**( E2) )
      BUFF(12)  =    10.D0**( E3)
      BUFF(13)  =    10.D0**(-E3)
      BUFF(14)  = -( 10.D0**( E3) )
      BUFF(15)  = -( 10.D0**(-E3) )
      BUFF(16)  =    DPMAX()
      BUFF(17)  =    DPMIN()

      XBUFF(1)  =    1.D0
      XBUFF(2)  =   -1.D0
      XBUFF(3)  =    0.D0
      XBUFF(4)  =    10.D0**( E1)
      XBUFF(5)  =    10.D0**(-E1)
      XBUFF(6)  = -( 10.D0**( E1) )
      XBUFF(7)  = -( 10.D0**(-E1) )
      XBUFF(8)  =    10.D0**( E2)
      XBUFF(9)  =    10.D0**(-E2)
      XBUFF(10) = -( 10.D0**( E2) )
      XBUFF(11) = -( 10.D0**( E2) )
      XBUFF(12) =    10.D0**( E3)
      XBUFF(13) =    10.D0**(-E3)
      XBUFF(14) = -( 10.D0**( E3) )
      XBUFF(15) = -( 10.D0**(-E3) )
      XBUFF(16) =    DPMAX()
      XBUFF(17) =    DPMIN()


      CALL MOVED  ( BUFF, NITEMS, BUFF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )

!
!     Case 2.
!
      CALL TCASE ( 'Test MOVED for a case known to be problematic.' )

      IC(1)  = -771000
      IC(2)  = -771900

      XIC(1) = -771000
      XIC(2) = -771900

      CALL MOVED  ( BUFF, 1, BUFF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'IC2', IC2, '=', XIC, 2, OK )

!
!     Case 3.
!
      CALL TCASE ( 'Test MOVED for a selection of bit patterns ' // &
                   'generated by equivalencing 2-integer arrays '// &
                   'with dp. numbers.'                            )

      MAXP = INT (  LOG10( DBLE(INTMAX()) )  )

      DO I = -1, MAXP + 1
!
!        Assign the first element of the IC array.
!
         IF ( I .EQ. -1 ) THEN

            IC(1) = 0

         ELSE IF ( I .EQ. MAXP+1 ) THEN

            IC(1) = INTMAX()

         ELSE

            IC(1) = 10**I

         END IF


         DO J = -1, MAXP + 1
!
!           Assign the second element of the IC array.
!
            IF ( J .EQ. -1 ) THEN

               IC(2) = 0

            ELSE IF ( J .EQ. MAXP+1 ) THEN

               IC(2) = INTMAX()

            ELSE

               IC(2) = 10**J

            END IF

!
!           Try every sign combination.
!
            DO K = 1, 2

               IC(1) = IC(1) * (-1)**K

               DO H = 1, 2

                  IC(2) = IC(2) * (-1)**H
!
!                 Test MOVED on the current artificially constructed
!                 d.p. variable.
!
                  CALL MOVEI  ( IC,   2, XIC   )
                  CALL MOVED  ( BUFF, 1, BUFF2 )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKAI ( 'IC2', IC2, '=', XIC, 2, OK )

               END DO

            END DO
!
!           We're done testing the current pair of integers for each
!           sign combination.
!
         END DO

      END DO

!
!     Case 4.
!
      CALL TCASE ( 'Test MOVED for NDIM = 0 and -1.'  )

      CALL FILLD  ( 1.0D0,    NITEMS, BUFF )

      CALL CLEARD ( NITEMS, BUFF2 )
      CALL CLEARD ( NITEMS, XBUFF )

      CALL MOVED  ( BUFF,   0,      BUFF2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )

      CALL MOVED  ( BUFF,  -1,      BUFF2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
