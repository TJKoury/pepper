!$Procedure F_DVOPS (Family of vector operations derivative tests)

      SUBROUTINE F_DVOPS ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine performs rudimentary tests on a collection
!     of derivative of vector operation
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
!     None.
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
!-    TSPICE Version 2.0.0, 27-SEP-2005 (NJB)
!
! $ Version
!
!-   TSPICE Version 2.1.0, 03-MAY-2010 (EDW)
!
!       Added test family for DVNORM.
!
!-   TSPICE Version 2.0.0, 27-SEP-2005 (NJB)
!
!       Updated to remove non-standard use of duplicate arguments
!       in VSCL, VPERP, and UNORM.
!
!-&

! $ Index_Entries
!
!     test DVDOT, DVCRSS, DVHAT, DUCRSS, DVNORM
!
!-&

!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-13 )

!
!     Local Variables
!
      DOUBLE PRECISION      CMP
      DOUBLE PRECISION      EXP
      DOUBLE PRECISION      LENGTH
      DOUBLE PRECISION      MAG_LOG
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      LLIM
      DOUBLE PRECISION      ULIM
      DOUBLE PRECISION      DVMAG
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      X1     ( 3 )
      DOUBLE PRECISION      PRT1   ( 3 )
      DOUBLE PRECISION      PRT2   ( 3 )
      DOUBLE PRECISION      EXPV   ( 6 )
      DOUBLE PRECISION      S1     ( 6 )
      DOUBLE PRECISION      S2     ( 6 )
      DOUBLE PRECISION      SOUT   ( 6 )

      CHARACTER*(64)        TXT

      INTEGER               I
      INTEGER               SEED1

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DVOPS' )

      S1( 1 ) =  1.0D0
      S1( 2 ) =  2.0D0
      S1( 3 ) =  1.0D0
      S1( 4 ) =  1.0D0
      S1( 5 ) = -1.0D0
      S1( 6 ) =  3.0D0

      S2( 1 ) = 2.0D0
      S2( 2 ) = 3.0D0
      S2( 3 ) = 4.0D0
      S2( 4 ) = 1.0D0
      S2( 5 ) = 2.0D0
      S2( 6 ) = 3.0D0

!
!     Case 1
!
      CALL TCASE ( 'Test DVDOT.' )


         EXP =  S1(1) * S2(4)   +  S1(4)*S2(1) &
             +  S1(2) * S2(5)   +  S1(5)*S2(2) &
             +  S1(3) * S2(6)   +  S1(6)*S2(3)

         CMP = DVDOT( S1, S2 )

         CALL CHCKXC ( .FALSE.,      ' ',             OK )
         CALL CHCKSD ( 'DVDOT', CMP, '=', EXP, 0.0D0, OK )


!
!     Case 2
!
      CALL TCASE ( 'Test DVCRSS.' )


         CALL VCRSS ( S1(1),   S2  (1), EXPV(1) )
         CALL VCRSS ( S1(1),   S2  (4), PRT1(1) )
         CALL VCRSS ( S1(4),   S2  (1), PRT2(1) )
         CALL VADD  ( PRT1(1), PRT2(1), EXPV(4) )

         CALL CHCKXC ( .FALSE., ' ',  OK   )
         CALL DVCRSS (  S1,      S2,  SOUT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )


!
!     Case 3
!
      CALL TCASE ( 'Test DVHAT with a state having non-zero position.' )

         CALL UNORM  ( S1,           EXPV,    LENGTH  )
         CALL VPERP  ( S1(4),        EXPV,    EXPV(4) )
         CALL VSCLIP ( 1.0D0/LENGTH,          EXPV(4) )

         CALL DVHAT ( S1,                    SOUT    )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )


!
!     Case 4
!
      CALL TCASE ( 'Test DVHAT with a state having zero position.' )

         S1(1)   = 0.0D0
         S1(2)   = 0.0D0
         S1(3)   = 0.0D0

         EXPV(1) = 0.0D0
         EXPV(2) = 0.0D0
         EXPV(3) = 0.0D0
         EXPV(4) = S1(4)
         EXPV(5) = S1(5)
         EXPV(6) = S1(6)

         CALL DVHAT  ( S1,            SOUT )

         CALL CHCKXC ( .FALSE., ' ',  OK   )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )


!
!     Case 5
!
      CALL TCASE ( 'Test DUCRSS' )

         S1( 1 ) =  1.0D0
         S1( 2 ) =  2.0D0
         S1( 3 ) =  1.0D0

         CALL VCRSS ( S1(1),   S2(1),        EXPV(1) )
         CALL VCRSS ( S1(1),   S2(4),        PRT1(1) )
         CALL VCRSS ( S1(4),   S2(1),        PRT2(1) )
         CALL VADD  ( PRT1(1), PRT2(1),      EXPV(4) )
         CALL UNORM ( EXPV,    VTEMP,        LENGTH  )
         CALL VEQU  ( VTEMP,                 EXPV    )
         CALL VPERP ( EXPV(4),      EXPV(1), VTEMP   )
         CALL VSCL  ( 1.0D0/LENGTH, VTEMP,   EXPV(4) )

         CALL DUCRSS ( S1, S2,  SOUT )

         CALL CHCKXC ( .FALSE., ' ',                      OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )



!
!     Case 6
!
!     Test DVNORM, parallel, anti-parallel, zero vector cases.
!

      SEED1 = -82763653
      LLIM  = -2.D0
      ULIM  = 25.D0

      X1(1) = 1.D0
      X1(2) = DSQRT(2.D0)
      X1(3) = DSQRT(3.D0)

      TXT = 'DVNORM ZERO VECTOR CASE'
      CALL TCASE( TXT )

      S1( 1 ) =  0.D0
      S1( 2 ) =  0.D0
      S1( 3 ) =  0.D0
      S1( 4 ) =  X1(1)
      S1( 5 ) =  X1(2)
      S1( 6 ) =  X1(3)

      DVMAG = DVNORM(S1)

      CALL CHCKSD ( 'DVNORM', DVMAG, '=', 0.D0, TIGHT, OK )

!
!     Create a set of 6x1 arrays with, the first three components
!     describing a vector with a random magnitude.
!
!
!     Note, 5000 is an arbitrary value.
!
      DO I = 1, 5000

         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM parallel %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

         S1( 1 ) =  X1(1) * MAG
         S1( 2 ) =  X1(2) * MAG
         S1( 3 ) =  X1(3) * MAG
         S1( 4 ) =  X1(1)
         S1( 5 ) =  X1(2)
         S1( 6 ) =  X1(3)

         DVMAG = DVNORM(S1)

         CALL CHCKSD ( TXT, DVMAG, '~', VNORM(X1), TIGHT, OK )


         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM anti-parallel %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

         S1( 1 ) =  X1(1) * MAG
         S1( 2 ) =  X1(2) * MAG
         S1( 3 ) =  X1(3) * MAG
         S1( 4 ) =  -X1(1)
         S1( 5 ) =  -X1(2)
         S1( 6 ) =  -X1(3)

         DVMAG = DVNORM(S1)

         CALL CHCKSD ( TXT, DVMAG, '~', -VNORM(X1), TIGHT, OK )


         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM orthogonal %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

!
!        Create a 6x1 array with the property the dot product of the
!        first three components with the second three components
!        has value zero.
!
         S2( 1 ) =  X1(1) * MAG
         S2( 2 ) =  X1(2) * MAG
         S2( 3 ) =  X1(3) * MAG
         S2( 4 ) =  1.D0/S2(1)
         S2( 5 ) =  1.D0/S2(2)
         S2( 6 ) = -2.D0/S2(3)

         DVMAG = DVNORM(S2)

         CALL CHCKSD ( TXT, DVMAG, '~', 0.D0, TIGHT, OK )


      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END
