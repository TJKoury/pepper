!$Procedure F_ZZGFSPQ ( ZZGFSPQ family tests )

      SUBROUTINE F_ZZGFSPQ ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZGFSPQ
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
!     This routine tests the low-level SPICELIB
!     geometry routine ZZGFSPQ.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.1, 13-DEC-2011 (EDW)
!
!        Deleted extraneous trailing whitespace.
!
!-    TSPICE Version 1.0.0, 08-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      VNORM



!
!     Local parameters
!
      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'gfsep.tls' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

!
!     ATOL is a tolerance value for computing arc sine.
!

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )


!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    REF
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIME0

      DOUBLE PRECISION      XR   ( 2, 4)
      DOUBLE PRECISION      PV1  ( 3 )
      DOUBLE PRECISION      PV2  ( 3 )
      DOUBLE PRECISION      RADA ( 3 )
      DOUBLE PRECISION      RADB ( 3 )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2

      INTEGER               HAN1
      INTEGER               DIM
      INTEGER               TARG1
      INTEGER               TARG2
      INTEGER               OBS
      INTEGER               I
      INTEGER               J
      INTEGER               K


!
!     Save all.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFSPQ' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create an LSK, load using FURNSH.
!
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )


!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the SPK for Nat's Solar System.
!
      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Create a confinement window from ET0 and ET1.
!
      TIME0 = '2000 JAN 1  12:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Case 1
!
      CALL TCASE ( 'Ephemeris data unavailable' )

      TARG1 = -203
      TARG2 = 499
      OBS   = 10
      R1    = 0.D0
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2, &
                     OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )


!
!     Case 2
!
      CALL TCASE ( 'Negative body radius R1 and R2' )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      R1    = -RADA(1)
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2, &
                     OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )

      R1    = 0.D0
      R2    = -RADB(1)

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2, &
                     OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )



!
!     Case 2
!

!
!     Check a simple calculation for all CORR.
!
      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      R1    = 0.D0
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      DO I = 1, NCORR

         CALL TCASE ( CORR(I) )

         CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2, &
                        OBS, CORR(I), REF, VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


!
!     Check a simple calculation for invalid CORR.
!
      DO I = 1, NCORR

         ABCORR = 'Z' // CORR(I)(:LNSIZE-1)

         CALL TCASE ( ABCORR )

         CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2, &
                        OBS, ABCORR, REF, VALUE )

         CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )

      END DO


!
!     Case 3
!

!
!     Run a loop over one orbit of ALPHA at 50 TDB second
!     intervals with each of the aborration corrections.
!     Any occultation event exists for longer than 50 seconds.
!     Compare the value returned from ZZGFSPQ with a direct
!     calculation.
!
      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      REF   = 'J2000'

      XR(1,1) = 0.D0
      XR(1,2) = RADA(1)
      XR(1,3) = 0.D0
      XR(1,4) = RADA(1)
      XR(2,1) = 0.D0
      XR(2,2) = 0.D0
      XR(2,3) = RADB(1)
      XR(2,4) = RADB(1)


      DO I=0, 86400, 50

         DO J = 1, NCORR

            ABCORR = CORR(J)

            DO K = 1, 4

               R1 = XR(1,K)
               R2 = XR(2,K)

               TITLE = 'ET = #1, R1 = #2, R2 = #3, CORR = #4'
               CALL REPMD ( TITLE, '#1', ET, 6, TITLE )
               CALL REPMD ( TITLE, '#2', R1, 6, TITLE )
               CALL REPMD ( TITLE, '#3', R2, 6, TITLE )
               CALL REPMI ( TITLE, '#4', J,     TITLE )
               CALL TCASE( TITLE )

               ET = ET0 + DBLE(I)

               CALL ZZGFSPQ ( ET, TARG1, TARG2, R1, R2, &
                              OBS, ABCORR, REF, VALUE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZP ( TARG1, ET, REF, ABCORR, OBS, PV1, LT )
               CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               RANGE1 = VNORM( PV1 )
               RANGE2 = VNORM( PV2 )
               ANG1   = DASINE( R1/RANGE1, ATOL )
               ANG2   = DASINE( R2/RANGE2, ATOL )

               THETA = VSEP ( PV1, PV2 ) - ANG1 - ANG2
!
!              The results should match to roundoff.
!
               CALL CHCKSD ( TITLE, VALUE, '=', THETA, TIGHT, OK )

            END DO

         END DO

      END DO



!
! Case n
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

!
!23456789012345678901234567890123456789012345678901234567890123456789012
!
