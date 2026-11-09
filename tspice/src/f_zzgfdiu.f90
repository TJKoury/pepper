!$Procedure F_ZZGFDIU ( ZZGFDIU family tests )

      SUBROUTINE F_ZZGFDIU ( OK )
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB utility package
!
!        ZZGFDIU
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
!     This routine tests the SPICELIB geometry utility package ZZGFDIU.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 18-NOV-2013 (EDW)
!
!        Test families update to accommodate ZZGFCOU modifications
!        required by ZZGFRELX.
!
!        UDF call replaced ZZUDF.
!
!-    TSPICE Version 1.0.0, 22-MAY-2008 (NJB)
!
!-&

      EXTERNAL              UDF
      EXTERNAL              ZZGFDIGQ

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               ODD

!
!     Local parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfdiu.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 50 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEPSZ
      DOUBLE PRECISION      XDIST

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J

      LOGICAL               DECRES
      LOGICAL               LSSTHN
      LOGICAL               XDECRS
      LOGICAL               XLSTHN

!
!     Save everything.
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
      CALL TOPEN ( 'F_ZZGFDIU' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!      CALL FURNSH ( 'de421.bsp' )
!      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized target or observer' )

      REFVAL = 1.D5
      CALL ZZGFREF ( REFVAL )

      CALL ZZGFDIIN ( 'MOOON', 'NONE',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'NONE',  'EEARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Target and observer are identical' )


      CALL ZZGFDIIN ( 'MOON', 'LT+S',  'MOON' )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction specifiers' )


      CALL ZZGFDIIN ( 'MOON', 'S',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'XS',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'RLT',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'XRLT',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'z',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFDIDC: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFDIDC ( UDF, ET, DECRES )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFDILT: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFDIGQ: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFDIGQ ( ET, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )




!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     Check values returned by ZZGFDIU entry points for each aberration
!     correction and a variety of times.
!
      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEPSZ = (1.D3/NSAMP) * SPD()


      DO I = 1, NCORR
!
!        Perform tests at a series of times.
!
         ABCORR = CORR(I)

         DO J = 1, NSAMP

            ET = ET0 + (J-1)*STEPSZ

!
!           Use different observer-target pairs.
!
            IF ( ODD(J) ) THEN

               OBSRVR = 'EARTH'
               TARGET = 'MOON'
            ELSE
               OBSRVR = 'SUN'
               TARGET = 'MARS'
            END IF

!
!---- Case -------------------------------------------------------------
!

            TITLE = 'ZZGFDIGQ: check #-# distance. ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )

            REFVAL = 4.D5
            CALL ZZGFREF ( REFVAL )

            CALL ZZGFDIIN ( TARGET, ABCORR, OBSRVR )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL ZZGFDIGQ ( ET, DIST )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL SPKEZR ( TARGET, ET,    'J2000', &
                          ABCORR, OBSRVR, STATE, LT )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            XDIST = VNORM( STATE )

!
!           We expect a pretty good match.
!
            CALL CHCKSD ( 'DIST', DIST, '~/', XDIST, VTIGHT, OK )


!
!---- Case -------------------------------------------------------------
!
            TITLE = 'ZZGFDILT: check #-# "is less than?" state. ' &
            //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XLSTHN = XDIST .LT. REFVAL

            CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'LSSTHN', LSSTHN, XLSTHN, OK )


!
!---- Case -------------------------------------------------------------
!
            TITLE = 'ZZGFREF: check #-# "is less than?" state. ' &
            //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XLSTHN = XDIST .LT. REFVAL
!
!           Set the reference value so as to invert the
!           expected relationship to the reference value.
!
            IF ( XLSTHN ) THEN
               CALL ZZGFREF ( DPMIN() )
             ELSE
               CALL ZZGFREF ( DPMAX() )
            END IF

            CALL CHCKXC   ( .FALSE., ' ', OK )

            XLSTHN = .NOT. XLSTHN

            CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'LSSTHN', LSSTHN, XLSTHN, OK )

!
!---- Case -------------------------------------------------------------
!
            TITLE = 'ZZGFDIDC: check #-# "is decreasing?" state. ' &
            //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XDECRS = VDOT( STATE, STATE(4) ) .LT. 0.D0

            CALL ZZGFDIDC ( UDF, ET, DECRES )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'DECRES', DECRES, XDECRS, OK )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END
