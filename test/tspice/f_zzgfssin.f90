!$Procedure      F_ZZGFSSIN ( Test surface intercept state computation )

      SUBROUTINE F_ZZGFSSIN ( OK )

! $ Abstract
!
!     Test the GF private surface intercept state routine ZZGFSSIN.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     This routine tests the private SPICELIB GF
!     surface intercept state routine ZZGFSSIN.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 12-MAY-2009 (NJB)
!
!        Removed error checks for ID codes that don't
!        map to names, as these are no longer applicable.
!
!-    SPICELIB Version 2.0.0, 23-SEP-2008 (NJB)
!
!        Relaxed tolerances to enable clean test runs
!        on all supported platforms. Changed quantity name
!        "sub-point" to "intercept" in test utility calls.
!
!-    SPICELIB Version 1.0.0, 09-MAY-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD


!
!     Local parameters
!
!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfssob.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfssob.bsp' )


      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-3 )

      DOUBLE PRECISION      MEDABS
      PARAMETER           ( MEDABS = 1.D-3 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-5 )

!
!     Time delta used for discrete derivatives. The 10-second
!     value used here was found by trial and error: it yields
!     better approximations than does the "standard" 1-second
!     value.
!
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D1 )



      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NREF
      PARAMETER           ( NREF  = 4 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 10 )


!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    EFRAME ( NREF )
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(FRNMLN)    JFRAME ( NREF )
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    METHOD

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPNT   ( 3, 2 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XPNT   ( 3 )
      DOUBLE PRECISION      XVEL   ( 3 )


      INTEGER               DCTR
      INTEGER               FRCLID
      INTEGER               FRCLSS
      INTEGER               FRCODE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               NC
      INTEGER               NF
      INTEGER               NS
      INTEGER               OBSID
      INTEGER               TRGID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FND
      LOGICAL               FOUND

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR   / 'NONE', &
                                     'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                     'XLT', 'XLT + S', 'XCN', 'XCN+S' &
                                   /

      DATA                  EFRAME / 'J2000',      'IAU_EARTH', &
                                     'ECLIPJ2000', 'IAU_MOON'  /

      DATA                  JFRAME / 'J2000',      'IAU_JUPITER', &
                                     'ECLIPJ2000', 'IAU_IO'    /





!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFSSIN' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Actual DE-based ephemerides yield better comparisons,
!     since these ephemerides have less noise than do those
!     produced by TSTSPK.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL FURNSH ( 'jup230.bsp' )
!


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      ABCORR = 'None.'
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

      ABCORR = 'None'
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'ITRF93'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame not centered on target' )

      ABCORR = 'None'
      METHOD = 'ellipsoid'
      TARGET = 'Mars'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )

      ABCORR = 'None'
      METHOD = 'ellipsoid'
      TARGET = 'GASPRA'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_GASPRA'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )

      ABCORR = 'None'
      METHOD = 'ellipsoid'
      TARGET = 'MOON'
      OBSRVR = 'GASPRA'
      FIXREF = 'IAU_MOON'
      DREF   = FIXREF
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF ephemeris data available' )

      ABCORR = 'LT'
      METHOD = 'ellipsoid'
      TARGET = 'MOON'
      OBSRVR = 'Earth'
      FIXREF = 'IAU_MOON'
      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF orientation data available' )

      ABCORR = 'LT'
      METHOD = 'ellipsoid'
      TARGET = 'MOON'
      OBSRVR = 'Earth'
      FIXREF = 'IAU_MOON'
      DREF   = 'ITRF93'
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                      DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     We're going to loop over all aberration corrections and
!     ray direction frames.
!

      DO NC = 1, NCORR

         DO NF = 1, NREF
!
!           Loop over the set of sample times.
!
            TIMSTR = '2008 MAY 1'

            CALL STR2ET ( TIMSTR, ET0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DELTA = 100 * SPD()


            DO NS = 1, NSAMP

!
! --- Case: ------------------------------------------------------
!
!
               ABCORR = CORR(NC)
               METHOD = 'Ellipsoid'
               DREF   = EFRAME(NF)
               TARGET = 'EARTH'
               OBSRVR = 'MOON'
               FIXREF = 'IAU_EARTH'

               CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODN2C ( TARGET, TRGID, FOUND )
               CALL BODN2C ( OBSRVR, OBSID, FOUND )

               TITLE = '#-# intercept. #; #; DREF = #; #'

               CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
               CALL REPMC ( TITLE, '#', TARGET, TITLE )
               CALL REPMC ( TITLE, '#', METHOD, TITLE )
               CALL REPMC ( TITLE, '#', ABCORR, TITLE )
               CALL REPMC ( TITLE, '#', DREF,   TITLE )

               ET = ET0 + (NS-1)*DELTA
               CALL ETCAL ( ET, TIMSTR )
               CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

               CALL TCASE ( TITLE )

!
!              Look up the direction vector.
!
               CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                             OBSRVR, DVEC, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL NAMFRM ( DREF, FRCODE )
               CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

               CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, &
                               OBSID,  DREF,  DCTR, DVEC,   RADII, &
                               STATE,  FOUND                       )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              By construction, the state should be found.
!
               CALL CHCKSL ( 'ZZGFSSIN found', FOUND, .TRUE., OK )

!               WRITE (*,*) 'STATE = ', STATE

!
!              Use SRFVEC and FIXREF as our ray direction and frame.
!
!              Estimate the surface intercept point velocity
!              numerically.
!
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

!
!                 DVEC is constant in frame DREF, so we don't need
!                 to re-compute it.
!
                  CALL SINCPT ( METHOD,    TARGET, T,      FIXREF, &
                                ABCORR,    OBSRVR, DREF,   DVEC, &
                                SPNT(1,I), TRGEPC, SRFVEC, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'ZZGFSSIN found (0)', &
                                 FOUND, .TRUE., OK    )

               END DO


               CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check the velocity from ZZGFSSIN. We expect an error of
!              less than 1 m/s, except when the frame is IAU_EARTH,
!              because the velocity of the intercept is quite high in
!              that frame.
!
               IF ( DREF .NE. 'IAU_EARTH' ) THEN
                  TOL = MEDABS
               ELSE
                  TOL = LOOSE
               END IF

               CALL CHCKAD ( 'Intercept vel (0)', STATE(4), '~~', &
                                                  XVEL,  3,  TOL, OK )

!               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) '>>>>>>>>>>>>>Speed = ', VNORM(XVEL)
!               END IF

!
!              Check the surface intercept.
!
               CALL SINCPT ( METHOD,  TARGET, ET,     FIXREF, &
                             ABCORR,  OBSRVR, DREF,   DVEC, &
                             XPNT,    TRGEPC, SRFVEC, FOUND )


!
!              The tolerance we use depends on the aberration
!              correction.
!
               CALL ZZPRSCOR ( ABCORR, ATTBLK )

               IF ( ATTBLK(CNVIDX) ) THEN

                  TOL = MEDABS

               ELSE IF ( ATTBLK(LTIDX) ) THEN

                  TOL = LOOSE
               ELSE
                  TOL = TIGHT
               END IF

               CALL CHCKAD ( 'Surface intercept point (0)', &
                             STATE, '~~',  XPNT,  3,  TOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!              Given the inputs above, make sure we do not find
!              the state if we reverse the ray's direction.'
!
               CALL PREFIX ( 'Earth non-intersection case:', 1, TITLE )
               CALL TCASE  ( TITLE )

               CALL VMINUS ( DVEC, NEGVEC )
               CALL VEQU   ( NEGVEC, DVEC )

               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, &
                               OBSID,  DREF,  DCTR, DVEC,   RADII, &
                               STATE,  FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              By construction, the state should NOT be found.
!
               CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
               ABCORR = CORR(NC)
               METHOD = 'ellipsoid'
               TARGET = 'JUPITER'
               OBSRVR = 'IO'
               FIXREF = 'IAU_JUPITER'
               DREF   = JFRAME(NF)

               TITLE = '#-# intercept. #; #; DREF = #; #'

               CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
               CALL REPMC ( TITLE, '#', TARGET, TITLE )
               CALL REPMC ( TITLE, '#', METHOD, TITLE )
               CALL REPMC ( TITLE, '#', ABCORR, TITLE )
               CALL REPMC ( TITLE, '#', DREF,   TITLE )

               CALL ETCAL ( ET, TIMSTR )

               CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

               CALL TCASE ( TITLE )

               CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODN2C ( TARGET, TRGID, FOUND )
               CALL BODN2C ( OBSRVR, OBSID, FOUND )

!
!              Look up the direction vector.
!
               CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                             OBSRVR, DVEC, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL NAMFRM ( DREF, FRCODE )
               CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

               CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, &
                               OBSID,  DREF,  DCTR, DVEC,   RADII, &
                               STATE,  FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              By construction, the state should be found.
!
               CALL CHCKSL ( 'ZZGFSSIN found', FOUND, .TRUE., OK )

!               WRITE (*,*) 'DREF, STATE = ', DREF, STATE

!
!              Use SRFVEC and FIXREF as our ray direction and frame.
!
!              Estimate the surface intercept point velocity
!              numerically.
!
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

!
!                 DVEC is constant in frame DREF, so we don't need
!                 to re-compute it.
!
                  CALL SINCPT ( METHOD,    TARGET, T,      FIXREF, &
                                ABCORR,    OBSRVR, DREF,   DVEC, &
                                SPNT(1,I), TRGEPC, SRFVEC, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'ZZGFSSIN found (2)', &
                                FOUND, .TRUE., OK     )

               END DO


               CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check the velocity from ZZGFSSIN. We expect an error of
!              less than 1 m/s.
!
!
!              The tolerance we use depends on the aberration
!              correction.
!
               CALL ZZPRSCOR ( ABCORR, ATTBLK )

               IF ( ATTBLK(CNVIDX) ) THEN

                  IF ( FIXREF .EQ. 'IAU_JUPITER' ) THEN
                     TOL = LOOSE
                  ELSE
                     TOL = MEDABS
                  END IF

               ELSE IF ( ATTBLK(LTIDX) ) THEN

                  TOL = LOOSE
               ELSE
                  TOL = MEDABS
               END IF



               CALL CHCKAD ( 'Intercept vel (1)', STATE(4), '~~', &
                                              XVEL,  3,  TOL, OK )
!               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) '>>>>>>>>>>>>>Speed = ', VNORM(XVEL)
!               END IF

!
!              Check the surface intercept.
!
               CALL SINCPT ( METHOD,  TARGET, ET,     FIXREF, &
                             ABCORR,  OBSRVR, DREF,   DVEC, &
                             XPNT,    TRGEPC, SRFVEC, FOUND )



               IF ( ATTBLK(CNVIDX) ) THEN

                  TOL = MEDABS

               ELSE IF ( ATTBLK(LTIDX) ) THEN

                  TOL = LOOSE
               ELSE
                  TOL = TIGHT
               END IF

               CALL CHCKAD ( 'Surface intercept point (1)', &
                             STATE, '~~',  XPNT,  3,  TOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!              Given the inputs above, make sure we do not find
!              the state if we reverse the ray's direction.'
!
               CALL PREFIX ( 'Non-intersection:', &
                             1, TITLE )
               CALL TCASE  ( TITLE )

               CALL VMINUS ( DVEC, NEGVEC )
               CALL VEQU   ( NEGVEC, DVEC )

               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, &
                               OBSID,  DREF,   DCTR,  DVEC, RADII, &
                               STATE,  FOUND                       )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              By construction, the state should NOT be found.
!
               CALL CHCKSL ( 'ZZGFSSIN found (3)', FOUND, .FALSE., OK )

            END DO
!
!           End of cases for the current epoch.
!
         END DO
!
!        End of cases for the current method.
!
      END DO
!
!     End of cases for the current aberration correction.
!


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




