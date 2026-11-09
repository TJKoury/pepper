!$Procedure      F_ZZGFCOST ( Test GF coordinate state computation )

      SUBROUTINE F_ZZGFCOST ( OK )
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private coordinate state computation routine
!     ZZGFCOST.
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
!     This routine tests the private SPICELIB GF coordinate state
!     computation routine ZZGFCOQ.
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
!-    SPICELIB Version 3.0.0, 13-MAY-2009 (NJB)
!
!        Removed error checks for ID codes that don't
!        map to names, as these are no longer applicable.
!
!-    SPICELIB Version 2.0.0, 26-MAY-2008 (NJB)
!
!        Added test case for bad values of VECDEF.
!
!-    SPICELIB Version 1.0.0, 14-MAY-2008 (NJB)
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

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )



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

      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )

      INTEGER               NVDEF
      PARAMETER           ( NVDEF  = 3 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )


!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    EFRAME ( NREF )
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VCDEFS ( NVDEF )
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               DCTR
      INTEGER               FRCLID
      INTEGER               FRCLSS
      INTEGER               FRCODE
      INTEGER               HANDLE
      INTEGER               N
      INTEGER               NC
      INTEGER               ND
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


!
!     Saved Variables
!

!
!     Initial values
!



!
!     Vector definition methods:
!
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFCOST' )


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
      CALL TCASE ( 'Bad value of VECDEF' )


      ABCORR = 'CN+S'
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      TRGID  = 399
      OBSRVR = 'MOON'
      OBSID  = 301
      REF    = 'IAU_EARTH'
      DREF   = REF
      DCTR   = 399

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )




      VECDEF = 'Surface xcept'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      VECDEF = SINDEF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      OBSID   = 301
      DCTR    = 399
      ABCORR  = 'None.'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

      METHOD = 'ellipsoid'
      REF    = 'ITRF93'


      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame not centered on target' )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      TRGID  = 499
      REF    = 'IAU_EARTH'


      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF    = 'IAU_GASPRA'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )

      TARGET = 'MOON'
      TRGID  = 301
      OBSRVR = 'GASPRA'
      OBSID  = 9511010
      REF    = 'IAU_MOON'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF ephemeris data available' )

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )



      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF orientation data available' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      TRGID  = 399
      OBSRVR = 'MOON'
      OBSID  = 301
      REF    = 'IAU_EARTH'
      DCTR   = 399
      DREF   = 'ITRF93'
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                      REF,    ABCORR, OBSID, DREF, &
                      DCTR,   DVEC,   RADII, STATE, FOUND )


      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      DCTR   = 399

      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're going to loop over all vector definitions,
!     aberration corrections, coordinate systems, and
!     coordinates.
!
!
      DO ND = 1, NVDEF

         VECDEF = VCDEFS(ND)

         IF ( VECDEF .EQ. SINDEF ) THEN

            METHOD = 'Ellipsoid'

         ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

            METHOD = 'Near point: ellipsoid'
         ELSE
            METHOD = ' '
         END IF


         DO NC = 1, NCORR

            ABCORR = CORR(NC )

            CALL ZZPRSCOR ( ABCORR,   ATTBLK )
            CALL CHCKXC   ( .FALSE., ' ', OK )

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
               IF ( VECDEF .EQ. POSDEF ) THEN

                  TITLE = '#-# position; #; #'

                  CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                  CALL REPMC ( TITLE, '#', TARGET, TITLE )
                  CALL REPMC ( TITLE, '#', ABCORR, TITLE )

               ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

                  TITLE = '# sub # point on #; #; #'

                  CALL REPMC ( TITLE, '#', METHOD, TITLE )
                  CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                  CALL REPMC ( TITLE, '#', TARGET, TITLE )
                  CALL REPMC ( TITLE, '#', ABCORR, TITLE )


               ELSE IF ( VECDEF .EQ. SINDEF ) THEN

                  NF   = MOD ( NS, 4 ) + 1
                  DREF = EFRAME(NF)

                  TITLE = '# #-# intercept. #; DREF = #; #'

                  CALL REPMC ( TITLE, '#', METHOD, TITLE )
                  CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                  CALL REPMC ( TITLE, '#', TARGET, TITLE )
                  CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                  CALL REPMC ( TITLE, '#', DREF,   TITLE )

               END IF

               ET = ET0 + (NS-1)*DELTA
               CALL ETCAL ( ET, TIMSTR )
               CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

               CALL TCASE ( TITLE )


               IF ( VECDEF .EQ. SINDEF ) THEN
!
!                 Look up the direction vector.
!
                  CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                                OBSRVR, DVEC, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL NAMFRM ( DREF, FRCODE )
                  CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

                  CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )

               END IF

!
!              Look up the expected state vector.
!
               IF ( VECDEF .EQ. POSDEF ) THEN

                  CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBSRVR, &
                                XSTATE, LT                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                  CALL ZZGFSSOB ( METHOD, TRGID,  ET, &
                                  REF, ABCORR, OBSID, &
                                  RADII,  XSTATE         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE

                  CALL ZZGFSSIN ( METHOD, TRGID, ET,     REF, &
                                  ABCORR, OBSID, DREF,   DCTR, &
                                  DVEC,   RADII, XSTATE, FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
!
!                 By construction, the intercept should be found.
!
                  CALL CHCKSL ( 'intercept found', &
                                FOUND, .TRUE., OK )

               END IF

!
!              Get the result from ZZGFCOST.
!
               CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                               REF,    ABCORR, OBSID, DREF, &
                               DCTR,   DVEC,   RADII, STATE, FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!              Check position.
!
               CALL CHCKAD ( 'Position', STATE,  '~~/', &
                                         XSTATE, 3, TIGHT, OK )

!
!              Check velocity.
!
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3, TIGHT, OK )


               IF ( VECDEF .EQ. SINDEF ) THEN
!
! --- Case: ------------------------------------------------------
!

!                 Given the inputs above, make sure we do not find
!                    the state if we reverse the ray's direction.'
!
                  CALL PREFIX ( 'Earth non-intersection case:', &
                                1, TITLE )
                  CALL TCASE  ( TITLE )

                  CALL VMINUS ( DVEC, NEGVEC )
                  CALL VEQU   ( NEGVEC, DVEC )

                  CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                                  REF,    ABCORR, OBSID, DREF, &
                                  DCTR,   DVEC,   RADII, STATE, FOUND )


                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 By construction, the state should NOT be found.
!
                  CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, &
                                .FALSE., OK )

               END IF


            END DO
!
!           End of cases for the current epoch.
!
         END DO
!
!        End of cases for the current aberration correction.
!
      END DO
!
!     End of cases for the current vector definition.
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




