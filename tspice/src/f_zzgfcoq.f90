!$Procedure      F_ZZGFCOQ ( Test GF coordinate quantity computation )

      SUBROUTINE F_ZZGFCOQ ( OK )
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private coordinate quantity computation routine
!     ZZGFCOQ.
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
!     This routine tests the private SPICELIB GF coordinate quantity
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
!-    TSPICE Version 4.0.0, 09-FEB-2016 (NJB)
!
!        Updated invalid METHOD test case: expected short
!        error message was updated.
!
!-    TSPICE Version 3.0.0, 12-MAY-2009 (NJB)
!
!        Removed error checks for ID codes that don't
!        map to names, as these are no longer applicable.
!
!-    TSPICE Version 2.0.0, 26-JUN-2008 (NJB)
!
!        Added test cases for bad coordinate system,
!        bad coordinate name, and bad vector definition.
!
!-    TSPICE Version 1.0.0, 06-JUN-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

      LOGICAL               ODD

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
      PARAMETER           ( TIGHT  = 1.D-16 )



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
!     Number of recognized coordinate systems.
!
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

!
!     Maximum length of a coordinate system name.
!
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

!
!     Maximum length of a coordinate name.
!
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

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
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    EFRAME ( NREF )
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(SYSLEN)    SYSNMS ( NSYS )
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(BDNMLN)    REFCTR
      CHARACTER*(VDFLEN)    VCDEFS ( NVDEF )
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      XCOORD ( 3 )
      DOUBLE PRECISION      XPOS   ( 3 )

      INTEGER               CI
      INTEGER               CTRID
      INTEGER               DCTR
      INTEGER               FI
      INTEGER               FRCLID
      INTEGER               FRCLSS
      INTEGER               FRCODE
      INTEGER               FXCID
      INTEGER               FXCLID
      INTEGER               FXCLSS
      INTEGER               FXCODE
      INTEGER               HANDLE
      INTEGER               N
      INTEGER               NC
      INTEGER               ND
      INTEGER               NF
      INTEGER               NS
      INTEGER               OBSID
      INTEGER               SI
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
!     Names of supported coordinate systems.
!
!     The Ith coordinate system in the array SYSNMS has coordinates
!     in the Ith row of the array CRDNMS. This association must be
!     preserved when this routine is updated.
!
      DATA                  SYSNMS /  RECSYS, &
                                      LATSYS, &
                                      RADSYS, &
                                      SPHSYS, &
                                      CYLSYS, &
                                      GEOSYS, &
                                      PGRSYS  /

!
!     Names of coordinate triples for the supported coordinate
!     systems.
!
!     The order of the coordinate names in the Ith row of this array
!     matches the order of the outputs of the corresponding
!     SPICELIB routine REC*, which maps a Cartesian vector to
!     the Ith coordinate system in the array SYSNMS. Again, this
!     order must be preserved.
!
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD, &
                                      RADCRD,   LONCRD,   LATCRD, &
                                      RNGCRD,   RACRD,    DECCRD, &
                                      RADCRD,   CLTCRD,   LONCRD, &
                                      RADCRD,   LONCRD,   ZCRD, &
                                      LONCRD,   LATCRD,   ALTCRD, &
                                      LONCRD,   LATCRD,   ALTCRD  /

!
!     Vector definition methods:
!
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFCOQ' )


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
      CALL TCASE ( 'Bad coordinate system' )


      ABCORR = 'CN+S'
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF = 'IAU_EARTH'
      DREF   = REF
      DCTR   = 399
      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      F  = ( RE - RADII(3) ) / RE


      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CTRID  = 399
      ABCORR = 'None'
      CRDSYS = 'GAUSSIAN'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CRDSYS = LATSYS


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate name' )

      CTRID  = 399
      ABCORR = 'None'
      CRDSYS = LATSYS
      CRDNAM = 'latitude'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CRDNAM = LATCRD

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad vector definition' )

      CTRID  = 399
      ABCORR = 'None'
      VECDEF = 'velocity'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      VECDEF = SINDEF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      CTRID  = 399
      ABCORR = 'None.'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

      METHOD = 'ellipsoid'
      REF    = 'ITRF93'


      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

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


      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF    = 'IAU_GASPRA'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

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

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF ephemeris data available' )

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )



      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )

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
      CTRID  = 399
      DREF   = 'ITRF93'
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )


      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                     REF,    ABCORR, OBSID, DREF, &
                     DVEC,   CRDSYS, CTRID, RE, &
                     F,      CRDNAM, VALUE, FOUND )


      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


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


            DO SI = 1, NSYS

               CRDSYS = SYSNMS(SI)
!
!              Loop over the set of sample times.
!
               TIMSTR = '2008 MAY 1'

               CALL STR2ET ( TIMSTR, ET0 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               DELTA = 100 * SPD()


               DO NS = 1, NSAMP

                  IF ( VECDEF .EQ. POSDEF ) THEN
!
!                    For vectors defined by positions, we don't
!                    have to use a frame centered on the target.
!                    In addition to the target-centered frames,
!                    we use body-fixed frames centered on a third body.
!
                     FI = MOD(NS,4) + 1

                     IF ( FI .EQ. 1 ) THEN

                        TARGET = 'EARTH'
                        OBSRVR = 'MOON'
                        REF    = 'IAU_EARTH'

                     ELSE IF ( FI .EQ. 2 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_JUPITER'

                     ELSE IF ( FI .EQ. 3 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_MARS'

                     ELSE IF ( FI .EQ. 4 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_MOON'

                     END IF

                  ELSE
!
!                    We use two different observer-target combinations.
!
                     IF ( ODD(NS) )THEN

                        TARGET = 'EARTH'
                        OBSRVR = 'MOON'
                        REF    = 'IAU_EARTH'

                     ELSE

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_JUPITER'

                     END IF

                  END IF

                  CALL NAMFRM ( REF,    FXCODE )
                  CALL FRINFO ( FXCODE, FXCID,  FXCLSS, FXCLID, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL BODC2N ( FXCID,  REFCTR, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )


                  CALL BODVRD ( REFCTR, 'RADII', 3, N, RADII )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  RE = RADII(1)
                  F  = ( RADII(1) - RADII(3) ) / RE

                  CALL BODN2C ( TARGET, TRGID, FOUND )
                  CALL BODN2C ( OBSRVR, OBSID, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )





!
! --- Case: ------------------------------------------------------
!
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     TITLE = '#; #-# position; #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
                     CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                     CALL REPMC ( TITLE, '#', TARGET, TITLE )
                     CALL REPMC ( TITLE, '#', ABCORR, TITLE )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

                     TITLE = '#; # sub # point on #; #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
                     CALL REPMC ( TITLE, '#', METHOD, TITLE )
                     CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                     CALL REPMC ( TITLE, '#', TARGET, TITLE )
                     CALL REPMC ( TITLE, '#', ABCORR, TITLE )


                  ELSE IF ( VECDEF .EQ. SINDEF ) THEN

                     NF   = MOD ( NS, 4 ) + 1
                     DREF = EFRAME(NF)

                     TITLE = '#; # #-# intercept. #; DREF = #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
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
!                    Look up the direction vector.
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
!                 Look up the expected position vector.
!
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     CALL SPKPOS ( TARGET, ET, REF,    ABCORR, OBSRVR, &
                                   XPOS,   LT                         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                     CALL SUBPNT ( METHOD, TARGET, ET, &
                                   REF,    ABCORR, OBSRVR, &
                                   XPOS,   TRGEPC, SRFVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SINCPT ( METHOD, TARGET, ET, &
                                   REF,    ABCORR, OBSRVR, &
                                   DREF,   DVEC,   XPOS, &
                                   TRGEPC, SRFVEC, FOUND  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    By construction, the intercept should be found.
!
                     CALL CHCKSL ( 'intercept found', &
                                   FOUND, .TRUE., OK )

                  END IF

!
!                 Convert position vector to current coordinate system.
!
                  IF ( CRDSYS .EQ. RECSYS ) THEN

                     CALL MOVED ( XPOS, 3, XCOORD )

                  ELSE IF ( CRDSYS .EQ. LATSYS ) THEN

                     CALL RECLAT ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. RADSYS ) THEN

                     CALL RECRAD ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. SPHSYS ) THEN

                     CALL RECSPH ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. CYLSYS ) THEN

                     CALL RECCYL ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. GEOSYS ) THEN

                     CALL RECGEO ( XPOS,      RE,        F, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. PGRSYS ) THEN

                     CALL RECPGR ( REFCTR,    XPOS,      RE,        F, &
                                   XCOORD(1), XCOORD(2), XCOORD(3)    )

                  ELSE

                     CALL SETMSG ( 'Unrecognized system #.' )
                     CALL ERRCH  ( '#', CRDSYS              )
                     CALL SIGERR ( 'SPICE(BUG)'             )
                     CALL CHCKXC ( .FALSE., ' ', OK         )

                  END IF

!
!                 Check each coordinate.
!
                  DO CI = 1, 3
!
!                    Get the result from ZZGFCOQ.
!
                     CRDNAM = CRDNMS(CI,SI)

                     CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                                    REF,    ABCORR, OBSID, DREF, &
                                    DVEC,   CRDSYS, FXCID, RE, &
                                    F,      CRDNAM, VALUE, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL CHCKSD ( CRDNAM, VALUE,     '~/', &
                                           XCOORD(CI), TIGHT, OK )

                  END DO




                  IF ( VECDEF .EQ. SINDEF ) THEN
!
! --- Case: ------------------------------------------------------
!

!                    Given the inputs above, make sure we do not find
!                    the state if we reverse the ray's direction.'
!
                     CALL PREFIX ( 'Earth non-intersection case:', &
                                   1, TITLE )
                     CALL TCASE  ( TITLE )

                     CALL VMINUS ( DVEC, NEGVEC )
                     CALL VEQU   ( NEGVEC, DVEC )

                     CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                                    REF,    ABCORR, OBSID, DREF, &
                                    DVEC,   CRDSYS, FXCID, RE, &
                                    F,      CRDNAM, VALUE, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )


                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    By construction, the state should NOT be found.
!
                     CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, &
                                   .FALSE., OK )

                  END IF


               END DO
!
!              End of cases for the current epoch.
!
            END DO
!
!           End of cases for the current coordinate system.
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




