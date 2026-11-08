!$Procedure      F_ZZGFLNG2 ( Test GF longitude solver, part 2 )

      SUBROUTINE F_ZZGFLNG2 ( OK )

! $ Abstract
!
!     Test the GF private longitude search routine ZZGFLONG. This
!     family tests equality and inequality relation processing.
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
      INCLUDE               'zzgf.inc'
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
!     This routine tests the private SPICELIB GF coordinate search
!     routine ZZGFLONG. This family tests equality and inequality
!     relation processing.
!
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
!-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
!
!        Added explicit declaration of GFBAIL type.
!
!-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      INTEGER               WNCARD

      LOGICAL               EQSTR
      LOGICAL               GFBAIL
      LOGICAL               ODD

!
!     EXTERNAL declarations
!
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFBAIL

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgflong.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgflong.bsp' )


      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = CNVTOL )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

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

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2*20000 )

      INTEGER               MW
      PARAMETER           ( MW     = MAXWIN )

      INTEGER               NW
      PARAMETER           ( NW     = NWMAX  )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               OPLEN
      PARAMETER           ( OPLEN  = 40 )

!
!     Number of aberration correction choices:
!
      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

!
!     Number of systems that support longitude:
!
      INTEGER               NLNSYS
      PARAMETER           ( NLNSYS = 6 )

!
!     Number of vector definitions:
!
      INTEGER               NVDEF
      PARAMETER           ( NVDEF = 3 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CRDLEN)    CNAM   ( NLNSYS )
      CHARACTER*(LNSIZE)    CORR   ( NCORR  )
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(SYSLEN)    CSYS   ( NLNSYS )
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(LNSIZE)    DSCSTR
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(OPLEN)     RELATE
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VDEF   ( NVDEF )
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      ALPHRD ( 3 )
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTOFF
      DOUBLE PRECISION      QTOL
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      TMPRAD ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      WORK   ( MW, NW )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      XET
      DOUBLE PRECISION      XLON
      DOUBLE PRECISION      XTIME

      INTEGER               AC
      INTEGER               CC
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               VI

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               BAIL
      LOGICAL               RPT
      LOGICAL               USELT
      LOGICAL               XMIT

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR   / 'None', &
                                     'Lt',     'cN', &
                                     'XlT',    'xCn', &
                                     'LT + s', 'xLt+ S', &
                                     'xCn',    ' XcN +s' /

      DATA                  CNAM   / 'Longitude', &
                                     'Right  ascension', &
                                     'Longitude', &
                                     'Longitude', &
                                     'Longitude', &
                                     'Longitude'        /

      DATA                  CSYS   / 'Latitudinal', &
                                     'Ra / Dec', &
                                     'Cylindrical', &
                                     'Geodetic', &
                                     'Planetographic', &
                                     'Spherical'        /

      DATA                  VDEF  / POSDEF, SOBDEF, SINDEF /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFLNG2' )


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
!     Create and load Nat's solar system SPK and PCK/FK
!     kernels.
!
      CALL NATPCK ( NPCK, .TRUE., .FALSE. )
      CALL NATSPK ( NSPK, .TRUE., HAN2    )

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
!*    Normal cases --- Nat's solar system
!*
!*********************************************************************


!*********************************************************************
!*
!*    Equality cases
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
!
!     Equality tests:
!
!     We want at least one case that exercises every combination of
!     aberration correction and coordinate system. This set of tests
!     does that. Later on, we'll test a more complete set of equality
!     constraints.
!
!     We're going to find times when the Sun-Beta vector crosses the
!     ALPHA_VIEW_XY prime meridian. This should occur every 24 hours,
!     starting at the J2000 epoch.
!
!     Loop over the aberration corrections.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 12:04 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 12:06 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      REF    = 'ALPHA_VIEW_XY'
      DREF   = ' '

      DO CC = 1, NLNSYS

         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO AC = 1, NCORR

!
! --- Case: ------------------------------------------------------
!
            ABCORR = CORR(AC)

            TITLE =  'Sun-Beta vector crosses the ALPHA_VIEW_XY prime ' &
            //       'meridian. #; #; #'

            CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM, TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )

            CALL TCASE ( TITLE  )

            CALL ZZPRSCOR ( ABCORR, ATTBLK )

            XMIT  = ATTBLK(XMTIDX)
            USELT = ATTBLK(LTIDX )

            CALL CLEARD  ( 3, DVEC )

            RELATE = '='
            REFVAL = 0.D0
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure we found 1 root.
!
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!           Make sure that the start and finish times are
!           those we expect. The crossing should occur at
!           the midpoint of the occultation of ALPHA by BETA,
!           which is 5 minutes after noon TDB, plus or minus
!           the light time offset.
!
            IF ( USELT ) THEN

               IF ( XMIT ) THEN

                  LTOFF = -1.D0
               ELSE
                  LTOFF =  1.D0
               END IF

            ELSE

               LTOFF = 0.D0

            END IF


            DO I = 1, N

               XTIME = (I-1)*SPD() + 300.D0 + LTOFF



               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               QNAME = 'Interval start # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, START,  '~', XTIME, TIMTOL, OK )

               QNAME = 'Interval stop  # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, FINISH, '~', XTIME, TIMTOL, OK )

            END DO

!
!           Check the longitude at the crossing time.
!
!           In order to decide how accurate the coordinate should be,
!           we'll use the angular rate of Beta together with the time
!           tolerance.
!
            QTOL = ( TWOPI() / SPD() ) * CNVTOL

            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKEZR ( TARGET, START,  REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL RECLAT ( STATE, R, LON, LAT )

               QNAME = 'Longitude # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, LON, '~', 0.D0, QTOL, OK )

            END DO

         END DO
!
!        End of aberration correction loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!


!
! --- Case: ------------------------------------------------------
!
!
!     This set of sets exercises every combination of vector definition
!     and coordinate system; we don't vary the aberration correction.
!     For sub-observer points, we vary the method but don't test every
!     combination of method and coordinate system.
!
!     We're going to find times when the each of the following vectors
!     crosses the
!     ALPHAFIXED prime meridian:
!
!        1) Position of Gamma with respect to Alpha
!
!        2) Sub-Gamma point on Alpha
!
!        3) Intercept of Gamma-to-Alpha ray on surface of Alpha
!
!     This should occur every 24 hours, starting at 6 hours past the
!     J2000 epoch.
!
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 17:59 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 2 18:01 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'NONE'

      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, NVDEF
!
!           Set the vector definition.
!
            VECDEF = VDEF(VI)

!
!           Set the definition method and ray's
!           direction.
!
            IF ( VECDEF .EQ. POSDEF ) THEN
!
!              Note that for positions, the vector
!              is the *opposite* of the sub-observer
!              or position vector. It's easiest to
!              switch observer and target for this case.
!
               DSCSTR = 'Alpha-Gamma position'
               OBSRVR = 'ALPHA'
               TARGET = 'GAMMA'
               REF    = 'ALPHAFIXED'

               METHOD = ' '
               DREF   = ' '
               CALL CLEARD  ( 3, DVEC )

            ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

               DSCSTR = 'Sub-Gamma point'
               TARGET = 'ALPHA'
               OBSRVR = 'GAMMA'
               REF    = 'ALPHAFIXED'
!
!              Vary the sub-observer point definition.
!
               IF ( ODD(CC) ) THEN
                  METHOD = 'Near point: ellipsoid'
               ELSE
                  METHOD = 'Intercept: ellipsoid'
               END IF

               DREF = ' '
               CALL CLEARD  ( 3, DVEC )

            ELSE IF ( VECDEF .EQ. SINDEF ) THEN

               DSCSTR = 'Gamma-Alpha ray intercept'
               TARGET = 'ALPHA'
               OBSRVR = 'GAMMA'
               REF    = 'ALPHAFIXED'

               METHOD = 'ellipsoid'
               DREF   = 'gammafixed'

!
!              The pointing direction is along the gammafixed
!              frame's +X axis.
!
               CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

            ELSE
!
!              This is a backstop case.
!
               CALL SETMSG ( 'VECDEF = #' )
               CALL ERRCH  ( '#', VECDEF  )
               CALL SIGERR ( 'SPICE(TSPICEBUG)' )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF


!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# on Alpha 0 ' &
            //       'meridian. #; #; #; #; #'

            CALL REPMC ( TITLE, '#', DSCSTR, TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM, TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', VECDEF, TITLE )
            CALL REPMC ( TITLE, '#', METHOD, TITLE )

            CALL TCASE ( TITLE  )

            RELATE = '='
            REFVAL = 0.D0
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure we found 2 roots.
!
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


            DO I = 1, N
!
!              We expect to cross the prime meridian at 6 pm TDB.
!
               XTIME = (I-1)*SPD() + 6*3600.D0

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               QNAME = 'Interval start # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, START,  '~', XTIME, TIMTOL, OK )

               QNAME = 'Interval stop  # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, FINISH, '~', XTIME, TIMTOL, OK )

            END DO

!
!           Check the longitude at the crossing time.
!
!           In order to decide how accurate the coordinate should be,
!           we'll use the angular rate of Gamma together with the time
!           tolerance.
!
            QTOL = ( TWOPI() / SPD() ) * CNVTOL

            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Compute the longitude of the vector we want.
!              Because of the special geometry we've set up,
!              we can use the observer-target state for all
!              cases; just negate the position for the
!              sub-observer and intercept definitions.
!
               CALL SPKEZR ( TARGET, START,  REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF (      ( VECDEF .EQ. SOBDEF ) &
                    .OR. ( VECDEF .EQ. SINDEF ) ) THEN

                  CALL VMINUS ( STATE, POS )
               ELSE
                  CALL VEQU   ( STATE, POS )
               END IF

               CALL RECLAT ( POS, R, LON, LAT )

               QNAME = 'Longitude # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, LON, '~', 0.D0, QTOL, OK )

            END DO

         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!



!
! --- Case: ------------------------------------------------------
!
!
!     More '=' cases follow.
!
!     At this point we've tested ZZGFLONG's ability to handle
!     vector definitions and aberration corrections in combination
!     with different coordinate systems. Now we want to perform
!     more robust tests on the root finding aspects of ZZGFLONG.
!     We'll stick with geometric position vectors from now on,
!     but we'll test each coordinate system.
!
!     Now we want to find the times when the Alpha-Gamma vector's
!     longitude hits each integer multiple of pi/8.
!     This should occur every 90 minutes.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 17:59 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      REF    = 'alphafixed'
      ABCORR = 'NONE'
      OBSRVR = 'Alpha'
      TARGET = 'Gamma'
      DSCSTR = 'Alpha-Gamma position'
      VECDEF = POSDEF
      DREF   = ' '
      CALL CLEARD ( 3, DVEC )


      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 18
!
!           Loop over each reference value.
!
            REFVAL = (VI-1) * PI() / 8.D0

!
!           For planetographic coordinates, set the reference
!           value to indicate the same position on the unit
!           circle as that used for the other systems.
!
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# on Alpha # (deg) ' &
            //       'meridian. #; #'


            CALL REPMC ( TITLE, '#', DSCSTR,              TITLE )
            CALL REPMF ( TITLE, '#', REFVAL*DPR(), 6,'F', TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS,              TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,              TITLE )


            CALL TCASE ( TITLE  )

            RELATE = '='
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           We're going to set the confinement window for each
!           search so we find two roots.
!
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0
            T2 = T1  + 32     * 5400.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure we found 2 roots.
!
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


            DO I = 1, N
!
!              We expect to cross the VIth reference value at
!
!                 24*(I-1) + 4.5 + 1.5*VI hours
!
!              past noon TDB.
!
               XTIME = ( 24*(I-1) + 1.5D0*VI + 4.5D0 ) * 3600.D0

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               QNAME = 'Interval start # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, START,  '~', XTIME, TIMTOL, OK )

               QNAME = 'Interval stop  # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, FINISH, '~', XTIME, TIMTOL, OK )

            END DO

!
!           Check the longitude at the crossing time.
!
!           In order to decide how accurate the coordinate should be,
!           we'll use the angular rate of Gamma together with the time
!           tolerance. Adjust the tolerance for the units we're using
!           in the CHCKSD call.
!
            QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Compute the longitude of the vector we want.
!
               CALL SPKEZR ( TARGET, START,  REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF

               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

!
!              Adjust the longitude by 2*pi if it's too far
!              from the expected value.
!
               IF (  LON  .GT. ( REFVAL + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( REFVAL - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', &
                                    REFVAL*DPR(), QTOL, OK )

            END DO

         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!



!*********************************************************************
!*
!*    "Less than" cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
!
!     Now we're going to work with inequality constraints.
!
!     We're going to start out with some very simple cases. We want to
!     find the times when the Alpha-Gamma vector's longitude is less
!     than each integer multiple of pi/8, where the confinement window
!     extends 1 second before and after the crossing time. Crossings of
!     these meridians should occur every 90 minutes.
!
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      REF    = 'alphafixed'
      ABCORR = 'NONE'
      OBSRVR = 'Alpha'
      TARGET = 'Gamma'
      DSCSTR = 'Alpha-Gamma position longitude'
      VECDEF = POSDEF
      DREF   = ' '
      CALL CLEARD ( 3, DVEC )


      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
!
!           Loop over each reference value. Include both 0 and 2*pi.
!
            REFVAL = (VI-1) * PI() / 8.D0

!
!           For planetographic coordinates, set the reference
!           value to indicate the same position on the unit
!           circle as that used for the other systems.
!
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# less than # (deg) #; #; 2-sec CONFINE'


            CALL REPMC ( TITLE, '#', DSCSTR,              TITLE )
            CALL REPMF ( TITLE, '#', REFVAL*DPR(), 6,'F', TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS,              TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,              TITLE )


            CALL TCASE ( TITLE  )

            RELATE = '<'
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           We're going to set the confinement window for each
!           search so we cover one reference value crossing.
!
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = T1                      + 2.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Filter the result window to remove spurious
!           intervals that may arise due to convergence
!           error.
!
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect to cross the VIth reference value at
!
!              4.5 + 1.5D0*VI hours
!
!           past noon TDB.
!
!
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


!
!           Normally we expect to find one solution interval, but
!           the result window should be empty if the constraint
!           can't be satisfied.
!
            N = WNCARD ( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

!              WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (      EQSTR( CRDSYS, CYLSYS ) &
                 .OR. EQSTR( CRDSYS, PGRSYS ) &
                 .OR. EQSTR( CRDSYS, RADSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!                 The condition
!
!                    longitude < REFVAL
!
!                 is never satisfied, so the window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
!
!                 Make sure we found 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            ELSE

!
!              Make sure we found 1 interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF

!
!           Note: in the code below, we don't expect to find
!           result windows containing multiple intervals.
!           But we format the code to handle multiple intervals
!           so we can easily re-use the code in cases where
!           multiple intervals are expected.
!
!
!           Now check the solution intervals.
!
            DO I = 1, N
!
!              Look up the Ith solution interval.
!
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )


               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN

                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    WRITE (*,*) 1, START, FINISH
!
!                    The stop time should be T2; the start time should
!                    be the time of the reference meridian crossing.
!
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, START, '~', XTIME, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, FINISH,  '~', T2, TIMTOL, OK )


                     XET = XTIME
                     ET  = START

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' ) &
                         .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
!
!                 Longitude is positive East. The branch cut is at 0.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    The start time should be T1. If the reference
!                    value is at the branch cut, the stop time should
!                    be T2. Otherwise, the stop time should be the time
!                    of the reference meridian crossing.
!
!
                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD ( QNAME, START,  '~', T1, TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
                        XET = T2
                        ET  = FINISH
                     ELSE
                        XET = XTIME
                        ET  = FINISH
                     END IF

                     CALL CHCKSD ( QNAME, FINISH, '~', &
                                          XET, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' ) &
                         .OR. EQSTR( CRDSYS, 'GEODETIC'    ) &
                         .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
!
!                 Longitude is positive East. The branch cut is at Pi.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    The start time should be T1. If the reference
!                    value is at the branch cut, the stop time should
!                    be T2. Otherwise, the stop time should be the time
!                    of the reference meridian crossing.
!
!
                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD ( QNAME, START,  '~', T1, TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
                        XET = T2
                     ELSE
                        XET = XTIME
                     END IF

                     ET = FINISH

                     CALL CHCKSD ( QNAME, ET, '~', &
                                          XET,    TIMTOL, OK )

                  END IF

               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


!
!              Check the longitude at the crossing time ET.
!
!              In order to decide how accurate the coordinate should
!              be, we'll use the angular rate of Gamma together with
!              the time tolerance. Adjust the tolerance for the units
!              we're using in the CHCKSD call.
!
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


!
!              Compute the longitude of the vector we expect and
!              the longitude at FINISH.
!
               CALL SPKEZR ( TARGET, XET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, &
                                XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

!
!              Adjust the longitude by 2*pi if it's too far
!              from the expected value.
!
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', &
                                    XLON*DPR(), QTOL, OK )

            END DO
!
!           We're done with the Ith interval.
!
         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!



!
! --- Case: ------------------------------------------------------
!
!     Repeat the previous case, but this time keep the confinement
!     window start time fixed at 18:00 Jan 1 2000 TDB. We want to find
!     the times when the Alpha-Gamma vector's longitude is less than
!     each integer multiple of pi/8, where the confinement window
!     extends 1 second before and after the crossing time. Crossings of
!     these meridians should occur every 90 minutes.
!
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      REF    = 'alphafixed'
      ABCORR = 'NONE'
      OBSRVR = 'Alpha'
      TARGET = 'Gamma'
      DSCSTR = 'Alpha-Gamma position longitude'
      VECDEF = POSDEF
      DREF   = ' '
      CALL CLEARD ( 3, DVEC )


      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF


         DO VI = 1, 17
!
!           Loop over each reference value. Include both 0 and 2*pi.
!
            REFVAL = (VI-1) * PI() / 8.D0

!
!           For planetographic coordinates, set the reference
!           value to indicate the same position on the unit
!           circle as that used for the other systems.
!
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# less than # (deg) #; #; fixed CNFINE start'


            CALL REPMC ( TITLE, '#', DSCSTR,              TITLE )
            CALL REPMF ( TITLE, '#', REFVAL*DPR(), 6,'F', TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS,              TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,              TITLE )


            CALL TCASE ( TITLE  )

            RELATE = '<'
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           We're going to set the confinement window for each
!           search so we cover one reference value crossing.
!
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + CNVTOL/10.D0
            T2 = ET0 + (VI-1)*5400.D0 + 1.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )


!
!           Filter the result window to remove spurious
!           intervals that may arise due to convergence
!           error.
!
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )



!
!           We expect to cross the VIth reference value at
!
!              4.5 + 1.5D0*VI hours
!
!              past noon TDB.
!
!
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


!
!           Normally we expect to find one solution interval, but
!           the result window should be empty if the constraint
!           can't be satisfied.
!
            N = WNCARD ( RESULT )

!           WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL



            IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!                 The condition
!
!                    longitude < REFVAL
!
!                 is never satisfied, so the window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
!
!                 Make sure we found 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            ELSE IF (      EQSTR( CRDSYS, CYLSYS ) &
                      .OR. EQSTR( CRDSYS, RADSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!                 The condition
!
!                    longitude < REFVAL
!
!                 is never satisfied, so the window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
!
!                 Make sure we found 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            ELSE IF ( EQSTR( CRDSYS, CYLSYS ) &
                 .OR. EQSTR( CRDSYS, RADSYS )  ) THEN
!
!              Make sure we found 1 interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )


            ELSE IF ( EQSTR( CRDSYS, LATSYS ) &
                 .OR. EQSTR( CRDSYS, GEOSYS ) &
                 .OR. EQSTR( CRDSYS, SPHSYS )  ) THEN
!
!              The number of expected intervals depends on the
!              case index.
!
               IF ( VI .EQ. 1 ) THEN
!
!                 The condition
!
!                    longitude < REFVAL
!
!                 is never satisfied, so the window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )
!
               ELSE
!
!                 Make sure we found 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            END IF

!
!           Now check the solution intervals.
!
            DO I = 1, N
!
!              Look up the Ith solution interval.
!
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN

                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    WRITE (*,*) 1, START, FINISH
!
!                    The stop time should be T2; the start time should
!                    be the time of the reference meridian crossing.
!
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, START, '~', XTIME, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, FINISH,  '~', T2, TIMTOL, OK )


                     XET = XTIME
                     ET  = START

                  ELSE
!
!                    We don't expect to have more than one
!                    interval for this case.
!
                     CALL SETMSG ( 'Unexpected interval count: #; ' &
                     //            'VI = #'                         )
                     CALL ERRINT ( '#', N                           )
                     CALL ERRINT ( '#', VI                          )
                     CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                     CALL CHCKXC ( .FALSE., ' ', OK                 )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' ) &
                         .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
!
!                 Longitude is positive East. The branch cut is at 0.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    The start time should be T1. If the reference
!                    value is at the branch cut, the stop time should
!                    be T2. Otherwise, the stop time should be the time
!                    of the reference meridian crossing.
!
!
                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD ( QNAME, START,  '~', T1, TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
                        XET = T2
                        ET  = FINISH
                     ELSE
                        XET = XTIME
                        ET  = FINISH
                     END IF

                     CALL CHCKSD ( QNAME, FINISH, '~', &
                                          XET, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' ) &
                         .OR. EQSTR( CRDSYS, 'GEODETIC'    ) &
                         .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
!
!                 Longitude is positive East. The branch cut is at Pi.
!
                     IF ( I .EQ. 1 ) THEN
!
!                       This is the normal case.
!
!                       If REFVAL is in the open upper half of the unit
!                       circle, the start time should be ET0 and the
!                       stop time should be the time of the reference
!                       meridian crossing.
!
!                       If REFVAL is pi, the solution set includes the
!                       interval from XTIME to T2. We expect this
!                       interval to be merged with the interval for the
!                       top half of the circle, so the expected end
!                       time becomes T2.
!
!                       Otherwise, the first interval should be the
!                       singleton [ET0, ET0].
!
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( SIN(REFVAL) .GT. 0.D0 ) THEN

                           CALL CHCKSD ( QNAME, START, '~', &
                                                ET0, TIMTOL, OK )
                        ELSE
                           CALL CHCKSD ( QNAME, START, '~', &
                                             ET0+SPD()/2, TIMTOL, OK )
                        END IF

!                        IF ( .NOT. OK ) THEN
!                           WRITE (*,*) 'VI = ', VI
!                        END IF

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
                           XET = T2
                        ELSE
                           XET = XTIME
                        END IF

                        ET = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', &
                                             XET,    TIMTOL, OK )
                     ELSE
!
!                       We don't expect to have more than one
!                       interval for this case.
!
                        CALL SETMSG ( 'Unexpected interval count: #; ' &
                        //            'VI = #'                         )
                        CALL ERRINT ( '#', N                           )
                        CALL ERRINT ( '#', VI                          )
                        CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                        CALL CHCKXC ( .FALSE., ' ', OK                 )

                     END IF


               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


!
!              Check the longitude at the crossing time ET.
!
!              In order to decide how accurate the coordinate should
!              be, we'll use the angular rate of Gamma together with
!              the time tolerance. Adjust the tolerance for the units
!              we're using in the CHCKSD call.
!
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


!
!              Compute the longitude of the vector we expect and
!              the longitude at FINISH.
!
               CALL SPKEZR ( TARGET, XET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, &
                                XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

!
!              Adjust the longitude by 2*pi if it's too far
!              from the expected value.
!
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', &
                                    XLON*DPR(), QTOL, OK )

            END DO
!
!           We're done with the Ith interval.
!
         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!

!*********************************************************************
!*
!*    "Greater than" cases
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!

!
!     '>' Tests
!
!     We're going to try some very simple '>' cases. We want to
!     find the times when the Alpha-Gamma vector's longitude is greater
!     than each integer multiple of pi/8, where the confinement window
!     extends 1 second before and after the crossing time. Crossings of
!     these meridians should occur every 90 minutes.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      REF    = 'alphafixed'
      ABCORR = 'NONE'
      OBSRVR = 'Alpha'
      TARGET = 'Gamma'
      DSCSTR = 'Alpha-Gamma position longitude'
      VECDEF = POSDEF
      DREF   = ' '
      CALL CLEARD ( 3, DVEC )


      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
!
!           Loop over each reference value. Include both 0 and 2*pi.
!
            REFVAL = (VI-1) * PI() / 8.D0

!
!           For planetographic coordinates, set the reference
!           value to indicate the same position on the unit
!           circle as that used for the other systems.
!
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# greater than # (deg) #; #; 2-sec CONFINE'


            CALL REPMC ( TITLE, '#', DSCSTR,              TITLE )
            CALL REPMF ( TITLE, '#', REFVAL*DPR(), 6,'F', TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS,              TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,              TITLE )


            CALL TCASE ( TITLE  )

            RELATE = '>'
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           We're going to set the confinement window for each
!           search so we cover one reference value crossing.
!
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = T1                      + 2.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )



!
!           We expect to cross the VIth reference value at
!
!              24*(I-1) + 4.5 + 1.5D0*VI hours
!
!              past noon TDB.
!
!
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


!
!           Filter the result window to remove spurious
!           intervals that may arise due to convergence
!           error.
!
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Normally we expect to find one solution interval, but
!           the result window should be empty if the constraint
!           can't be satisfied.
!
            N = WNCARD ( RESULT )

!              WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (      EQSTR( CRDSYS, CYLSYS ) &
                 .OR. EQSTR( CRDSYS, PGRSYS ) &
                 .OR. EQSTR( CRDSYS, RADSYS )  ) THEN

!
!              Make sure we found 1 interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            ELSE

               IF ( COS(REFVAL) .EQ. -1 ) THEN
!
!                 We don't expect to find any intervals.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
!
!                 Make sure we found 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            END IF

!
!           Note: in the code below, we don't expect to find
!           result windows containing multiple intervals.
!           But we format the code to handle multiple intervals
!           so we can easily re-use the code in cases where
!           multiple intervals are expected.
!
!
!           Now check the solution intervals.
!
            DO I = 1, N
!
!              Look up the Ith solution interval.
!
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )


               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN

                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    WRITE (*,*) 1, START, FINISH
!
                     IF ( COS(REFVAL) .EQ. 1 ) THEN
!
!                       The stop time actually should be T2, since the
!                       interval from T1 to XTIME satisfies the
!                       inequality.
!
                        XET = T2

                     ELSE
!
!                       The start time should be XTIME. The stop time
!                       should be T2.
!
                        XET = XTIME

                     END IF


!                    The start time should be T1; the stop time should
!                    be the time of the reference meridian crossing.
!
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, START, '~', T1, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = FINISH

                     CALL CHCKSD( QNAME, ET,   '~', &
                                         XET,  TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' ) &
                         .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
!
!                 Longitude is positive East. The branch cut is at 0.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
                     IF ( COS(REFVAL) .EQ. 1 ) THEN
!
!                       The start time actually should be T1, since the
!                       interval from T1 to XTIME satisfies the
!                       inequality.
!
                        XET = T1

                     ELSE
!
!                       The start time should be XTIME. The stop time
!                       should be T2.
!
                        XET = XTIME

                     END IF

                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = START

                     CALL CHCKSD ( QNAME, ET,    '~', &
                                   XET,   TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )


                     CALL CHCKSD ( QNAME, FINISH, '~', &
                                          T2, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' ) &
                         .OR. EQSTR( CRDSYS, 'GEODETIC'    ) &
                         .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
!
!                 Longitude is positive East. The branch cut is at Pi.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    The start time should be XTIME. The stop time
!                    should be T2.
!
                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = START
                     XET = XTIME

                     CALL CHCKSD ( QNAME, ET,  '~', &
                                          XET, TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD ( QNAME, FINISH, '~', &
                                          T2,     TIMTOL, OK )

                  END IF

               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


!
!              Check the longitude at the crossing time ET.
!
!              In order to decide how accurate the coordinate should
!              be, we'll use the angular rate of Gamma together with
!              the time tolerance. Adjust the tolerance for the units
!              we're using in the CHCKSD call.
!
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


!
!              Compute the longitude of the vector we expect and
!              the longitude at FINISH.
!
               CALL SPKEZR ( TARGET, XET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, &
                                XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

!
!              Adjust the longitude by 2*pi if it's too far
!              from the expected value.
!
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', &
                                    XLON*DPR(), QTOL, OK )

            END DO
!
!           We're done with the Ith interval.
!
         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!



!
! --- Case: ------------------------------------------------------
!
!     Repeat the previous case, but this time keep the confinement
!     window stop time fixed at 18:00 Jan 2 2000 TDB. We want to
!     find the times when the Alpha-Gamma vector's longitude is greater
!     than each integer multiple of pi/8. Crossings of
!     these meridians should occur every 90 minutes.
!
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 18:00:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET1 = ET0 + SPD()


      REF    = 'alphafixed'
      ABCORR = 'NONE'
      OBSRVR = 'Alpha'
      TARGET = 'Gamma'
      DSCSTR = 'Alpha-Gamma position longitude'
      VECDEF = POSDEF
      DREF   = ' '
      CALL CLEARD ( 3, DVEC )


      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Set the longer equatorial radius of Alpha equal to
!           the shorter: unequal equatorial radii are not supported
!           for these systems.
!
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Note: use of the subscript "2" in the first argument
!           below is intentional.
!
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
!
!           Loop over each reference value. Include both 0 and 2*pi.
!
            REFVAL = (VI-1) * PI() / 8.D0

!
!           For planetographic coordinates, set the reference
!           value to indicate the same position on the unit
!           circle as that used for the other systems.
!
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


!
! --- Case: ------------------------------------------------------
!

            TITLE =  '# greater than # (deg) #; #; ' &
            //       'fixed CNFINE stop'


            CALL REPMC ( TITLE, '#', DSCSTR,              TITLE )
            CALL REPMF ( TITLE, '#', REFVAL*DPR(), 6,'F', TITLE )
            CALL REPMC ( TITLE, '#', CRDSYS,              TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,              TITLE )


            CALL TCASE ( TITLE  )

            RELATE = '>'
            TOL    = CNVTOL
            ADJUST = 0.D0

            RPT  = .FALSE.
            BAIL = .FALSE.

!
!           Set the search step size.
!
            CALL GFSSTP ( 10000.D0 )

!
!           We're going to set the confinement window for each
!           search so we cover one reference value crossing.
!
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = ET1

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Perform the search.
!
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect to cross the VIth reference value at
!
!              24*(I-1) + 4.5 + 1.5D0*VI hours
!
!              past noon TDB.
!
!
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0

!
!           Filter the result window to remove spurious
!           intervals that may arise due to convergence
!           error.
!
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Normally we expect to find one solution interval, but
!           the result window should be empty if the constraint
!           can't be satisfied.
!
            N = WNCARD ( RESULT )

!           WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN
!
!              Make sure we found 1 interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            ELSE IF (      EQSTR( CRDSYS, CYLSYS ) &
                      .OR. EQSTR( CRDSYS, RADSYS )  ) THEN
!
!              Make sure we found 1 interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )


            ELSE IF ( EQSTR( CRDSYS, LATSYS ) &
                 .OR. EQSTR( CRDSYS, GEOSYS ) &
                 .OR. EQSTR( CRDSYS, SPHSYS )  ) THEN
!
!              The number of expected intervals depends on the
!              case index.
!
               IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
!
!                 Make sure we found no intervals.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE IF ( VI .EQ. 17 ) THEN
!
!                 Make sure we found no intervals.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
!
!                 We should have 1 interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            END IF

!
!           Now check the solution intervals.
!
            DO I = 1, N
!
!              Look up the Ith solution interval.
!
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!               WRITE (*,*) START, FINISH

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN


                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
!                    WRITE (*,*) 1, START, FINISH
!
                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!                       This is a special case: the < condition
!                       is considered to be met on the entire
!                       confinement window.
!
                        QNAME = 'Interval start  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, START, '~', T1, TIMTOL, OK )

                        XET = T2
                        ET  = FINISH

                        QNAME = 'Interval stop # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, ET,  '~', XET, TIMTOL, OK )

                     ELSE
!
!                       The start time should be T1. The stop time
!                       should be the time of the reference meridian
!                       crossing.

                        QNAME = 'Interval start  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, START, '~', T1, TIMTOL, OK )

                        XET = XTIME
                        ET  = FINISH

                        QNAME = 'Interval stop # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, ET,  '~', XET, TIMTOL, OK )

                     END IF

                  ELSE
!
!                    We don't expect to have more than one
!                    interval for this case.
!
                     CALL SETMSG ( 'Unexpected interval count: #; ' &
                     //            'VI = #'                         )
                     CALL ERRINT ( '#', N                           )
                     CALL ERRINT ( '#', VI                          )
                     CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                     CALL CHCKXC ( .FALSE., ' ', OK                 )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' ) &
                         .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
!
!                 Longitude is positive East. The branch cut is at 0.
!
                  IF ( I .EQ. 1 ) THEN
!
!                    This is the normal case.
!
                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!                       This is a special case: the > condition
!                       is considered to be met on the whole
!                       confinement window.
!
                        XET = T1
                        ET  = START

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, ET,  '~', XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        CALL CHCKSD ( QNAME, FINISH, '~', &
                                             T2, TIMTOL, OK )

                     ELSE
!
!                       The start time should be the REFVAL meridian
!                       crossing time; the end time should be T2.
!
                        XET = XTIME
                        ET  = START

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, ET,  '~', XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        CALL CHCKSD ( QNAME, FINISH, '~', &
                                             T2, TIMTOL, OK )

                     END IF

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' ) &
                         .OR. EQSTR( CRDSYS, 'GEODETIC'    ) &
                         .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
!
!                 Longitude is positive East. The branch cut is at Pi.
!
                  IF ( VI .LT. 17 ) THEN

                     IF ( I .EQ. 1 ) THEN
!
!                       This is the normal case.
!
!                       If REFVAL is in the upper half of the unit
!                       circle, including 0 and pi, the start time
!                       should be the time of the reference meridian
!                       crossing and the stop time should be ET0 +
!                       12 h.
!
!                       If REFVAL is in the lower half of the unit
!                       circle, the start time should be the time of
!                       the reference meridian crossing and the stop
!                       time should be T2 (the end of the confinement
!                       window).
!
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        XET = XTIME
                        ET  = START

                        CALL CHCKSD ( QNAME, START, '~', &
                                             XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( SIN(REFVAL) .GE. 0.D0 ) THEN


                           CALL CHCKSD ( QNAME, FINISH, '~', &
                                         ET0+SPD()/2, TIMTOL, OK )

                        ELSE
                           CALL CHCKSD ( QNAME, FINISH, '~', &
                                         T2,    TIMTOL, OK )
                        END IF


                     ELSE
!
!                       We don't expect to have more than one
!                       interval for this case.
!
                        CALL SETMSG ( 'Unexpected interval count: #; ' &
                        //            'VI = #'                         )
                        CALL ERRINT ( '#', N                           )
                        CALL ERRINT ( '#', VI                          )
                        CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                        CALL CHCKXC ( .FALSE., ' ', OK                 )

                     END IF

                  ELSE
!
!                    This case corresponds to the first interval
!                    when REFVAL is 0 and VI is 17. The first interval
!                    should be the singleton [ET0, ET0].
!
                     IF ( I .EQ. 1 ) THEN

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, START, '~', &
                                             ET0, TIMTOL, OK )

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        XET = ET0

                        ET  = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', &
                                             XET,    TIMTOL, OK )

                     ELSE IF ( I .EQ. 2 ) THEN
!
!                       This case corresponds to the second interval
!                       when REFVAL is 0 and VI is 17. The second
!                       interval should cover from ET0+12h to ET0+24h.
!
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, START,       '~', &
                                             ET0+SPD()/2, TIMTOL, OK )

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        XET = ET0+SPD()

                        ET  = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', &
                                             XET,    TIMTOL, OK )

                     ELSE
!
!                       We don't expect to have more than two
!                       intervals for this case.
!
                        CALL SETMSG ( 'Unexpected interval count: #; ' &
                        //            'VI = #'                         )
                        CALL ERRINT ( '#', N                           )
                        CALL ERRINT ( '#', VI                          )
                        CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                        CALL CHCKXC ( .FALSE., ' ', OK                 )

                     END IF

                  END IF

               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


!
!              Check the longitude at the crossing time ET.
!
!              In order to decide how accurate the coordinate should
!              be, we'll use the angular rate of Gamma together with
!              the time tolerance. Adjust the tolerance for the units
!              we're using in the CHCKSD call.
!
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


!
!              Compute the longitude of the vector we expect and
!              the longitude at FINISH.
!
               CALL SPKEZR ( TARGET, XET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, &
                                XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, &
                             ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

!
!              Adjust the longitude by 2*pi if it's too far
!              from the expected value.
!
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', &
                                    XLON*DPR(), QTOL, OK )

            END DO
!
!           We're done with the Ith interval.
!
         END DO
!
!        End of vector definition loop.
!

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
!
!     End of coordinate system loop.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END



