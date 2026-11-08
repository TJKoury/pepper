!$Procedure      F_ZZGFLNG3 ( Test GF longitude solver, part 3 )

      SUBROUTINE F_ZZGFLNG3 ( OK )

! $ Abstract
!
!     Test the GF private longitude search routine ZZGFLONG. This
!     family tests processing of relations involving relative,
!     absolute, and adjusted absolute extrema.
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
!     routine ZZGFLONG. This family tests processing of relations
!     involving relative, absolute, and adjusted absolute extrema.
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
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      INTEGER               WNCARD

      LOGICAL               EQSTR
      LOGICAL               GFBAIL

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
      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 1*CNVTOL )



      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgflong.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgflong.bsp' )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )



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
!     Number of systems that support longitude:
!
      INTEGER               NLNSYS
      PARAMETER           ( NLNSYS = 7 )

!
!     Frame definition buffer size:
!
      INTEGER               FRBFSZ
      PARAMETER           ( FRBFSZ = 100 )

!
!     Longitude sense string length:
!
      INTEGER               SNSLEN
      PARAMETER           ( SNSLEN = 4 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CRDLEN)    CNAM   ( NLNSYS )
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(SYSLEN)    CSYS   ( NLNSYS )
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(LNSIZE)    FRMTXT ( FRBFSZ )
      CHARACTER*(SNSLEN)    LONSNS
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(OPLEN)     RELATE
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      ALPHRD ( 3 )
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      COORD
      DOUBLE PRECISION      COORD0
      DOUBLE PRECISION      COORD2
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      TMPRAD ( 3 )
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( MW, NW )
      DOUBLE PRECISION      XTIME
      DOUBLE PRECISION      XTIME2

      INTEGER               CC
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
!     INTEGER               J
      INTEGER               N
      INTEGER               NLINES
      INTEGER               SI

      LOGICAL               BAIL
      LOGICAL               RPT

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CNAM   / 'Longitude', &
                                     'Right  ascension', &
                                     'Longitude', &
                                     'Longitude', &
                                     'Longitude', &
                                     'Longitude', &
                                     'Longitude'        /

      DATA                  CSYS   / 'Latitudinal', &
                                     'Ra / Dec', &
                                     'Cylindrical', &
                                     'Geodetic', &
                                     'Planetographic', &
                                     'Planetographic_2', &
                                     'Spherical'        /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFLNG3' )


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
!*    Absolute maximum tests
!*
!*********************************************************************




!
! --- Case: ------------------------------------------------------
!
!
!     We're going to find absolute maxima of the alphafixed longitude
!     of the Alpha-gamma vector.
!
!
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll loop over the various coordinate systems.
!
      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)

         IF (  EQSTR(CRDSYS, PGRSYS)  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'WEST' )

            LONSNS = 'WEST'

         ELSE IF (  EQSTR(CRDSYS, 'Planetographic_2' )  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'EAST' )

            LONSNS = 'EAST'
            CRDSYS = PGRSYS

         ELSE

            LONSNS = 'EAST'

         END IF

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

         CRDNAM = CNAM(CC)

!
!        Loop over different confinement windows.
!        We pick the window end times so that the
!        maxima will be multiples of pi/8.
!
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0
               T2 = ET0 + SPD()
!
!              Set the expected time of the maximum.
!
               XTIME = T1

            ELSE

               IF ( SI .LE. 9 ) THEN

                  T1 = ET0

               ELSE

                  T1 = ET0 + SPD()/2

               END IF

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0


!
!              Set the expected time of the maximum.
!
               XTIME = T2


            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            REFVAL = (SI-1) * PI()/8.D0

!
! --- Case: ------------------------------------------------------
!
            TITLE =  'Alpha-gamma maximum; #; #; ref val #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  )

!
!           Set parameters required for the search.
!
            VECDEF = POSDEF
            METHOD = ' '
            TARGET = 'GAMMA'
            OBSRVR = 'ALPHA'
            ABCORR = 'NONE'
            REF    = 'ALPHAFIXED'

            DREF   = ' '
            CALL CLEARD ( 3, DVEC )

            RELATE = 'ABSMAX'
            REFVAL = 0.D0

            TOL    = CNVTOL
            ADJUST = 0.D0

            CALL GFSSTP ( 10000.D0 )

            RPT    = .FALSE.
            BAIL   = .FALSE.

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
!
!                 There's a branch cut at 0: however, we've
!                 made the confinement window the whole unit
!                 circle. So we expect a result.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE IF (      EQSTR(CRDSYS,RADSYS) &
                         .OR. EQSTR(CRDSYS,PGRSYS) &
                         .OR. EQSTR(CRDSYS,CYLSYS) ) THEN
!
!                 There's a branch cut at 0: the search window is
!                 bounded away from the branch cut, while the
!                 confinement window contains nothing but the branch
!                 cut. The result window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
!
!                 The confinement window should be a singleton set
!                 at which the absolute maximum is found.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            ELSE
!
!              We expect the result window to contain one
!              solution interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF

            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!              Check that START is equal to XTIME.
!
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )

!
!              Check that START and FINISH match.
!
               QNAME = 'START vs FINISH'
               CALL CHCKSD ( QNAME, START, '~', FINISH, TIMTOL, OK )

            END IF

         END DO
!
!        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


!*********************************************************************
!*
!*    Adjusted absolute maximum tests
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
!
!     We're going to find adjusted absolute maxima of the alphafixed
!     longitude of the Alpha-gamma vector.
!
!
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ADJUST = 15.D0 * RPD() / 3600.D0

!
!     We'll loop over the various coordinate systems.
!
      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF (  EQSTR(CRDSYS, PGRSYS)  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'WEST' )

            LONSNS = 'WEST'

         ELSE IF (  EQSTR(CRDSYS, 'Planetographic_2' )  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'EAST' )

            LONSNS = 'EAST'
            CRDSYS = PGRSYS

         ELSE

            LONSNS = 'EAST'

         END IF


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

!
!        Loop over different confinement windows.
!        We pick the window end times so that the
!        maxima will be multiples of pi/8.
!
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN


               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               T2 = ET0 + SPD()
!
!              Set the expected time of the maximum and the
!              adjusted maximum. XTIME is the expected
!              start time; XTIME2 is the expected stop time.
!
               XTIME  = T1
               XTIME2 = MIN( T1 + 1.D0, T2 )

            ELSE

               IF ( SI .LE. 9 ) THEN

                  T1 = ET0

               ELSE

                  T1 = ET0 + SPD()/2

               END IF

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
!
!              Set the expected time of the maximum and the
!              adjusted maximum. XTIME is the expected

!              start time; XTIME2 is the expected stop time.

               XTIME =  MAX( T2 - 1.D0, ET0 )
               XTIME2 = T2

            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Contract the confinement window a bit to avoid
!           spurious maxima. However, don't wipe out the
!           confinement interval altogether.
!
            REFVAL = (SI-1) * PI()/8.D0



            CALL WNFETD ( CNFINE, 1, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
            TITLE =  'Alpha-gamma adjusted maximum; #; #; max lon #; ' &
            //       'ADJUST = #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL REPMF ( TITLE, '#', DPR()*ADJUST, 6, 'F', TITLE )
            CALL TCASE ( TITLE  )

!
!           Set parameters required for the search.
!
            VECDEF = POSDEF
            METHOD = ' '
            TARGET = 'GAMMA'
            OBSRVR = 'ALPHA'
            ABCORR = 'NONE'
            REF    = 'ALPHAFIXED'

            DREF   = ' '
            CALL CLEARD ( 3, DVEC )

            RELATE = 'ABSMAX'
            REFVAL = 0.D0

            TOL    = CNVTOL

            CALL GFSSTP ( 10000.D0 )

            RPT    = .FALSE.
            BAIL   = .FALSE.

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )


            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
!
!                 We expect the result window to contain one
!                 solution interval.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE

                  IF (       EQSTR(CRDSYS, PGRSYS) &
                       .OR.  EQSTR(CRDSYS, CYLSYS) &
                       .OR.  EQSTR(CRDSYS, RADSYS)  ) THEN
!
!                    These systems have a singularity at
!                    0 longitude/RA, so the result window
!                    should be empty.
!
                     CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

                  ELSE
!
!                    The result should be a singleton set.
!
                     CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

                  END IF

               END IF

            ELSE
!
!              We expect the result window to contain one
!              solution interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!              IF ( .NOT. OK ) THEN
!
!                 DO J = 1, WNCARD(CNFINE)
!                    CALL WNFETD ( CNFINE, J, START, FINISH )
!                    WRITE (*,'(2(5PE24.12))') START, FINISH
!                 END DO
!
!                 DO J = 1, WNCARD(RESULT)
!                    CALL WNFETD ( RESULT, J, START, FINISH )
!                    WRITE (*,'(2(5PE24.12))') START, FINISH
!                 END DO
!
!              END IF
!
            END IF


            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!              Check that START is equal to XTIME.
!
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )

!
!              Check that FINISH is equal to XTIME2.
!
               QNAME = 'FINISH vs XTIME2'
               CALL CHCKSD ( QNAME, FINISH, '~', XTIME2, TIMTOL, OK )

            END IF

         END DO
!
!        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO



!*********************************************************************
!*
!*    Absolute minimum tests
!*
!*********************************************************************



!
! --- Case: ------------------------------------------------------
!
!
!     We're going to find absolute minima of the alphafixed longitude
!     of the Alpha-gamma vector.
!
!
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll loop over the various coordinate systems.
!
      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF (  EQSTR(CRDSYS, PGRSYS)  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'WEST' )

            LONSNS = 'WEST'

         ELSE IF (  EQSTR(CRDSYS, 'Planetographic_2' )  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'EAST' )

            LONSNS = 'EAST'
            CRDSYS = PGRSYS

         ELSE

            LONSNS = 'EAST'

         END IF


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

!
!        Loop over different confinement windows.
!        We pick the window end times so that the
!        minima will be multiples of pi/8.
!
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
!
!              Set the expected time of the minimum.
!
               XTIME = T2

            ELSE

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               IF ( SI .LE. 8 ) THEN

                  T2 = ET0 + SPD()/2

               ELSE

                  T2 = ET0 + SPD()

               END IF


!
!              Set the expected time of the minimum.
!
               XTIME = T1


            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

             REFVAL = (SI-1) * PI()/8.D0

!
! --- Case: ------------------------------------------------------
!
            TITLE =  'Alpha-gamma abs minimum; #; #; ref val #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  )

!
!           Set parameters required for the search.
!
            VECDEF = POSDEF
            METHOD = ' '
            TARGET = 'GAMMA'
            OBSRVR = 'ALPHA'
            ABCORR = 'NONE'
            REF    = 'ALPHAFIXED'

            DREF   = ' '
            CALL CLEARD ( 3, DVEC )

            RELATE = 'ABSMIN'
            REFVAL = 0.D0

            TOL    = CNVTOL
            ADJUST = 0.D0

            CALL GFSSTP ( 10000.D0 )

            RPT    = .FALSE.
            BAIL   = .FALSE.

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
!
!                 There's a branch cut at 0: the confinement window
!                 lies in the deleted sector that encloses the branch
!                 cut. The result window should be empty.
!
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE IF (      EQSTR(CRDSYS,RADSYS) &
                         .OR. EQSTR(CRDSYS,PGRSYS) &
                         .OR. EQSTR(CRDSYS,CYLSYS) ) THEN
!
!                 There's a branch cut at 0: however we've made
!                 the confinement window cover the upper half
!                 circle. The result window should be non-empty.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE
!
!                 The confinement window should be a singleton set
!                 at which the absolute minimum is found.
!
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            ELSE
!
!              We expect the result window to contain one
!              solution interval.
!
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF

            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

!              Check that START is equal to XTIME.
!
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )

!
!              Check that START and FINISH match.
!
               QNAME = 'START vs FINISH'
               CALL CHCKSD ( QNAME, START, '~', FINISH, TIMTOL, OK )

            END IF

         END DO
!
!        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO



!*********************************************************************
!*
!*    Adjusted absolute minimum tests
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
!
!     We're going to find adjusted absolute minima of the alphafixed
!     longitude of the Alpha-gamma vector. In each case we'll use
!     an adjustment value of 15 arcseconds, which should shift the
!     minima by 1 second.
!
!
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ADJUST = 15.D0 * RPD() / 3600.D0

!
!     We'll loop over the various coordinate systems.
!

      DO CC = 1, NLNSYS
!
!        Set the coordinate system and coordinate.
!
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF (  EQSTR(CRDSYS, PGRSYS)  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'WEST' )

            LONSNS = 'WEST'

         ELSE IF (  EQSTR(CRDSYS, 'Planetographic_2' )  ) THEN

            CALL PCPOOL ( 'BODY1000_PGR_POSITIVE_LON', 1, 'EAST' )

            LONSNS = 'EAST'
            CRDSYS = PGRSYS

         ELSE

            LONSNS = 'EAST'

         END IF


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

!
!        Loop over different confinement windows.
!        We pick the window end times so that the
!        minima will be multiples of pi/4.
!
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0
               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
!
!              Set the expected time of the minimum
!              and the adjusted minimum.
!
               XTIME  = MAX( T2 - 1.D0, T1 )
               XTIME2 = T2

            ELSE

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               IF ( SI .LE. 8 ) THEN

                  T2 = ET0 + SPD()/2

               ELSE

                  T2 = ET0 + SPD()

               END IF
!
!              Set the expected time of the minimum
!              and the adjusted minimum.
!
               XTIME  = T1
               XTIME2 = MIN( T1 + 1.D0, T2 )

            END IF

            CALL SCARDD ( 0,      CNFINE )

            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )


            REFVAL = (SI-1) * PI()/8.D0

            IF ( EQSTR( CRDSYS, PGRSYS ) ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF

!
! --- Case: ------------------------------------------------------
!
            TITLE =  'Adjusted alpha-gamma minimum; #; #; #; min lon #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*ADJUST, 6, 'F', TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  )

!
!           Set parameters required for the search.
!
            VECDEF = POSDEF
            METHOD = ' '
            TARGET = 'GAMMA'
            OBSRVR = 'ALPHA'
            ABCORR = 'NONE'
            REF    = 'ALPHAFIXED'

            DREF   = ' '
            CALL CLEARD ( 3, DVEC )

            RELATE = 'ABSMIN'
            REFVAL = 0.D0

            TOL    = CNVTOL

            CALL GFSSTP ( 10000.D0 )

            RPT    = .FALSE.
            BAIL   = .FALSE.

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                            GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                            BAIL,   GFBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           We expect the result window to contain one
!           solution interval.
!
            IF (  LONSNS .EQ. 'WEST'  ) THEN

               IF ( SI .GT. 1 ) THEN

                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
               ELSE
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )
               END IF

            ELSE

               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF


            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Check that START is equal to XTIME.
!
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )

!
!              Check that FINISH is equal to XTIME2.
!
               QNAME = 'FINISH vs XTIME2'
               CALL CHCKSD ( QNAME, FINISH, '~', XTIME2, TIMTOL, OK )

            END IF

         END DO
!
!        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
!
!           Restore Alpha's radii.
!
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


!*********************************************************************
!*
!*    Local maximum/minimum tests
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
!
!     Local max/min tests:
!
!     We're going to find local extrema of the J2000 longitude
!     of the Sun-IO vector. Jupiter's orbital angular rate is
!     slow enough so that Io exhibits periods of retrograde
!     motion.
!
!     In order to be able to use geodetic and planetographic
!     coordinate systems, we need a reference frame aligned with
!     the J2000 frame, but whose center is the Sun. We create
!     such a frame specification below and insert it into the
!     kernel pool via LMPOOL.
!
      FRMTXT(1) = 'FRAME_SUNJ2000             = 1400000'
      FRMTXT(2) = 'FRAME_1400000_NAME         = ''SUNJ2000'' '
      FRMTXT(3) = 'FRAME_1400000_CLASS        = 4'
      FRMTXT(4) = 'FRAME_1400000_CLASS_ID     = 1400000'
      FRMTXT(5) = 'FRAME_1400000_CENTER       = 10'
      FRMTXT(6) = 'TKFRAME_1400000_RELATIVE   = ''J2000'' '
      FRMTXT(7) = 'TKFRAME_1400000_SPEC       = ''MATRIX'' '
      FRMTXT(8) = 'TKFRAME_1400000_MATRIX     = ( 1,0,0,0,1,0,0,0,1 )'
      FRMTXT(9) = 'OBJECT_SUN_FRAME           = ''SUNJ2000'' '

      NLINES = 9

      CALL LMPOOL ( FRMTXT, NLINES )

!
!     Our confinement window will sample from Jupiter's orbit
!     in such a way that all branches of ZZGFLONG's local
!     extremum algorithm will be exercised. We'll use the
!     first few days of each year out of a 12 year period
!     as our confinement window.
!
      CALL STR2ET ( '2000 JAN 1 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 12

         T1 = ET0 +  (I-1)   * JYEAR()
         T2 = T1  +   3.5D0  * SPD()

         CALL WNINSD ( T1, T2, CNFINE )

      END DO

!
!     We'll loop over the various coordinate systems.
!
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

!
! --- Case: ------------------------------------------------------
!
         TITLE =  'Sun-Io local maximum #; #'

         CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
         CALL REPMC ( TITLE, '#', CRDNAM, TITLE )

         CALL TCASE ( TITLE  )


         IF (  EQSTR(CRDSYS, PGRSYS)  ) THEN

            CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON', 1, 'WEST' )

            LONSNS = 'WEST'

         ELSE IF (  EQSTR(CRDSYS, 'Planetographic_2' )  ) THEN

            CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON', 1, 'EAST' )

            LONSNS = 'EAST'
            CRDSYS = PGRSYS

         ELSE

            LONSNS = 'EAST'

         END IF


!
!        Set parameters required for the search.
!
         VECDEF = POSDEF
         METHOD = ' '
         TARGET = 'IO'
         OBSRVR = 'SUN'
         ABCORR = 'NONE'
         REF    = 'SUNJ2000'

         DREF   = ' '
         CALL CLEARD ( 3, DVEC )

         RELATE = 'LOCMAX'
         REFVAL = 0.D0

!
!        Use a loose tolerance to speed things up a bit.
!
         TOL    = 1.D-2
         ADJUST = 0.D0

         CALL GFSSTP ( 5*3600.D0 )

         RPT    = .FALSE.
         BAIL   = .FALSE.

         CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                         OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                         RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                         GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                         BAIL,   GFBAIL, MW,     NW,     WORK, &
                         CNFINE, RESULT                         )
         CALL CHCKXC ( .FALSE., ' ', OK )

         N = WNCARD( RESULT )

!
!        We expect each confinement interval to contain two
!        solutions, so we expect 24 solutions in all.
!
         CALL CHCKSI ( 'N (0)', N, '=', 24, 0, OK )

         DO I = 1, N

            CALL WNFETD  ( RESULT, I, START, FINISH )

            CALL ETCAL   ( FINISH, TIMSTR )

!
!           Check the coordinate at the event time +/- TDELTA
!           seconds.
!
            TDELTA = 3.D0

            CALL ZZGFCOG ( FINISH-TDELTA, COORD0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFCOG ( FINISH, COORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFCOG ( FINISH+TDELTA, COORD2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            QNAME = '#: COORD vs COORD0'
            CALL REPMI ( QNAME, '#', I, QNAME )

            IF ( LONSNS .EQ. 'WEST'  ) THEN

               CALL CHCKSD ( QNAME, COORD, '<', COORD0, 0.D0, OK )
            ELSE
               CALL CHCKSD ( QNAME, COORD, '>', COORD0, 0.D0, OK )
            END IF


            QNAME = '#: COORD2 vs COORD'
            CALL REPMI ( QNAME, '#', I, QNAME )

            IF (  LONSNS .EQ. 'WEST'  ) THEN

               CALL CHCKSD ( QNAME, COORD, '<', COORD2, 0.D0, OK )
            ELSE
               CALL CHCKSD ( QNAME, COORD, '>', COORD2, 0.D0, OK )
            END IF

         END DO
!
!        End of extreme value check loop.
!


!
! --- Case: ------------------------------------------------------
!
         TITLE =  'Sun-Io local minimum #; #'

         CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
         CALL REPMC ( TITLE, '#', CRDNAM, TITLE )

         CALL TCASE ( TITLE  )

!
!        Set parameters required for the search.
!
         VECDEF = POSDEF
         METHOD = ' '
         TARGET = 'IO'
         OBSRVR = 'SUN'
         ABCORR = 'NONE'
         REF    = 'SUNJ2000'

         DREF   = ' '
         CALL CLEARD ( 3, DVEC )

         RELATE = 'LOCMIN'
         REFVAL = 0.D0

!
!        Use a loose tolerance to speed things up a bit.
!
         TOL    = 1.D-2
         ADJUST = 0.D0

         CALL GFSSTP ( 5*3600.D0 )

         RPT    = .FALSE.
         BAIL   = .FALSE.

         CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                         OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                         RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                         GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                         BAIL,   GFBAIL, MW,     NW,     WORK, &
                         CNFINE, RESULT                         )
         CALL CHCKXC ( .FALSE., ' ', OK )


         N = WNCARD( RESULT )

!
!        We expect each confinement interval to contain two
!        solutions, so we expect 24 solutions in all.
!
         CALL CHCKSI ( 'N (0)', N, '=', 24, 0, OK )

         DO I = 1, N

            CALL WNFETD  ( RESULT, I, START, FINISH )

            CALL ETCAL   ( FINISH, TIMSTR )

!
!           Check the coordinate at the event time +/- TDELTA
!           seconds.
!
            TDELTA = 3.D0

            CALL ZZGFCOG ( FINISH-TDELTA, COORD0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFCOG ( FINISH, COORD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFCOG ( FINISH+TDELTA, COORD2 )
            CALL CHCKXC ( .FALSE., ' ', OK )


            QNAME = '#: COORD vs COORD0'
            CALL REPMI ( QNAME, '#', I, QNAME )

            IF ( LONSNS .EQ. 'WEST' ) THEN

               CALL CHCKSD ( QNAME, COORD, '>', COORD0, 0.D0, OK )
            ELSE
               CALL CHCKSD ( QNAME, COORD, '<', COORD0, 0.D0, OK )
            END IF


            QNAME = '#: COORD2 vs COORD'
            CALL REPMI ( QNAME, '#', I, QNAME )

            IF ( LONSNS .EQ. 'WEST' ) THEN

               CALL CHCKSD ( QNAME, COORD, '>', COORD2, 0.D0, OK )
            ELSE
               CALL CHCKSD ( QNAME, COORD, '<', COORD2, 0.D0, OK )
            END IF

         END DO
!
!        End of extreme value check loop.
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



