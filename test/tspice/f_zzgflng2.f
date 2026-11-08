C$Procedure      F_ZZGFLNG2 ( Test GF longitude solver, part 2 )
  
      SUBROUTINE F_ZZGFLNG2 ( OK )

C$ Abstract
C
C     Test the GF private longitude search routine ZZGFLONG. This
C     family tests equality and inequality relation processing.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB GF coordinate search
C     routine ZZGFLONG. This family tests equality and inequality
C     relation processing.
C
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
C
C        Added explicit declaration of GFBAIL type.
C
C-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      INTEGER               WNCARD

      LOGICAL               EQSTR
      LOGICAL               GFBAIL
      LOGICAL               ODD

C
C     EXTERNAL declarations
C
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFBAIL
      
C     
C     Local parameters
C
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

C
C     Maximum length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )
      
C
C     Maximum length of a coordinate name.
C
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

C
C     Number of aberration correction choices:
C
      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

C
C     Number of systems that support longitude:
C
      INTEGER               NLNSYS
      PARAMETER           ( NLNSYS = 6 )

C
C     Number of vector definitions:
C
      INTEGER               NVDEF
      PARAMETER           ( NVDEF = 3 )
 
C
C     Local variables
C
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

C
C     Save everything.
C
      SAVE
 
C
C     Initial values 
C
      DATA                  CORR   / 'None',   
     .                               'Lt',     'cN', 
     .                               'XlT',    'xCn',
     .                               'LT + s', 'xLt+ S', 
     .                               'xCn',    ' XcN +s' /

      DATA                  CNAM   / 'Longitude',
     .                               'Right  ascension',
     .                               'Longitude',
     .                               'Longitude',
     .                               'Longitude',
     .                               'Longitude'        /

      DATA                  CSYS   / 'Latitudinal',
     .                               'Ra / Dec',
     .                               'Cylindrical',
     .                               'Geodetic',
     .                               'Planetographic',
     .                               'Spherical'        /

      DATA                  VDEF  / POSDEF, SOBDEF, SINDEF /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFLNG2' )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load a PCK.
C     
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

C
C     Load an SPK file as well.
C     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load Nat's solar system SPK and PCK/FK
C     kernels.
C     
      CALL NATPCK ( NPCK, .TRUE., .FALSE. )
      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      
C
C     Actual DE-based ephemerides yield better comparisons,
C     since these ephemerides have less noise than do those
C     produced by TSTSPK.
C     
C      CALL FURNSH ( 'de421.bsp' )
C      CALL FURNSH ( 'jup230.bsp' )
C 



C*********************************************************************
C*
C*    Normal cases --- Nat's solar system
C*
C*********************************************************************


C*********************************************************************
C*
C*    Equality cases
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C
C
C     Equality tests:
C
C     We want at least one case that exercises every combination of
C     aberration correction and coordinate system. This set of tests
C     does that. Later on, we'll test a more complete set of equality
C     constraints.
C
C     We're going to find times when the Sun-Beta vector crosses the
C     ALPHA_VIEW_XY prime meridian. This should occur every 24 hours,
C     starting at the J2000 epoch.
C
C     Loop over the aberration corrections.
C
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
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO AC = 1, NCORR

C
C --- Case: ------------------------------------------------------
C
            ABCORR = CORR(AC)            

            TITLE =  'Sun-Beta vector crosses the ALPHA_VIEW_XY prime '
     .      //       'meridian. #; #; #'

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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure we found 1 root.
C
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C           Make sure that the start and finish times are
C           those we expect. The crossing should occur at
C           the midpoint of the occultation of ALPHA by BETA,
C           which is 5 minutes after noon TDB, plus or minus
C           the light time offset.
C
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

C
C           Check the longitude at the crossing time. 
C
C           In order to decide how accurate the coordinate should be,
C           we'll use the angular rate of Beta together with the time 
C           tolerance.
C
            QTOL = ( TWOPI() / SPD() ) * CNVTOL

            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKEZR ( TARGET, START,  REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL RECLAT ( STATE, R, LON, LAT )

               QNAME = 'Longitude # (0)'
               CALL REPMI ( QNAME, '#', I, QNAME )

               CALL CHCKSD ( QNAME, LON, '~', 0.D0, QTOL, OK )

            END DO

         END DO
C
C        End of aberration correction loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C


C
C --- Case: ------------------------------------------------------
C
C
C     This set of sets exercises every combination of vector definition
C     and coordinate system; we don't vary the aberration correction.
C     For sub-observer points, we vary the method but don't test every
C     combination of method and coordinate system.
C
C     We're going to find times when the each of the following vectors
C     crosses the
C     ALPHAFIXED prime meridian:
C
C        1) Position of Gamma with respect to Alpha
C
C        2) Sub-Gamma point on Alpha
C
C        3) Intercept of Gamma-to-Alpha ray on surface of Alpha
C
C     This should occur every 24 hours, starting at 6 hours past the
C     J2000 epoch.
C
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, NVDEF
C
C           Set the vector definition.
C
            VECDEF = VDEF(VI)

C
C           Set the definition method and ray's
C           direction.
C
            IF ( VECDEF .EQ. POSDEF ) THEN
C
C              Note that for positions, the vector
C              is the *opposite* of the sub-observer 
C              or position vector. It's easiest to
C              switch observer and target for this case.
C
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
C
C              Vary the sub-observer point definition.
C               
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

C
C              The pointing direction is along the gammafixed
C              frame's +X axis.
C
               CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )
            
            ELSE
C
C              This is a backstop case.
C
               CALL SETMSG ( 'VECDEF = #' )
               CALL ERRCH  ( '#', VECDEF  )
               CALL SIGERR ( 'SPICE(TSPICEBUG)' )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

      
C
C --- Case: ------------------------------------------------------
C

            TITLE =  '# on Alpha 0 '
     .      //       'meridian. #; #; #; #; #'

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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure we found 2 roots.
C
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


            DO I = 1, N
C
C              We expect to cross the prime meridian at 6 pm TDB.
C               
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

C
C           Check the longitude at the crossing time. 
C
C           In order to decide how accurate the coordinate should be,
C           we'll use the angular rate of Gamma together with the time 
C           tolerance.
C
            QTOL = ( TWOPI() / SPD() ) * CNVTOL

            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Compute the longitude of the vector we want.
C              Because of the special geometry we've set up,
C              we can use the observer-target state for all
C              cases; just negate the position for the 
C              sub-observer and intercept definitions.
C
               CALL SPKEZR ( TARGET, START,  REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF (      ( VECDEF .EQ. SOBDEF ) 
     .              .OR. ( VECDEF .EQ. SINDEF ) ) THEN

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
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C



C
C --- Case: ------------------------------------------------------
C
C
C     More '=' cases follow.
C
C     At this point we've tested ZZGFLONG's ability to handle
C     vector definitions and aberration corrections in combination
C     with different coordinate systems. Now we want to perform
C     more robust tests on the root finding aspects of ZZGFLONG.
C     We'll stick with geometric position vectors from now on,
C     but we'll test each coordinate system.
C
C     Now we want to find the times when the Alpha-Gamma vector's
C     longitude hits each integer multiple of pi/8.
C     This should occur every 90 minutes.
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 18
C
C           Loop over each reference value.
C
            REFVAL = (VI-1) * PI() / 8.D0

C
C           For planetographic coordinates, set the reference
C           value to indicate the same position on the unit
C           circle as that used for the other systems.
C
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

C
C --- Case: ------------------------------------------------------
C

            TITLE =  '# on Alpha # (deg) '
     .      //       'meridian. #; #'

            
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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           We're going to set the confinement window for each 
C           search so we find two roots.
C
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0
            T2 = T1  + 32     * 5400.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure we found 2 roots.
C
            N = WNCARD ( RESULT )
            CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


            DO I = 1, N
C
C              We expect to cross the VIth reference value at 
C
C                 24*(I-1) + 4.5 + 1.5*VI hours
C
C              past noon TDB.
C               
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

C
C           Check the longitude at the crossing time. 
C
C           In order to decide how accurate the coordinate should be,
C           we'll use the angular rate of Gamma together with the time 
C           tolerance. Adjust the tolerance for the units we're using
C           in the CHCKSD call.
C
            QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL


            DO I = 1, N

               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Compute the longitude of the vector we want.
C
               CALL SPKEZR ( TARGET, START,  REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF

               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

C
C              Adjust the longitude by 2*pi if it's too far
C              from the expected value.
C
               IF (  LON  .GT. ( REFVAL + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( REFVAL - 1.D0 )  ) THEN
                  
                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', 
     .                              REFVAL*DPR(), QTOL, OK )

            END DO

         END DO
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C



C*********************************************************************
C*
C*    "Less than" cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
C
C     Now we're going to work with inequality constraints.
C
C     We're going to start out with some very simple cases. We want to
C     find the times when the Alpha-Gamma vector's longitude is less
C     than each integer multiple of pi/8, where the confinement window
C     extends 1 second before and after the crossing time. Crossings of
C     these meridians should occur every 90 minutes.
C
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
C
C           Loop over each reference value. Include both 0 and 2*pi.
C
            REFVAL = (VI-1) * PI() / 8.D0

C
C           For planetographic coordinates, set the reference
C           value to indicate the same position on the unit
C           circle as that used for the other systems.
C
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


C
C --- Case: ------------------------------------------------------
C

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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           We're going to set the confinement window for each 
C           search so we cover one reference value crossing.
C          
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = T1                      + 2.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Filter the result window to remove spurious
C           intervals that may arise due to convergence
C           error.
C
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect to cross the VIth reference value at 
C
C              4.5 + 1.5D0*VI hours
C
C           past noon TDB.
C               
C
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


C
C           Normally we expect to find one solution interval, but
C           the result window should be empty if the constraint
C           can't be satisfied.
C           
            N = WNCARD ( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C              WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (      EQSTR( CRDSYS, CYLSYS )
     .           .OR. EQSTR( CRDSYS, PGRSYS )
     .           .OR. EQSTR( CRDSYS, RADSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C                 The condition 
C
C                    longitude < REFVAL
C
C                 is never satisfied, so the window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
C
C                 Make sure we found 1 interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            ELSE

C
C              Make sure we found 1 interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF

C
C           Note: in the code below, we don't expect to find
C           result windows containing multiple intervals. 
C           But we format the code to handle multiple intervals
C           so we can easily re-use the code in cases where
C           multiple intervals are expected.
C
C
C           Now check the solution intervals. 
C
            DO I = 1, N
C
C              Look up the Ith solution interval.
C
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )
  

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN
 
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    WRITE (*,*) 1, START, FINISH
C
C                    The stop time should be T2; the start time should
C                    be the time of the reference meridian crossing.
C
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, START, '~', XTIME, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, FINISH,  '~', T2, TIMTOL, OK )


                     XET = XTIME
                     ET  = START

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' )
     .                   .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
C
C                 Longitude is positive East. The branch cut is at 0.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    The start time should be T1. If the reference
C                    value is at the branch cut, the stop time should
C                    be T2. Otherwise, the stop time should be the time
C                    of the reference meridian crossing.
C                    
C
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

                     CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                    XET, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' )
     .                   .OR. EQSTR( CRDSYS, 'GEODETIC'    )
     .                   .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
C
C                 Longitude is positive East. The branch cut is at Pi.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    The start time should be T1. If the reference
C                    value is at the branch cut, the stop time should
C                    be T2. Otherwise, the stop time should be the time
C                    of the reference meridian crossing.
C                    
C
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

                     CALL CHCKSD ( QNAME, ET, '~', 
     .                                    XET,    TIMTOL, OK )

                  END IF

               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


C
C              Check the longitude at the crossing time ET.
C
C              In order to decide how accurate the coordinate should
C              be, we'll use the angular rate of Gamma together with
C              the time tolerance. Adjust the tolerance for the units
C              we're using in the CHCKSD call.
C
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL
        

C
C              Compute the longitude of the vector we expect and
C              the longitude at FINISH.
C
               CALL SPKEZR ( TARGET, XET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, 
     .                          XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

C
C              Adjust the longitude by 2*pi if it's too far
C              from the expected value.
C
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', 
     .                              XLON*DPR(), QTOL, OK )

            END DO
C
C           We're done with the Ith interval.
C
         END DO
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C



C
C --- Case: ------------------------------------------------------
C
C     Repeat the previous case, but this time keep the confinement
C     window start time fixed at 18:00 Jan 1 2000 TDB. We want to find
C     the times when the Alpha-Gamma vector's longitude is less than
C     each integer multiple of pi/8, where the confinement window
C     extends 1 second before and after the crossing time. Crossings of
C     these meridians should occur every 90 minutes.
C
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF


         DO VI = 1, 17
C
C           Loop over each reference value. Include both 0 and 2*pi.
C
            REFVAL = (VI-1) * PI() / 8.D0

C
C           For planetographic coordinates, set the reference
C           value to indicate the same position on the unit
C           circle as that used for the other systems.
C
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


C
C --- Case: ------------------------------------------------------
C

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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           We're going to set the confinement window for each 
C           search so we cover one reference value crossing.
C          
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + CNVTOL/10.D0
            T2 = ET0 + (VI-1)*5400.D0 + 1.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )


C
C           Filter the result window to remove spurious
C           intervals that may arise due to convergence
C           error.
C
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )



C
C           We expect to cross the VIth reference value at 
C
C              4.5 + 1.5D0*VI hours
C
C              past noon TDB.
C               
C
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


C
C           Normally we expect to find one solution interval, but
C           the result window should be empty if the constraint
C           can't be satisfied.
C           
            N = WNCARD ( RESULT )

C           WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL



            IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C                 The condition 
C
C                    longitude < REFVAL
C
C                 is never satisfied, so the window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
C
C                 Make sure we found 1 interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            ELSE IF (      EQSTR( CRDSYS, CYLSYS )
     .                .OR. EQSTR( CRDSYS, RADSYS )  ) THEN


               IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C                 The condition 
C
C                    longitude < REFVAL
C
C                 is never satisfied, so the window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE
C
C                 Make sure we found 1 interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            ELSE IF ( EQSTR( CRDSYS, CYLSYS )
     .           .OR. EQSTR( CRDSYS, RADSYS )  ) THEN
C
C              Make sure we found 1 interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )


            ELSE IF ( EQSTR( CRDSYS, LATSYS )
     .           .OR. EQSTR( CRDSYS, GEOSYS )
     .           .OR. EQSTR( CRDSYS, SPHSYS )  ) THEN
C
C              The number of expected intervals depends on the
C              case index.
C          
               IF ( VI .EQ. 1 ) THEN
C
C                 The condition 
C
C                    longitude < REFVAL
C
C                 is never satisfied, so the window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )
C
               ELSE
C    
C                 Make sure we found 1 interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            END IF

C
C           Now check the solution intervals.
C
            DO I = 1, N
C
C              Look up the Ith solution interval.
C
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN

                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    WRITE (*,*) 1, START, FINISH
C
C                    The stop time should be T2; the start time should
C                    be the time of the reference meridian crossing.
C
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME ) 

                     CALL CHCKSD( QNAME, START, '~', XTIME, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, FINISH,  '~', T2, TIMTOL, OK )


                     XET = XTIME
                     ET  = START

                  ELSE
C
C                    We don't expect to have more than one
C                    interval for this case.
C
                     CALL SETMSG ( 'Unexpected interval count: #; '
     .               //            'VI = #'                         )
                     CALL ERRINT ( '#', N                           )
                     CALL ERRINT ( '#', VI                          )
                     CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                     CALL CHCKXC ( .FALSE., ' ', OK                 )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' )
     .                   .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
C
C                 Longitude is positive East. The branch cut is at 0.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    The start time should be T1. If the reference
C                    value is at the branch cut, the stop time should
C                    be T2. Otherwise, the stop time should be the time
C                    of the reference meridian crossing.
C                    
C
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

                     CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                    XET, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' )
     .                   .OR. EQSTR( CRDSYS, 'GEODETIC'    )
     .                   .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
C
C                 Longitude is positive East. The branch cut is at Pi.
C
                     IF ( I .EQ. 1 ) THEN
C
C                       This is the normal case.
C
C                       If REFVAL is in the open upper half of the unit
C                       circle, the start time should be ET0 and the
C                       stop time should be the time of the reference
C                       meridian crossing.
C
C                       If REFVAL is pi, the solution set includes the
C                       interval from XTIME to T2. We expect this
C                       interval to be merged with the interval for the
C                       top half of the circle, so the expected end
C                       time becomes T2.
C
C                       Otherwise, the first interval should be the
C                       singleton [ET0, ET0].
C
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( SIN(REFVAL) .GT. 0.D0 ) THEN

                           CALL CHCKSD ( QNAME, START, '~', 
     .                                          ET0, TIMTOL, OK )
                        ELSE
                           CALL CHCKSD ( QNAME, START, '~', 
     .                                       ET0+SPD()/2, TIMTOL, OK )
                        END IF

C                        IF ( .NOT. OK ) THEN
C                           WRITE (*,*) 'VI = ', VI
C                        END IF

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
                           XET = T2
                        ELSE
                           XET = XTIME
                        END IF

                        ET = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', 
     .                                       XET,    TIMTOL, OK )
                     ELSE
C
C                       We don't expect to have more than one
C                       interval for this case.
C
                        CALL SETMSG ( 'Unexpected interval count: #; '
     .                  //            'VI = #'                         )
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


C
C              Check the longitude at the crossing time ET.
C
C              In order to decide how accurate the coordinate should
C              be, we'll use the angular rate of Gamma together with
C              the time tolerance. Adjust the tolerance for the units
C              we're using in the CHCKSD call.
C
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL
        

C
C              Compute the longitude of the vector we expect and
C              the longitude at FINISH.
C
               CALL SPKEZR ( TARGET, XET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, 
     .                          XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

C
C              Adjust the longitude by 2*pi if it's too far
C              from the expected value.
C
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', 
     .                              XLON*DPR(), QTOL, OK )

            END DO
C
C           We're done with the Ith interval.
C
         END DO
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C

C*********************************************************************
C*
C*    "Greater than" cases
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C

C
C     '>' Tests
C
C     We're going to try some very simple '>' cases. We want to
C     find the times when the Alpha-Gamma vector's longitude is greater
C     than each integer multiple of pi/8, where the confinement window
C     extends 1 second before and after the crossing time. Crossings of
C     these meridians should occur every 90 minutes.
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
C
C           Loop over each reference value. Include both 0 and 2*pi.
C
            REFVAL = (VI-1) * PI() / 8.D0

C
C           For planetographic coordinates, set the reference
C           value to indicate the same position on the unit
C           circle as that used for the other systems.
C
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF

            IF ( REFVAL .EQ. TWOPI() ) THEN

               REFVAL = 0.D0

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


C
C --- Case: ------------------------------------------------------
C

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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           We're going to set the confinement window for each 
C           search so we cover one reference value crossing.
C          
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = T1                      + 2.D0

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )



C
C           We expect to cross the VIth reference value at 
C
C              24*(I-1) + 4.5 + 1.5D0*VI hours
C
C              past noon TDB.
C               
C
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0


C
C           Filter the result window to remove spurious
C           intervals that may arise due to convergence
C           error.
C
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Normally we expect to find one solution interval, but
C           the result window should be empty if the constraint
C           can't be satisfied.
C           
            N = WNCARD ( RESULT )

C              WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (      EQSTR( CRDSYS, CYLSYS )
     .           .OR. EQSTR( CRDSYS, PGRSYS )
     .           .OR. EQSTR( CRDSYS, RADSYS )  ) THEN

C
C              Make sure we found 1 interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            ELSE

               IF ( COS(REFVAL) .EQ. -1 ) THEN
C
C                 We don't expect to find any intervals.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
C
C                 Make sure we found 1 interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            END IF

C
C           Note: in the code below, we don't expect to find
C           result windows containing multiple intervals. 
C           But we format the code to handle multiple intervals
C           so we can easily re-use the code in cases where
C           multiple intervals are expected.
C
C
C           Now check the solution intervals. 
C
            DO I = 1, N
C
C              Look up the Ith solution interval.
C
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )
  

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN
 
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    WRITE (*,*) 1, START, FINISH
C
                     IF ( COS(REFVAL) .EQ. 1 ) THEN
C
C                       The stop time actually should be T2, since the
C                       interval from T1 to XTIME satisfies the
C                       inequality.
C
                        XET = T2

                     ELSE
C
C                       The start time should be XTIME. The stop time
C                       should be T2.
C                    
                        XET = XTIME

                     END IF


C                    The start time should be T1; the stop time should
C                    be the time of the reference meridian crossing.
C
                     QNAME = 'Interval start  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD( QNAME, START, '~', T1, TIMTOL, OK )

                     QNAME = 'Interval stop # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = FINISH

                     CALL CHCKSD( QNAME, ET,   '~', 
     .                                   XET,  TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' )
     .                   .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
C
C                 Longitude is positive East. The branch cut is at 0.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
                     IF ( COS(REFVAL) .EQ. 1 ) THEN
C
C                       The start time actually should be T1, since the
C                       interval from T1 to XTIME satisfies the
C                       inequality.
C
                        XET = T1

                     ELSE
C
C                       The start time should be XTIME. The stop time
C                       should be T2.
C                    
                        XET = XTIME

                     END IF

                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = START

                     CALL CHCKSD ( QNAME, ET,    '~', 
     .                             XET,   TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     
                     CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                    T2, TIMTOL, OK )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' )
     .                   .OR. EQSTR( CRDSYS, 'GEODETIC'    )
     .                   .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
C
C                 Longitude is positive East. The branch cut is at Pi.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    The start time should be XTIME. The stop time
C                    should be T2.
C                    
                     QNAME = 'Interval start # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     ET  = START
                     XET = XTIME

                     CALL CHCKSD ( QNAME, ET,  '~', 
     .                                    XET, TIMTOL, OK )


                     QNAME = 'Interval stop  # (0)'
                     CALL REPMI ( QNAME, '#', 1, QNAME )

                     CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                    T2,     TIMTOL, OK )

                  END IF

               ELSE
                  CALL SETMSG ( 'Unexpected system: #' )
                  CALL ERRCH  ( '#', CRDSYS            )
                  CALL SIGERR ( 'SPICE(TSPICEBUG)'     )
                  CALL CHCKXC ( .FALSE., ' ', OK       )
               END IF


C
C              Check the longitude at the crossing time ET.
C
C              In order to decide how accurate the coordinate should
C              be, we'll use the angular rate of Gamma together with
C              the time tolerance. Adjust the tolerance for the units
C              we're using in the CHCKSD call.
C
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL
        

C
C              Compute the longitude of the vector we expect and
C              the longitude at FINISH.
C
               CALL SPKEZR ( TARGET, XET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, 
     .                          XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

C
C              Adjust the longitude by 2*pi if it's too far
C              from the expected value.
C
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', 
     .                              XLON*DPR(), QTOL, OK )

            END DO
C
C           We're done with the Ith interval.
C
         END DO
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C



C
C --- Case: ------------------------------------------------------
C
C     Repeat the previous case, but this time keep the confinement
C     window stop time fixed at 18:00 Jan 2 2000 TDB. We want to
C     find the times when the Alpha-Gamma vector's longitude is greater
C     than each integer multiple of pi/8. Crossings of
C     these meridians should occur every 90 minutes.
C
C
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
C
C        Set the coordinate system and coordinate.
C
         CRDSYS = CSYS(CC)
         CRDNAM = CNAM(CC)


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Set the longer equatorial radius of Alpha equal to 
C           the shorter: unequal equatorial radii are not supported
C           for these systems.
C
            CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Note: use of the subscript "2" in the first argument
C           below is intentional.
C
            CALL VPACK  ( ALPHRD(2), ALPHRD(2), ALPHRD(3), TMPRAD )

            CALL PDPOOL ( 'BODY1000_RADII', 3, TMPRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         DO VI = 1, 17
C
C           Loop over each reference value. Include both 0 and 2*pi.
C
            REFVAL = (VI-1) * PI() / 8.D0

C
C           For planetographic coordinates, set the reference
C           value to indicate the same position on the unit
C           circle as that used for the other systems.
C
            IF (  EQSTR(CRDSYS, PGRSYS )  ) THEN

               REFVAL = TWOPI() - REFVAL

            END IF


            IF ( REFVAL .LT. 0.D0 ) THEN
               REFVAL = REFVAL + TWOPI()
            END IF


C
C --- Case: ------------------------------------------------------
C

            TITLE =  '# greater than # (deg) #; #; '
     .      //       'fixed CNFINE stop'

            
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

C
C           Set the search step size.
C
            CALL GFSSTP ( 10000.D0 )

C
C           We're going to set the confinement window for each 
C           search so we cover one reference value crossing.
C          
            CALL SCARDD ( 0, CNFINE )

            T1 = ET0 + (VI-1) * 5400.D0  - 1.D0
            T2 = ET1

            CALL WNINSD ( T1, T2,   CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Perform the search.
C
            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, 
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, 
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                          )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect to cross the VIth reference value at 
C
C              24*(I-1) + 4.5 + 1.5D0*VI hours
C
C              past noon TDB.
C               
C
            XTIME = ( 1.5D0*VI + 4.5D0 ) * 3600.D0

C
C           Filter the result window to remove spurious
C           intervals that may arise due to convergence
C           error.
C
            CALL WNFLTD ( TOL, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Normally we expect to find one solution interval, but
C           the result window should be empty if the constraint
C           can't be satisfied.
C           
            N = WNCARD ( RESULT )

C           WRITE (*,'(A,I15,E25.16)') 'N, REFVAL = ', N, REFVAL

            IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN
C
C              Make sure we found 1 interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            ELSE IF (      EQSTR( CRDSYS, CYLSYS )
     .                .OR. EQSTR( CRDSYS, RADSYS )  ) THEN
C
C              Make sure we found 1 interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )


            ELSE IF ( EQSTR( CRDSYS, LATSYS )
     .           .OR. EQSTR( CRDSYS, GEOSYS )
     .           .OR. EQSTR( CRDSYS, SPHSYS )  ) THEN
C
C              The number of expected intervals depends on the
C              case index.
C          
               IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
C    
C                 Make sure we found no intervals.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE IF ( VI .EQ. 17 ) THEN
C    
C                 Make sure we found no intervals.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
C
C                 We should have 1 interval.  
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF

            END IF

C
C           Now check the solution intervals.
C
            DO I = 1, N
C
C              Look up the Ith solution interval.
C
               CALL WNFETD ( RESULT, I, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )
  
C               WRITE (*,*) START, FINISH

               IF ( EQSTR( CRDSYS, 'PLANETOGRAPHIC' )  ) THEN


                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
C                    WRITE (*,*) 1, START, FINISH
C                    
                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C                       This is a special case: the < condition
C                       is considered to be met on the entire
C                       confinement window.
C
                        QNAME = 'Interval start  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, START, '~', T1, TIMTOL, OK )

                        XET = T2
                        ET  = FINISH

                        QNAME = 'Interval stop # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD( QNAME, ET,  '~', XET, TIMTOL, OK )
                        
                     ELSE
C
C                       The start time should be T1. The stop time
C                       should be the time of the reference meridian
C                       crossing.

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
C
C                    We don't expect to have more than one
C                    interval for this case.
C
                     CALL SETMSG ( 'Unexpected interval count: #; '
     .               //            'VI = #'                         )
                     CALL ERRINT ( '#', N                           )
                     CALL ERRINT ( '#', VI                          )
                     CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                     CALL CHCKXC ( .FALSE., ' ', OK                 )

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'CYLINDRICAL' )
     .                   .OR. EQSTR( CRDSYS, 'RA/DEC'      ) ) THEN
C
C                 Longitude is positive East. The branch cut is at 0.
C
                  IF ( I .EQ. 1 ) THEN
C
C                    This is the normal case.
C
                     IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
C
C                       This is a special case: the > condition
C                       is considered to be met on the whole
C                       confinement window.
C
                        XET = T1
                        ET  = START

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, ET,  '~', XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                       T2, TIMTOL, OK )
                        
                     ELSE
C
C                       The start time should be the REFVAL meridian
C                       crossing time; the end time should be T2.
C                       
                        XET = XTIME
                        ET  = START

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, ET,  '~', XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                       T2, TIMTOL, OK )

                     END IF

                  END IF


               ELSE IF (      EQSTR( CRDSYS, 'LATITUDINAL' )
     .                   .OR. EQSTR( CRDSYS, 'GEODETIC'    )
     .                   .OR. EQSTR( CRDSYS, 'SPHERICAL'   ) ) THEN
C
C                 Longitude is positive East. The branch cut is at Pi.
C
                  IF ( VI .LT. 17 ) THEN

                     IF ( I .EQ. 1 ) THEN
C
C                       This is the normal case.
C
C                       If REFVAL is in the upper half of the unit
C                       circle, including 0 and pi, the start time
C                       should be the time of the reference meridian
C                       crossing and the stop time should be ET0 + 
C                       12 h.
C
C                       If REFVAL is in the lower half of the unit
C                       circle, the start time should be the time of
C                       the reference meridian crossing and the stop
C                       time should be T2 (the end of the confinement
C                       window).
C                      
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )


                        XET = XTIME
                        ET  = START

                        CALL CHCKSD ( QNAME, START, '~', 
     .                                       XET, TIMTOL, OK )


                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        IF ( SIN(REFVAL) .GE. 0.D0 ) THEN

 
                           CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                   ET0+SPD()/2, TIMTOL, OK )

                        ELSE
                           CALL CHCKSD ( QNAME, FINISH, '~', 
     .                                   T2,    TIMTOL, OK )
                        END IF


                     ELSE
C
C                       We don't expect to have more than one
C                       interval for this case.
C
                        CALL SETMSG ( 'Unexpected interval count: #; '
     .                  //            'VI = #'                         )
                        CALL ERRINT ( '#', N                           )
                        CALL ERRINT ( '#', VI                          )
                        CALL SIGERR ( 'SPICE(TSPICEBUG)'               )
                        CALL CHCKXC ( .FALSE., ' ', OK                 )
                        
                     END IF

                  ELSE
C
C                    This case corresponds to the first interval
C                    when REFVAL is 0 and VI is 17. The first interval
C                    should be the singleton [ET0, ET0].
C                  
                     IF ( I .EQ. 1 ) THEN

                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, START, '~', 
     .                                       ET0, TIMTOL, OK )

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        XET = ET0

                        ET  = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', 
     .                                       XET,    TIMTOL, OK )

                     ELSE IF ( I .EQ. 2 ) THEN
C
C                       This case corresponds to the second interval
C                       when REFVAL is 0 and VI is 17. The second
C                       interval should cover from ET0+12h to ET0+24h.
C                  
                        QNAME = 'Interval start # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        CALL CHCKSD ( QNAME, START,       '~', 
     .                                       ET0+SPD()/2, TIMTOL, OK )

                        QNAME = 'Interval stop  # (0)'
                        CALL REPMI ( QNAME, '#', 1, QNAME )

                        XET = ET0+SPD()

                        ET  = FINISH

                        CALL CHCKSD ( QNAME, ET, '~', 
     .                                       XET,    TIMTOL, OK )

                     ELSE
C
C                       We don't expect to have more than two
C                       intervals for this case.
C
                        CALL SETMSG ( 'Unexpected interval count: #; '
     .                  //            'VI = #'                         )
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


C
C              Check the longitude at the crossing time ET.
C
C              In order to decide how accurate the coordinate should
C              be, we'll use the angular rate of Gamma together with
C              the time tolerance. Adjust the tolerance for the units
C              we're using in the CHCKSD call.
C
               QTOL = DPR() * ( TWOPI() / SPD() ) * CNVTOL
        

C
C              Compute the longitude of the vector we expect and
C              the longitude at FINISH.
C
               CALL SPKEZR ( TARGET, XET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, 
     .                          XLON,   LAT, ALT        )
               ELSE
                  CALL RECLAT ( POS, R, XLON, LAT )
               END IF


               CALL SPKEZR ( TARGET, ET,     REF, 
     .                       ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL VEQU   ( STATE, POS )

               IF (  EQSTR( CRDSYS, PGRSYS )  ) THEN

                  CALL RECPGR ( OBSRVR, POS, 1.D0, 0.D0, LON, LAT, ALT )
               ELSE
                  CALL RECLAT ( POS, R, LON, LAT )
               END IF


               QNAME = 'Longitude # (deg)'
               CALL REPMI ( QNAME, '#', I, QNAME )

C
C              Adjust the longitude by 2*pi if it's too far
C              from the expected value.
C
               IF (  LON  .GT. ( XLON + 1.D0 )  ) THEN

                  LON = LON - TWOPI()

               ELSE IF (  LON  .LT.  ( XLON - 1.D0 )  ) THEN

                  LON = LON + TWOPI()

               END IF

               CALL CHCKSD ( QNAME, LON*DPR(),    '~', 
     .                              XLON*DPR(), QTOL, OK )

            END DO
C
C           We're done with the Ith interval.
C
         END DO
C
C        End of vector definition loop.
C

         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO
C
C     End of coordinate system loop.
C

C
C --- Case: ------------------------------------------------------
C
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



