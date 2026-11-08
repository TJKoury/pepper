C$Procedure      F_ZZGFLNG3 ( Test GF longitude solver, part 3 )
  
      SUBROUTINE F_ZZGFLNG3 ( OK )

C$ Abstract
C
C     Test the GF private longitude search routine ZZGFLONG. This
C     family tests processing of relations involving relative,
C     absolute, and adjusted absolute extrema.
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
C     routine ZZGFLONG. This family tests processing of relations
C     involving relative, absolute, and adjusted absolute extrema.
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
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      INTEGER               WNCARD

      LOGICAL               EQSTR
      LOGICAL               GFBAIL

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
C     Number of systems that support longitude:
C
      INTEGER               NLNSYS
      PARAMETER           ( NLNSYS = 7 )

C
C     Frame definition buffer size:
C
      INTEGER               FRBFSZ
      PARAMETER           ( FRBFSZ = 100 )

C
C     Longitude sense string length:
C
      INTEGER               SNSLEN
      PARAMETER           ( SNSLEN = 4 )

C
C     Local variables
C
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
C     INTEGER               J
      INTEGER               N
      INTEGER               NLINES
      INTEGER               SI

      LOGICAL               BAIL
      LOGICAL               RPT

C
C     Save everything.
C
      SAVE
 
C
C     Initial values 
C
      DATA                  CNAM   / 'Longitude',
     .                               'Right  ascension',
     .                               'Longitude',
     .                               'Longitude',
     .                               'Longitude',
     .                               'Longitude',
     .                               'Longitude'        /

      DATA                  CSYS   / 'Latitudinal',
     .                               'Ra / Dec',
     .                               'Cylindrical',
     .                               'Geodetic',
     .                               'Planetographic',
     .                               'Planetographic_2',
     .                               'Spherical'        /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFLNG3' )


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
C*    Absolute maximum tests 
C*
C*********************************************************************




C
C --- Case: ------------------------------------------------------
C     
C
C     We're going to find absolute maxima of the alphafixed longitude
C     of the Alpha-gamma vector.
C
C
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We'll loop over the various coordinate systems.
C
      DO CC = 1, NLNSYS
C
C        Set the coordinate system and coordinate.
C
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

         CRDNAM = CNAM(CC)

C
C        Loop over different confinement windows. 
C        We pick the window end times so that the
C        maxima will be multiples of pi/8.
C       
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0
               T2 = ET0 + SPD()
C
C              Set the expected time of the maximum.
C
               XTIME = T1

            ELSE
               
               IF ( SI .LE. 9 ) THEN

                  T1 = ET0

               ELSE

                  T1 = ET0 + SPD()/2

               END IF

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0

 
C
C              Set the expected time of the maximum.
C
               XTIME = T2


            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            REFVAL = (SI-1) * PI()/8.D0

C
C --- Case: ------------------------------------------------------
C
            TITLE =  'Alpha-gamma maximum; #; #; ref val #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  ) 

C
C           Set parameters required for the search.
C
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

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
C
C                 There's a branch cut at 0: however, we've
C                 made the confinement window the whole unit
C                 circle. So we expect a result.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE IF (      EQSTR(CRDSYS,RADSYS)
     .                   .OR. EQSTR(CRDSYS,PGRSYS)
     .                   .OR. EQSTR(CRDSYS,CYLSYS) ) THEN
C
C                 There's a branch cut at 0: the search window is
C                 bounded away from the branch cut, while the
C                 confinement window contains nothing but the branch
C                 cut. The result window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

               ELSE
C
C                 The confinement window should be a singleton set
C                 at which the absolute maximum is found.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF


            ELSE
C
C              We expect the result window to contain one
C              solution interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF
            
            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

C              Check that START is equal to XTIME.
C
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )
   
C
C              Check that START and FINISH match.
C
               QNAME = 'START vs FINISH'
               CALL CHCKSD ( QNAME, START, '~', FINISH, TIMTOL, OK )

            END IF
            
         END DO
C
C        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


C*********************************************************************
C*
C*    Adjusted absolute maximum tests 
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C     
C
C     We're going to find adjusted absolute maxima of the alphafixed
C     longitude of the Alpha-gamma vector.
C
C
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ADJUST = 15.D0 * RPD() / 3600.D0

C
C     We'll loop over the various coordinate systems.
C
      DO CC = 1, NLNSYS
C
C        Set the coordinate system and coordinate.
C
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

C
C        Loop over different confinement windows. 
C        We pick the window end times so that the
C        maxima will be multiples of pi/8.
C       
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN


               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               T2 = ET0 + SPD()
C
C              Set the expected time of the maximum and the
C              adjusted maximum. XTIME is the expected
C              start time; XTIME2 is the expected stop time.
C
               XTIME  = T1 
               XTIME2 = MIN( T1 + 1.D0, T2 )

            ELSE

               IF ( SI .LE. 9 ) THEN

                  T1 = ET0

               ELSE

                  T1 = ET0 + SPD()/2

               END IF

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
C
C              Set the expected time of the maximum and the
C              adjusted maximum. XTIME is the expected

C              start time; XTIME2 is the expected stop time.

               XTIME =  MAX( T2 - 1.D0, ET0 )
               XTIME2 = T2

            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Contract the confinement window a bit to avoid
C           spurious maxima. However, don't wipe out the
C           confinement interval altogether.
C
            REFVAL = (SI-1) * PI()/8.D0



            CALL WNFETD ( CNFINE, 1, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
            TITLE =  'Alpha-gamma adjusted maximum; #; #; max lon #; '
     .      //       'ADJUST = #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL REPMF ( TITLE, '#', DPR()*ADJUST, 6, 'F', TITLE )
            CALL TCASE ( TITLE  ) 

C
C           Set parameters required for the search.
C
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

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )


            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
C
C                 We expect the result window to contain one
C                 solution interval.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE

                  IF (       EQSTR(CRDSYS, PGRSYS)
     .                 .OR.  EQSTR(CRDSYS, CYLSYS) 
     .                 .OR.  EQSTR(CRDSYS, RADSYS)  ) THEN
C
C                    These systems have a singularity at
C                    0 longitude/RA, so the result window 
C                    should be empty.
C
                     CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

                  ELSE
C
C                    The result should be a singleton set.
C
                     CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

                  END IF

               END IF
               
            ELSE
C
C              We expect the result window to contain one
C              solution interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C              IF ( .NOT. OK ) THEN
C
C                 DO J = 1, WNCARD(CNFINE)
C                    CALL WNFETD ( CNFINE, J, START, FINISH )
C                    WRITE (*,'(2(5PE24.12))') START, FINISH
C                 END DO
C
C                 DO J = 1, WNCARD(RESULT)
C                    CALL WNFETD ( RESULT, J, START, FINISH )
C                    WRITE (*,'(2(5PE24.12))') START, FINISH
C                 END DO
C
C              END IF
C
            END IF

            
            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

C              Check that START is equal to XTIME.
C
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )
   
C
C              Check that FINISH is equal to XTIME2.
C
               QNAME = 'FINISH vs XTIME2'
               CALL CHCKSD ( QNAME, FINISH, '~', XTIME2, TIMTOL, OK )

            END IF
            
         END DO
C
C        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO



C*********************************************************************
C*
C*    Absolute minimum tests 
C*
C*********************************************************************



C
C --- Case: ------------------------------------------------------
C     
C
C     We're going to find absolute minima of the alphafixed longitude
C     of the Alpha-gamma vector.
C
C
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We'll loop over the various coordinate systems.
C
      DO CC = 1, NLNSYS
C
C        Set the coordinate system and coordinate.
C
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

C
C        Loop over different confinement windows. 
C        We pick the window end times so that the
C        minima will be multiples of pi/8.
C       
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0

               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
C
C              Set the expected time of the minimum.
C
               XTIME = T2

            ELSE

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               IF ( SI .LE. 8 ) THEN

                  T2 = ET0 + SPD()/2

               ELSE

                  T2 = ET0 + SPD()

               END IF

 
C
C              Set the expected time of the minimum.
C
               XTIME = T1


            END IF

            CALL SCARDD ( 0,      CNFINE )
            CALL WNINSD ( T1, T2, CNFINE )
            CALL CHCKXC ( .FALSE., ' ', OK )

             REFVAL = (SI-1) * PI()/8.D0

C
C --- Case: ------------------------------------------------------
C
            TITLE =  'Alpha-gamma abs minimum; #; #; ref val #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  ) 

C
C           Set parameters required for the search.
C
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

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( SI .EQ. 1 ) THEN

               IF ( LONSNS .EQ. 'WEST' ) THEN
C
C                 There's a branch cut at 0: the confinement window
C                 lies in the deleted sector that encloses the branch
C                 cut. The result window should be empty.
C
                  CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


               ELSE IF (      EQSTR(CRDSYS,RADSYS)
     .                   .OR. EQSTR(CRDSYS,PGRSYS)
     .                   .OR. EQSTR(CRDSYS,CYLSYS) ) THEN
C
C                 There's a branch cut at 0: however we've made
C                 the confinement window cover the upper half 
C                 circle. The result window should be non-empty.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               ELSE
C
C                 The confinement window should be a singleton set
C                 at which the absolute minimum is found.
C
                  CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

               END IF               

            ELSE
C
C              We expect the result window to contain one
C              solution interval.
C
               CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

            END IF
            
            IF ( N .EQ. 1 ) THEN

               CALL WNFETD  ( RESULT, 1, START, FINISH )
               CALL CHCKXC ( .FALSE., ' ', OK )

C              Check that START is equal to XTIME.
C
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )
   
C
C              Check that START and FINISH match.
C
               QNAME = 'START vs FINISH'
               CALL CHCKSD ( QNAME, START, '~', FINISH, TIMTOL, OK )

            END IF
            
         END DO
C
C        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO



C*********************************************************************
C*
C*    Adjusted absolute minimum tests 
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C     
C
C     We're going to find adjusted absolute minima of the alphafixed
C     longitude of the Alpha-gamma vector. In each case we'll use
C     an adjustment value of 15 arcseconds, which should shift the
C     minima by 1 second.
C
C
      CALL STR2ET ( '2000 JAN 1 18:00 TDB ', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SSIZED ( MW, CNFINE )
      CALL SSIZED ( MW, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ADJUST = 15.D0 * RPD() / 3600.D0

C
C     We'll loop over the various coordinate systems.
C

      DO CC = 1, NLNSYS
C
C        Set the coordinate system and coordinate.
C
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

C
C        Loop over different confinement windows. 
C        We pick the window end times so that the
C        minima will be multiples of pi/4.
C       
         DO SI = 1, 16

            IF ( LONSNS .EQ. 'WEST' ) THEN

               T1 = ET0
               T2 = ET0 + (SI-1) * 1.5D0 * 3600.D0
C
C              Set the expected time of the minimum
C              and the adjusted minimum.
C
               XTIME  = MAX( T2 - 1.D0, T1 )
               XTIME2 = T2

            ELSE

               T1 = ET0 + (SI-1) * 1.5D0 * 3600.D0

               IF ( SI .LE. 8 ) THEN

                  T2 = ET0 + SPD()/2

               ELSE

                  T2 = ET0 + SPD()

               END IF
C
C              Set the expected time of the minimum
C              and the adjusted minimum.
C
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
               
C
C --- Case: ------------------------------------------------------
C
            TITLE =  'Adjusted alpha-gamma minimum; #; #; #; min lon #'

            CALL REPMC ( TITLE, '#', CRDSYS,               TITLE )
            CALL REPMC ( TITLE, '#', CRDNAM,               TITLE )
            CALL REPMF ( TITLE, '#', DPR()*ADJUST, 6, 'F', TITLE )
            CALL REPMF ( TITLE, '#', DPR()*REFVAL, 6, 'F', TITLE )
            CALL TCASE ( TITLE  ) 

C
C           Set parameters required for the search.
C
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

            CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                      BAIL,   GFBAIL, MW,     NW,     WORK,
     .                      CNFINE, RESULT                         )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           We expect the result window to contain one
C           solution interval.
C
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
C
C              Check that START is equal to XTIME.
C
               QNAME = 'START vs XTIME'
               CALL CHCKSD ( QNAME, START, '~', XTIME, TIMTOL, OK )
   
C
C              Check that FINISH is equal to XTIME2.
C
               QNAME = 'FINISH vs XTIME2'
               CALL CHCKSD ( QNAME, FINISH, '~', XTIME2, TIMTOL, OK )

            END IF
            
         END DO
C
C        End of extreme value check loop.


         IF ( EQSTR(CRDSYS,GEOSYS) .OR. EQSTR(CRDSYS,PGRSYS) ) THEN
C
C           Restore Alpha's radii.
C
            CALL PDPOOL ( 'BODY1000_RADII', 3, ALPHRD )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

      END DO


C*********************************************************************
C*
C*    Local maximum/minimum tests 
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C
C
C     Local max/min tests:
C
C     We're going to find local extrema of the J2000 longitude
C     of the Sun-IO vector. Jupiter's orbital angular rate is
C     slow enough so that Io exhibits periods of retrograde
C     motion.
C
C     In order to be able to use geodetic and planetographic
C     coordinate systems, we need a reference frame aligned with
C     the J2000 frame, but whose center is the Sun. We create
C     such a frame specification below and insert it into the
C     kernel pool via LMPOOL.
C
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

C
C     Our confinement window will sample from Jupiter's orbit
C     in such a way that all branches of ZZGFLONG's local
C     extremum algorithm will be exercised. We'll use the
C     first few days of each year out of a 12 year period
C     as our confinement window.
C
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

C
C     We'll loop over the various coordinate systems.
C
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

C
C --- Case: ------------------------------------------------------
C
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


C
C        Set parameters required for the search.
C
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

C
C        Use a loose tolerance to speed things up a bit.
C
         TOL    = 1.D-2
         ADJUST = 0.D0

         CALL GFSSTP ( 5*3600.D0 )

         RPT    = .FALSE.
         BAIL   = .FALSE.

         CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                   OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                   RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                   GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                   BAIL,   GFBAIL, MW,     NW,     WORK,
     .                   CNFINE, RESULT                         )
         CALL CHCKXC ( .FALSE., ' ', OK )

         N = WNCARD( RESULT )

C
C        We expect each confinement interval to contain two
C        solutions, so we expect 24 solutions in all.
C
         CALL CHCKSI ( 'N (0)', N, '=', 24, 0, OK )

         DO I = 1, N

            CALL WNFETD  ( RESULT, I, START, FINISH )

            CALL ETCAL   ( FINISH, TIMSTR )

C
C           Check the coordinate at the event time +/- TDELTA
C           seconds.
C
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
C
C        End of extreme value check loop.
C


C
C --- Case: ------------------------------------------------------
C
         TITLE =  'Sun-Io local minimum #; #'

         CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
         CALL REPMC ( TITLE, '#', CRDNAM, TITLE )

         CALL TCASE ( TITLE  ) 

C
C        Set parameters required for the search.
C
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

C
C        Use a loose tolerance to speed things up a bit.
C
         TOL    = 1.D-2
         ADJUST = 0.D0

         CALL GFSSTP ( 5*3600.D0 )

         RPT    = .FALSE.
         BAIL   = .FALSE.

         CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR,
     .                   OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM,
     .                   RELATE, REFVAL, TOL,    ADJUST, GFSTEP, 
     .                   GFREFN, RPT,    GFREPI, GFREPU, GFREPF,
     .                   BAIL,   GFBAIL, MW,     NW,     WORK,
     .                   CNFINE, RESULT                         )
         CALL CHCKXC ( .FALSE., ' ', OK )


         N = WNCARD( RESULT )

C
C        We expect each confinement interval to contain two
C        solutions, so we expect 24 solutions in all.
C
         CALL CHCKSI ( 'N (0)', N, '=', 24, 0, OK )

         DO I = 1, N

            CALL WNFETD  ( RESULT, I, START, FINISH )

            CALL ETCAL   ( FINISH, TIMSTR )

C
C           Check the coordinate at the event time +/- TDELTA
C           seconds.
C
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
C
C        End of extreme value check loop.
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



