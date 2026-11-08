C$Procedure      F_ZZGFCOQ ( Test GF coordinate quantity computation )
  
      SUBROUTINE F_ZZGFCOQ ( OK )

C$ Abstract
C
C     Test the GF private coordinate quantity computation routine
C     ZZGFCOQ.
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
C     This routine tests the private SPICELIB GF coordinate quantity
C     computation routine ZZGFCOQ.
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
C-    TSPICE Version 4.0.0, 09-FEB-2016 (NJB)
C
C        Updated invalid METHOD test case: expected short
C        error message was updated.
C
C-    TSPICE Version 3.0.0, 12-MAY-2009 (NJB)
C
C        Removed error checks for ID codes that don't
C        map to names, as these are no longer applicable.
C
C-    TSPICE Version 2.0.0, 26-JUN-2008 (NJB)
C
C        Added test cases for bad coordinate system,
C        bad coordinate name, and bad vector definition.
C
C-    TSPICE Version 1.0.0, 06-JUN-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD

      LOGICAL               ODD

C
C     Local parameters
C
C     
C     Local parameters
C
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
 
C
C     Number of recognized coordinate systems.
C
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

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
      
      INTEGER               NVDEF
      PARAMETER           ( NVDEF  = 3 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )
      

C
C     Local variables
C
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

C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C
      DATA                  CORR   / 'NONE', 
     .                               'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                               'XLT', 'XLT + S', 'XCN', 'XCN+S'   
     .                             /

      DATA                  EFRAME / 'J2000',      'IAU_EARTH', 
     .                               'ECLIPJ2000', 'IAU_MOON'  /


C
C     Saved Variables
C

C
C     Initial values 
C

C
C     Names of supported coordinate systems.
C
C     The Ith coordinate system in the array SYSNMS has coordinates
C     in the Ith row of the array CRDNMS. This association must be
C     preserved when this routine is updated.
C
      DATA                  SYSNMS /  RECSYS, 
     .                                LATSYS, 
     .                                RADSYS, 
     .                                SPHSYS, 
     .                                CYLSYS, 
     .                                GEOSYS,
     .                                PGRSYS  /

C
C     Names of coordinate triples for the supported coordinate
C     systems.
C
C     The order of the coordinate names in the Ith row of this array
C     matches the order of the outputs of the corresponding
C     SPICELIB routine REC*, which maps a Cartesian vector to
C     the Ith coordinate system in the array SYSNMS. Again, this
C     order must be preserved.
C     
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD,
     .                                RADCRD,   LONCRD,   LATCRD,
     .                                RNGCRD,   RACRD,    DECCRD,
     .                                RADCRD,   CLTCRD,   LONCRD,
     .                                RADCRD,   LONCRD,   ZCRD,
     .                                LONCRD,   LATCRD,   ALTCRD,
     .                                LONCRD,   LATCRD,   ALTCRD  /

C
C     Vector definition methods:
C
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFCOQ' )


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
C     Actual DE-based ephemerides yield better comparisons,
C     since these ephemerides have less noise than do those
C     produced by TSTSPK.
C     
C      CALL FURNSH ( 'de421.bsp' )
C      CALL FURNSH ( 'jup230.bsp' )
C 


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************







C
C --- Case: ------------------------------------------------------
C
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
      
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CRDSYS = LATSYS


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate name' )

      CTRID  = 399
      ABCORR = 'None'
      CRDSYS = LATSYS
      CRDNAM = 'latitude'
      
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CRDNAM = LATCRD

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad vector definition' )

      CTRID  = 399
      ABCORR = 'None'
      VECDEF = 'velocity'
      
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      VECDEF = SINDEF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction' )

      CTRID  = 399
      ABCORR = 'None.'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target orientation data available' )

      METHOD = 'ellipsoid'
      REF    = 'ITRF93'


      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame not centered on target' )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      TRGID  = 499
      REF    = 'IAU_EARTH'
     

      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF    = 'IAU_GASPRA'
 
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )

      TARGET = 'MOON'
      TRGID  = 301
      OBSRVR = 'GASPRA'
      OBSID  = 9511010
      REF    = 'IAU_MOON'
 
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF ephemeris data available' )

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )
 
 
 
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
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

 
      CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .               REF,    ABCORR, OBSID, DREF,
     .               DVEC,   CRDSYS, CTRID, RE,
     .               F,      CRDNAM, VALUE, FOUND )
 

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C     We're going to loop over all vector definitions,
C     aberration corrections, coordinate systems, and
C     coordinates.
C     
C
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
C
C              Loop over the set of sample times.
C
               TIMSTR = '2008 MAY 1' 

               CALL STR2ET ( TIMSTR, ET0 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               DELTA = 100 * SPD()


               DO NS = 1, NSAMP

                  IF ( VECDEF .EQ. POSDEF ) THEN
C
C                    For vectors defined by positions, we don't
C                    have to use a frame centered on the target.
C                    In addition to the target-centered frames,
C                    we use body-fixed frames centered on a third body.
C
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
C
C                    We use two different observer-target combinations.
C
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





C
C --- Case: ------------------------------------------------------
C
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
C
C                    Look up the direction vector.
C
                     CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                             OBSRVR, DVEC, LT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL NAMFRM ( DREF, FRCODE )
                     CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

                     CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


                  END IF

C
C                 Look up the expected position vector.
C
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     CALL SPKPOS ( TARGET, ET, REF,    ABCORR, OBSRVR,
     .                             XPOS,   LT                         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                     CALL SUBPNT ( METHOD, TARGET, ET,  
     .                             REF,    ABCORR, OBSRVR, 
     .                             XPOS,   TRGEPC, SRFVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     
                  ELSE

                     CALL SINCPT ( METHOD, TARGET, ET,  
     .                             REF,    ABCORR, OBSRVR, 
     .                             DREF,   DVEC,   XPOS,
     .                             TRGEPC, SRFVEC, FOUND  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    By construction, the intercept should be found.
C
                     CALL CHCKSL ( 'intercept found', 
     .                             FOUND, .TRUE., OK )

                  END IF

C
C                 Convert position vector to current coordinate system.
C
                  IF ( CRDSYS .EQ. RECSYS ) THEN

                     CALL MOVED ( XPOS, 3, XCOORD )

                  ELSE IF ( CRDSYS .EQ. LATSYS ) THEN

                     CALL RECLAT ( XPOS, 
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. RADSYS ) THEN

                     CALL RECRAD ( XPOS, 
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. SPHSYS ) THEN

                     CALL RECSPH ( XPOS, 
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. CYLSYS ) THEN

                     CALL RECCYL ( XPOS, 
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. GEOSYS ) THEN

                     CALL RECGEO ( XPOS,      RE,        F,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. PGRSYS ) THEN
                     
                     CALL RECPGR ( REFCTR,    XPOS,      RE,        F,
     .                             XCOORD(1), XCOORD(2), XCOORD(3)    )

                  ELSE 

                     CALL SETMSG ( 'Unrecognized system #.' )
                     CALL ERRCH  ( '#', CRDSYS              )
                     CALL SIGERR ( 'SPICE(BUG)'             )
                     CALL CHCKXC ( .FALSE., ' ', OK         )

                  END IF

C
C                 Check each coordinate.
C
                  DO CI = 1, 3
C
C                    Get the result from ZZGFCOQ.
C
                     CRDNAM = CRDNMS(CI,SI)

                     CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .                              REF,    ABCORR, OBSID, DREF,
     .                              DVEC,   CRDSYS, FXCID, RE,
     .                              F,      CRDNAM, VALUE, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL CHCKSD ( CRDNAM, VALUE,     '~/', 
     .                                     XCOORD(CI), TIGHT, OK )

                  END DO




                  IF ( VECDEF .EQ. SINDEF ) THEN
C
C --- Case: ------------------------------------------------------
C

C                    Given the inputs above, make sure we do not find
C                    the state if we reverse the ray's direction.'
C
                     CALL PREFIX ( 'Earth non-intersection case:', 
     .                             1, TITLE ) 
                     CALL TCASE  ( TITLE )

                     CALL VMINUS ( DVEC, NEGVEC )
                     CALL VEQU   ( NEGVEC, DVEC )
 
                     CALL ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,
     .                              REF,    ABCORR, OBSID, DREF,
     .                              DVEC,   CRDSYS, FXCID, RE,
     .                              F,      CRDNAM, VALUE, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )


                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    By construction, the state should NOT be found.
C
                     CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, 
     .                             .FALSE., OK )

                  END IF


               END DO
C
C              End of cases for the current epoch.
C             
            END DO
C
C           End of cases for the current coordinate system.
C
         END DO
C
C        End of cases for the current aberration correction.
C
      END DO
C
C     End of cases for the current vector definition.
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END




