C$Procedure      F_ZZGFSSIN ( Test surface intercept state computation )
  
      SUBROUTINE F_ZZGFSSIN ( OK )

C$ Abstract
C
C     Test the GF private surface intercept state routine ZZGFSSIN.
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
C     This routine tests the private SPICELIB GF
C     surface intercept state routine ZZGFSSIN.
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
C-    SPICELIB Version 3.0.0, 12-MAY-2009 (NJB)
C
C        Removed error checks for ID codes that don't
C        map to names, as these are no longer applicable.
C
C-    SPICELIB Version 2.0.0, 23-SEP-2008 (NJB)
C
C        Relaxed tolerances to enable clean test runs
C        on all supported platforms. Changed quantity name
C        "sub-point" to "intercept" in test utility calls.
C
C-    SPICELIB Version 1.0.0, 09-MAY-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD


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


      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-3 )

      DOUBLE PRECISION      MEDABS
      PARAMETER           ( MEDABS = 1.D-3 )
  
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-5 )

C
C     Time delta used for discrete derivatives. The 10-second
C     value used here was found by trial and error: it yields
C     better approximations than does the "standard" 1-second
C     value.
C
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
 
 
C
C     Local variables
C
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

      DATA                  JFRAME / 'J2000',      'IAU_JUPITER', 
     .                               'ECLIPJ2000', 'IAU_IO'    /





C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFSSIN' )


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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,  
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )
 
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID,
     .                DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C     We're going to loop over all aberration corrections and 
C     ray direction frames.
C

      DO NC = 1, NCORR

         DO NF = 1, NREF
C
C           Loop over the set of sample times.
C
            TIMSTR = '2008 MAY 1' 

            CALL STR2ET ( TIMSTR, ET0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DELTA = 100 * SPD()


            DO NS = 1, NSAMP

C
C --- Case: ------------------------------------------------------
C
C
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

C
C              Look up the direction vector.
C
               CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                       OBSRVR, DVEC, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL NAMFRM ( DREF, FRCODE )
               CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

               CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR,  
     .                         OBSID,  DREF,  DCTR, DVEC,   RADII,  
     .                         STATE,  FOUND                       )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              By construction, the state should be found.
C
               CALL CHCKSL ( 'ZZGFSSIN found', FOUND, .TRUE., OK )

C               WRITE (*,*) 'STATE = ', STATE

C
C              Use SRFVEC and FIXREF as our ray direction and frame.
C
C              Estimate the surface intercept point velocity
C              numerically.
C
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

C
C                 DVEC is constant in frame DREF, so we don't need
C                 to re-compute it.
C
                  CALL SINCPT ( METHOD,    TARGET, T,      FIXREF,
     .                          ABCORR,    OBSRVR, DREF,   DVEC,   
     .                          SPNT(1,I), TRGEPC, SRFVEC, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'ZZGFSSIN found (0)', 
     .                           FOUND, .TRUE., OK    )

               END DO
      

               CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Check the velocity from ZZGFSSIN. We expect an error of
C              less than 1 m/s, except when the frame is IAU_EARTH,
C              because the velocity of the intercept is quite high in
C              that frame.
C
               IF ( DREF .NE. 'IAU_EARTH' ) THEN
                  TOL = MEDABS
               ELSE
                  TOL = LOOSE
               END IF

               CALL CHCKAD ( 'Intercept vel (0)', STATE(4), '~~', 
     .                                            XVEL,  3,  TOL, OK )

C               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) '>>>>>>>>>>>>>Speed = ', VNORM(XVEL)
C               END IF

C
C              Check the surface intercept.
C 
               CALL SINCPT ( METHOD,  TARGET, ET,     FIXREF,
     .                       ABCORR,  OBSRVR, DREF,   DVEC,   
     .                       XPNT,    TRGEPC, SRFVEC, FOUND )


C
C              The tolerance we use depends on the aberration
C              correction.
C
               CALL ZZPRSCOR ( ABCORR, ATTBLK )

               IF ( ATTBLK(CNVIDX) ) THEN

                  TOL = MEDABS

               ELSE IF ( ATTBLK(LTIDX) ) THEN
               
                  TOL = LOOSE
               ELSE
                  TOL = TIGHT
               END IF

               CALL CHCKAD ( 'Surface intercept point (0)', 
     .                       STATE, '~~',  XPNT,  3,  TOL, OK )


C
C --- Case: ------------------------------------------------------
C

C
C              Given the inputs above, make sure we do not find
C              the state if we reverse the ray's direction.'
C
               CALL PREFIX ( 'Earth non-intersection case:', 1, TITLE ) 
               CALL TCASE  ( TITLE )

               CALL VMINUS ( DVEC, NEGVEC )
               CALL VEQU   ( NEGVEC, DVEC )

               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR,
     .                         OBSID,  DREF,  DCTR, DVEC,   RADII,  
     .                         STATE,  FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              By construction, the state should NOT be found.
C
               CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C              Look up the direction vector.
C
               CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                       OBSRVR, DVEC, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL NAMFRM ( DREF, FRCODE )
               CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

               CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, 
     .                         OBSID,  DREF,  DCTR, DVEC,   RADII,  
     .                         STATE,  FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              By construction, the state should be found.
C
               CALL CHCKSL ( 'ZZGFSSIN found', FOUND, .TRUE., OK )

C               WRITE (*,*) 'DREF, STATE = ', DREF, STATE

C
C              Use SRFVEC and FIXREF as our ray direction and frame.
C
C              Estimate the surface intercept point velocity
C              numerically.
C
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

C
C                 DVEC is constant in frame DREF, so we don't need
C                 to re-compute it.
C
                  CALL SINCPT ( METHOD,    TARGET, T,      FIXREF,
     .                          ABCORR,    OBSRVR, DREF,   DVEC,   
     .                          SPNT(1,I), TRGEPC, SRFVEC, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'ZZGFSSIN found (2)', 
     .                          FOUND, .TRUE., OK     )

               END DO


               CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Check the velocity from ZZGFSSIN. We expect an error of
C              less than 1 m/s.
C
C
C              The tolerance we use depends on the aberration
C              correction.
C
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



               CALL CHCKAD ( 'Intercept vel (1)', STATE(4), '~~', 
     .                                        XVEL,  3,  TOL, OK )
C               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) '>>>>>>>>>>>>>Speed = ', VNORM(XVEL)
C               END IF

C
C              Check the surface intercept.
C
               CALL SINCPT ( METHOD,  TARGET, ET,     FIXREF,
     .                       ABCORR,  OBSRVR, DREF,   DVEC,   
     .                       XPNT,    TRGEPC, SRFVEC, FOUND )



               IF ( ATTBLK(CNVIDX) ) THEN
   
                  TOL = MEDABS

               ELSE IF ( ATTBLK(LTIDX) ) THEN

                  TOL = LOOSE
               ELSE
                  TOL = TIGHT
               END IF

               CALL CHCKAD ( 'Surface intercept point (1)', 
     .                       STATE, '~~',  XPNT,  3,  TOL, OK )


C
C --- Case: ------------------------------------------------------
C

C
C              Given the inputs above, make sure we do not find
C              the state if we reverse the ray's direction.'
C
               CALL PREFIX ( 'Non-intersection:', 
     .                       1, TITLE ) 
               CALL TCASE  ( TITLE )

               CALL VMINUS ( DVEC, NEGVEC )
               CALL VEQU   ( NEGVEC, DVEC )

               CALL ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR,  
     .                         OBSID,  DREF,   DCTR,  DVEC, RADII,  
     .                         STATE,  FOUND                       )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              By construction, the state should NOT be found.
C
               CALL CHCKSL ( 'ZZGFSSIN found (3)', FOUND, .FALSE., OK )
             
            END DO
C
C           End of cases for the current epoch.
C
         END DO
C
C        End of cases for the current method.
C
      END DO
C
C     End of cases for the current aberration correction.
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




