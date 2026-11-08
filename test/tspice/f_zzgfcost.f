C$Procedure      F_ZZGFCOST ( Test GF coordinate state computation )
  
      SUBROUTINE F_ZZGFCOST ( OK )

C$ Abstract
C
C     Test the GF private coordinate state computation routine
C     ZZGFCOST.
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
C     This routine tests the private SPICELIB GF coordinate state
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
C-    SPICELIB Version 3.0.0, 13-MAY-2009 (NJB)
C
C        Removed error checks for ID codes that don't
C        map to names, as these are no longer applicable.
C
C-    SPICELIB Version 2.0.0, 26-MAY-2008 (NJB)
C
C        Added test case for bad values of VECDEF.
C
C-    SPICELIB Version 1.0.0, 14-MAY-2008 (NJB)
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
      

C
C     Local variables
C
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
C     Vector definition methods:
C
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFCOST' )


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

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      VECDEF = SINDEF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction' )

      OBSID   = 301
      DCTR    = 399
      ABCORR  = 'None.'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )
 
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target orientation data available' )

      METHOD = 'ellipsoid'
      REF    = 'ITRF93'


      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

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
     

      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF    = 'IAU_GASPRA'
 
      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

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
 
      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF ephemeris data available' )

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )
 
 
 
      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )

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
      DCTR   = 399
      DREF   = 'ITRF93'
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

 
      CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                REF,    ABCORR, OBSID, DREF,  
     .                DCTR,   DVEC,   RADII, STATE, FOUND )
 

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      DCTR   = 399

      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

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
C
C                 Look up the direction vector.
C
                  CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, 
     .                          OBSRVR, DVEC, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL NAMFRM ( DREF, FRCODE )
                  CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

                  CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )

               END IF

C
C              Look up the expected state vector.
C
               IF ( VECDEF .EQ. POSDEF ) THEN

                  CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBSRVR,
     .                          XSTATE, LT                         )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                  CALL ZZGFSSOB ( METHOD, TRGID,  ET,  
     .                            REF, ABCORR, OBSID, 
     .                            RADII,  XSTATE         )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                     
               ELSE

                  CALL ZZGFSSIN ( METHOD, TRGID, ET,     REF, 
     .                            ABCORR, OBSID, DREF,   DCTR,
     .                            DVEC,   RADII, XSTATE, FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )
C
C                 By construction, the intercept should be found.
C
                  CALL CHCKSL ( 'intercept found', 
     .                          FOUND, .TRUE., OK )

               END IF

C
C              Get the result from ZZGFCOST.
C
               CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                         REF,    ABCORR, OBSID, DREF,  
     .                         DCTR,   DVEC,   RADII, STATE, FOUND )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C              Check position.
C
               CALL CHCKAD ( 'Position', STATE,  '~~/', 
     .                                   XSTATE, 3, TIGHT, OK )

C
C              Check velocity.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )


               IF ( VECDEF .EQ. SINDEF ) THEN
C
C --- Case: ------------------------------------------------------
C

C                 Given the inputs above, make sure we do not find
C                    the state if we reverse the ray's direction.'
C
                  CALL PREFIX ( 'Earth non-intersection case:', 
     .                          1, TITLE ) 
                  CALL TCASE  ( TITLE )

                  CALL VMINUS ( DVEC, NEGVEC )
                  CALL VEQU   ( NEGVEC, DVEC )
 
                  CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                            REF,    ABCORR, OBSID, DREF,  
     .                            DCTR,   DVEC,   RADII, STATE, FOUND )


                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 By construction, the state should NOT be found.
C
                  CALL CHCKSL ( 'ZZGFSSIN found (1)', FOUND, 
     .                          .FALSE., OK )

               END IF


            END DO
C
C           End of cases for the current epoch.
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




