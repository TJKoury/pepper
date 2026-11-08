C$Procedure F_ZZGFDIU ( ZZGFDIU family tests )

      SUBROUTINE F_ZZGFDIU ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB utility package
C
C        ZZGFDIU
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the SPICELIB geometry utility package ZZGFDIU.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 18-NOV-2013 (EDW)
C
C        Test families update to accommodate ZZGFCOU modifications
C        required by ZZGFRELX.
C
C        UDF call replaced ZZUDF.
C
C-    TSPICE Version 1.0.0, 22-MAY-2008 (NJB)
C
C-&

      EXTERNAL              UDF
      EXTERNAL              ZZGFDIGQ

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               ODD

C
C     Local parameters
C
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfdiu.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 50 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEPSZ
      DOUBLE PRECISION      XDIST

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J

      LOGICAL               DECRES
      LOGICAL               LSSTHN
      LOGICAL               XDECRS
      LOGICAL               XLSTHN

C
C     Save everything.
C
      SAVE

C
C     Initial values
C
      DATA                  CORR / 'NONE',
     .                             'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                             'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFDIU' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load an SPK file as well.
C
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C      CALL FURNSH ( 'de421.bsp' )
C      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized target or observer' )

      REFVAL = 1.D5
      CALL ZZGFREF ( REFVAL )

      CALL ZZGFDIIN ( 'MOOON', 'NONE',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'NONE',  'EEARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and observer are identical' )


      CALL ZZGFDIIN ( 'MOON', 'LT+S',  'MOON' )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction specifiers' )


      CALL ZZGFDIIN ( 'MOON', 'S',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'XS',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'RLT',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'XRLT',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


      CALL ZZGFDIIN ( 'MOON', 'z',  'EARTH' )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFDIDC: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFDIDC ( UDF, ET, DECRES )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFDILT: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFDIGQ: Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIIN ( 'GASPRA', 'LT+S', 'EARTH' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFDIGQ ( ET, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )




C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C     Check values returned by ZZGFDIU entry points for each aberration
C     correction and a variety of times.
C
      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEPSZ = (1.D3/NSAMP) * SPD()


      DO I = 1, NCORR
C
C        Perform tests at a series of times.
C
         ABCORR = CORR(I)

         DO J = 1, NSAMP

            ET = ET0 + (J-1)*STEPSZ

C
C           Use different observer-target pairs.
C
            IF ( ODD(J) ) THEN

               OBSRVR = 'EARTH'
               TARGET = 'MOON'
            ELSE
               OBSRVR = 'SUN'
               TARGET = 'MARS'
            END IF

C
C---- Case -------------------------------------------------------------
C

            TITLE = 'ZZGFDIGQ: check #-# distance. ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )

            REFVAL = 4.D5
            CALL ZZGFREF ( REFVAL )

            CALL ZZGFDIIN ( TARGET, ABCORR, OBSRVR )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL ZZGFDIGQ ( ET, DIST )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL SPKEZR ( TARGET, ET,    'J2000',
     .                    ABCORR, OBSRVR, STATE, LT )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            XDIST = VNORM( STATE )

C
C           We expect a pretty good match.
C
            CALL CHCKSD ( 'DIST', DIST, '~/', XDIST, VTIGHT, OK )


C
C---- Case -------------------------------------------------------------
C
            TITLE = 'ZZGFDILT: check #-# "is less than?" state. '
     .      //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XLSTHN = XDIST .LT. REFVAL

            CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'LSSTHN', LSSTHN, XLSTHN, OK )


C
C---- Case -------------------------------------------------------------
C
            TITLE = 'ZZGFREF: check #-# "is less than?" state. '
     .      //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XLSTHN = XDIST .LT. REFVAL
C
C           Set the reference value so as to invert the
C           expected relationship to the reference value.
C
            IF ( XLSTHN ) THEN
               CALL ZZGFREF ( DPMIN() )
             ELSE
               CALL ZZGFREF ( DPMAX() )
            END IF

            CALL CHCKXC   ( .FALSE., ' ', OK )

            XLSTHN = .NOT. XLSTHN

            CALL ZZGFUDLT( ZZGFDIGQ, ET, LSSTHN )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'LSSTHN', LSSTHN, XLSTHN, OK )

C
C---- Case -------------------------------------------------------------
C
            TITLE = 'ZZGFDIDC: check #-# "is decreasing?" state. '
     .      //      'ABCORR = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMC ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )


            XDECRS = VDOT( STATE, STATE(4) ) .LT. 0.D0

            CALL ZZGFDIDC ( UDF, ET, DECRES )
            CALL CHCKXC   ( .FALSE., ' ',   OK )

            CALL CHCKSL ( 'DECRES', DECRES, XDECRS, OK )

         END DO

      END DO



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
