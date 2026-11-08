!$Procedure      F_ZZGFDIQ ( ZZGFDIQ tests )

      SUBROUTINE F_ZZGFDIQ ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB utility ZZGFDIQ.
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
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
!     This routine tests the SPICELIB geometry utility ZZGFDIQ.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 22-MAY-2008 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      LOGICAL               ODD

!
!     Local parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfdiq.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )



      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 50 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      STEPSZ
      DOUBLE PRECISION      XDIST

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               OBSRVR
      INTEGER               TARGET


!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFDIQ' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!      CALL FURNSH ( 'de421.bsp' )
!      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction specifiers' )


      CALL ZZGFDIQ ( 301, 0.D0, 'S',  399, POS )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',   OK )


      CALL ZZGFDIQ ( 301, 0.D0, 'XS',  399, POS )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',   OK )


      CALL ZZGFDIQ ( 301, 0.D0, 'RLT',  399, POS )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',   OK )


      CALL ZZGFDIQ ( 301, 0.D0, 'XRLT',  399, POS )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',   OK )


      CALL ZZGFDIQ ( 301, 0.D0, 'z',  399, POS )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Ephemeris data unavailable' )


      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFDIQ ( 9511010, ET, 'LT+S', 399, POS )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )





!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     Check values returned by ZZGFDIQ entry points for each aberration
!     correction and a variety of times.
!
      TIMSTR = '2008 MAY 22'
      CALL STR2ET ( TIMSTR, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEPSZ = (1.D3/NSAMP) * SPD()


      DO I = 1, NCORR
!
!        Perform tests at a series of times.
!
         ABCORR = CORR(I)


         DO J = 1, NSAMP

            ET = ET0 + (J-1)*STEPSZ

!
!           Use different observer-target pairs.
!
            IF ( ODD(J) ) THEN

               OBSRVR = 399
               TARGET = 301
            ELSE
               OBSRVR = 10
               TARGET = 499
            END IF

!
!---- Case -------------------------------------------------------------
!

            TITLE = 'ZZGFDIQ: check #-# distance. ABCORR = #.'
            CALL REPMI ( TITLE, '#', OBSRVR,    TITLE )
            CALL REPMI ( TITLE, '#', TARGET,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,    TITLE )

            CALL TCASE ( TITLE )

            CALL ZZGFDIQ ( TARGET, ET, ABCORR, OBSRVR, DIST )
            CALL CHCKXC  ( .FALSE., ' ',   OK )

            CALL SPKEZP ( TARGET, ET,    'J2000', &
                          ABCORR, OBSRVR, POS,   LT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            XDIST = VNORM( POS )

!
!           We expect a pretty good match.
!
            CALL CHCKSD ( 'DIST', DIST, '~/', XDIST, VTIGHT, OK )


         END DO

      END DO



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
