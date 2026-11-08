!$Procedure      F_LS ( Test solar longitude routines )

      SUBROUTINE F_LS ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        LSPCN
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
! $ Version
!
!-    TSPICE Version 1.0.0, 07-JAN-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      LSPCN

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK = 'f_ls.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK = 'f_ls.bsp' )


      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-3 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 100 )

      INTEGER               NCORR
      PARAMETER           ( NCORR  = 3 )

      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 15 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

!
!     Local variables
!
      CHARACTER*(CORLEN)    ABCORR ( NCORR )
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LS
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J

!
!     Saved variables
!
      SAVE                  ABCORR

!
!     Initial values
!
      DATA                  ABCORR / 'NONE', 'LT', 'LT+S' /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_LS' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create and load kernels.' )
!
!     Create, load, and delete a PCK, an SPK and an LSK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test for all aberration corrections.
!
      DO I = 1, NCORR
!
!        For a variety of times of year, find the RA of
!        the earth-sun vector in the ECLIPJ2000 frame.
!
         DO J = 1, NCASE

!
! --- Case: ------------------------------------------------------
!
            ET    = (J-1) * JYEAR() / NCASE

            TITLE = 'ET = #; ABCORR = #'

            CALL ETCAL  ( ET, TIMSTR )
            CALL REPMC  ( TITLE, '#', TIMSTR,    TITLE )
            CALL REPMC  ( TITLE, '#', ABCORR(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( TITLE )

            CALL SPKEZR ( 'SUN',     ET,      'ECLIPJ2000', &
                          ABCORR(I), 'EARTH', SSTATE,        LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECRAD ( SSTATE, RANGE, RA, DEC )

!
!           Find Ls.
!
            LS = LSPCN  ( 'EARTH', ET, ABCORR(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( 'LS',  LS, '~', RA, LOOSE, OK )

         END DO


      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete SPK file.' )
!
!     Get rid of the SPK file.  First unload using the routine
!     corresponding to the loader called by TSTSPK.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
