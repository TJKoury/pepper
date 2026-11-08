C$Procedure      F_LS ( Test solar longitude routines )

      SUBROUTINE F_LS ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        LSPCN
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
C$ Version
C
C-    TSPICE Version 1.0.0, 07-JAN-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      LSPCN

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE                  ABCORR

C
C     Initial values
C
      DATA                  ABCORR / 'NONE', 'LT', 'LT+S' /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_LS' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create and load kernels.' )
C
C     Create, load, and delete a PCK, an SPK and an LSK.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test for all aberration corrections.     
C
      DO I = 1, NCORR
C
C        For a variety of times of year, find the RA of
C        the earth-sun vector in the ECLIPJ2000 frame.  
C
         DO J = 1, NCASE

C
C --- Case: ------------------------------------------------------
C
            ET    = (J-1) * JYEAR() / NCASE

            TITLE = 'ET = #; ABCORR = #'

            CALL ETCAL  ( ET, TIMSTR )
            CALL REPMC  ( TITLE, '#', TIMSTR,    TITLE )
            CALL REPMC  ( TITLE, '#', ABCORR(I), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL TCASE  ( TITLE )

            CALL SPKEZR ( 'SUN',     ET,      'ECLIPJ2000', 
     .                    ABCORR(I), 'EARTH', SSTATE,        LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECRAD ( SSTATE, RANGE, RA, DEC )
            
C
C           Find Ls.
C
            LS = LSPCN  ( 'EARTH', ET, ABCORR(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( 'LS',  LS, '~', RA, LOOSE, OK )

         END DO


      END DO



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete SPK file.' )
C
C     Get rid of the SPK file.  First unload using the routine
C     corresponding to the loader called by TSTSPK.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL DELFIL ( SPK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
