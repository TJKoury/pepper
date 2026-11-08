C$Procedure F_ZZGFPAU ( ZZGFPAU family tests )

      SUBROUTINE F_ZZGFPAU ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB utility package
C
C        ZZGFPAU
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
C     This routine tests the SPICELIB geometry utility package ZZGFPAU.
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
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 18-NOV-2013 (EDW)
C
C-&

      EXTERNAL              UDF

C
C     Local parameters
C

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'nat.pck' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'nat.spk' )

      INTEGER               NRCORR
      PARAMETER           ( NRCORR = 5 )

      INTEGER               NXCORR
      PARAMETER           ( NXCORR = 4 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


C
C     Local variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(CORLEN)    XABCOR
      CHARACTER*(CORLEN)    RCORR  (NRCORR)
      CHARACTER*(CORLEN)    XCORR  (NXCORR)
      CHARACTER*(BDNMLN)    TARGET ( 6 )
      CHARACTER*(BDNMLN)    ILLUMN ( 6 )
      CHARACTER*(BDNMLN)    OBSRVR ( 6 )
      CHARACTER*(BDNMLN)    TARG
      CHARACTER*(BDNMLN)    ILLUM
      CHARACTER*(BDNMLN)    OBS
      CHARACTER*(2*LNSIZE)  TXT

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RVL

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               XTARG
      INTEGER               XILLUM
      INTEGER               XOBS
      INTEGER               YTARG
      INTEGER               YILLUM
      INTEGER               YOBS

      LOGICAL               XABLK (NABCOR)
      LOGICAL               ABLK  (NABCOR)
      LOGICAL               DECRES
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE                  RCORR
      SAVE                  XCORR
      SAVE                  TARGET
      SAVE                  ILLUMN
      SAVE                  OBSRVR

      DATA                  RCORR /  'nOne',
     .                               'lT',
     .                               'Cn',
     .                               'Lt+s',
     .                               'cN+S' /

      DATA                  XCORR /  'xlT',
     .                               'xCn',
     .                               'XLt+s',
     .                               'XcN+S'   /

C
C     Indices 1:3 for Invalid body name test, 4:6 for not distinct
C     body names test.
C
      DATA     TARGET / 'ALPHA', 'ALPHA', 'X', 'ALPHA', 'SUN', 'ALPHA'/

      DATA     ILLUMN / 'SUN', 'X',  'SUN', 'ALPHA', 'SUN', 'SUN'     /

      DATA     OBSRVR / 'X', 'BETA', 'BETA', 'BETA', 'ALPHA', 'SUN'   /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFPAU' )




C
C     Case 1: Create test kernels.
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create the PCK for Nat's Solar System.
C
      CALL NATPCK ( PCK,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )



C
C     Case 2: Invalid body names.
C
      ABCORR = RCORR(1)

      DO I=1, 3

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Invalid body name test. TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )
         CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      END DO




C
C     Case 3: Invalid aberration corrections.
C

C
C     Set appropriate values for TARGET, ILLUMN, OBSRVR.
C
      TARG  = TARGET(1)
      ILLUM = ILLUMN(1)
      OBS   = OBSRVR(2)

      DO I=1, NXCORR

         ABCORR = XCORR(I)

         TXT = 'Invalid aberration correction. ABCOR = #'
         CALL REPMC ( TXT, '#', ABCORR,  TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO




C
C     Case 4: Not distinct body names.
C

C
C     Set appropriate values for ABCORR.
C
      ABCORR = RCORR(1)

      DO I=4, 6

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Not distinct body name test. '
     .       // 'TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )
         CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      END DO




C
C     Case 5: Confirm initialized values are correctly saved.
C

      TARG   = TARGET(1)
      ILLUM  = ILLUMN(1)
      OBS    = OBSRVR(2)
      ABCORR = RCORR(1)


      CALL BODS2C ( TARG,  YTARG,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODS2C ( ILLUM, YILLUM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODS2C ( OBS,   YOBS,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Aberration correction NONE.
C

      DO J=1, ABATSZ
         ABLK (J) = .FALSE.
         XABLK(J) = .FALSE.
      END DO
         
      TXT = 'Initialize then check saved values in ZZGFPAIN. '
     .       // 'TARG = #, ILLUM = #, OBS = #, ABCORR = #.'
      CALL REPMC ( TXT, '#', TARG,   TXT )
      CALL REPMC ( TXT, '#', ILLUM,  TXT )
      CALL REPMC ( TXT, '#', OBS,    TXT )
      CALL REPMC ( TXT, '#', ABCORR, TXT )

      CALL TCASE (TXT)

      CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )

      CALL ZZGFPAX ( XTARG, XILLUM, XABCOR, XOBS, XABLK )

      CALL CHCKSI ( 'TARG X vs Y',  XTARG,  '=', YTARG,  0, OK )
      CALL CHCKSI ( 'ILLUM X vs Y', XILLUM, '=', YILLUM, 0, OK )
      CALL CHCKSI ( 'OBS X vs Y',   XOBS,   '=', YOBS,   0, OK )

      CALL UCASE  ( ABCORR, ABCORR )
      CALL CHCKSC ( 'ABCORR vs XABCORR',  XABCOR, '=', ABCORR, OK )

      CALL ZZVALCOR ( ABCORR, ABLK )

      DO J=1, ABATSZ
         CALL CHCKSL ( 'ABLK', XABLK(J), ABLK(J), OK )
      END DO



C
C     Aberration correction not NONE.
C

      DO I = 2, NRCORR

         DO J=1, ABATSZ
            ABLK (J) = .FALSE.
            XABLK(J) = .FALSE.
         END DO

         TXT = 'Initialize then check saved values in ZZGFPAIN. '
     .       // 'TARG = #, ILLUM = #, OBS = #, ABCORR = #.'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         ABCORR = RCORR(I)

         CALL REPMC ( TXT, '#', ABCORR, TXT )

         CALL ZZVALCOR ( ABCORR, ABLK )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGFPAX ( XTARG, XILLUM, XABCOR, XOBS, XABLK )

         CALL UCASE  ( ABCORR, ABCORR )
         CALL CHCKSC ( 'ABCORR vs Y',  XABCOR, '=', ABCORR, OK )

         DO J=1, ABATSZ
            CALL CHCKSL ( TXT, XABLK(J), ABLK(J), OK )
         END DO

      END DO


C
C     Case 6: Check calculation of phase angle derivative.
C

C
C     Occultation of ALPHA by BETA as observed from Sol starts
C     at 0 TDB secs from J2000 epoch (geometric). The occultation
C     lasts ten minutes.
C
C     Confirm the phase angle decreases prior to occultation
C     and increases post occultation.
C
      TARG   = 'ALPHA'
      ILLUM  = 'SUN'
      OBS    = 'BETA'

C
C     Loop over all aberration corrections.
C
      DO I = 1, NRCORR

         ABCORR = RCORR(I)

         TXT = 'Phase angle decreasing prior and increasing post '
     .       // 'occultaion. ABCORR = #.'
         CALL REPMC ( TXT, '#', ABCORR, TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )

         DECRES = .FALSE.

C
C        Ten minutes prior to start of occultation.
C
         ET = -10.D0 * 60.D0
         CALL ZZGFPAGQ ( ET, RVL )
         CALL ZZGFPADC ( UDF, ET, DECRES )

         CALL CHCKSL ( TXT, DECRES, .TRUE., OK )

C
C        Ten minutes post to start of occultation.
C
         ET = 10.D0 * 60.D0
         CALL ZZGFPAGQ ( ET, RVL )
         CALL ZZGFPADC ( UDF, ET, DECRES )

         CALL CHCKSL ( TXT, DECRES, .FALSE., OK )

      END DO


C
C     Case N:
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR()

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
