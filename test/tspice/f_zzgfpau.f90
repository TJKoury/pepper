!$Procedure F_ZZGFPAU ( ZZGFPAU family tests )

      SUBROUTINE F_ZZGFPAU ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB utility package
!
!        ZZGFPAU
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
!     This routine tests the SPICELIB geometry utility package ZZGFPAU.
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
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 18-NOV-2013 (EDW)
!
!-&

      EXTERNAL              UDF

!
!     Local parameters
!

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


!
!     Local variables
!
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

!
!     Saved variables
!
      SAVE                  RCORR
      SAVE                  XCORR
      SAVE                  TARGET
      SAVE                  ILLUMN
      SAVE                  OBSRVR

      DATA                  RCORR /  'nOne', &
                                     'lT', &
                                     'Cn', &
                                     'Lt+s', &
                                     'cN+S' /

      DATA                  XCORR /  'xlT', &
                                     'xCn', &
                                     'XLt+s', &
                                     'XcN+S'   /

!
!     Indices 1:3 for Invalid body name test, 4:6 for not distinct
!     body names test.
!
      DATA     TARGET / 'ALPHA', 'ALPHA', 'X', 'ALPHA', 'SUN', 'ALPHA'/

      DATA     ILLUMN / 'SUN', 'X',  'SUN', 'ALPHA', 'SUN', 'SUN'     /

      DATA     OBSRVR / 'X', 'BETA', 'BETA', 'BETA', 'ALPHA', 'SUN'   /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFPAU' )




!
!     Case 1: Create test kernels.
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK ( PCK,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )



!
!     Case 2: Invalid body names.
!
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




!
!     Case 3: Invalid aberration corrections.
!

!
!     Set appropriate values for TARGET, ILLUMN, OBSRVR.
!
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




!
!     Case 4: Not distinct body names.
!

!
!     Set appropriate values for ABCORR.
!
      ABCORR = RCORR(1)

      DO I=4, 6

         TARG  = TARGET(I)
         ILLUM = ILLUMN(I)
         OBS   = OBSRVR(I)

         TXT = 'Not distinct body name test. ' &
             // 'TARG = #, ILLUM = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', ILLUM, TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )
         CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      END DO




!
!     Case 5: Confirm initialized values are correctly saved.
!

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

!
!     Aberration correction NONE.
!

      DO J=1, ABATSZ
         ABLK (J) = .FALSE.
         XABLK(J) = .FALSE.
      END DO

      TXT = 'Initialize then check saved values in ZZGFPAIN. ' &
             // 'TARG = #, ILLUM = #, OBS = #, ABCORR = #.'
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



!
!     Aberration correction not NONE.
!

      DO I = 2, NRCORR

         DO J=1, ABATSZ
            ABLK (J) = .FALSE.
            XABLK(J) = .FALSE.
         END DO

         TXT = 'Initialize then check saved values in ZZGFPAIN. ' &
             // 'TARG = #, ILLUM = #, OBS = #, ABCORR = #.'
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


!
!     Case 6: Check calculation of phase angle derivative.
!

!
!     Occultation of ALPHA by BETA as observed from Sol starts
!     at 0 TDB secs from J2000 epoch (geometric). The occultation
!     lasts ten minutes.
!
!     Confirm the phase angle decreases prior to occultation
!     and increases post occultation.
!
      TARG   = 'ALPHA'
      ILLUM  = 'SUN'
      OBS    = 'BETA'

!
!     Loop over all aberration corrections.
!
      DO I = 1, NRCORR

         ABCORR = RCORR(I)

         TXT = 'Phase angle decreasing prior and increasing post ' &
             // 'occultaion. ABCORR = #.'
         CALL REPMC ( TXT, '#', ABCORR, TXT )

         CALL TCASE (TXT)

         CALL ZZGFPAIN ( TARG, ILLUM, ABCORR, OBS )

         DECRES = .FALSE.

!
!        Ten minutes prior to start of occultation.
!
         ET = -10.D0 * 60.D0
         CALL ZZGFPAGQ ( ET, RVL )
         CALL ZZGFPADC ( UDF, ET, DECRES )

         CALL CHCKSL ( TXT, DECRES, .TRUE., OK )

!
!        Ten minutes post to start of occultation.
!
         ET = 10.D0 * 60.D0
         CALL ZZGFPAGQ ( ET, RVL )
         CALL ZZGFPADC ( UDF, ET, DECRES )

         CALL CHCKSL ( TXT, DECRES, .FALSE., OK )

      END DO


!
!     Case N:
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR()

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
