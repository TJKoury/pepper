!$Procedure F_ZZGFRRU ( ZZGFRRU family tests )

      SUBROUTINE F_ZZGFRRU ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB utility package
!
!        ZZGFRRU
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

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'gf.inc'

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
!     This routine tests the SPICELIB geometry utility package ZZGFRRU.
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
!-    TSPICE Version 1.0.0, 19-JUN-2013 (EDW)(BVS)
!
!-&

!
!     Local parameters
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


!
!     Local variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(CORLEN)    XABCOR
      CHARACTER*(BDNMLN)    TARGET ( 3 )
      CHARACTER*(BDNMLN)    OBSRVR ( 3 )
      CHARACTER*(BDNMLN)    TARG
      CHARACTER*(BDNMLN)    OBS
      CHARACTER*(2*LNSIZE)  TXT

      INTEGER               I
      INTEGER               XTARG
      INTEGER               XOBS
      INTEGER               YTARG
      INTEGER               YOBS

      LOGICAL               FOUND

      DOUBLE PRECISION      DT
      DOUBLE PRECISION      XDT

!
!     Saved variables
!
      SAVE                  TARGET
      SAVE                  OBSRVR

!
!     Indices 1:2 for Invalid body name test, 3 for not distinct
!     body names test.
!
      DATA     TARGET / 'EARTH', 'X', 'EARTH'/

      DATA     OBSRVR / 'X', 'MARS', 'EARTH' /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFRRU' )



!
!     Case 1: Invalid body names.
!
      ABCORR = 'NONE'
      DT = 1.D0

      DO I=1, 2

         TARG  = TARGET(I)
         OBS   = OBSRVR(I)

         TXT = 'Invalid body name test. TARG = #, OBS = #'
         CALL REPMC ( TXT, '#', TARG,  TXT )
         CALL REPMC ( TXT, '#', OBS,   TXT )

         CALL TCASE (TXT)

         CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
         CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      END DO



!
!     Case 2: Not distinct body names.
!

      ABCORR = 'NONE'
      I      = 3
      DT     = 1.D0
      TARG   = TARGET(I)
      OBS    = OBSRVR(I)

      TXT = 'Not distinct body name test. ' &
             // 'TARG = #, OBS = #'
      CALL REPMC ( TXT, '#', TARG,  TXT )
      CALL REPMC ( TXT, '#', OBS,   TXT )

      CALL TCASE (TXT)

      CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



!
!     Case 3: DT = 0.
!

      ABCORR = 'NONE'
      DT     = 0.D0
      TARG   = 'EARTH'
      OBS    = 'MOON'

      TXT = 'Delta value zero'

      CALL TCASE (TXT)

      CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )



!
!     Case 4: Confirm initialized values are correctly saved.
!

      ABCORR = 'NONE'
      DT     = 1.D0
      TARG   = 'MERCURY'
      OBS    = 'MARS'


      CALL BODS2C ( TARG,  YTARG,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODS2C ( OBS,   YOBS,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TXT = 'Initialize then check saved values in ZZGFRRIN. ' &
             // 'TARG = #, OBS = #, ABCORR = #, DT = #'
      CALL REPMC ( TXT, '#', TARG,   TXT )
      CALL REPMC ( TXT, '#', OBS,    TXT )
      CALL REPMC ( TXT, '#', ABCORR, TXT )
      CALL REPMD ( TXT, '#', DT, 2,  TXT )

      CALL TCASE (TXT)

      CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFRRX ( XTARG, XABCOR, XOBS, XDT )

      CALL CHCKSI ( 'TARG X vs Y',  XTARG,  '=', YTARG,  0, OK )
      CALL CHCKSI ( 'OBS X vs Y',   XOBS,   '=', YOBS,   0, OK )

      CALL UCASE  ( ABCORR, ABCORR )
      CALL CHCKSC ( 'ABCORR vs XABCORR',  XABCOR, '=', ABCORR, OK )

      CALL CHCKSD ( 'DT vs XDT', DT, '=', XDT, 0.D0, OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
