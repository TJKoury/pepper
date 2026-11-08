C$Procedure F_ZZGFRRU ( ZZGFRRU family tests )

      SUBROUTINE F_ZZGFRRU ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB utility package
C
C        ZZGFRRU
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

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'gf.inc'

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
C     This routine tests the SPICELIB geometry utility package ZZGFRRU.
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
C-    TSPICE Version 1.0.0, 19-JUN-2013 (EDW)(BVS)
C
C-&

C
C     Local parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE                  TARGET
      SAVE                  OBSRVR

C
C     Indices 1:2 for Invalid body name test, 3 for not distinct
C     body names test.
C
      DATA     TARGET / 'EARTH', 'X', 'EARTH'/

      DATA     OBSRVR / 'X', 'MARS', 'EARTH' /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFRRU' )



C
C     Case 1: Invalid body names.
C
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



C
C     Case 2: Not distinct body names.
C

      ABCORR = 'NONE'
      I      = 3
      DT     = 1.D0
      TARG   = TARGET(I)
      OBS    = OBSRVR(I)

      TXT = 'Not distinct body name test. '
     .       // 'TARG = #, OBS = #'
      CALL REPMC ( TXT, '#', TARG,  TXT )
      CALL REPMC ( TXT, '#', OBS,   TXT )

      CALL TCASE (TXT)

      CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



C
C     Case 3: DT = 0.
C

      ABCORR = 'NONE'
      DT     = 0.D0
      TARG   = 'EARTH'
      OBS    = 'MOON'

      TXT = 'Delta value zero'

      CALL TCASE (TXT)

      CALL ZZGFRRIN ( TARG, ABCORR, OBS, DT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )



C
C     Case 4: Confirm initialized values are correctly saved.
C

      ABCORR = 'NONE'
      DT     = 1.D0
      TARG   = 'MERCURY'
      OBS    = 'MARS'


      CALL BODS2C ( TARG,  YTARG,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODS2C ( OBS,   YOBS,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TXT = 'Initialize then check saved values in ZZGFRRIN. '
     .       // 'TARG = #, OBS = #, ABCORR = #, DT = #'
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
