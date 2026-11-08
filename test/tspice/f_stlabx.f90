!$Procedure  F_STLABX ( Family of tests for STLABX )

      SUBROUTINE F_STLABX ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests STLABX.
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
! $ Particulars
!
!     Test STLABX by comparing its results to those obtained by
!     an alternate method.
!
! $ Version
!
!-    Version 1.0.0 12-NOV-2001 (NJB)
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      VSEP

!
!     Local parameters
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL  = 1.D-14 )

!
!     Local variables
!
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      OBSVEL ( 3 )
      DOUBLE PRECISION      SAVEC  ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      EXPPOS ( 3 )

!
!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_STLABX' )

!
!     Case 1.
!
      CALL TCASE ( 'Check error handling for excessive speed.' )

      CALL VPACK ( 3.D8, 2.D8, -4.D8,  TRGPOS )
      CALL VPACK ( 1.D10, 1.D9, 1.D10, OBSVEL )

      CALL STLABX ( TRGPOS, OBSVEL, CORPOS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
!     Case 2.
!
      CALL TCASE ( 'Try a routine computation.' )

!
!     Set up a reasonable velocity vector.
!
      CALL VPACK ( 1.D1, 3.D0, 9.D0, OBSVEL )

!
!     Get the transmission stellar aberration correction from
!     STLABX.
!
      CALL STLABX ( TRGPOS, OBSVEL, CORPOS )
      CALL CHCKXC ( .FALSE., ' ',   OK     )

!
!     Obtain the target position vector, corrected for the usual
!     stellar aberration effect.
!
      CALL STELAB ( TRGPOS, OBSVEL, SAVEC )
      CALL CHCKXC ( .FALSE., ' ',   OK    )

!
!     Find the rotation axis about which POJB was rotated to
!     obtain SAVEC.  Find the angular separation between TRGPOS
!     and SAVEC as well.
!
      CALL UCRSS   ( TRGPOS, SAVEC, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ANGLE = VSEP ( TRGPOS, SAVEC )

!
!     The corrected vector we seek is obtained by rotating TRGPOS
!     about AXIS in the opposite direction required to obtain
!     SAVEC.
!
      CALL VROTV ( TRGPOS, AXIS, -ANGLE, EXPPOS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'CORPOS', CORPOS, '~~/', EXPPOS, 3, TOL, OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
