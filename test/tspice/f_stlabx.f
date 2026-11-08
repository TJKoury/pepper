C$Procedure  F_STLABX ( Family of tests for STLABX )
 
      SUBROUTINE F_STLABX ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests STLABX.
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
C$ Particulars
C
C     Test STLABX by comparing its results to those obtained by
C     an alternate method.
C
C$ Version
C
C-    Version 1.0.0 12-NOV-2001 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP
 
C
C     Local parameters
C 
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL  = 1.D-14 )

C
C     Local variables
C 
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      OBSVEL ( 3 )
      DOUBLE PRECISION      SAVEC  ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      EXPPOS ( 3 )

C 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_STLABX' )
 
C
C     Case 1.
C
      CALL TCASE ( 'Check error handling for excessive speed.' )
 
      CALL VPACK ( 3.D8, 2.D8, -4.D8,  TRGPOS )
      CALL VPACK ( 1.D10, 1.D9, 1.D10, OBSVEL )

      CALL STLABX ( TRGPOS, OBSVEL, CORPOS )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

C
C     Case 2.
C
      CALL TCASE ( 'Try a routine computation.' )

C
C     Set up a reasonable velocity vector.
C     
      CALL VPACK ( 1.D1, 3.D0, 9.D0, OBSVEL )

C
C     Get the transmission stellar aberration correction from 
C     STLABX.
C
      CALL STLABX ( TRGPOS, OBSVEL, CORPOS )
      CALL CHCKXC ( .FALSE., ' ',   OK     )

C     
C     Obtain the target position vector, corrected for the usual
C     stellar aberration effect.
C 
      CALL STELAB ( TRGPOS, OBSVEL, SAVEC )
      CALL CHCKXC ( .FALSE., ' ',   OK    )

C
C     Find the rotation axis about which POJB was rotated to
C     obtain SAVEC.  Find the angular separation between TRGPOS
C     and SAVEC as well.
C
      CALL UCRSS   ( TRGPOS, SAVEC, AXIS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ANGLE = VSEP ( TRGPOS, SAVEC )

C
C     The corrected vector we seek is obtained by rotating TRGPOS 
C     about AXIS in the opposite direction required to obtain
C     SAVEC.
C
      CALL VROTV ( TRGPOS, AXIS, -ANGLE, EXPPOS )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'CORPOS', CORPOS, '~~/', EXPPOS, 3, TOL, OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
