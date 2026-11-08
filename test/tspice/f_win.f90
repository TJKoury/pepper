!$Procedure F_WIN ( Window family tests )

      SUBROUTINE F_WIN ( OK )

! $ Abstract
!
!     This routine tests SPICELIB window routines.
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
!     This routine does not generate errors. Routines in its
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
!     None.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 05-OCT-2021 (EDW)
!
!        Altered string length constant, 116 to 115, to
!        reflect typo corrections in ZZWNINSD.
!
!-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               POS

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 20000 )


!
!     Local variables
!
      CHARACTER*(320)       MSG

      DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL )

      INTEGER               M
      DOUBLE PRECISION      MEAS
      DOUBLE PRECISION      AVG
      DOUBLE PRECISION      STDDEV
      INTEGER               SHORT
      INTEGER               LONG

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_WIN' )


!
!   Case 1
!
!
      CALL TCASE ( 'Check ZZWNINSD error cases' )

      CALL SSIZED ( MAXVAL, RESULT )
      CALL SCARDD ( 0,      RESULT )

!
!     Try to add an invalid interval to the window.
!
      CALL ZZWNINSD ( 0.D0, -1.D0, 'TEST_STRING_1', RESULT  )

      CALL GETMSG ( 'LONG' , MSG )
      CALL CHCKXC ( .TRUE., 'SPICE(BADENDPOINTS)', OK )

      M = POS ( MSG, 'TEST_STRING_1', 1 )
      CALL CHCKSI ( 'TEST_STRING_1', M, '=', 115, 0, OK )

!
!     Empty the window, then reduce size to zero.
!
      CALL SSIZED ( 0, RESULT )
      CALL SCARDD ( 0, RESULT )

!
!     Try to add an interval to the window.
!
      CALL ZZWNINSD ( 0.D0,  1.D0, 'TEST_STRING_2', RESULT )

      CALL GETMSG ( 'LONG' , MSG )
      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)', OK )

      M = POS ( MSG, 'TEST_STRING_2', 1 )
      CALL CHCKSI ( 'TEST_STRING_2', M, '=', 90, 0, OK )




!
!   Case 2
!
!
      CALL TCASE ( 'Check WNSUMD error case' )

!
!     An odd cardinality for a WNSUMD input window should
!     signal an error.
!
      CALL SSIZED ( MAXVAL, RESULT )
      CALL SCARDD ( 13,      RESULT )

      CALL WNSUMD ( RESULT, MEAS,  AVG, STDDEV, &
                            SHORT, LONG )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCARDINALITY)', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



!
!23456789012345678901234567890123456789012345678901234567890123456789012
!
