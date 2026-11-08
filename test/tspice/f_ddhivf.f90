!$Procedure F_DDHIVF ( ZZDDHIVF Test Family )

      SUBROUTINE F_DDHIVF ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHIVF
!     routine.
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

      INCLUDE              'zzddhman.inc'

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
! $ Files
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
! $ Particulars
!
!     This routine exercises ZZDDHIVF's logic.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     Local Parameters
!
      INTEGER               DPSIZE
      PARAMETER           ( DPSIZE = 8 )

!
!     Local Variables
!
      CHARACTER*(DPSIZE)    NSUM

      INTEGER               BFF

      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHIVF' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'VAX D-Floating Value ''1.0D0''' )

!
!     Prepare the inputs and output default values.
!
      BFF   = 0
      FOUND = .FALSE.

!
!     We need to store the following sequence of characters in NSUM:
!
!        128 64 0 0 0 0 0 0
!
      NSUM(1:1) = CHAR(128)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)

!
!     Invoke the module.
!
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXDFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'VAX D-Floating Value ''3.0D0''' )

!
!     Prepare the inputs and output default values.
!
      BFF   = 0
      FOUND = .FALSE.

!
!     We need to store the following sequence of characters in NSUM:
!
!        64 65 0 0 0 0 0 0
!
      NSUM(1:1) = CHAR(64)
      NSUM(2:2) = CHAR(65)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)

!
!     Invoke the module.
!
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXDFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'VAX G-Floating Value ''3.0D0''' )

!
!     Prepare the inputs and output default values.
!
      BFF   = 0
      FOUND = .FALSE.

!
!     We need to store the following sequence of characters in NSUM:
!
!        40 64 0 0 0 0 0 0
!
      NSUM(1:1) = CHAR(40)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)

!
!     Invoke the module.
!
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXGFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bogus fall through pattern.' )

!
!     Prepare the inputs and output default values.
!
      BFF   = 0
      FOUND = .FALSE.

!
!     We need to store the following sequence of characters in NSUM:
!
!       144 64 0 0 0 0 0 0
!
      NSUM(1:1) = CHAR(144)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)

!
!     Invoke the module.
!
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSI ( 'BFF',   BFF,   '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
