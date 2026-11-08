C$Procedure F_DDHIVF ( ZZDDHIVF Test Family )
 
      SUBROUTINE F_DDHIVF ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHIVF
C     routine.
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
 
      INCLUDE              'zzddhman.inc'
 
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
C$ Files
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
C$ Particulars
C
C     This routine exercises ZZDDHIVF's logic.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     Local Parameters
C
      INTEGER               DPSIZE
      PARAMETER           ( DPSIZE = 8 )
 
C
C     Local Variables
C
      CHARACTER*(DPSIZE)    NSUM
 
      INTEGER               BFF
 
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHIVF' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'VAX D-Floating Value ''1.0D0''' )
 
C
C     Prepare the inputs and output default values.
C
      BFF   = 0
      FOUND = .FALSE.
 
C
C     We need to store the following sequence of characters in NSUM:
C
C        128 64 0 0 0 0 0 0
C
      NSUM(1:1) = CHAR(128)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)
 
C
C     Invoke the module.
C
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXDFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'VAX D-Floating Value ''3.0D0''' )
 
C
C     Prepare the inputs and output default values.
C
      BFF   = 0
      FOUND = .FALSE.
 
C
C     We need to store the following sequence of characters in NSUM:
C
C        64 65 0 0 0 0 0 0
C
      NSUM(1:1) = CHAR(64)
      NSUM(2:2) = CHAR(65)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)
 
C
C     Invoke the module.
C
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXDFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'VAX G-Floating Value ''3.0D0''' )
 
C
C     Prepare the inputs and output default values.
C
      BFF   = 0
      FOUND = .FALSE.
 
C
C     We need to store the following sequence of characters in NSUM:
C
C        40 64 0 0 0 0 0 0
C
      NSUM(1:1) = CHAR(40)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)
 
C
C     Invoke the module.
C
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'BFF',   BFF,   '=', VAXGFL, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bogus fall through pattern.' )
 
C
C     Prepare the inputs and output default values.
C
      BFF   = 0
      FOUND = .FALSE.
 
C
C     We need to store the following sequence of characters in NSUM:
C
C       144 64 0 0 0 0 0 0
C
      NSUM(1:1) = CHAR(144)
      NSUM(2:2) = CHAR(64)
      NSUM(3:3) = CHAR(0)
      NSUM(4:4) = CHAR(0)
      NSUM(5:5) = CHAR(0)
      NSUM(6:6) = CHAR(0)
      NSUM(7:7) = CHAR(0)
      NSUM(8:8) = CHAR(0)
 
C
C     Invoke the module.
C
      CALL ZZDDHIVF ( NSUM, BFF, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'BFF',   BFF,   '=', 0, 0, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
