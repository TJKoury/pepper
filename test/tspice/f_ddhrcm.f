C$Procedure F_DDHRCM ( ZZDDHRCM Test Family )
 
      SUBROUTINE F_DDHRCM ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHRCM
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
C     This routine exercises ZZDDHRCM's logic.
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
C     SPICELIB Functions
C
      INTEGER               INTMAX
 
C
C     Local Variables
C
      INTEGER               CKCST ( UTSIZE )
      INTEGER               I
      INTEGER               NUT
      INTEGER               REQCNT
      INTEGER               UTCST ( UTSIZE )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHRCM' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty table with REQCNT less than INTMAX.' )
 
C
C     Prepare the inputs and output default values.
C
      UTCST(1) = 500
      NUT      = 0
 
      REQCNT   = 10000
 
C
C     Invoke the module.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', 10001, 0, OK )
 
      CALL CHCKSI ( 'NUT', NUT, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty table with REQCNT at INTMAX.' )
 
C
C     Prepare the inputs and output default values.
C
      UTCST(1) = 500
      NUT      = 0
 
      REQCNT   = INTMAX()
 
C
C     Invoke the module.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', INTMAX()/2 + 1, 0, OK )
 
      CALL CHCKSI ( 'NUT', NUT, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-empty table, REQCNT less than INTMAX.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, NUT
 
         UTCST(I) = 10000*I
         CKCST(I) = 10000*I
 
      END DO
 
      REQCNT = 101000000
 
C
C     Invoke the module.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', 101000001, 0, OK )
 
      CALL CHCKSI ( 'UTCST(1)',  UTCST(1),  '=', CKCST(1),  0, OK )
      CALL CHCKSI ( 'UTCST(2)',  UTCST(2),  '=', CKCST(2),  0, OK )
      CALL CHCKSI ( 'UTCST(3)',  UTCST(3),  '=', CKCST(3),  0, OK )
      CALL CHCKSI ( 'UTCST(4)',  UTCST(4),  '=', CKCST(4),  0, OK )
      CALL CHCKSI ( 'UTCST(5)',  UTCST(5),  '=', CKCST(5),  0, OK )
      CALL CHCKSI ( 'UTCST(6)',  UTCST(6),  '=', CKCST(6),  0, OK )
      CALL CHCKSI ( 'UTCST(7)',  UTCST(7),  '=', CKCST(7),  0, OK )
      CALL CHCKSI ( 'UTCST(8)',  UTCST(8),  '=', CKCST(8),  0, OK )
      CALL CHCKSI ( 'UTCST(9)',  UTCST(9),  '=', CKCST(9),  0, OK )
      CALL CHCKSI ( 'UTCST(10)', UTCST(10), '=', CKCST(10), 0, OK )
 
      CALL CHCKSI ( 'NUT', NUT, '=', 10, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-empty table, REQCNT is INTMAX.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, NUT
 
         UTCST(I) = 10000*I
         CKCST(I) =  5000*I
 
      END DO
 
C
C     Set REQCNT to INTMAX and UTCST(7) to 1 to test the
C     MAX(1,...) logic.
C
      REQCNT   = INTMAX()
      UTCST(7) = 1
      CKCST(7) = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'REQCNT', REQCNT, '=', INTMAX()/2 + 1, 0, OK )
 
      CALL CHCKSI ( 'UTCST(1)',  UTCST(1),  '=', CKCST(1),  0, OK )
      CALL CHCKSI ( 'UTCST(2)',  UTCST(2),  '=', CKCST(2),  0, OK )
      CALL CHCKSI ( 'UTCST(3)',  UTCST(3),  '=', CKCST(3),  0, OK )
      CALL CHCKSI ( 'UTCST(4)',  UTCST(4),  '=', CKCST(4),  0, OK )
      CALL CHCKSI ( 'UTCST(5)',  UTCST(5),  '=', CKCST(5),  0, OK )
      CALL CHCKSI ( 'UTCST(6)',  UTCST(6),  '=', CKCST(6),  0, OK )
      CALL CHCKSI ( 'UTCST(7)',  UTCST(7),  '=', CKCST(7),  0, OK )
      CALL CHCKSI ( 'UTCST(8)',  UTCST(8),  '=', CKCST(8),  0, OK )
      CALL CHCKSI ( 'UTCST(9)',  UTCST(9),  '=', CKCST(9),  0, OK )
      CALL CHCKSI ( 'UTCST(10)', UTCST(10), '=', CKCST(10), 0, OK )
 
      CALL CHCKSI ( 'NUT', NUT, '=', 10, 0, OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
