C$Procedure F_DDHCLU ( ZZDDHCLU Test Family )
 
      SUBROUTINE F_DDHCLU ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHCLU
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
C     This routine exercises ZZDDHCLU's logic.
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
      INTEGER               ZZDDHCLU
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               NUT
      INTEGER               OUTPUT
 
      LOGICAL               UTLCK ( UTSIZE )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHCLU' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty table exceptional case.' )
 
C
C     Prepare the inputs and output default values.
C
      UTLCK(1) = .TRUE.
      NUT      = 0
 
      OUTPUT   = 10
 
C
C     Invoke the module.
C
      OUTPUT = ZZDDHCLU ( UTLCK, NUT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'ZZDDHCLU', OUTPUT, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-empty table, nominal case.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, NUT
 
         UTLCK(I) = .FALSE.
 
      END DO
 
      UTLCK(NUT) = .TRUE.
      UTLCK(1)   = .TRUE.
 
      OUTPUT = 0
 
C
C     Invoke the module.
C
      OUTPUT = ZZDDHCLU ( UTLCK, NUT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'ZZDDHCLU', OUTPUT, '=', 2, 0, OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
