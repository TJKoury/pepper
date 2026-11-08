C$Procedure F_DDHRMU ( ZZDDHRMU Test Family )
 
      SUBROUTINE F_DDHRMU ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHRMU
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
C     This routine exercises ZZDDHRMU's logic.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 13-DEC-2011 (EDW)
C
C       All variables to SAVE for f2c'd version compiled with Borland.
C       Implemented a proper SPICE header.
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               LUN
      INTEGER               NFT
      INTEGER               NUT
      INTEGER               UINDEX
      LOGICAL               RESRVD
 
C
C     The unit table columns
C
      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHRMU' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty unit table exceptional case.' )
 
C
C     Prepare the inputs and output default values.
C
      UINDEX = 1
      NUT    = 0
      NFT    = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                UTLCK,  UTLUN, NUT           )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'UINDEX out of range exception.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, NUT
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX = 5000
      NFT    = 12
 
C
C     Invoke the module.
C
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                UTLCK,  UTLUN, NUT           )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test table compression case.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 4
 
      DO I = 1, NUT
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX = 2
      NFT    = 3
 
C
C     Invoke the module.
C
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                UTLCK,  UTLUN, NUT           )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=', 10, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=', 1,  0, OK )
      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '=', 1,  0, OK )
 
      CALL CHCKSI ( 'UTCST(2)', UTCST(2), '=', 30, 0, OK )
      CALL CHCKSI ( 'UTHAN(2)', UTHAN(2), '=', 3,  0, OK )
      CALL CHCKSL ( 'UTLCK(2)', UTLCK(2), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(2)', UTLUN(2), '=', 3,  0, OK )
 
      CALL CHCKSI ( 'UTCST(3)', UTCST(3), '=', 40, 0, OK )
      CALL CHCKSI ( 'UTHAN(3)', UTHAN(3), '=', 4,  0, OK )
      CALL CHCKSL ( 'UTLCK(3)', UTLCK(3), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(3)', UTLUN(3), '=', 4,  0, OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=', 3,  0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Zero row test case.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 4
 
      DO I = 1, NUT
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX = 2
      NFT    = 4
 
C
C     Setup row 2 with an actual logical unit.
C
      CALL GETLUN ( LUN )
      UTLUN(2) = LUN
 
C
C     Invoke the module.
C
      CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN,
     .                UTLCK,  UTLUN, NUT           )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs
C
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=', 10, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=', 1,  0, OK )
      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '=', 1,  0, OK )
 
      CALL CHCKSI ( 'UTCST(2)', UTCST(2), '=', 0,   0, OK )
      CALL CHCKSI ( 'UTHAN(2)', UTHAN(2), '=', 0,   0, OK )
      CALL CHCKSL ( 'UTLCK(2)', UTLCK(2), .FALSE.,     OK )
      CALL CHCKSI ( 'UTLUN(2)', UTLUN(2), '=', LUN, 0, OK )
 
      CALL CHCKSI ( 'UTCST(3)', UTCST(3), '=', 30, 0, OK )
      CALL CHCKSI ( 'UTHAN(3)', UTHAN(3), '=', 3,  0, OK )
      CALL CHCKSL ( 'UTLCK(3)', UTLCK(3), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(3)', UTLUN(3), '=', 3,  0, OK )
 
      CALL CHCKSI ( 'UTCST(4)', UTCST(4), '=', 40, 0, OK )
      CALL CHCKSI ( 'UTHAN(4)', UTHAN(4), '=', 4,  0, OK )
      CALL CHCKSL ( 'UTLCK(4)', UTLCK(4), .FALSE.,    OK )
      CALL CHCKSI ( 'UTLUN(4)', UTLUN(4), '=', 4,  0, OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=', 4,  0, OK )
 
C
C     Check to see if LUN is reserved.
C
      CALL T_TSTRLN ( LUN, RESRVD )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the value of RESRVD.
C
      CALL CHCKSL ( 'RESRVD', RESRVD, .TRUE., OK )
 
C
C     Free up the logical unit.
C
      CALL FRELUN ( LUN )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
