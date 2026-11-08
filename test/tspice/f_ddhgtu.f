C$Procedure F_DDHGTU ( ZZDDHGTU Test Family )
 
      SUBROUTINE F_DDHGTU ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHGTU
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
C     This routine exercises ZZDDHGTU's logic.  Functionality it
C     does not properly test at the moment:
C
C     (1) When GETLUN signals an error, does this routine return
C         UINDEX into a row with a -1 value for UTLUN(UINDEX).
C
C     Point (1) could be tested by adding a pile of RESLUN calls
C     to this module.
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
C     Local Variables
C
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               LUN
      INTEGER               UINDEX
 
      LOGICAL               OPENED
      LOGICAL               RESRVD
 
C
C     Unit Table
C
      INTEGER               NUT
 
      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHGTU' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Empty unit table insertion.' )
 
C
C     Prepare the inputs and output default values.
C
      UTCST (1) = 1
      UTHAN (1) = 1
      UTLCK (1) = .TRUE.
      UTLUN (1) = 1
 
      NUT       = 0
 
      UINDEX    = -1
 
C
C     Invoke the module.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now check the values.
C
      CALL CHCKSI ( 'UTCST(1)', UTCST(1), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(1)', UTHAN(1), '=',  0, 0, OK )
 
      CALL CHCKSL ( 'UTLCK(1)', UTLCK(1), .FALSE.,    OK )
 
      CALL CHCKSI ( 'UTLUN(1)', UTLUN(1), '>=', 0, 0, OK )
      CALL CHCKSI ( 'NUT',      NUT,      '=',  1, 0, OK )
 
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=',  1, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Zero cost row exists in the unit table' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, 10
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
C
C     Create the zero cost row.
C
      UTCST (5) = 0
 
C
C     Actually get a logical unit from GETLUN and lock it down.
C
      CALL GETLUN ( LUN )
      CALL RESLUN ( LUN )
      UTLUN (5) = LUN
 
      UINDEX    = -1
 
C
C     Exercise the module. (Note: It will 'FRELUN' UTLUN(5) )
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception, one should not have been signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now check the relevant values.
C
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=',  5, 0, OK )
 
      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  5, 0, OK )
 
      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=',  10, 0, OK )
 
C
C     Check to see if LUN is still reserved with RESLUN.
C
      CALL T_TSTRLN ( LUN, RESRVD )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the value of RESRVD.
C
      CALL CHCKSL ( 'RESRVD', RESRVD, .FALSE., OK )
 
C
C     Just to be safe, free LUN.
C
      CALL FRELUN ( LUN )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No zero cost rows, expand table case.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = 10
 
      DO I = 1, 10
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX    = -1
 
C
C     Prepare the initial values for the row that will be returned.
C
      UTCST (11) = -1
      UTHAN (11) = -1
      UTLCK (11) = .TRUE.
      UTLUN (11) = -1
 
C
C     Exercise the module.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception, one should not have been signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now check the relevant values.
C
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', 11, 0, OK )
 
      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  0, 0, OK )
 
      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )
 
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '>=', 0, 0, OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=',  11, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Full table, no zero-cost rows.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = UTSIZE
 
      DO I = 1, UTSIZE
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX    = -1
 
C
C     Lock a few low cost rows to exercise that logic.
C
      UTLCK (1) = .TRUE.
      UTLCK (2) = .TRUE.
      UTLCK (3) = .TRUE.
 
C
C     Since the fourth row will be selected by ZZDDHGTU, retrieve a
C     logical unit for it.
C
      CALL GETLUN ( LUN )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Store this unit in the LUN column of the unit table.
C
      UTLUN(4) = LUN
 
C
C     Now open a scratch file to connect to the unit.
C
      OPEN ( UNIT   = LUN,
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'SCRATCH',
     .       IOSTAT = IOSTAT     )
 
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Exercise the module.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception, one should not have been signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now check the relevant values.
C
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', 4, 0, OK )
 
C
C     Determine if ZZDDHGTU properly closed the unit.
C
      INQUIRE ( UNIT   = LUN,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT         )
 
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE., OK )
 
      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',   0, 0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',   0, 0, OK )
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '=', LUN, 0, OK )
 
      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=',  UTSIZE, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Full table, monotonic decreasing cost, no zero.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = UTSIZE
 
      DO I = 1, UTSIZE
 
         UTCST (I) = (UTSIZE + 1 - I)*10
         UTHAN (I) = I
         UTLCK (I) = .FALSE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX    = -1
 
C
C     Lock a few low cost rows to exercise that logic.
C
      UTLCK (UTSIZE - 2) = .TRUE.
      UTLCK (UTSIZE - 1) = .TRUE.
      UTLCK (UTSIZE    ) = .TRUE.
 
C
C     Since the fourth row will be selected by ZZDDHGTU, retrieve a
C     logical unit for it and open a scratch file.
C
      CALL GETLUN ( LUN )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      UTLUN (UTSIZE - 3) = LUN
 
      OPEN ( UNIT   = LUN,
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'SCRATCH',
     .       IOSTAT = IOSTAT     )
 
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Exercise the module.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception, one should not have been signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now check the relevant values.
C
      CALL CHCKSI ( 'UINDEX',   UINDEX,   '=', UTSIZE-3, 0, OK )
 
C
C     Determine if ZZDDHGTU properly closed the unit.
C
      INQUIRE ( UNIT   = LUN,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT         )
 
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
      CALL CHCKSL ( 'OPENED', OPENED, .FALSE., OK )
 
      CALL CHCKSI ( 'UTCST(UINDEX)', UTCST(UINDEX), '=',  0,   0, OK )
      CALL CHCKSI ( 'UTHAN(UINDEX)', UTHAN(UINDEX), '=',  0,   0, OK )
      CALL CHCKSI ( 'UTLUN(UINDEX)', UTLUN(UINDEX), '=',  LUN, 0, OK )
 
      CALL CHCKSL ( 'UTLCK(UINDEX)', UTLCK(UINDEX), .FALSE.,    OK )
 
      CALL CHCKSI ( 'NUT',      NUT,      '=',  UTSIZE, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Full table, all rows locked exception.' )
 
C
C     Prepare the inputs and output default values.
C
      NUT = UTSIZE
 
      DO I = 1, UTSIZE
 
         UTCST (I) = 10*I
         UTHAN (I) = I
         UTLCK (I) = .TRUE.
         UTLUN (I) = I
 
      END DO
 
      UINDEX    = -1
 
C
C     Exercise the module.
C
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )
 
C
C     Check for an exception, SPICE(BUG) should have been signaled.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
