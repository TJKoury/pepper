C$Procedure F_DDHCLS ( ZZDDHCLS Test Family )
 
      SUBROUTINE F_DDHCLS ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHCLS
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
C     This routine exercises ZZDDHCLS's logic.
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
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 06-SEP-2001 (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EXISTS
 
C
C     Local Variables
C
      CHARACTER*(20)        ALTARC
      CHARACTER*(FILEN)     ALTFNM
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD
      CHARACTER*(FILEN)     TMPFNM
 
 
      INTEGER               ALTUNT
      INTEGER               HANDLE
      INTEGER               HANLST ( UTSIZE )
      INTEGER               I
      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHCLS' )
 
C
C     Set the filename pattern.
C
      FNMPAT = 'test#.fil'
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Handle not found NO-OP exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Alternate-signed handle NO-OP exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open a new file to close.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if the file is closed.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Architecture mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
 
      ALTARC = 'DAS'
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open a new file to close.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ALTARC, .FALSE. )
 
C
C     Check for the presence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     See if the file is closed, it should not be.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
      CALL CHCKSI ( 'INTARC', INTARC, '=', DAF, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', NEW, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if the file is closed.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
 
      ALTARC = 'UNK'
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open a new file to close.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ALTARC, .FALSE. )
 
C
C     Check for the presence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     See if the file is closed, it should not be.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
      CALL CHCKSI ( 'INTARC', INTARC, '=', DAF, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', NEW, 0, OK )
 
C
C     Close the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if the file is closed.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KILL set to TRUE, no UNIT attached exception.' )
 
C
C     Setup the inputs and outputs.
C
      METHOD = 'NEW'
      ARCH   = 'DAF'
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Open an additional file to bump out the first loaded
C     one.
C
      FNAME = 'test.fil'
      CALL KILFIL   ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Close the first file with KILL set.
C
      CALL ZZDDHCLS ( HANLST(1), ARCH, .TRUE. )
 
C
C     Check for the expected exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTCONNECTED)', OK )
 
C
C     Check to see that the file is actually closed.
C
      CALL ZZDDHNFO ( HANLST(1), ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL REPMI  ( FNMPAT, '#', 1, TMPFNM )
      CALL KILFIL ( TMPFNM )
 
      DO I = 2, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO
 
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Execution.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open a new file to close.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if the file is closed.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Execution - KILL set to TRUE.' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open a new file to close.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .TRUE. )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if the file is closed.
C
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     See if the file was properly deleted.
C
      CALL CHCKSL ( 'EXISTS', EXISTS(FNAME), .FALSE., OK )
 
C
C     Clean up.
C
      CALL KILFIL ( FNAME )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unit Table Clean Up Execution.' )
 
C
C     This test case is a bit unusual... so I am going to list
C     the steps we will take out here before getting started.
C
C     (1) Open UTSIZE files.
C     (2) Use ZZDDHHLU to fetch the unit assigned to the first
C         file loaded.
C     (3) Open one additional file to bump out the first loaded
C         file from the unit table.
C     (4) Close this additional file.  Now at this point we have
C         UTSIZE files loaded in the file table, and UTSIZE-1
C         "active" rows in the unit table and one inactive one.
C     (5) Close the first file loaded.  This will force ZZDDHCLS
C         to clean up the unit table and remove the row created
C         when the first file was opened.
C     (6) Using RESLUN reserve the unit originally assigned to
C         the first file.
C     (7) Open a new file.
C     (8) Fetch its unit using ZZDDHHLU.
C     (9) This unit must disagree with the unit we reserved,
C         and this disagreement indicates the zero handle row
C         was properly compressed out of the unit table.
C     (-) Clean up.
C
C
C     Setup the inputs and outputs.
C
      METHOD = 'NEW'
      ARCH   = 'DAF'
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Retrieve the unit assigned to the first file.
C
      CALL ZZDDHHLU ( HANLST(1), ARCH, .FALSE., UNIT )
 
C
C     Open an additional file to bump out the first loaded
C     one.
C
      FNAME = 'test.fil'
      CALL KILFIL   ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Close this file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
 
C
C     Close the first loaded file.
C
      CALL ZZDDHCLS ( HANLST(1), ARCH, .FALSE. )
 
C
C     Reserve the original unit.
C
      CALL RESLUN ( UNIT )
 
C
C     Open another new file.
C
      ALTFNM = 'alt.fil'
      CALL KILFIL   ( ALTFNM )
      CALL ZZDDHOPN ( ALTFNM, METHOD, ARCH, HANDLE )
 
C
C     Retrieve the unit to which it was assigned.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., ALTUNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that UNIT and ALTUNT do not agree.
C
      CALL CHCKSI ( 'UNIT', UNIT, '!=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      DO I = 2, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( ALTFNM )
 
      CALL KILFIL ( FNAME  )
 
      CALL REPMI ( FNMPAT, '#', 1, TMPFNM )
      CALL KILFIL ( TMPFNM )
 
      CALL FRELUN ( UNIT )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
