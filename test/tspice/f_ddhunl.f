C$Procedure F_DDHUNL ( ZZDDHUNL Test Family )
 
      SUBROUTINE F_DDHUNL ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHUNL
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
C     This routine exercises ZZDDHUNL's logic.  A few SPICE(BUG)
C     exceptions are not easily diagnosed, since it requires the
C     file and unit tables going out of sync in the ZZDDHMAN umbrella.
C     (If it is coded correctly, this will never happen.)
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
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     Local Variables
C
      CHARACTER*(20)        ALTARC
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD
      CHARACTER*(FILEN)     TMPFNM
 
      INTEGER               ALTUNT
      INTEGER               HANDLE
      INTEGER               HANLST ( UTSIZE )
      INTEGER               I
      INTEGER               UNIT
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHUNL' )
 
C
C     Set the filename pattern we will use.
C
      FNMPAT = 'test#.fil'
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Zero Handle NO-OP Exception' )
 
C
C     Set the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
 
C
C     Invoke the module.
C
      CALL ZZDDHUNL ( HANDLE, ARCH )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing Handle NO-OP Exception' )
 
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
C     Open FNAME as a new file, close it and delete it.  This will
C     allocate a HANDLE, and once a HANDLE is allocated it will not
C     be used again.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C     Now at this point, HANDLE has been used but is not currently
C     in use.  Invoke the module.
C
      CALL ZZDDHUNL ( HANDLE, ARCH )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Alternate Sign Handle NO-OP Exception' )
 
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
C     Open FNAME as a new file, but this time close and delete it
C     afterwards.  We will go to ZZDDHUNL with -HANDLE.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHUNL ( -HANDLE, ARCH )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Close and delete the file.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Handle-Unit Not Locked NO-OP Exception' )
 
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
C     Open FNAME as a new file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHUNL ( HANDLE, ALTARC )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Architecture Mismatch Exception' )
 
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
C     Open FNAME as a new file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     We have to lock the handle to its unit to reach the
C     the architecture exception.  (This may be a bad design.)
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )
 
C
C     Invoke the module.
C
      CALL ZZDDHUNL ( HANDLE, ALTARC )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad Architecture Code Exception' )
 
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
C     Open FNAME as a new file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     We have to lock the handle to its unit to reach the
C     the architecture exception.  (This may be a bad design.)
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )
 
C
C     Invoke the module.
C
      CALL ZZDDHUNL ( HANDLE, ALTARC )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Execution' )
 
C
C     Setup the inputs and outputs.
C
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
      UNIT   = 0
      ALTUNT = 0
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Now to perform this test, we need to verify that the unlock
C     actually occurs.  To do this we will create UTSIZE additional
C     files and load them, then proceed by making logical unit
C     requests for each of them to cycle FNAME out of the unit table.
C
      DO I = 1, UTSIZE
         CALL REPMI  ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL ( TMPFNM )
      END DO
 
C
C     Open test.fil.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Lock HANDLE to it's UNIT and retrieve the unit.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )
 
C
C     Check to see that UNIT is not zero.
C
      CALL CHCKSI ( 'UNIT', UNIT, '!=', 0, 0, OK )
 
C
C     Unlock the unit.
C
      CALL ZZDDHUNL ( HANDLE, ARCH )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now open the UTSIZE files and to cycle through the units.
C
      DO I = 1, UTSIZE
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
      END DO
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now request the UNIT for HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that ALTUNT is not UNIT.  What follows is a brief
C     discussion as to why this works.  Note it relies heavily
C     on the current implementation of ZZDDHMAN and it's
C     subroutines:
C
C        At the start of this routine, we assume no files
C        are loaded in the handle manager.  This is expected for
C        a test family.
C
C        When FNAME is open, it is assigned a unit and placed
C        at the head of the unit table.
C
C        Once the UTSIZE files are loaded, the cost bumping
C        system works such that the last file loaded into the
C        handle manager takes FNAME's place.
C
C        Then when we make the second ZZDDHUNL call the file
C        FNAME gets assigned the logical unit that was assigned
C        to the first file in the list of UTSIZE files.
C
C
C     As convoluted as that is, it works.  Do the consistency check.
C
      CALL CHCKSI ( 'ALTUNT', ALTUNT, '!=', UNIT, 0, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
      DO I = 1, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Scratch File Execution' )
 
C
C     Setup the inputs and outputs.
C
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
      UNIT   = 0
      ALTUNT = 0
 
C
C     Now to perform this test, we need to verify that the unlock
C     actually occurs.  To do this we will create UTSIZE additional
C     files and load them, then proceed by making logical unit
C     requests for each of them to cycle FNAME out of the unit table.
C
      DO I = 1, UTSIZE
         CALL REPMI  ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL ( TMPFNM )
      END DO
 
C
C     Open test.fil.
C
      CALL ZZDDHOPN ( ' ', 'SCRATCH', ARCH, HANDLE )
 
C
C     Retrieve the logical unit.  Scratch files are locked to their
C     units by the open routine.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., UNIT )
 
C
C     Check to see that UNIT is not zero.
C
      CALL CHCKSI ( 'UNIT', UNIT, '!=', 0, 0, OK )
 
C
C     Attempt to unlock the unit.
C
      CALL ZZDDHUNL ( HANDLE, ARCH )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now open the UTSIZE files and to cycle through the units.
C
      DO I = 1, UTSIZE
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
      END DO
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now request the UNIT for HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Since SCRATCH files must always be locked to their logical units,
C     the call to ZZDDHUNL is effectively a no-op.  From the nominal
C     execution case, we know that opening UTSIZE files should force
C     the original scratch file to be rotated out of the unit table
C     in ZZDDHMAN and be reassigned a different unit.  Since this can
C     not happen, check to see if ALTUNT is UNIT (verifying the lock).
C
      CALL CHCKSI ( 'ALTUNT', ALTUNT, '=', UNIT, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
      DO I = 1, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
