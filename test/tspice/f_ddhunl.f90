!$Procedure F_DDHUNL ( ZZDDHUNL Test Family )

      SUBROUTINE F_DDHUNL ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHUNL
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
!     This routine exercises ZZDDHUNL's logic.  A few SPICE(BUG)
!     exceptions are not easily diagnosed, since it requires the
!     file and unit tables going out of sync in the ZZDDHMAN umbrella.
!     (If it is coded correctly, this will never happen.)
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
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     Local Variables
!
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

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHUNL' )

!
!     Set the filename pattern we will use.
!
      FNMPAT = 'test#.fil'

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero Handle NO-OP Exception' )

!
!     Set the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'

!
!     Invoke the module.
!
      CALL ZZDDHUNL ( HANDLE, ARCH )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing Handle NO-OP Exception' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open FNAME as a new file, close it and delete it.  This will
!     allocate a HANDLE, and once a HANDLE is allocated it will not
!     be used again.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
!     Now at this point, HANDLE has been used but is not currently
!     in use.  Invoke the module.
!
      CALL ZZDDHUNL ( HANDLE, ARCH )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Alternate Sign Handle NO-OP Exception' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open FNAME as a new file, but this time close and delete it
!     afterwards.  We will go to ZZDDHUNL with -HANDLE.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHUNL ( -HANDLE, ARCH )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close and delete the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Handle-Unit Not Locked NO-OP Exception' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open FNAME as a new file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHUNL ( HANDLE, ALTARC )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Architecture Mismatch Exception' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0

      ALTARC = 'DAS'

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open FNAME as a new file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     We have to lock the handle to its unit to reach the
!     the architecture exception.  (This may be a bad design.)
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Invoke the module.
!
      CALL ZZDDHUNL ( HANDLE, ALTARC )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad Architecture Code Exception' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0

      ALTARC = 'UNK'

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open FNAME as a new file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     We have to lock the handle to its unit to reach the
!     the architecture exception.  (This may be a bad design.)
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Invoke the module.
!
      CALL ZZDDHUNL ( HANDLE, ALTARC )

!
!     Check for the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Execution' )

!
!     Setup the inputs and outputs.
!
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
      UNIT   = 0
      ALTUNT = 0

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Now to perform this test, we need to verify that the unlock
!     actually occurs.  To do this we will create UTSIZE additional
!     files and load them, then proceed by making logical unit
!     requests for each of them to cycle FNAME out of the unit table.
!
      DO I = 1, UTSIZE
         CALL REPMI  ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL ( TMPFNM )
      END DO

!
!     Open test.fil.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Lock HANDLE to it's UNIT and retrieve the unit.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Check to see that UNIT is not zero.
!
      CALL CHCKSI ( 'UNIT', UNIT, '!=', 0, 0, OK )

!
!     Unlock the unit.
!
      CALL ZZDDHUNL ( HANDLE, ARCH )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now open the UTSIZE files and to cycle through the units.
!
      DO I = 1, UTSIZE
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
      END DO

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now request the UNIT for HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that ALTUNT is not UNIT.  What follows is a brief
!     discussion as to why this works.  Note it relies heavily
!     on the current implementation of ZZDDHMAN and it's
!     subroutines:
!
!        At the start of this routine, we assume no files
!        are loaded in the handle manager.  This is expected for
!        a test family.
!
!        When FNAME is open, it is assigned a unit and placed
!        at the head of the unit table.
!
!        Once the UTSIZE files are loaded, the cost bumping
!        system works such that the last file loaded into the
!        handle manager takes FNAME's place.
!
!        Then when we make the second ZZDDHUNL call the file
!        FNAME gets assigned the logical unit that was assigned
!        to the first file in the list of UTSIZE files.
!
!
!     As convoluted as that is, it works.  Do the consistency check.
!
      CALL CHCKSI ( 'ALTUNT', ALTUNT, '!=', UNIT, 0, OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

      DO I = 1, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO

      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Scratch File Execution' )

!
!     Setup the inputs and outputs.
!
      METHOD = 'NEW'
      ARCH   = 'DAF'
      HANDLE = 0
      UNIT   = 0
      ALTUNT = 0

!
!     Now to perform this test, we need to verify that the unlock
!     actually occurs.  To do this we will create UTSIZE additional
!     files and load them, then proceed by making logical unit
!     requests for each of them to cycle FNAME out of the unit table.
!
      DO I = 1, UTSIZE
         CALL REPMI  ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL ( TMPFNM )
      END DO

!
!     Open test.fil.
!
      CALL ZZDDHOPN ( ' ', 'SCRATCH', ARCH, HANDLE )

!
!     Retrieve the logical unit.  Scratch files are locked to their
!     units by the open routine.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., UNIT )

!
!     Check to see that UNIT is not zero.
!
      CALL CHCKSI ( 'UNIT', UNIT, '!=', 0, 0, OK )

!
!     Attempt to unlock the unit.
!
      CALL ZZDDHUNL ( HANDLE, ARCH )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now open the UTSIZE files and to cycle through the units.
!
      DO I = 1, UTSIZE
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )
      END DO

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now request the UNIT for HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Since SCRATCH files must always be locked to their logical units,
!     the call to ZZDDHUNL is effectively a no-op.  From the nominal
!     execution case, we know that opening UTSIZE files should force
!     the original scratch file to be rotated out of the unit table
!     in ZZDDHMAN and be reassigned a different unit.  Since this can
!     not happen, check to see if ALTUNT is UNIT (verifying the lock).
!
      CALL CHCKSI ( 'ALTUNT', ALTUNT, '=', UNIT, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

      DO I = 1, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
