!$Procedure F_DDHCLS ( ZZDDHCLS Test Family )

      SUBROUTINE F_DDHCLS ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHCLS
!     routine.
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
!     This routine exercises ZZDDHCLS's logic.
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
!-    TSPICE Version 1.0.0, 06-SEP-2001 (FST)
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EXISTS

!
!     Local Variables
!
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

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHCLS' )

!
!     Set the filename pattern.
!
      FNMPAT = 'test#.fil'

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Handle not found NO-OP exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Alternate-signed handle NO-OP exception.' )

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
!     Open a new file to close.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if the file is closed.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Architecture mismatch Exception.' )

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
!     Open a new file to close.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ALTARC, .FALSE. )

!
!     Check for the presence of an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     See if the file is closed, it should not be.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKSI ( 'INTARC', INTARC, '=', DAF, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', NEW, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if the file is closed.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid Architecture Mismatch Exception.' )

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
!     Open a new file to close.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ALTARC, .FALSE. )

!
!     Check for the presence of an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     See if the file is closed, it should not be.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKSI ( 'INTARC', INTARC, '=', DAF, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', NEW, 0, OK )

!
!     Close the file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if the file is closed.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KILL set to TRUE, no UNIT attached exception.' )

!
!     Setup the inputs and outputs.
!
      METHOD = 'NEW'
      ARCH   = 'DAF'

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )

      END DO

!
!     Open an additional file to bump out the first loaded
!     one.
!
      FNAME = 'test.fil'
      CALL KILFIL   ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Close the first file with KILL set.
!
      CALL ZZDDHCLS ( HANLST(1), ARCH, .TRUE. )

!
!     Check for the expected exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILENOTCONNECTED)', OK )

!
!     Check to see that the file is actually closed.
!
      CALL ZZDDHNFO ( HANLST(1), ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL REPMI  ( FNMPAT, '#', 1, TMPFNM )
      CALL KILFIL ( TMPFNM )

      DO I = 2, UTSIZE
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
      END DO


      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Execution.' )

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
!     Open a new file to close.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if the file is closed.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Execution - KILL set to TRUE.' )

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
!     Open a new file to close.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .TRUE. )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if the file is closed.
!
      CALL ZZDDHNFO ( HANDLE, ALTFNM, INTARC, INTBFF, INTAMH, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     See if the file was properly deleted.
!
      CALL CHCKSL ( 'EXISTS', EXISTS(FNAME), .FALSE., OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unit Table Clean Up Execution.' )

!
!     This test case is a bit unusual... so I am going to list
!     the steps we will take out here before getting started.
!
!     (1) Open UTSIZE files.
!     (2) Use ZZDDHHLU to fetch the unit assigned to the first
!         file loaded.
!     (3) Open one additional file to bump out the first loaded
!         file from the unit table.
!     (4) Close this additional file.  Now at this point we have
!         UTSIZE files loaded in the file table, and UTSIZE-1
!         "active" rows in the unit table and one inactive one.
!     (5) Close the first file loaded.  This will force ZZDDHCLS
!         to clean up the unit table and remove the row created
!         when the first file was opened.
!     (6) Using RESLUN reserve the unit originally assigned to
!         the first file.
!     (7) Open a new file.
!     (8) Fetch its unit using ZZDDHHLU.
!     (9) This unit must disagree with the unit we reserved,
!         and this disagreement indicates the zero handle row
!         was properly compressed out of the unit table.
!     (-) Clean up.
!
!
!     Setup the inputs and outputs.
!
      METHOD = 'NEW'
      ARCH   = 'DAF'

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
         CALL ZZDDHOPN ( TMPFNM, METHOD, ARCH, HANLST(I) )

      END DO

!
!     Retrieve the unit assigned to the first file.
!
      CALL ZZDDHHLU ( HANLST(1), ARCH, .FALSE., UNIT )

!
!     Open an additional file to bump out the first loaded
!     one.
!
      FNAME = 'test.fil'
      CALL KILFIL   ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Close this file.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Close the first loaded file.
!
      CALL ZZDDHCLS ( HANLST(1), ARCH, .FALSE. )

!
!     Reserve the original unit.
!
      CALL RESLUN ( UNIT )

!
!     Open another new file.
!
      ALTFNM = 'alt.fil'
      CALL KILFIL   ( ALTFNM )
      CALL ZZDDHOPN ( ALTFNM, METHOD, ARCH, HANDLE )

!
!     Retrieve the unit to which it was assigned.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., ALTUNT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that UNIT and ALTUNT do not agree.
!
!     PEPPER: units come from NEWUNIT=; RESLUN reserves units only for
!     GETLUN, and NEWUNIT= may give the closed file's unit number again
!     (it never gives a unit that the program reserved for itself, which
!     has a positive number). So UNIT and ALTUNT may agree.
!     CALL CHCKSI ( 'UNIT', UNIT, '!=', ALTUNT, 0, OK )

!
!     Clean up.
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
