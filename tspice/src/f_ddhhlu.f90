!$Procedure F_DDHHLU ( ZZDDHHLU Test Family )

      SUBROUTINE F_DDHHLU ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHHLU
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
!     This routine exercises ZZDDHHLU's logic.  A few IOSTAT based
!     exceptions are not properly exercised by this test module, as
!     well as a few SPICE(BUG) exceptions.
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
!     SPICELIB Functions
!

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
      INTEGER               IOSTAT
      INTEGER               UNIT

      LOGICAL               LOCK

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHHLU' )

!
!     Set the filename pattern.
!
      FNMPAT = 'test#.fil'

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Missing Handle Exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

!
!     Now check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )

!
!     Check the value of UNIT, it should be 0.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Alternate Signed Handle Exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open the new test file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( -HANDLE, ARCH, LOCK, UNIT )

!
!     Now check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )

!
!     Check the value of UNIT, it should be 0.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ALTARC = 'DAS'

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open the new test file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ALTARC, LOCK, UNIT )

!
!     Now check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check the value of UNIT, it should be 0.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid Architecture Mismatch Exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ALTARC = 'UNK'

!
!     Kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Open the new test file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ALTARC, LOCK, UNIT )

!
!     Now check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )

!
!     Check the value of UNIT, it should be 0.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Handle Lock Failure Exception.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .TRUE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Open (UTSIZE - RSVUNT - SCRUNT - 1) files, and lock them to
!     their units.
!
      DO I = 1, (UTSIZE - RSVUNT - SCRUNT)

         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
         CALL ZZDDHHLU ( HANLST(I), ARCH,   LOCK, UNIT      )

      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

!
!     Check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(HLULOCKFAILED)', OK )

!
!     Check that UNIT is 0.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )

!
!     Clean up.
!
      DO I = 1, (UTSIZE - RSVUNT - SCRUNT)

         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )

      END DO

      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Non-lock No LUN Rotate HLU Operation.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     At this point we know FNAME is attached to a UNIT.  Do an
!     INQUIRE to determine which one.
!
      INQUIRE ( FILE   = FNAME, &
                NUMBER = ALTUNT, &
                IOSTAT = IOSTAT  )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

!
!     Check for the absence of the exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that UNIT is ALTUNT.  We know this must be true,
!     since no other operations have caused HLU to cycle units.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Nominal Non-lock LUN Rotate HLU Operation.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )

      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now perform an INQUIRE on the first file opened to retrieve
!     it's logical unit.  The cost function system guarentees that
!     we will retrieve (as long as this test is run before INTMAX
!     requests for logical units have been made...) the unit for
!     this file when we open the next one.
!
      CALL REPMI ( FNMPAT, '#', 1, TMPFNM )

      INQUIRE ( FILE   = TMPFNM, &
                NUMBER = ALTUNT, &
                IOSTAT = IOSTAT  )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

!
!     Check for the absence of the exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that UNIT is ALTUNT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Clean up.
!
      DO I = 1, UTSIZE

         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )

      END DO

      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Rotate in no-lock HLU Operation.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )

      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now perform an INQUIRE on the first file opened to retrieve
!     it's logical unit.  The cost function system guarentees that
!     we will retrieve (as long as this test is run before INTMAX
!     requests for logical units have been made...) the unit for
!     this file when we request a unit for FNAME.
!
      CALL REPMI ( FNMPAT, '#', 1, TMPFNM )

      INQUIRE ( FILE   = TMPFNM, &
                NUMBER = ALTUNT, &
                IOSTAT = IOSTAT  )

!
!     Check IOSTAT.
!
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!     Invoke the module.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

!
!     Check for the absence of the exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that UNIT is ALTUNT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Clean up.
!
      DO I = 1, UTSIZE

         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )

      END DO

      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Lock before cycle HLU Operation.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Lock the UNIT.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )

      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Since we opened UTSIZE files, if the lock is not functioning,
!     then we would have cycled off HANDLE from the unit table.
!     Check.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., ALTUNT )

!
!     Check for the absence of any exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the value of ALTUNT matches UNIT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Lastly cycle the UTSIZE files again to verify that the
!     addition of the .FALSE. in the LOCK argument of the last call
!     to ZZDDHHLU does not inadvertantly unlock the file.
!
      DO I = 1, UTSIZE
         CALL ZZDDHHLU ( HANLST(I), ARCH, .FALSE., ALTUNT )
      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the unit attached to HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )

!
!     Check for the absence of any exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the value of ALTUNT matches UNIT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Clean up.
!
      DO I = 1, UTSIZE

         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )

      END DO

      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Lock after cycle HLU Operation.' )

!
!     Setup the inputs and outputs.
!
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'

!
!     Now open FNAME.
!
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Open UTSIZE files.
!
      DO I = 1, UTSIZE

         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )

      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Since we opened UTSIZE files, we have cycled HANDLE out
!     of the unit table.  Restore and lock it in place.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Check for the absence of any exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now cycle the UTSIZE files to verify that the lock on
!     the UNIT for HANDLE is not broken.
!
      DO I = 1, UTSIZE
         CALL ZZDDHHLU ( HANLST(I), ARCH, .FALSE., ALTUNT )
      END DO

!
!     Check for any rogue exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the unit attached to HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )

!
!     Check for the absence of any exceptions.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the value of ALTUNT matches UNIT.
!
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )

!
!     Clean up.
!
      DO I = 1, UTSIZE

         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )

      END DO

      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
