C$Procedure F_DDHHLU ( ZZDDHHLU Test Family )
 
      SUBROUTINE F_DDHHLU ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHHLU
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
C     This routine exercises ZZDDHHLU's logic.  A few IOSTAT based
C     exceptions are not properly exercised by this test module, as
C     well as a few SPICE(BUG) exceptions.
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
C     SPICELIB Functions
C
 
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
      INTEGER               IOSTAT
      INTEGER               UNIT
 
      LOGICAL               LOCK
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHHLU' )
 
C
C     Set the filename pattern.
C
      FNMPAT = 'test#.fil'
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Missing Handle Exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C
C     Now check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )
 
C
C     Check the value of UNIT, it should be 0.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Alternate Signed Handle Exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open the new test file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( -HANDLE, ARCH, LOCK, UNIT )
 
C
C     Now check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )
 
C
C     Check the value of UNIT, it should be 0.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ALTARC = 'DAS'
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open the new test file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ALTARC, LOCK, UNIT )
 
C
C     Now check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check the value of UNIT, it should be 0.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid Architecture Mismatch Exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
      ALTARC = 'UNK'
 
C
C     Kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Open the new test file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ALTARC, LOCK, UNIT )
 
C
C     Now check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCMISMATCH)', OK )
 
C
C     Check the value of UNIT, it should be 0.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Handle Lock Failure Exception.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .TRUE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Open (UTSIZE - RSVUNT - SCRUNT - 1) files, and lock them to
C     their units.
C
      DO I = 1, (UTSIZE - RSVUNT - SCRUNT)
 
         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
         CALL ZZDDHHLU ( HANLST(I), ARCH,   LOCK, UNIT      )
 
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(HLULOCKFAILED)', OK )
 
C
C     Check that UNIT is 0.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', 0, 0, OK )
 
C
C     Clean up.
C
      DO I = 1, (UTSIZE - RSVUNT - SCRUNT)
 
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
 
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Non-lock No LUN Rotate HLU Operation.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     At this point we know FNAME is attached to a UNIT.  Do an
C     INQUIRE to determine which one.
C
      INQUIRE ( FILE   = FNAME,
     .          NUMBER = ALTUNT,
     .          IOSTAT = IOSTAT  )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C
C     Check for the absence of the exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that UNIT is ALTUNT.  We know this must be true,
C     since no other operations have caused HLU to cycle units.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Nominal Non-lock LUN Rotate HLU Operation.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now perform an INQUIRE on the first file opened to retrieve
C     it's logical unit.  The cost function system guarentees that
C     we will retrieve (as long as this test is run before INTMAX
C     requests for logical units have been made...) the unit for
C     this file when we open the next one.
C
      CALL REPMI ( FNMPAT, '#', 1, TMPFNM )
 
      INQUIRE ( FILE   = TMPFNM,
     .          NUMBER = ALTUNT,
     .          IOSTAT = IOSTAT  )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C
C     Check for the absence of the exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that UNIT is ALTUNT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      DO I = 1, UTSIZE
 
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
 
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Rotate in no-lock HLU Operation.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now perform an INQUIRE on the first file opened to retrieve
C     it's logical unit.  The cost function system guarentees that
C     we will retrieve (as long as this test is run before INTMAX
C     requests for logical units have been made...) the unit for
C     this file when we request a unit for FNAME.
C
      CALL REPMI ( FNMPAT, '#', 1, TMPFNM )
 
      INQUIRE ( FILE   = TMPFNM,
     .          NUMBER = ALTUNT,
     .          IOSTAT = IOSTAT  )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Invoke the module.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )
 
C
C     Check for the absence of the exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that UNIT is ALTUNT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      DO I = 1, UTSIZE
 
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
 
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Lock before cycle HLU Operation.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Lock the UNIT.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Since we opened UTSIZE files, if the lock is not functioning,
C     then we would have cycled off HANDLE from the unit table.
C     Check.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., ALTUNT )
 
C
C     Check for the absence of any exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the value of ALTUNT matches UNIT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Lastly cycle the UTSIZE files again to verify that the
C     addition of the .FALSE. in the LOCK argument of the last call
C     to ZZDDHHLU does not inadvertantly unlock the file.
C
      DO I = 1, UTSIZE
         CALL ZZDDHHLU ( HANLST(I), ARCH, .FALSE., ALTUNT )
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the unit attached to HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )
 
C
C     Check for the absence of any exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the value of ALTUNT matches UNIT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      DO I = 1, UTSIZE
 
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
 
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Lock after cycle HLU Operation.' )
 
C
C     Setup the inputs and outputs.
C
      HANDLE = 0
      ARCH   = 'DAF'
      LOCK   = .FALSE.
      FNAME  = 'test.fil'
      METHOD = 'NEW'
 
C
C     Now open FNAME.
C
      CALL KILFIL ( FNAME )
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Open UTSIZE files.
C
      DO I = 1, UTSIZE
 
         CALL REPMI    ( FNMPAT,    '#',    I,    TMPFNM    )
         CALL KILFIL   ( TMPFNM                             )
         CALL ZZDDHOPN ( TMPFNM,    METHOD, ARCH, HANLST(I) )
 
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Since we opened UTSIZE files, we have cycled HANDLE out
C     of the unit table.  Restore and lock it in place.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )
 
C
C     Check for the absence of any exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now cycle the UTSIZE files to verify that the lock on
C     the UNIT for HANDLE is not broken.
C
      DO I = 1, UTSIZE
         CALL ZZDDHHLU ( HANLST(I), ARCH, .FALSE., ALTUNT )
      END DO
 
C
C     Check for any rogue exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the unit attached to HANDLE.
C
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., ALTUNT )
 
C
C     Check for the absence of any exceptions.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the value of ALTUNT matches UNIT.
C
      CALL CHCKSI ( 'UNIT', UNIT, '=', ALTUNT, 0, OK )
 
C
C     Clean up.
C
      DO I = 1, UTSIZE
 
         CALL ZZDDHCLS ( HANLST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, TMPFNM )
         CALL KILFIL   ( TMPFNM )
 
      END DO
 
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
