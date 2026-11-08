!$Procedure F_DDHLUH ( ZZDDHLUH Test Family )

      SUBROUTINE F_DDHLUH ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHLUH
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
!     This routine exercises ZZDDHLUH's logic.
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
!-    TSPICE Version 1.0.0, 06-NOV-2001 (FST)
!
!-&


!
!     Local Variables
!
      CHARACTER*(20)        ARCH
      CHARACTER*(20)        BINFMT
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD

      INTEGER               ALTHAN
      INTEGER               HANDLE
      INTEGER               HANTST ( FTSIZE )
      INTEGER               I
      INTEGER               UNIT

      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHLUH' )

!
!     Setup the filename pattern for loop tests.
!
      FNMPAT = 'test#.fil'

!
!     Retrive the native binary file format string to use
!     when creating test files.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', BINFMT )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Nominal Logic' )

!
!     Load a file into the handle manager.
!
      FNAME  = 'test.fil'
      METHOD = 'READ'
      ARCH   = 'DAF'
      HANDLE = 0
      ALTHAN = 0

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/CK  '  )

!
!     Open the file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the UNIT for HANDLE.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .FALSE., UNIT )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now retrieve the HANDLE for this UNIT.
!
      CALL ZZDDHLUH ( UNIT, ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the values.
!
      CALL CHCKSI ( 'HANDLE', HANDLE, '=',   ALTHAN, 0, OK )
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK            )

!
!     Close the file and clean up.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Exceptional Case Logic' )

!
!     Open a handful of files.
!
      DO I = 1, (UTSIZE + 5)
         CALL REPMI ( FNMPAT, '#', I, FNAME )
         CALL ZZDDHOPN ( FNAME, 'NEW', 'DAF', HANTST(I) )
      END DO

      FNAME  = 'test.fil'
      ARCH   = 'DAF'
      METHOD = 'READ'
      HANDLE = 0
      FOUND  = .TRUE.

      CALL KILFIL ( FNAME )

!
!     Create FNAME.
!
      CALL T_CPTFIL ( FNAME, &
                      DAF, &
                      2, &
                      BINFMT, &
                      'ABCD', &
                      'EFGH', &
                      'IJKL', &
                      .TRUE., &
                      .FALSE., &
                      'DAF/CK  '  )

!
!     Open the file.
!
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Lock HANDLE to its unit.
!
      CALL ZZDDHHLU ( HANDLE, ARCH, .TRUE., UNIT )

!
!     Now close HANDLE, since we have more than UTSIZE files loaded,
!     UNIT will end up being reserved by ZZDDHCLS.
!
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )

!
!     Now ask for the handle associated with UNIT.
!
      CALL ZZDDHLUH ( UNIT, ALTHAN, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the outputs.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'HANDLE', ALTHAN, '=',    0, 0, OK )

!
!     Clean up.
!
      CALL KILFIL ( FNAME )

      DO I = 1, UTSIZE + 5
         CALL ZZDDHCLS ( HANTST(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, FNAME )
         CALL KILFIL   ( FNAME                 )
      END DO

!
!     Lastly, check to see if we send it a unit that is not in use by
!     the handle manager it returns the appropriate values.
!
      CALL GETLUN ( UNIT )

      FOUND  = .TRUE.
      HANDLE = 1

!
!     Invoke the module.
!
      CALL ZZDDHLUH ( UNIT, HANDLE, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the values.
!
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'HANDLE', HANDLE, '=',    0, 0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
