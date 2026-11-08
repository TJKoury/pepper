!$Procedure F_DDHISN ( ZZDDHISN Test Family )

      SUBROUTINE F_DDHISN ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHISN
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
!     This routine exercises ZZDDHISN's logic.
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
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EQSTR

!
!     Local Variables
!
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD
      CHARACTER*(STRSIZ)    STRNAT
      CHARACTER*(STRSIZ)    STRNOW
      CHARACTER*(STRLEN)    RDSBFF

      INTEGER               HANDLE ( NUMBFF )
      INTEGER               I
      INTEGER               NATFID
      INTEGER               NUMFIL

      LOGICAL               FOUND
      LOGICAL               NATIVE

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHISN' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DDHISN Initialization' )

!
!     Get the native binary file format for this platform.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', STRNAT )
      CALL UCASE ( STRNAT, STRNAT )

!
!     Get the list of supported binary file formats for this platform.
!
      CALL ZZPLATFM ( 'READS_BFF', RDSBFF )
      CALL UCASE ( RDSBFF, RDSBFF )

!
!     Set the filename pattern we will use.
!
      FNMPAT = 'test#.fil'

!
!     Now create a file of each supported type.  We are just going to
!     trick the system into thinking it's an actual file of this type.
!
      I = 1
      CALL NEXTWD ( RDSBFF, STRNOW, RDSBFF )

      DO WHILE ( STRNOW .NE. ' ' )

!
!        Build the name of the file we intend to use.
!
         CALL REPMI ( FNMPAT, '#', I, FNAME )

!
!        Check to see if this is the native format.
!
         IF ( EQSTR( STRNOW, STRNAT ) ) THEN
            NATFID = I
         END IF

!
!        Create the file.
!
         CALL T_CPTFIL ( FNAME, &
                         DAF, &
                         2, &
                         STRNOW, &
                         'ABCD', &
                         'EFGH', &
                         'IJKL', &
                         .TRUE., &
                         .FALSE., &
                         'DAF/SPK '     )

!
!        Open the file into the handle manager.
!
         METHOD = 'READ'
         ARCH   = 'DAF'

         CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE(I) )

!
!        Strip off the next word from RDSBFF and increment I.
!
         CALL NEXTWD ( RDSBFF, STRNOW, RDSBFF )
         I = I + 1

      END DO

!
!     Store the number of files we placed into the handle list.
!
      NUMFIL = I - 1

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now that we have finished all of the initialization stuff,
!     break the tests into two cases.  NATIVE files and NON-NATIVE
!     files.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Native File Logic' )

!
!     Setup the inputs and outputs.
!
      NATIVE = .FALSE.
      FOUND  = .FALSE.

!
!     Invoke the module.
!
      CALL ZZDDHISN ( HANDLE(NATFID), NATIVE, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'NATIVE', NATIVE, .TRUE., OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Non-Native File Logic' )

      DO I = 1, NUMFIL

!
!        First check to see that this file is not of the native
!        format.
!
         IF ( I .NE. NATFID ) THEN

!
!           Setup the inputs and outputs.
!
            NATIVE = .TRUE.
            FOUND  = .FALSE.

!
!           Invoke the module.
!
            CALL ZZDDHISN ( HANDLE(I), NATIVE, FOUND )

!
!           Check for the absence of an exception.
!
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check outputs.
!
            CALL CHCKSL ( 'NATIVE', NATIVE, .FALSE., OK )
            CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,  OK )

         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Unknown Handle Exception Logic' )

!
!     Setup inputs and outputs for the zero-valued handle test.
!
      NATIVE = .FALSE.
      FOUND  = .TRUE.

!
!     Invoke the module.
!
      CALL ZZDDHISN ( 0, NATIVE, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the outputs.  Since FOUND is FALSE, NATIVE should remain
!     unchanged.
!
      CALL CHCKSL ( 'NATIVE', NATIVE, .FALSE., OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE., OK )

!
!     Setup the inputs and outputs for the opposite-sign handle test.
!
      NATIVE = .TRUE.
      FOUND  = .FALSE.

!
!     Invoke the module.
!
      CALL ZZDDHISN ( -HANDLE(1), NATIVE, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'NATIVE', NATIVE, .TRUE.,  OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE., OK )

!
! ----------------------------------------------------------------
!
!     Now clean up.
!
      DO I = 1, NUMFIL
         CALL ZZDDHCLS ( HANDLE(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, FNAME )
         CALL KILFIL   ( FNAME )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
