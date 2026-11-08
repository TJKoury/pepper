!$Procedure ZZPLTCHK ( Private --- Platform Check )

      SUBROUTINE ZZPLTCHK ( OK )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Validate the runtime environment against values assumed by the
!     current toolkit source package.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   Logical indicating the runtime environment is ok.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical when set to .TRUE. indicates that the
!                runtime environment passes any checks implemented
!                by this routine against the configured code.
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
!     1) This routine signals SPICE(BUG) if it determines the runtime
!        environment is incompatible with the configured binary file
!        format.
!
! $ Particulars
!
!     This routine encapsulates a series of checks to diagnose the
!     runtime environment against assumptions configured in the
!     source code.  Configuration errors are reported via the error
!     SPICE(BUG).
!
! $ Examples
!
!     See ZZDDHOPN for sample usage.
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
!-    SPICELIB Version 1.0.0, 07-AUG-2002 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    STRBFF
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    RTEBFF

      INTEGER               BFF

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZPLTCHK' )
      END IF

!
!     Verify that the runtime environment's binary file format agrees
!     with the value listed in ZZPLATFM.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', STRBFF )

!
!     Determine what the runtime environment binary file format appears
!     to be.
!
      CALL ZZGETBFF ( BFF )
      CALL ZZDDHGSD ( 'BFF', BFF, RTEBFF )

!
!     Check results, signal SPICE(BUG) if a discrepancy appears.
!
      IF ( STRBFF .NE. RTEBFF ) THEN

         CALL SETMSG   ( 'This version of SPICELIB was originally ' &
         //              'packaged by NAIF for # hardware using ' &
         //              '# with the # compiler.  This ' &
         //              'environment has a binary ' &
         //              'file format of #; however the software is ' &
         //              'running on an environment that has a binary ' &
         //              'file format of #.  This is a severe problem ' &
         //              'and may be because the software package was ' &
         //              'intended for use on a different computer ' &
         //              'system.  It also may be the result of an ' &
         //              'improper port; please contact NAIF.'         )

         CALL ZZPLATFM ( 'SYSTEM',   VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ZZPLATFM ( 'O/S',      VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ZZPLATFM ( 'COMPILER', VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ERRCH    ( '#',        STRBFF                            )

         IF ( RTEBFF .EQ. ' ' ) THEN
            CALL ERRCH ( '#', 'UNKNOWN' )
         ELSE
            CALL ERRCH ( '#',  RTEBFF   )
         END IF

         CALL SIGERR   ( 'SPICE(BUG)'                                  )
         CALL CHKOUT   ( 'ZZPLTCHK'                                    )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZPLTCHK' )
      RETURN

      END
