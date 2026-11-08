!$Procedure ZZPLTCHK ( Private --- Platform Check )

      subroutine zzpltchk ( ok )
      use pepper_kinds, only: ip

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

      implicit none

      include              'zzddhman.inc'

      logical               ok

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
      logical               return

!
!     Local Parameters
!
      integer(ip)           wdsize
      parameter           ( wdsize = 32 )

!
!     Local Variables
!
      character(wdsize)     strbff
      character(wdsize)     value
      character(wdsize)     rtebff

      integer(ip)           bff

!
!     Standard SPICE error handling
!
      if ( return() ) return

!
!     Verify that the runtime environment's binary file format agrees
!     with the value listed in ZZPLATFM.
!
      call zzplatfm ( 'FILE_FORMAT', strbff )

!
!     Determine what the runtime environment binary file format appears
!     to be.
!
      call zzgetbff ( bff )
      call zzddhgsd ( 'BFF', bff, rtebff )

!
!     Check results, signal SPICE(BUG) if a discrepancy appears.
!
      if ( strbff /= rtebff ) then

         call setmsg   ( 'This version of SPICELIB was originally ' &
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

         call zzplatfm ( 'SYSTEM',   value                             )
         call errch    ( '#',        value                             )

         call zzplatfm ( 'O/S',      value                             )
         call errch    ( '#',        value                             )

         call zzplatfm ( 'COMPILER', value                             )
         call errch    ( '#',        value                             )

         call errch    ( '#',        strbff                            )

         if ( rtebff == ' ' ) then
            call errch ( '#', 'UNKNOWN' )
         else
            call errch ( '#',  rtebff   )
         end if

         call sigerr   ( 'SPICE(BUG)'                                  )
         return

      end if

      return

      end
