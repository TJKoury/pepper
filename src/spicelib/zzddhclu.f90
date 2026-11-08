!$Procedure ZZDDHCLU ( Private --- DDH Count Locks )

      integer(ip) function zzddhclu ( utlck, nut )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Retrieve the number of locked units in the unit table.
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

      logical               utlck ( * )
      integer(ip)           nut


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UTLCK      I   Lock column of the unit table.
!     NUT        I   Number of entries in the unit table.
!
!     This function return the number of locked units in the unit table.
!
! $ Detailed_Input
!
!     UTLCK      is the lock column of the unit table.  TRUE entries in
!                this column indicate a handle is locked to a particular
!                unit.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     The function returns the number of locked units in the unit table.
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
!     Error free.
!
! $ Particulars
!
!     This routine simply encapsulates some logic used in several
!     places in ZZDDHMAN.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
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
!-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      integer(ip)           i

!
!     Loop through UTLCK counting the number of TRUE values.
!
      zzddhclu = 0

      do i = 1, nut

         if ( utlck(i) ) then
            zzddhclu = zzddhclu + 1
         end if

      end do

      return
      end
