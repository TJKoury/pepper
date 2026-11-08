!$Procedure    ZZSCLK ( Is there and SCLK for a CKID )

      logical function zzsclk ( ckid, sclkid )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine whether or not an SCLK kernel is available for mapping
!     ET to Ticks and back again for a particular C-kernel ID.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTITILITY
!
! $ Declarations

      implicit none
      integer(ip)           ckid
      integer(ip)           sclkid

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CKID       I   CK ID-code for the object of interest.
!     SCLKID     I   Idcode to pass to the SCLOCK routines for CKID
!
!     The function returns TRUE is SCLK information is available.
!
! $ Detailed_Input
!
!     CKID       is the C-kernel ID-code for some object for which
!                and SCLK is required.
!
!     SCLKID     is the ID-code to pass to SCE2C to convert ET times
!                to ticks.
!
! $ Detailed_Output
!
!     The function returns TRUE if an SCLK specification is present
!     in the kernel pool that is suitable for mapping ticks to ET and
!     back for the C-kernel object specified by CKID.  If such
!     information is not available, or is deemed to be corrupt or
!     incomplete, the function returns FALSE.
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
!     None.
!
! $ Particulars
!
!     This is a utility for checking that sufficient SCLK information
!     is available for mapping between ET and SCLK for the object
!     specified by CKID
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
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 15-AUG-2000 (WLT)
!
!        Removed the check fo the SCLK Time system as it is not
!        formally required for an SCLK specification to be complete.
!
!-    SPICELIB Version 1.0.0, 17-FEB-2000 (WLT)
!
!-&

! $ Index_Entries
!
!     Determine whether a file specifies and SCLK
!
!-&
!     SPICELIB Functions
!
      integer(ip)           cardi
      integer(ip)           sizei

      logical               return
      logical               elemi

!
!     Local Variables
!
      integer(ip), parameter :: wdsize = 32

      integer(ip), parameter :: lbcell = -5

      integer(ip), parameter :: maxids = 10


      character(wdsize)     sclk
      character(wdsize)     sclkvr ( 7 )
      character(wdsize)     type
      character(wdsize)     agent

      integer(ip)           dtsize ( 7 )
      integer(ip)           i
      integer(ip)           n

      integer(ip)           known  ( lbcell : maxids )
      integer(ip)           passed ( lbcell : maxids )

      logical               keepid
      logical               update
      logical               watch
      logical               first

      logical               found

      save                  known
      save                  passed
      save                  first
      save                  dtsize

      data                  first / .true. /


      zzsclk = .false.
!
!     Standard SPICE error handling.
!
      if ( return() ) return


      if ( first ) then
         first = .false.

         dtsize(1) = 1
         dtsize(2) = 1
         dtsize(3) = 1
         dtsize(4) = 1
         dtsize(5) = 3
         dtsize(6) = 1
         dtsize(7) = 1

         call ssizei ( maxids, known )
         call ssizei ( maxids, passed )
      end if
!
!     We've got a text kernel (or meta kernel). See if there is an
!     SCLK kernel loaded for the CKID provided in the calling inputs.
!     If not, we'll use the default -CKID/1000 for the SCLK ID.
!
      call intstr ( -sclkid, sclk )
      agent = 'ZZSCLK' // sclk

!
!     See if this is an ID-code we've encountered before.  If it
!     is we can make use of stored knowledge about this ID-code.
!
      if ( elemi( sclkid, known ) ) then

         watch  = .false.
         keepid = .true.
         call cvpool ( agent, update )

      else if ( cardi(known) < sizei(known) ) then
!
!        The SCLKID specified is not in the list of SCLKIDs for
!        this routine and there is room left in the pool of
!        SCLKIDs to keep track of one more.  Put this ID into
!        the list of known IDS
!
         call insrti( sclkid, known )
         update = .true.
         watch  = .true.
         keepid = .true.

      else

         update = .true.
         keepid = .false.
         watch  = .false.

      end if

      if ( .not. update ) then
!
!        Nothing has changed in the kernel pool w.r.t this agent.
!        The test for an SCLK will not have changed either.
!
         zzsclk = elemi( sclkid, passed )
         return

      end if
!
!     If we are still here, we need to look in the kernel pool
!     to see if we have an SCLK for this object.
!
!     Construct all of the expected SCLK variables are
!     available for this SCLK.
!
      sclkvr(1) = 'SCLK_DATA_TYPE_'       // sclk
      sclkvr(2) = 'SCLK01_N_FIELDS_'      // sclk
      sclkvr(3) = 'SCLK01_MODULI_'        // sclk
      sclkvr(4) = 'SCLK01_OFFSETS_'       // sclk
      sclkvr(5) = 'SCLK01_COEFFICIENTS_'  // sclk
      sclkvr(6) = 'SCLK_PARTITION_START_' // sclk
      sclkvr(7) = 'SCLK_PARTITION_END_'   // sclk
!
!     If we are supposed to watch for this agent, we add him to
!     the list of kernel pool agents.
!
      if ( watch ) then
         call swpool ( agent, 7, sclkvr )
         call cvpool ( agent,    update )
      end if
!
!     Check for all of the required variables and structure in
!     the kernel pool.
!
      do i = 1, 7

         call dtpool ( sclkvr(i), found, n, type )

         if (       .not. found &
              .or.  type /= 'N' &
              .or. (n/dtsize(i))*dtsize(i) /= n   )then
!
!           We don't have adequate SCLK data for the specified
!           object.  Remove this AGENT from the list of agents
!           that have passed the test.
!
            call removi ( sclkid, passed )
            return
         end if

      end do
!
!     Once we get to this point, we know we have SCLK data.  If
!     there is room to WATCH for this agent,
!
      if ( keepid ) then
         call insrti ( sclkid, passed )
      end if

!
!     As far as we can tell, everything looks ok.
!
      zzsclk = .true.
      return
      end
