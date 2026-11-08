!$Procedure ZZGAPOOL ( Private: get agent set for watched variable )

      subroutine zzgapool ( varnam, wtvars, wtptrs, &
                            wtpool, wtagnt, agtset )
      use pepper_kinds, only: ip


! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return a SPICE set containing the names of agents watching
!     a specified kernel variable.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!     PRIVATE
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      character*(*)         varnam
      character*(*)         wtvars ( lbcell : * )
      integer(ip)           wtptrs ( * )
      integer(ip)           wtpool ( 2, lbpool : * )
      character*(*)         wtagnt ( * )
      character*(*)         agtset ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VARNAM     I   Kernel variable name.
!     WTVARS     I   Watched kernel variable set.
!     WTPTRS     I   Pointers from variables into the watch pool.
!     WTPOOL     I   Watch pool used for managing agent names.
!     WTAGNT     I   Array of agent names.
!     AGTSET     O   Set of agents for VARNAM.
!
! $ Detailed_Input
!
!     VARNAM      is the name of a kernel variable.
!
!     WTVARS      is a SPICE set containing the contents of the kernel
!                 pool watcher system's set WTVARS.
!
!     WTPTRS      is an array containing the contents of the kernel
!                 pool watcher system's array WTPTRS.
!
!     WTPOOL      is a SPICE doubly linked list pool containing the
!                 contents of the kernel pool watcher system's pool
!                 WTPOOL.
!
!     WTAGNT      is an array containing the contents of the kernel
!                 pool watcher system's array WTAGNT.
!
! $ Detailed_Output
!
!     AGTSET      is a SPICE set containing the names of the agents
!                 associated with the kernel variable designated by
!                 VARNAM.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the output set AGTSET is too small to hold the set of
!        agents watching VARNAM, the error will be diagnosed by routines
!        in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is not part of the SPICELIB API. This routine
!     may be removed in a later version of the SPICE Toolkit, or
!     its interface may change.
!
!     SPICE-based application code should not call this routine.
!
! $ Examples
!
!     See POOL entry point SWPOOL.
!
! $ Restrictions
!
!     1) This is a private routine. See $Particulars above.
!
!     2) Contents of the input arrays are assumed to be valid.
!        The output returned by this routine is meaningless
!        otherwise.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 17-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     get agent set for watched kernel variable
!
!-&

! $ Revisions
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      integer(ip)           lnknxt
      integer(ip)           sizec

      logical               return

!
!     Local variables
!
      integer(ip)           loc
      integer(ip)           nfetch
      integer(ip)           node


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGAPOOL' )

!
!     The output agent set is empty until we find any
!     agents.
!
      call scardc ( 0, agtset )

!
!     Find the location of VARNAM in the set of watched
!     variables.
!
      loc = bsrchc ( varnam, cardc(wtvars), wtvars(1) )

      if ( loc .eq. 0 ) then
!
!        This variable is not watched. The agent set is
!        empty.
!
         call chkout ( 'ZZGAPOOL' )
         return

      end if

!
!     Set NODE to the head node of the agent list for VARNAM.
!     Traverse the agent list for VARNAM. Collect the agents
!     as an unordered list, then turn the list into a set.
!
      node   = wtptrs(loc)
      nfetch = 0

      do while ( node .gt. 0 )

         nfetch         = nfetch + 1
         agtset(nfetch) = wtagnt(node)

         node           = lnknxt( node, wtpool )

      end do

      call validc ( sizec(agtset), nfetch, agtset )


      call chkout ( 'ZZGAPOOL' )
      return
      end
