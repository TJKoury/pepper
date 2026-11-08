!$Procedure ZZSWFINI ( Private, switch frame initialization )

      subroutine zzswfini ( hdfram, frpool, fidlst, basbeg, &
                            free,   prvat,  prvfrm, samfrm  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize switch frame bookkeeping data structures.
!
!     Arrays of base frame attributes are not initialized by this
!     routine.
!
! $ Required_Reading
!
!     FRAMES
!     KERNEL
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     Private
!
! $ Declarations

      implicit none

      include 'zzswtchf.inc'

      integer(ip)           hdfram ( * )
      integer(ip)           frpool ( lbsngl : * )
      integer(ip)           fidlst ( * )
      integer(ip)           basbeg ( * )
      integer(ip)           free
      integer(ip)           prvat
      integer(ip)           prvfrm
      logical               samfrm

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HDFRAM     O   Array of hash collision list head nodes.
!     FRPOOL     O   Hash collision list pool.
!     FIDLST     O   Frame IDs corresponding to list nodes.
!     BASBEG     O   Start indices of base lists of switch frames.
!     FREE       O   First free index in base and time lists.
!     PRVAT      O   Index of previous input frame in frame list.
!     PRVFRM     O   Previous input frame ID.
!     SAMFRM     O   Flag indicating input frame matches previous one.
!     LBSNGL     P   Lower bound of singly linked list pool.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     HDFRAM      is the initialized head node array.
!
!     FRPOOL      is the initialized hash collision list pool.
!
!     FIDLST      is the initialized switch frame ID list.
!
!     BASBEG      is the initialized base frame start index list.
!
!     FREE        is the common index of the first free entries in the
!                 arrays of base frame attributes. The index is set to
!                 1.
!
!     PRVAT       is the index of the previous ZZSWFXFM input frame
!                 in the frame list.
!
!     PRVFRM      is the frame ID of the previous ZZSWFXFM input frame.
!
!     SAMFRM      is a logical flag indicating whether the current
!                 ZZSWFXFM input frame matches the previous one.
!
! $ Parameters
!
!     LBSNGL      is the lower bound of a singly linked list pool
!                 managed by the integer hash subsystem.
!
!     See the include file zzswtchf.inc for SPICE-private parameters
!     defining sizes of buffers used by the switch frame subsystem.
!
! $ Exceptions
!
!     1)  If integer hash initialization fails, the error is signaled
!         by a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine centralizes initialization of bookkeeping data
!     structures maintained by ZZSWFXFM.
!
! $ Examples
!
!     None. See usage in ZZSWFXFM.
!
! $ Restrictions
!
!     1)  This routine is SPICE-private. User applications must not
!         call it.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize switch frame bookkeeping data structures
!
!-&

!
!     SPICELIB functions
!
      logical               return
      logical               failed

      if ( return() ) return

!
!     Initialize the frame hash head node array, hash collision list
!     pool, and parallel frame ID list. Initialize the array of start
!     indices of associated base frame information. Indicate the base
!     frame data buffers are empty by setting FREE to 1.
!
      call zzhsiini ( maxfrm, hdfram, frpool )

      if ( failed() ) then
!
!        This code should be unreachable. It's provided for safety.
!
         return

      end if

      call cleari ( maxfrm, fidlst )
      call cleari ( maxfrm, basbeg )

      free = 1

!
!     Initialize all saved frame identity information.
!
      prvfrm = 0
      prvat  = 0
      samfrm = .false.

      return
      end
