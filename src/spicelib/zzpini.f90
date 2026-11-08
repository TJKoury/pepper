!$Procedure      ZZPINI ( Private --- kernel pool initialization )

      subroutine zzpini ( first, &
                          maxvar, maxval, maxlin, &
                          begdat, begtxt, &
                          nmpool, dppool, chpool, &
                          namlst, datlst, &
                          maxagt, mxnote, wtvars, &
                          wtptrs, wtpool, wtagnt, &
                          agents, active, notify, subctr  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine initializes the data structures needed for
!     maintaining the kernel pool and initializes the hash function
!     used for the name list in the kernel pool.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       PRIVATE UTILITY
!
! $ Declarations

      implicit none

      include              'zzctr.inc'

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               lbpool
      parameter           ( lbpool = -5 )

      logical               first
      integer               maxvar
      integer               maxval
      integer               maxlin
      character*(*)         begdat
      character*(*)         begtxt
      integer               nmpool ( 2,  lbpool : maxvar )
      integer               dppool ( 2,  lbpool : maxval )
      integer               chpool ( 2,  lbpool : maxlin )
      integer               namlst (              maxvar )
      integer               datlst (              maxvar )
      integer               maxagt
      integer               mxnote
      character*(*)         wtvars ( lbcell : maxvar )
      integer               wtptrs ( maxvar )
      integer               wtpool ( 2, lbpool:mxnote )
      character*(*)         wtagnt ( mxnote )
      character*(*)         agents ( lbcell : * )
      character*(*)         active ( lbcell : * )
      character*(*)         notify ( lbcell : * )
      integer               subctr ( ctrsiz )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      FIRST     I/O  Used to determine if this is the first pass
!      MAXVAR     I   Maximum number of variables in the pool
!      MAXVAL     I   Maximum number of d.p. values in the pool
!      MAXLIN     I   Maximum number of string values in the pool
!      BEGDAT     O   Marker used to begin data section of a kernel
!      BEGTXT     O   Marker used to begin text section of a kernel
!      NMPOOL     O   Linked list for resolving hash collisions of names
!      DPPOOL     O   Linked list for maintaining d.p. values.
!      CHPOOL     O   Linked list for maintaining string values
!      NAMLST     O   Heads of collision resolution lists
!      DATLST     O   Heads of data values lists
!      MAXAGT     I   Maximum number of agents that can be supported
!      MXNOTE     I   Maximum number of agents that can be notified
!      WTPTR     O   Name array of watcher symbol table
!      WATPTR     O   Pointer array of watcher symbol table
!      WATVAL     O   Values array of watcher symbol table.
!      AGENTS     O   Set of agents
!      ACTIVE     O   Watchers that are active.
!      NOTIFY     O   Agents to notify
!      SUBCTR     O   POOL state counter.
!
! $ Detailed_Input
!
!     FIRST       is a logical indicating whether or not this is
!                 the first call to this routine.  If FIRST is .TRUE.
!                 the various items are initialized and FIRST is
!                 set to .FALSE.  If FIRST is .FALSE. no action is
!                 taken by this routine.
!
!     MAXVAR      is the maximum number of variables that the
!                 kernel pool may contain at any one time.
!
!
!     MAXVAL      is the maximum number of distinct values that
!                 may belong to the variables in the kernel pool.
!
!     MAXLIN      is the maximum number of character strings that
!                 can be stored as data for kernel pool variables.
!
!     MXNOTE      is the maximum number of distinct variable-agents
!                 pairs that can be maintained by the kernel pool.
!                 (A variable is "paired" with an agent, if that agent
!                 is to be notified whenever the variable is updated.)
!
!     MAXAGT      is the maximum number of agents that can be kept
!                 on the distribution list for notification of updates
!                 to kernel variables.
!
! $ Detailed_Output
!
!      FIRST      is set to .FALSE. on output.
!
!      BEGDAT     Marker used to begin data section of a kernel
!
!      BEGTXT     Marker used to begin text section of a kernel
!
!      NMPOOL     Linked list pool for resolving hash collisions
!                 of names of kernel pool variables.  Each list
!                 other than the free list, is a sequence of pointers
!                 to names that have the same hash value.  On output
!                 from this routine all nodes of the pool are in the
!                 free list.
!
!      DPPOOL     Linked list pool for maintaining d.p. values.
!                 On output all nodes in the pool are in the free list
!                 of DPPOOL
!
!      CHPOOL     Linked list pool for maintaining string values.
!                 On output all nodes in the pool are in the free list
!                 of CHPOOL
!
!      NAMLST     is an array that contains the heads of lists from
!                 NMPOOL.  NAMLST( ZZHASH( NAME ) ) points to the head
!                 of the first name in the collision resolution list
!                 for NAME.  If there is no head for the collision
!                 resolution list for NAME (i.e. no name with the
!                 same hash value as name has been stored)
!                 NAMLST( ZZHASH(NAME) ) will be zero.  On output from
!                 this routine all values in NAMLST are set to zero.
!
!      DATLST     is an array that contains the "heads" of lists of
!                 pointers to the values associated with a variable.
!                 Suppose that NAME has been located in the list of
!                 variable names at location LOC.  Then DATLST(LOC)
!                 is the head node of the list of pointers to the
!                 values of NAME.  If DATLST(LOC) is positive then
!                 the values are d.p.'s If the value of DATLST(LOC)
!                 is negative, the values are strings.  The absolute
!                 value of DATLST(LOC) is the head node to the list
!                 of values associated with NAME.  If DATLST(LOC) is
!                 zero then no values have been assigned to the variable
!                 NAME.  On output all entries of DATLST are set to
!                 zero.
!
!      WTPTR     is a symbol table of variables to watch for.  WTPTR
!      WATPTR     contains the names of variables to watch. The
!      WATVAL     values associated with a name are the names of agents
!                 that have requested that the variable be watched.
!
!      AGENTS     Agents contains the list of agents that need to be
!                 notified about updates to their variables.
!
!      ACTIVE     A temporary set.
!      NOTIFY     A temporary set.
!
!      SUBCTR     Initialized POOL state counter.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This is a utility routine that centralizes the initialization
!     code that is common to all entry points of POOL.
!
! $ Examples
!
!     See POOL.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 30-JUL-2013 (BVS)
!
!        Added POOL state counter to the argument list, and included
!        'zzctr.inc' to provide the counter array dimension.
!
!-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
!
!        Argument list was changed to accommodate re-implementation
!        of watcher system. Initialization tasks performed by this
!        routine were updated accordingly.
!
!-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
!
!        An integer variable was renamed to better indicate
!        its role in the routine and to make maintenance a bit
!        easier
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
!
!        An integer variable was renamed to better indicate
!        its role in the routine and to make maintenance a bit
!        easier.  The integer variable was 'DONE' which looks
!        a lot like a logical.  It's been changed to 'DUMMY'.
!
!-&

!
!     SPICELIB Functions.
!
      integer               touchi
      integer               zzshsh

      logical               failed

!
!     Local parameters
!
      integer               shrtln
      parameter           ( shrtln =  9 )

      integer               bslash
      parameter           ( bslash = 92 )

      character*(shrtln)    dat
      parameter           ( dat = 'begindata' )

      character*(shrtln)    txt
      parameter           ( txt = 'begintext' )

!
!     Local variables
!

      integer               dummy
      integer               i


      if ( first ) then

         call chkin ( 'ZZPINI' )

         do i = 1, maxvar
            namlst(i) = 0
            datlst(i) = 0
         end do

!
!        Set up hash function. Use TOUCHI to suppress
!        compiler warnings.
!
         dummy  = zzshsh ( maxvar )
         dummy  = touchi ( dummy  )

         begdat = char(bslash) // dat
         begtxt = char(bslash) // txt

         call lnkini ( maxvar, nmpool )
         call lnkini ( maxval, dppool )
         call lnkini ( maxlin, chpool )

         call ssizec ( maxvar, wtvars )
         call cleari ( maxvar, wtptrs )
         call lnkini ( mxnote, wtpool )
         call clearc ( mxnote, wtagnt )

         call ssizec ( mxnote, agents )
         call ssizec ( mxnote, active )
         call ssizec ( mxnote, notify )

         call zzctrsin ( subctr )

         if ( .not. failed() ) then
            first = .false.
         end if

         call chkout ( 'ZZPINI' )
         return

      end if

      return
      end
