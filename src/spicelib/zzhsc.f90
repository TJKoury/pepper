!$Procedure ZZHSC ( Private---Add-only Character Hash )

      subroutine zzhsc ( hashsz, hedlst, collst, items, &
                         item, itemat, new, avail )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Manipulate add-only character hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           hashsz
      integer(ip)           hedlst  (          * )
      integer(ip)           collst  ( lbpool : * )
      character(*)          items   (          * )
      character(*)          item
      integer(ip)           itemat
      logical               new
      integer(ip)           avail

! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     HASHSZ     I   ZZHSCINI
!     HEDLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF
!     COLLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCAVL, ZZHSCINF
!     ITEMS     I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF
!     ITEM       I   ZZHSCADD, ZZHSCCHK, ZZHSCINF
!     ITEMAT     O   ZZHSCADD, ZZHSCCHK
!     NEW        O   ZZHSCADD
!     AVAIL      O   ZZHSCAVL, ZZHSCINF
!
!     LBPOOL     P   (All)
!
! $ Detailed_Input
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     1) If ZZHSC is called directly, the error SPICE(BOGUSENTRY) is
!        signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     ZZHSC should never be called directly, but should instead be
!     accessed only through its entry points.
!
!     The purpose of this routine is to manipulate add-only character
!     hashes used for buffering purposes by various SPICE subsystems.
!     This umbrella has the following entry points:
!
!           ZZHSCINI       Initialize a hash.
!
!           ZZHSCADD       Add an item to a hash.
!
!           ZZHSCCHK       Check if a item is in a hash.
!
!           ZZHSCAVL       Get available room in hash.
!
!           ZZHSCINF       Get h/k information about hash.
!
!     An add-only hash consists of the head node pointer list (HEDLST),
!     hash node collision list (COLLST), and the item list (ITEMS).
!
!     The function ZZHASH2 computes the index of an element in the head
!     node pointer list (HEDLST).
!
!     The HEDLST element at this index contains either 0, indicating
!     that there is not yet data for this hash value in the hash, or
!     the index of the head node and its item in the hash collision
!     (COLLST) and item (ITEMS) lists.
!
!     The COLLST element at the head node index contain either 0,
!     indicating that there is only one item for this hash value in the
!     hash, or the index of the next node and its item in the hash
!     collision (COLLST) and item (ITEMS) lists.
!
!     The COLLST element at the next node index contain either 0,
!     indicating that this is the last item for this hash value in the
!     hash, or the index of the next node and its item in the hash
!     collision (COLLST) and item (ITEMS) lists.
!
!     And so on.
!
!     Pictorially the hash looks like this:
!
!                     List of head   List of hash      List of
!                        nodes        collisions        items
!
!                       HEDLST          COLLST          ITEMS
!
!                                     +---------+
!                                     |         |
!                                     +---------+
!                                         ...
!                                     +---------+
!                                     | 1st Free|
!                                     +---------+
!                                     |  Size   |
!                     +---------+     +---------+     +---------+
!                     |         |     |         |     |         |
!                     +---------+ !=0 +---------+     +---------+
!                  .->|Head Node| -.  |         |     |         |
!                  |  +---------+  |  +---------+     +---------+
!       ZZHASH2(ITEM) |         |  |  |         |     |         |
!                     +---------+  |  +---------+     +---------+
!                     |         |  `->|Head of  |     |Item     |
!                     |         |     |collision| !=0 |corresp. |
!                     |         |     |list for | -.  |to head  |
!                     |         |     | ITEM    |  |  |of list  |
!                     +---------+     +---------+  |  +---------+
!                     |         |     |         |  |  |         |
!                     +---------+     +---------+  |  +---------+
!                     |         |     |         |<-'  |         |
!                     |         |     |Next Node| !=0 |Next Item|
!                     |         |     |         |--.  |         |
!                     +---------+     +---------+  |  +---------+
!                     |         |     |         |  |  |         |
!                     +---------+     +---------+  |  +---------+
!                     |         |     |         |  |  |         |
!                     +---------+     +---------+  |  +---------+
!                     |         |     |Next Node|<-'  |Next Item|
!                     +---------+     +---------+ etc +---------+
!                        ...              ...            ...
!                     +---------+     +---------+     +---------+
!                     |         |     |         |     |         |
!                     +---------+     +---------+     +---------+
!
!
! $ Examples
!
!     An add-only hash used together with flat data arrays provides a
!     simple, fast-access buffering mechanism. The subsystems that need
!     such buffering would normally initialize the hash, add items to
!     the hash and buffer data associated with items in flat data
!     arrays, and find items in the hash and, if found, use buffered
!     data associated with the items rather than computing/getting
!     their data again.
!
!     An add-only hash is normally used in one of the following ways --
!     set up to never be filled up, stop buffering when filled up, and
!     trash when filled up and start buffering again. Three examples
!     below illustrate each of these ways.
!
!     Example 1: Set up to never be filled up
!     ---------------------------------------
!
!     C
!     C     Parameters: pool lower boundary, hash size and item
!     C     size.
!     C
!           INTEGER               LBPOOL
!           PARAMETER           ( LBPOOL = -5 )
!
!           INTEGER               HSHSIZ
!           PARAMETER           ( HSHSIS = 5003 )
!
!           INTEGER               NAMSIZ
!           PARAMETER           ( NAMSIZ = 32 )
!
!     C
!     C     Hash arrays and data buffer.
!     C
!           INTEGER               HEDLST  (          HSHSIZ )
!           INTEGER               COLLST  ( LBPOOL : HSHSIZ )
!           CHARACTER*(NAMSIZ)    ITEMS   (          HSHSIZ )
!           INTEGER               DATBUF  (          HSHSIZ )
!
!     C
!     C     Miscellaneous variables.
!     C
!           CHARACTER*(NAMSIZ)    ITEM
!
!           INTEGER               ITEMAT
!           INTEGER               MYDATA
!
!           LOGICAL               FIRST
!           INTEGER               AVAIL
!           LOGICAL               NEW
!
!     C
!     C     Data items.
!     C
!           DATA FIRST             / .TRUE.  /
!
!     C
!     C     Initialize hash.
!     C
!           IF ( FIRST ) THEN
!              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!              FIRST = .FALSE.
!           END IF
!           ...
!     C
!     C     Check for presence of or add an item to the hash. If the
!     C     item is in the hash, NEW will be .FALSE. If the item is
!     C     not in the hash, it will be added and NEW will be .TRUE.
!     C     This call can fail only if the hash fills up which
!     C     should not happen because it was declared to never fill
!     C     up.
!     C
!           CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!          .                                ITEMAT, NEW )
!           IF ( FAILED() ) THEN
!              RETURN
!           END IF
!
!           IF ( .NOT. NEW ) THEN
!     C
!     C        Simply use buffered value.
!     C
!              MYDATA = DATBUF( ITEMAT )
!
!           ELSE
!     C
!     C        Get value.
!     C
!              ...
!     C
!     C        Buffer value.
!     C
!              DATBUF( ITEMAT ) = MYDATA
!
!           END IF
!
!
!
!     Example 2: Stop buffering when filled up
!     ----------------------------------------
!
!     C
!     C     Use the same declarations as the first example.
!     C
!           ...
!     C
!     C     Initialize hash.
!     C
!           IF ( FIRST ) THEN
!              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!              FIRST = .FALSE.
!           END IF
!           ...
!     C
!     C     Check if this item is in the hash.
!     C
!           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!
!           IF ( ITEMAT .NE. 0 ) THEN
!     C
!     C        Simply use buffered value.
!     C
!              MYDATA = DATBUF( ITEMAT )
!
!           ELSE
!     C
!     C        Get value.
!     C
!              ...
!     C
!     C        Buffer value, but only if the hash is not full.
!     C
!              CALL ZZHSCAVL( COLLST, AVAIL )
!              IF ( AVAIL .GT. 0 ) THEN
!                 CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!          .                                  ITEMAT, NEW )
!                 DATBUF( ITEMAT ) = MYDATA
!              END IF
!
!           END IF
!
!
!     Example 3: Trash when filled up and start buffering again
!     ---------------------------------------------------------
!
!     C
!     C     Use the same declarations as the first example.
!     C
!           ...
!     C
!     C     Initialize hash.
!     C
!           IF ( FIRST ) THEN
!              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!              FIRST = .FALSE.
!           END IF
!           ...
!     C
!     C     Check if this item is in the hash.
!     C
!           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!
!           IF ( ITEMAT .NE. 0 ) THEN
!     C
!     C        Simply use buffered value.
!     C
!              MYDATA = DATBUF( ITEMAT )
!
!           ELSE
!     C
!     C        Get value.
!     C
!              ...
!     C
!     C        Buffer value, if the hash is full trash re-initialize
!     C        it first.
!     C
!              CALL ZZHSCAVL( COLLST, AVAIL )
!              IF ( AVAIL .LE. 0 ) THEN
!                 CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!              END IF
!              CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!          .                                  ITEMAT, NEW )
!              DATBUF( ITEMAT ) = MYDATA
!
!           END IF
!
! $ Restrictions
!
!     For sake of speed all entry points do no or minimal error
!     checking. It is the responsibility of the caller to ensure that
!     all inputs are properly initialized.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-OCT-2020 (JDR) (BVS)
!
!        Changed entry point ZZHSCINI to not check RETURN()
!        on entry. That entry point now initializes the hash
!        as long as the size parameter is greater than zero.
!
!        Fixed short error message in umbrella routine.
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     manipulate add-only character hash
!
!-&

!
!     Hash control area items.
!
      integer(ip)           sizidx
      parameter           ( sizidx =   0 )

      integer(ip)           freidx
      parameter           ( freidx =  -1 )

!
!     SPICELIB functions.
!
      integer(ip)           zzhash2

      logical               failed
      logical               return

!
!     Local variables.
!
      integer(ip)           i
      integer(ip)           lookat
      integer(ip)           node

      logical               full
      logical               lfound

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZHSC' )
      end if

!
!     Signal bogus entry error and check out.
!
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZHSC' )

      return


!$Procedure ZZHSCINI ( Private---Initialize Add-only Character Hash )

      entry zzhscini ( hashsz, hedlst, collst )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize an add-only character hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               HASHSZ
!     INTEGER               HEDLST  (          * )
!     INTEGER               COLLST  ( LBPOOL : * )
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     HASHSZ     I   Hash size
!     HEDLST    I/O  Hash head node pointer list
!     COLLST    I/O  Hash node collision list
!
!     LBPOOL     P   Collision list array lower bound
!
! $ Detailed_Input
!
!     HASHSZ      is the hash size. For efficiency reasons it must be a
!                 prime number in the range from 1 to INTMAX/68 - 1.
!
!     HEDLST
!     COLLST      are the head node pointer list and the node collision
!                 list components of an add-only character hash.
!
! $ Detailed_Output
!
!     HEDLST
!     COLLST      are the head node pointer list and the node collision
!                 list components of an add-only character hash with all
!                 elements of the head node pointer list HEDLST set to
!                 zero and size and first-free elements of the head
!                 node pointer list COLLST set to HASHSZ and 1
!                 correspondingly.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     1) If HASHSZ is less than 1 or greater than the value that is
!        guaranteed not to result in eventual integer overflow in
!        ZZHASH2, the error will be signaled by ZZHASH2.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Examples
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Restrictions
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-OCT-2020 (BVS)
!
!        Changed to not check RETURN() on entry. The entry point
!        now initializes the hash as long as the size parameter
!        is greater than zero.
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     initialize add-only character hash
!
!-&

!
!     Standard SPICE error handling.
!
      call chkin  ( 'ZZHSCINI' )

      if ( hashsz > 0 ) then

!
!        Wipe out head node pointer list.
!
         do i = 1, hashsz
            hedlst(i) = 0
         end do

!
!        Reset control area.
!
         collst( sizidx ) = hashsz
         collst( freidx ) = 1

      end if

!
!     The requested number of nodes must be in the valid range. If it
!     is not, ZZHASH2 will signal an error.
!
      i = zzhash2( ' ', hashsz )

      if ( failed() ) then
         call chkout ( 'ZZHSCINI' )
         return
      end if

      call chkout ( 'ZZHSCINI' )

      return


!$Procedure ZZHSCADD ( Private---Add Item to Add-only Character Hash )

      entry zzhscadd ( hedlst, collst, items, item, itemat, new )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find or add, if not present, an item to an add-only character
!     hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               HEDLST  (          * )
!     INTEGER               COLLST  ( LBPOOL : * )
!     CHARACTER*(*)         ITEMS   (          * )
!     CHARACTER*(*)         ITEM
!     INTEGER               ITEMAT
!     LOGICAL               NEW
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     HEDLST    I/O  Hash head node pointer list
!     COLLST    I/O  Hash node collision list
!     ITEMS     I/O  Hash item list
!     ITEM       I   Item to be checked for/added
!     ITEMAT     O   Item index in node collision and item lists
!     NEW        O   Flag indicting if item was added
!
!     LBPOOL     P   Collision list array lower bound
!
! $ Detailed_Input
!
!     HEDLST
!     COLLST
!     ITEMS       are the components of an add-only character
!                 hash.
!
!     ITEM        is an item to find and, if needed, add to the hash.
!
! $ Detailed_Output
!
!     HEDLST
!     COLLST
!     ITEMS       are the components of an add-only character
!                 hash with the new item added to it if needed.
!
!     ITEMAT      is the index of the item in the node collision and
!                 item lists. If the item could not be found in or
!                 added to the hash, ITEMAT is set to 0.
!
!     NEW         is a flag indicting if item was added to the hash.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     1) If the hash is full, the error SPICE(HASHISFULL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Examples
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Restrictions
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     add an item to add-only character hash
!
!-&

!
!     Standard SPICE error handling. No checking-in here. We will do it
!     when we have to.
!
      if ( return() ) then
         return
      end if

!
!     Set flag indicating whether the hash is full.
!
      full   = collst ( freidx ) > collst ( sizidx )

!
!     Use hash function to get index of the head node.
!
      lookat = zzhash2 ( item, collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound = .false.
      new    = .false.

!
!     See if this item (or one colliding with it in the hash scheme)
!     has already been stored in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

!
!        If we didn't find this item on the conflict resolution list
!        and our hash is not full we will add this item to it.
!
         if ( .not. lfound .and. .not. full   ) then

!
!           Get next free node.
!
            node = collst ( freidx )

!
!           Increment next free node pointer.
!
            collst ( freidx ) = collst ( freidx ) + 1

!
!           Set current node pointer to the node we just picked for
!           this item.
!
            collst ( itemat ) = node

!
!           Set new node pointer to 0, just in case.
!
            collst ( node ) = 0

!
!           Save item.
!
            items ( node ) = item

!
!           Set output node ID and new and found flags.
!
            itemat =  node
            new    = .true.
            lfound = .true.

         end if

      else if ( .not. full ) then

!
!        Nothing like this item (in the hashing sense) has been stored
!        so far and the hash is not full.
!
!        Get next free node.
!
         node = collst ( freidx )

!
!        Increment next free node pointer.
!
         collst ( freidx ) = collst ( freidx ) + 1

!
!        Set new node pointer to 0, just in case.
!
         collst ( node ) = 0

!
!        Set the head node pointer to this node.
!
         hedlst ( lookat ) = node

!
!        Save item.
!
         items ( node ) = item

!
!        Set output node ID and new and found flags.
!
         itemat =  node
         new    = .true.
         lfound = .true.

      end if

!
!     Set ITEMAT to 0 if LFOUND is FALSE.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

!
!     If the item hash was full and we didn't find this item we've got
!     an error. Report it and return.
!
      if ( full .and. .not. lfound ) then

         call chkin  ( 'ZZHSCADD' )

         call setmsg ( 'The hash has no room for any more items.')
         call sigerr ( 'SPICE(HASHISFULL)' )

         call chkout ( 'ZZHSCADD' )
         return

      end if

      return


!$Procedure ZZHSCCHK ( Private---Find Item in Add-only Character Hash )

      entry zzhscchk ( hedlst, collst, items, item, itemat )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find an item in an add-only character hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               HEDLST  (          * )
!     INTEGER               COLLST  ( LBPOOL : * )
!     CHARACTER*(*)         ITEMS   (          * )
!     CHARACTER*(*)         ITEM
!     INTEGER               ITEMAT
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     HEDLST     I   Hash head node pointer list
!     COLLST     I   Hash node collision list
!     ITEMS      I   Hash item list
!     ITEM       I   Item to find
!     ITEMAT     O   Item index in node collision and item lists
!
!     LBPOOL     P   Collision list array lower bound
!
! $ Detailed_Input
!
!     HEDLST
!     COLLST
!     ITEMS       are the components of an add-only character
!                 hash.
!
!     ITEM        is an item to find in the hash.
!
! $ Detailed_Output
!
!     ITEMAT      is the index of the item in the node collision and
!                 item lists. If item is not in the hash, ITEMAT is set
!                 to 0.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Examples
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Restrictions
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     find item in add-only character hash
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

!
!     Use hash function to get index of the head node.
!
      lookat = zzhash2 ( item, collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound  = .false.

!
!     See if this item (or one colliding with it in the hash scheme) is
!     in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

      end if

!
!     If LFOUND is false, set ITEMAT to 0.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

      return


!$Procedure ZZHSCAVL ( Private---Get room available in Add-only Hash )

      entry zzhscavl ( collst, avail )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get room available in an add-only character hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               COLLST  ( LBPOOL : * )
!     INTEGER               AVAIL
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     COLLST     I   Hash node collision list
!     AVAIL      O   Room available in the hash
!
!     LBPOOL     P   Collision list array lower bound
!
! $ Detailed_Input
!
!     COLLST      is the add-only character hash node collision
!                 list.
!
! $ Detailed_Output
!
!     AVAIL       is the room (number of vacant slots) available in
!                 the hash.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Examples
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Restrictions
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     get room available in add-only character hash
!
!-&

!
!     Set the number of unoccupied slots in the hash.
!
      avail = collst ( sizidx ) - collst ( freidx ) + 1

      return


!$Procedure ZZHSCINF ( Private---Get Information about Add-only Hash )

      entry zzhscinf ( hedlst, collst, items, item, avail )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get information about an add-only character hash.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               HEDLST  (          * )
!     INTEGER               COLLST  ( LBPOOL : * )
!     CHARACTER*(*)         ITEMS   (          * )
!     CHARACTER*(*)         ITEM
!     INTEGER               AVAIL
!
! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     HEDLST     I   Hash head node pointer list
!     COLLST     I   Hash node collision list
!     ITEMS      I   Hash item list
!     ITEM       I   parameter to report
!     AVAIL      O   parameter Value
!
!     LBPOOL     P   Collision list array lower bound
!
! $ Detailed_Input
!
!     HEDLST
!     COLLST
!     ITEMS       are the components of an add-only character
!                 hash.
!
!     ITEM        is the parameter to report:
!
!                    'HASH SIZE'
!                    'USED HEADNODE COUNT'
!                    'UNUSED HEADNODE COUNT'
!                    'USED ITEM COUNT'
!                    'UNUSED ITEM COUNT'
!                    'LONGEST LIST SIZE'
!
! $ Detailed_Output
!
!     AVAIL       is the value of the parameter of interest.
!
! $ Parameters
!
!     LBPOOL      is the lower bound of the collision list array.
!
! $ Exceptions
!
!     1) If the input ITEM is not recognized, the error
!        SPICE(ITEMNOTRECOGNIZED) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     See the header of the umbrella routine ZZHSC.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     get information about add-only character hash
!
!-&

!
!     Get the hash size.
!
      if      ( item == 'HASH SIZE' ) then

         avail = collst ( sizidx )

!
!     Get the count of used nodes in the head list.
!
      else if ( item == 'USED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) /= 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of unused nodes in the head list.
!
      else if ( item == 'UNUSED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) == 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of used slots in the item list.
!
      else if ( item == 'USED ITEM COUNT' ) then

         avail = collst ( freidx ) - 1

!
!     Get the count of unused slots in the item list.
!
      else if ( item == 'UNUSED ITEM COUNT' ) then

         avail = collst ( sizidx ) - collst ( freidx ) + 1

!
!     Get the size of the longest item list for any hash value.
!
      else if ( item == 'LONGEST LIST SIZE' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            node = hedlst ( i )
            lookat = 0
            do while ( node > 0 )
               lookat = lookat + 1
               node   = collst ( node )
            end do
            avail = max( avail, lookat )
         end do

!
!     This parameter is not supported.
!
      else

         avail = 0

         call chkin  ( 'ZZHSCINF'                           )
         call setmsg ( 'Parameter ''#'' is not recognized.' )
         call errch  ( '#', item                            )
         call sigerr ( 'SPICE(ITEMNOTRECOGNIZED)'           )
         call chkout ( 'ZZHSCINF'                           )

      end if

      return

      end
