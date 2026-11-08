!$Procedure      ZZEKTR1S ( EK tree, one-shot load )

      subroutine zzektr1s ( handle, tree, size, values )
      use pepper_kinds, only: ip

! $ Abstract
!
!     One-shot tree load:  insert an entire array into an empty
!     tree.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      implicit none

      include 'ekpage.inc'
      include 'ektree.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           size
      integer(ip)           values  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     SIZE       I   Size of tree.
!     VALUES     I   Values to insert.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!                    The tree must be empty.
!
!     SIZE           is the size of the tree to create:  SIZE is the
!                    number of values that will be inserted into the
!                    tree.
!
!     VALUES         is an array of integer values to be inserted into
!                    the tree.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
!     3)  If the input tree is not empty, the error SPICE(NONEMPTYTREE)
!         is signaled.
!
!     4)  If the depth of the tree needed to hold the number of values
!         indicated by SIZE exceeds the maximum depth limit, the error
!         SPICE(COUNTTOOLARGE) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine creates an EK tree and loads the tree with the
!     integer values supplied in the array VALUES.  The ordinal
!     positions of the values in the tree correspond to the positions
!     of the values in the input array:  for example, the 10th element
!     of the array is pointed to by the key 10.
!
!     This routine loads a tree much faster than can be done by
!     sequentially loading the set of values by successive calls to
!     ZZEKTRIN.  On the other hand, the caller must declare an array
!     large enough to hold all of the values to be loaded.  Note that
!     a partially full tree cannot be extended using this routine.
!
! $ Examples
!
!     See EKFFLD.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
!         3/Sorting and Searching" 1973, pp 471-479.
!
!         EK trees are closely related to the B* trees described by
!         Knuth.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
!
!        Now uses ERRHAN
!
!        Cleaned up use of unnecessary variables and unneeded
!        declarations.
!
!-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed redundant calls to CHKIN
!
!-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Non-SPICELIB functions
!
      integer(ip)           zzektrbs
      integer(ip)           zzektrsz

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           basidx
      integer(ip)           bigsiz
      integer(ip)           child
      integer(ip)           d
      integer(ip)           div
      integer(ip)           i
      integer(ip)           key
      integer(ip)           kidbas
      integer(ip)           level
      integer(ip)           maxsiz
      integer(ip)           n
      integer(ip)           nbig
      integer(ip)           next
      integer(ip)           nkeys
      integer(ip)           nkids
      integer(ip)           nnodes
      integer(ip)           node
      integer(ip)           npred
      integer(ip)           nsmall
      integer(ip)           page  ( pgsizi )
      integer(ip)           q
      integer(ip)           reqsiz
      integer(ip)           s
      integer(ip)           stlsiz ( trmxdp )
      integer(ip)           stnbig ( trmxdp )
      integer(ip)           stnext ( trmxdp )
      integer(ip)           stnkey ( trmxdp )
      integer(ip)           stnode ( trmxdp )
      integer(ip)           stnbas ( trmxdp )
      integer(ip)           subd
      integer(ip)           subsiz
      integer(ip)           totnod
      integer(ip)           tsize

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKTR1S' )

!
!     Make sure the input tree is empty.
!
      tsize  =  zzektrsz ( handle, tree )

      if ( tsize .gt. 0 ) then

         call setmsg ( 'Tree has size #; should be empty.' // &
                       'EK = #; TREE = #.'                 )
         call errint ( '#',  tsize                         )
         call errhan ( '#',  handle                        )
         call errint ( '#',  tree                          )
         call sigerr ( 'SPICE(NONEMPTYTREE)'               )
         call chkout ( 'ZZEKTR1S'                          )
         return

      end if

!
!     Compute the tree depth required.  The largest tree of a given
!     depth D contains the root node plus S(D) child nodes, where
!
!
!        S(1)  =  1
!
!
!     and if D is at least 2,
!                                    D - 2
!                                    ____
!                                    \              i
!        S(D)  =  MAX_SIZE      *    /      MAX_SIZE
!                         Root       ----           Child
!                                    i = 0
!
!
!                                    D - 2
!                                    ____
!                                    \            i
!              =  MXKIDR        *    /      MXKIDC
!                                    ----
!                                    i = 0
!
!
!                                         D-1
!                                   MXKIDC   -  1
!              =  MXKIDR        *   -------------
!                                    MXKIDC  - 1
!
!
!     If all of these nodes are full, the number of keys that
!     can be held in this tree is
!
!        MXKEYR  +  S(D) * MXKEYC
!
!     We want the minimum value of D such that this expression
!     is greater than or equal to SIZE.
!

      tsize  =  mxkeyr
      d      =  1
      s      =  1

      do while ( tsize .lt. size )

         d  =  d  +  1

         if ( d .eq. 2 ) then

            s  =  mxkeyr

         else
!
!           For computational purposes, the relationship
!
!              S(D+1)  =   MXKIDR  +  MXKIDC * S(D)
!
!           is handy.
!
!
            s  =  mxkidr  +  mxkidc * s

         end if

         tsize   =   mxkeyr  +  s * mxkeyc

      end do

!
!     If the tree must be deeper than we expected, we've a problem.
!
      if ( d .gt. trmxdp ) then

         call setmsg ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         call errint ( '#',  d                                       )
         call errint ( '#',  trmxdp                                  )
         call errhan ( '#',  handle                                  )
         call errint ( '#',  tree                                    )
         call sigerr ( 'SPICE(COUNTTOOLARGE)'                        )
         call chkout ( 'ZZEKTR1S'                                    )
         return

      end if

!
!     The basic error checks are done.  At this point, we can build the
!     tree.
!
!     The approach is to fill in the tree in a top-down fashion.
!     We decide how big each subtree of the root will be; this
!     information allows us to decide which keys actually belong
!     in the root.  Having filled in the root, we repeat the process
!     for each subtree of the root in left-to-right order.
!
!     We use a stack to keep track of the ancestors of the
!     node we're currently considering.  The table below shows the
!     items we save on the stack and the stack variables associated
!     with those items:
!
!
!        Item                                 Stack Variable
!        ----                                 ---------------
!        Node number                          STNODE
!
!        Number of keys in node               STNKEY
!
!        Larger subtree size                  STLSIZ
!
!        Number of large subtrees             STNBIG
!
!        Index of next subtree to visit       STNEXT
!
!        Base index of node                   STNBAS
!
!
      node              =  tree
      subsiz            =  size
      next              =  1
      level             =  1
      basidx            =  0

      do while ( level .gt. 0 )
!
!        At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         if ( next .eq. 1 ) then
!
!           This node has not been visited yet.  We'll fill in this
!           node before proceeding to fill in its descendants.  The
!           first step is to compute the number and sizes of the
!           subtrees of this node.
!
!           Decide the large subtree size and the number of subtrees of
!           this node.  The depth SUBD of the subtrees of this node is
!           D - LEVEL.  Each subtree has size bounded by the sizes of
!           the subtree of depth SUBD in which all nodes contain MNKEYC
!           keys and the by the subtree of depth SUBD in which all nodes
!           contain MXKEYC keys.  If this node is not the root and is
!           not a leaf node, the number of subtrees must be between
!           MNKIDC and MXKIDC.
!
            if ( level .eq. 1 ) then
!
!              We're working on the root.  The number of subtrees is
!              anywhere between 0 and MXKIDR, inclusive.  We'll create
!              a tree with the minimum number of subtrees of the root.
!
               if ( d .gt. 1 ) then
!
!                 We'll find the number of subtrees of maximum size
!                 that we would need to hold the non-root keys of the
!                 tree.  We'll then determine the actual required sizes
!                 of these subtrees.
!
                  subd    =  d - 1

                  nnodes  =  0

                  do i = 1, subd
                     nnodes =  mxkidc * nnodes  +  1
                  end do

                  maxsiz  =  nnodes * mxkeyc

!
!                 If we had NKIDS subtrees of size MAXSIZ, NKIDS
!                 would be the smallest integer such that
!
!                    ( NKIDS - 1 )  +  NKIDS * MAXSIZ  >  SUBSIZ
!                                                      -
!
!                 or equivalently,
!
!                     NKIDS * ( MAXSIZ + 1 )  >  SUBSIZ + 1
!                                             -
!
!                 We'll compute this value of NKIDS.
!
!
                  q       =  subsiz + 1
                  div     =  maxsiz + 1
                  nkids  =  ( q + div - 1 )  / div

!
!                 The minimum number of keys we must store in child
!                 nodes is the number of keys in the tree, minus those
!                 that can be accommodated in the root:
!
                  n       =   subsiz - ( nkids - 1 )

!
!                 Now we can figure out how large the subtrees would
!                 have to be in order to hold N keys, if all subtrees
!                 had the same size.
!
                  bigsiz  =  ( n + nkids - 1 ) / nkids

!
!                 We may have more capacity than we need if all subtrees
!                 have size BIGSIZ.  So, we'll allow some subtrees to
!                 have size BIGSIZ-1.  Not all subtrees can have the
!                 smaller size (otherwise BIGSIZ would have been
!                 smaller).  The first NBIG subtrees will have the
!                 larger size.
!
                  nsmall  =  ( nkids * bigsiz ) - n
                  nbig    =    nkids - nsmall

                  nkeys   =  nkids  -  1


               else
!
!                 All keys are in the root.
!
                  nkeys   =  size
                  nkids   =  0

               end if

!
!              Read in the root page.
!
               call zzekpgri ( handle, tree, page )

!
!              We have enough information to fill in the root node.
!              We'll allocate nodes for the immediate children.
!              There is one key `between' each child pointer.
!
               do i = 1, nkeys
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the root.
!
                  if ( i .eq. 1 ) then
                     npred = 0
                  else
                     npred = page( trkeyr+i-1 )
                  end if

                  if ( d .gt. 1 ) then
!
!                    The tree contains subtrees.
!
                     if ( i .le. nbig ) then
                        key   =  npred  + bigsiz + 1
                     else
                        key   =  npred  + bigsiz
                     end if

                  else

                     key  =  i

                  end if

                  page ( trkeyr + i ) =  key
                  page ( trdatr + i ) =  values ( key )

               end do


               totnod  =  1

               do i = 1, nkids
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  call zzekpgal ( handle, int, child, base )
                  page ( trkidr + i )  =  child
                  totnod               =  totnod + 1

               end do

!
!              Fill in the root's metadata.  There is one item that
!              we'll have to fill in when we're done:  the number of
!              nodes in the tree.  We know the rest of the information
!              now.
!
               page ( trnkey )  =  size
               page ( trdpth )  =  d
               page ( trkeyr )  =  nkeys
               page ( trnnod )  =  0

!
!              Write out the root.
!
               call zzekpgwi ( handle, tree, page )


            else if ( level .lt. d ) then
!
!              The current node is a non-leaf child node.
!
               call cleari ( pgsizi, page )
!
!              The tree headed by this node has depth D-LEVEL+1 and
!              must hold SUBSIZ keys.  We must figure out the size
!              and number of subtrees of the current node.  Unlike in
!              the case of the root, we must have between MNKIDC
!              and MXKIDC subtrees of this node.  We start out by
!              computing the required subtree size if there were
!              exactly MNKIDC subtrees.  In this case, the total
!              number of keys in the subtrees would be
!
!                 SUBSIZ  -  MNKEYC
!
!
               n       =  subsiz - mnkeyc
               reqsiz  =  ( n + mnkeyc - 1 )  /  mnkeyc

!
!              Compute the maximum allowable number of keys in
!              a subtree.
!
               subd    =  d - level

               nnodes  =  0

               do i = 1, subd
                  nnodes =  mxkidc * nnodes  +  1
               end do

               maxsiz  =  nnodes * mxkeyc

!
!              If the number REQSIZ we came up with is a valid size,
!              we'll be able to get the correct number of children
!              by using subtrees of size REQSIZ and REQSIZ-1.  Note
!              that it's impossible for REQSIZ to be too small,
!              since the smallest possible number of subtrees is
!              MNKIDC.
!
               if ( reqsiz .le. maxsiz ) then
!
!                 Decide how many large and small subtrees we need.
!
                  nkids   =  mnkidc
                  bigsiz  =  reqsiz
                  nsmall  =  bigsiz * nkids  -  n
                  nbig    =  nkids  -  nsmall

               else
!
!
!                 See how many subtrees of size MAXSIZ it would take
!                 to hold the requisite number of keys.  We know the
!                 number is more than MNKIDC.  If we have NKIDS
!                 subtrees of size MAXSIZ, the total number of
!                 keys in the subtree headed by NODE is
!
!                   ( NKIDS - 1 )  +  ( NKIDS * MAXSIZ )
!
!                 or
!
!                     NKIDS * ( MAXSIZ + 1 )   -   1
!
!                 We must find the smallest value of NKIDS such
!                 that the above quantity is greater than or equal
!                 to SUBSIZ.
!
                  q      =  subsiz + 1
                  div    =  maxsiz + 1
                  nkids  =  ( q + div - 1 )  / div

!
!                 We know that NKIDS subtrees of size MAXSIZ, plus
!                 NKIDS-1 keys in NODE, can hold at least SUBSIZ
!                 keys.  We now want to find the smallest subtree
!                 size such that NKIDS subtrees of that size,
!                 together with the NKIDS-1 keys in NODE, contain
!                 at least SUBSIZ keys.  The size we seek will
!                 become BIGSIZ, the larger of the two subtree
!                 sizes we'll use.  So BIGSIZ is the smallest
!                 integer such that
!
!                    ( NKIDS - 1 ) + ( NKIDS * BIGSIZ )  >  SUBSIZ
!                                                        -
!
!                 or equivalently
!
!                    BIGSIZ * NKIDS  >  SUBSIZ - NKIDS + 1
!                                    -
!
                  q       =  subsiz - nkids + 1
                  div     =  nkids
                  bigsiz  =  ( q + div - 1 )  / div

                  nsmall  =  bigsiz * nkids  -  q
                  nbig    =  nkids  -  nsmall

               end if

!
!              Fill in the keys for the current node.
!
               nkeys   =  nkids  -  1

               do i = 1, nkeys
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the current node.
!
                  if ( i .eq. 1 ) then
                     npred = basidx
                  else
                     npred = basidx + page( trkeyc+i-1 )
                  end if

                  if ( i .le. nbig ) then
                     key   =  npred  + bigsiz + 1
                  else
                     key   =  npred  + bigsiz
                  end if

                  page ( trkeyc + i ) =  key - basidx
                  page ( trdatc + i ) =  values ( key )

               end do


               do i = 1, nkids
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  call zzekpgal ( handle, int, child, base )

                  page ( trkidc + i )  =  child
                  totnod               =  totnod + 1

               end do

!
!              We can now fill in the metadata for the current node.
!
               page ( trnkc )  =  nkeys

               call zzekpgwi ( handle, node, page )

            end if

!
!           Unless the current node is a leaf node, prepare to visit
!           the first child of the current node.
!
            if ( level .lt. d ) then
!
!              Push our current state.
!
               stnode ( level )  = node
               stnkey ( level )  = nkeys
               stlsiz ( level )  = bigsiz
               stnbig ( level )  = nbig
               stnext ( level )  = 2
               stnbas ( level )  = basidx

!
!              NEXT is already set to 1.  BASIDX is set, since the
!              base index of the first child is that of the parent.
!
               if ( level .eq. 1 ) then
                  kidbas  =  trkidr
               else
                  kidbas  =  trkidc
               end if

               level   =  level + 1
               node    =  page ( kidbas+1 )
               subsiz  =  bigsiz


            else if ( level .gt. 1 ) then
!
!              The current node is a child leaf node.  There are no
!              calculations to do; we simply assign keys and pointers,
!              write out metadata, and pop our state.
!
               nkeys  =  subsiz

               do i = 1, nkeys

                  key                 =  basidx + i
                  page ( trkeyc + i ) =  i
                  page ( trdatc + i ) =  values ( key )

               end do

!
!              We can now fill in the metadata for the current node.
!
               page ( trnkc )  =  nkeys

               call zzekpgwi ( handle, node, page )

!
!              A leaf node is a subtree unto itself, and we're
!              done with this subtree.  Pop our state.
!
               level  =  level - 1

               if ( level .ge. 1 ) then

                  node    =  stnode ( level )
                  nkeys   =  stnkey ( level )
                  bigsiz  =  stlsiz ( level )
                  nbig    =  stnbig ( level )
                  next    =  stnext ( level )
                  basidx  =  stnbas ( level )
                  nkids   =  nkeys + 1

!
!                 Read in the current node.
!
                  call zzekpgri ( handle, node, page )

               end if


            else
!
!              The only node is the root.  Pop out.
!
               level  =  0

            end if
!
!           We've decided which node to go to next at this point.
!           At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         else
!
!           The current node has been visited already.  Visit the
!           next child, if there is one.
!
            if ( next .le. nkids ) then
!
!              Prepare to visit the next child of the current node.
!
               stnext ( level )   =   next  +  1


               if ( level .eq. 1 ) then
                  kidbas  =  trkidr
               else
                  kidbas  =  trkidc
               end if


               node    =  page ( kidbas + next )


               if ( next .le. nbig ) then
                  subsiz  =  stlsiz(level)
               else
                  subsiz  =  stlsiz(level) - 1
               end if


               if ( next .le. nbig+1 ) then

                  basidx  =      stnbas(level) &
                             + ( next-1 ) * stlsiz(level) &
                             + ( next-1 )

               else

                  basidx  =      stnbas(level) &
                             +   nbig           *   stlsiz(level) &
                             + ( next-nbig-1 )  * ( stlsiz(level) - 1 ) &
                             + ( next-1 )
               end if

               level  =  level + 1
               next   =  1

!
!              LEVEL, NEXT, NODE, SUBSIZ, and BASIDX are set.
!

            else
!
!              We're done with the current subtree.  Pop the stack.
!
               level   =  level - 1

               if ( level .ge. 1 ) then

                  node    =  stnode ( level )
                  nkeys   =  stnkey ( level )
                  bigsiz  =  stlsiz ( level )
                  nbig    =  stnbig ( level )
                  next    =  stnext ( level )
                  basidx  =  stnbas ( level )
                  nkids   =  nkeys + 1
!
!                 Read in the current node.
!
                  call zzekpgri ( handle, node, page )

               end if

            end if


         end if

!
!        On this pass through the loop, we either---
!
!           - Visited a node for the first time and filled in the
!             node.
!
!           - Advanced to a new node that has not yet been visited.
!
!           - Exited from a completed subtree.
!
!        Each of these actions can be performed a finite number of
!        times.  Therefore, we made progress toward loop termination.
!
      end do


!
!     The last chore is setting the total number of nodes in the root.
!
      base  = zzektrbs ( tree )

      call dasudi ( handle, base+trnnod, base+trnnod, totnod )


      call chkout ( 'ZZEKTR1S' )
      return
      end
