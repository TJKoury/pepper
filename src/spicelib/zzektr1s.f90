!$Procedure      ZZEKTR1S ( EK tree, one-shot load )

      SUBROUTINE ZZEKTR1S ( HANDLE, TREE, SIZE, VALUES )

! $ Abstract
!
!     One-shot tree load:  insert an entire array into an empty
!     tree.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               SIZE
      INTEGER               VALUES  ( * )

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
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRSZ

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               BASIDX
      INTEGER               BIGSIZ
      INTEGER               CHILD
      INTEGER               D
      INTEGER               DIV
      INTEGER               I
      INTEGER               KEY
      INTEGER               KIDBAS
      INTEGER               LEVEL
      INTEGER               MAXSIZ
      INTEGER               N
      INTEGER               NBIG
      INTEGER               NEXT
      INTEGER               NKEYS
      INTEGER               NKIDS
      INTEGER               NNODES
      INTEGER               NODE
      INTEGER               NPRED
      INTEGER               NSMALL
      INTEGER               PAGE  ( PGSIZI )
      INTEGER               Q
      INTEGER               REQSIZ
      INTEGER               S
      INTEGER               STLSIZ ( TRMXDP )
      INTEGER               STNBIG ( TRMXDP )
      INTEGER               STNEXT ( TRMXDP )
      INTEGER               STNKEY ( TRMXDP )
      INTEGER               STNODE ( TRMXDP )
      INTEGER               STNBAS ( TRMXDP )
      INTEGER               SUBD
      INTEGER               SUBSIZ
      INTEGER               TOTNOD
      INTEGER               TSIZE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKTR1S' )

!
!     Make sure the input tree is empty.
!
      TSIZE  =  ZZEKTRSZ ( HANDLE, TREE )

      IF ( TSIZE .GT. 0 ) THEN

         CALL SETMSG ( 'Tree has size #; should be empty.' // &
                       'EK = #; TREE = #.'                 )
         CALL ERRINT ( '#',  TSIZE                         )
         CALL ERRHAN ( '#',  HANDLE                        )
         CALL ERRINT ( '#',  TREE                          )
         CALL SIGERR ( 'SPICE(NONEMPTYTREE)'               )
         CALL CHKOUT ( 'ZZEKTR1S'                          )
         RETURN

      END IF

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

      TSIZE  =  MXKEYR
      D      =  1
      S      =  1

      DO WHILE ( TSIZE .LT. SIZE )

         D  =  D  +  1

         IF ( D .EQ. 2 ) THEN

            S  =  MXKEYR

         ELSE
!
!           For computational purposes, the relationship
!
!              S(D+1)  =   MXKIDR  +  MXKIDC * S(D)
!
!           is handy.
!
!
            S  =  MXKIDR  +  MXKIDC * S

         END IF

         TSIZE   =   MXKEYR  +  S * MXKEYC

      END DO

!
!     If the tree must be deeper than we expected, we've a problem.
!
      IF ( D .GT. TRMXDP ) THEN

         CALL SETMSG ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         CALL ERRINT ( '#',  D                                       )
         CALL ERRINT ( '#',  TRMXDP                                  )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL ERRINT ( '#',  TREE                                    )
         CALL SIGERR ( 'SPICE(COUNTTOOLARGE)'                        )
         CALL CHKOUT ( 'ZZEKTR1S'                                    )
         RETURN

      END IF

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
      NODE              =  TREE
      SUBSIZ            =  SIZE
      NEXT              =  1
      LEVEL             =  1
      BASIDX            =  0

      DO WHILE ( LEVEL .GT. 0 )
!
!        At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         IF ( NEXT .EQ. 1 ) THEN
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
            IF ( LEVEL .EQ. 1 ) THEN
!
!              We're working on the root.  The number of subtrees is
!              anywhere between 0 and MXKIDR, inclusive.  We'll create
!              a tree with the minimum number of subtrees of the root.
!
               IF ( D .GT. 1 ) THEN
!
!                 We'll find the number of subtrees of maximum size
!                 that we would need to hold the non-root keys of the
!                 tree.  We'll then determine the actual required sizes
!                 of these subtrees.
!
                  SUBD    =  D - 1

                  NNODES  =  0

                  DO I = 1, SUBD
                     NNODES =  MXKIDC * NNODES  +  1
                  END DO

                  MAXSIZ  =  NNODES * MXKEYC

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
                  Q       =  SUBSIZ + 1
                  DIV     =  MAXSIZ + 1
                  NKIDS  =  ( Q + DIV - 1 )  / DIV

!
!                 The minimum number of keys we must store in child
!                 nodes is the number of keys in the tree, minus those
!                 that can be accommodated in the root:
!
                  N       =   SUBSIZ - ( NKIDS - 1 )

!
!                 Now we can figure out how large the subtrees would
!                 have to be in order to hold N keys, if all subtrees
!                 had the same size.
!
                  BIGSIZ  =  ( N + NKIDS - 1 ) / NKIDS

!
!                 We may have more capacity than we need if all subtrees
!                 have size BIGSIZ.  So, we'll allow some subtrees to
!                 have size BIGSIZ-1.  Not all subtrees can have the
!                 smaller size (otherwise BIGSIZ would have been
!                 smaller).  The first NBIG subtrees will have the
!                 larger size.
!
                  NSMALL  =  ( NKIDS * BIGSIZ ) - N
                  NBIG    =    NKIDS - NSMALL

                  NKEYS   =  NKIDS  -  1


               ELSE
!
!                 All keys are in the root.
!
                  NKEYS   =  SIZE
                  NKIDS   =  0

               END IF

!
!              Read in the root page.
!
               CALL ZZEKPGRI ( HANDLE, TREE, PAGE )

!
!              We have enough information to fill in the root node.
!              We'll allocate nodes for the immediate children.
!              There is one key `between' each child pointer.
!
               DO I = 1, NKEYS
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the root.
!
                  IF ( I .EQ. 1 ) THEN
                     NPRED = 0
                  ELSE
                     NPRED = PAGE( TRKEYR+I-1 )
                  END IF

                  IF ( D .GT. 1 ) THEN
!
!                    The tree contains subtrees.
!
                     IF ( I .LE. NBIG ) THEN
                        KEY   =  NPRED  + BIGSIZ + 1
                     ELSE
                        KEY   =  NPRED  + BIGSIZ
                     END IF

                  ELSE

                     KEY  =  I

                  END IF

                  PAGE ( TRKEYR + I ) =  KEY
                  PAGE ( TRDATR + I ) =  VALUES ( KEY )

               END DO


               TOTNOD  =  1

               DO I = 1, NKIDS
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  CALL ZZEKPGAL ( HANDLE, INT, CHILD, BASE )
                  PAGE ( TRKIDR + I )  =  CHILD
                  TOTNOD               =  TOTNOD + 1

               END DO

!
!              Fill in the root's metadata.  There is one item that
!              we'll have to fill in when we're done:  the number of
!              nodes in the tree.  We know the rest of the information
!              now.
!
               PAGE ( TRNKEY )  =  SIZE
               PAGE ( TRDPTH )  =  D
               PAGE ( TRKEYR )  =  NKEYS
               PAGE ( TRNNOD )  =  0

!
!              Write out the root.
!
               CALL ZZEKPGWI ( HANDLE, TREE, PAGE )


            ELSE IF ( LEVEL .LT. D ) THEN
!
!              The current node is a non-leaf child node.
!
               CALL CLEARI ( PGSIZI, PAGE )
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
               N       =  SUBSIZ - MNKEYC
               REQSIZ  =  ( N + MNKEYC - 1 )  /  MNKEYC

!
!              Compute the maximum allowable number of keys in
!              a subtree.
!
               SUBD    =  D - LEVEL

               NNODES  =  0

               DO I = 1, SUBD
                  NNODES =  MXKIDC * NNODES  +  1
               END DO

               MAXSIZ  =  NNODES * MXKEYC

!
!              If the number REQSIZ we came up with is a valid size,
!              we'll be able to get the correct number of children
!              by using subtrees of size REQSIZ and REQSIZ-1.  Note
!              that it's impossible for REQSIZ to be too small,
!              since the smallest possible number of subtrees is
!              MNKIDC.
!
               IF ( REQSIZ .LE. MAXSIZ ) THEN
!
!                 Decide how many large and small subtrees we need.
!
                  NKIDS   =  MNKIDC
                  BIGSIZ  =  REQSIZ
                  NSMALL  =  BIGSIZ * NKIDS  -  N
                  NBIG    =  NKIDS  -  NSMALL

               ELSE
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
                  Q      =  SUBSIZ + 1
                  DIV    =  MAXSIZ + 1
                  NKIDS  =  ( Q + DIV - 1 )  / DIV

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
                  Q       =  SUBSIZ - NKIDS + 1
                  DIV     =  NKIDS
                  BIGSIZ  =  ( Q + DIV - 1 )  / DIV

                  NSMALL  =  BIGSIZ * NKIDS  -  Q
                  NBIG    =  NKIDS  -  NSMALL

               END IF

!
!              Fill in the keys for the current node.
!
               NKEYS   =  NKIDS  -  1

               DO I = 1, NKEYS
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the current node.
!
                  IF ( I .EQ. 1 ) THEN
                     NPRED = BASIDX
                  ELSE
                     NPRED = BASIDX + PAGE( TRKEYC+I-1 )
                  END IF

                  IF ( I .LE. NBIG ) THEN
                     KEY   =  NPRED  + BIGSIZ + 1
                  ELSE
                     KEY   =  NPRED  + BIGSIZ
                  END IF

                  PAGE ( TRKEYC + I ) =  KEY - BASIDX
                  PAGE ( TRDATC + I ) =  VALUES ( KEY )

               END DO


               DO I = 1, NKIDS
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  CALL ZZEKPGAL ( HANDLE, INT, CHILD, BASE )

                  PAGE ( TRKIDC + I )  =  CHILD
                  TOTNOD               =  TOTNOD + 1

               END DO

!
!              We can now fill in the metadata for the current node.
!
               PAGE ( TRNKC )  =  NKEYS

               CALL ZZEKPGWI ( HANDLE, NODE, PAGE )

            END IF

!
!           Unless the current node is a leaf node, prepare to visit
!           the first child of the current node.
!
            IF ( LEVEL .LT. D ) THEN
!
!              Push our current state.
!
               STNODE ( LEVEL )  = NODE
               STNKEY ( LEVEL )  = NKEYS
               STLSIZ ( LEVEL )  = BIGSIZ
               STNBIG ( LEVEL )  = NBIG
               STNEXT ( LEVEL )  = 2
               STNBAS ( LEVEL )  = BASIDX

!
!              NEXT is already set to 1.  BASIDX is set, since the
!              base index of the first child is that of the parent.
!
               IF ( LEVEL .EQ. 1 ) THEN
                  KIDBAS  =  TRKIDR
               ELSE
                  KIDBAS  =  TRKIDC
               END IF

               LEVEL   =  LEVEL + 1
               NODE    =  PAGE ( KIDBAS+1 )
               SUBSIZ  =  BIGSIZ


            ELSE IF ( LEVEL .GT. 1 ) THEN
!
!              The current node is a child leaf node.  There are no
!              calculations to do; we simply assign keys and pointers,
!              write out metadata, and pop our state.
!
               NKEYS  =  SUBSIZ

               DO I = 1, NKEYS

                  KEY                 =  BASIDX + I
                  PAGE ( TRKEYC + I ) =  I
                  PAGE ( TRDATC + I ) =  VALUES ( KEY )

               END DO

!
!              We can now fill in the metadata for the current node.
!
               PAGE ( TRNKC )  =  NKEYS

               CALL ZZEKPGWI ( HANDLE, NODE, PAGE )

!
!              A leaf node is a subtree unto itself, and we're
!              done with this subtree.  Pop our state.
!
               LEVEL  =  LEVEL - 1

               IF ( LEVEL .GE. 1 ) THEN

                  NODE    =  STNODE ( LEVEL )
                  NKEYS   =  STNKEY ( LEVEL )
                  BIGSIZ  =  STLSIZ ( LEVEL )
                  NBIG    =  STNBIG ( LEVEL )
                  NEXT    =  STNEXT ( LEVEL )
                  BASIDX  =  STNBAS ( LEVEL )
                  NKIDS   =  NKEYS + 1

!
!                 Read in the current node.
!
                  CALL ZZEKPGRI ( HANDLE, NODE, PAGE )

               END IF


            ELSE
!
!              The only node is the root.  Pop out.
!
               LEVEL  =  0

            END IF
!
!           We've decided which node to go to next at this point.
!           At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         ELSE
!
!           The current node has been visited already.  Visit the
!           next child, if there is one.
!
            IF ( NEXT .LE. NKIDS ) THEN
!
!              Prepare to visit the next child of the current node.
!
               STNEXT ( LEVEL )   =   NEXT  +  1


               IF ( LEVEL .EQ. 1 ) THEN
                  KIDBAS  =  TRKIDR
               ELSE
                  KIDBAS  =  TRKIDC
               END IF


               NODE    =  PAGE ( KIDBAS + NEXT )


               IF ( NEXT .LE. NBIG ) THEN
                  SUBSIZ  =  STLSIZ(LEVEL)
               ELSE
                  SUBSIZ  =  STLSIZ(LEVEL) - 1
               END IF


               IF ( NEXT .LE. NBIG+1 ) THEN

                  BASIDX  =      STNBAS(LEVEL) &
                             + ( NEXT-1 ) * STLSIZ(LEVEL) &
                             + ( NEXT-1 )

               ELSE

                  BASIDX  =      STNBAS(LEVEL) &
                             +   NBIG           *   STLSIZ(LEVEL) &
                             + ( NEXT-NBIG-1 )  * ( STLSIZ(LEVEL) - 1 ) &
                             + ( NEXT-1 )
               END IF

               LEVEL  =  LEVEL + 1
               NEXT   =  1

!
!              LEVEL, NEXT, NODE, SUBSIZ, and BASIDX are set.
!

            ELSE
!
!              We're done with the current subtree.  Pop the stack.
!
               LEVEL   =  LEVEL - 1

               IF ( LEVEL .GE. 1 ) THEN

                  NODE    =  STNODE ( LEVEL )
                  NKEYS   =  STNKEY ( LEVEL )
                  BIGSIZ  =  STLSIZ ( LEVEL )
                  NBIG    =  STNBIG ( LEVEL )
                  NEXT    =  STNEXT ( LEVEL )
                  BASIDX  =  STNBAS ( LEVEL )
                  NKIDS   =  NKEYS + 1
!
!                 Read in the current node.
!
                  CALL ZZEKPGRI ( HANDLE, NODE, PAGE )

               END IF

            END IF


         END IF

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
      END DO


!
!     The last chore is setting the total number of nodes in the root.
!
      BASE  = ZZEKTRBS ( TREE )

      CALL DASUDI ( HANDLE, BASE+TRNNOD, BASE+TRNNOD, TOTNOD )


      CALL CHKOUT ( 'ZZEKTR1S' )
      RETURN
      END
