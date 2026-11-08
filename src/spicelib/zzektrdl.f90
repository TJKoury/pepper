!$Procedure      ZZEKTRDL ( EK tree, delete value )

      SUBROUTINE ZZEKTRDL ( HANDLE, TREE, KEY )

! $ Abstract
!
!     Delete a value from an EK tree at a specified location.
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

      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key at which to delete value.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key indicating the deletion
!                    location.  In EK trees, absolute keys are just
!                    ordinal positions relative to the leftmost element
!                    of the tree, with the leftmost element having
!                    position 1.  So setting KEY to 10, for example,
!                    indicates that the input VALUE is the 10th item in
!                    the tree.
!
!                    KEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree prior to
!                    the deletion.
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
!     3)  If the input key is out of range, the error is diagnosed by
!         routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine deletes a value from an EK tree at a specified
!     location.  The successor of the value and all higher-indexed
!     values have their indexes decremented.  Since keys are stored in
!     subtree-relative form, the only keys actually modified by the
!     deletion itself are higher-indexed keys in the node from which
!     the deletion is done, and higher-indexed keys in the chain of
!     ancestors of this node.
!
!     The deletion is always done from a leaf node.  If KEY is not in a
!     leaf node, the value corresponding to KEY is swapped with that of
!     an immediate neighbor, and the neighbor is deleted.  This is
!     possible because every key is either in a leaf or has the property
!     that its predecessor and successor are both located in leaf nodes.
!
!     The deletion is not the end of the story, however:  it's possible
!     that the node from which the deletion is done (the `target node')
!     will underflow.  If underflow occurs, this routine will restore
!     the tree to its normal form as follows:
!
!        1)  If a neighbor of the target node contains at least one more
!            key than the minimum allowed number, data will be `rotated'
!            from the neighbor node, through the target's parent,
!            and into the target.  The deletion is complete at this
!            point.
!
!        2)  If the target node has only one neighbor, but that neighbor
!            is neighbor to a sibling that can contribute a key, data
!            will be rotated from the second sibling, through the
!            siblings' parent, into the first sibling, and then from
!            the first sibling through the target's parent, and into
!            the target.  The deletion is complete at this point.
!
!        3)  If the target is not a child of the root, and if
!            the target has two neighbors, but neither neighbor has a
!            key to spare, then the target node and its neighbors will
!            be merged into two nodes:   this is called a `3-2 merge'.
!            The parent node is modified appropriately so that all
!            values are in the proper order and all subtree-relative
!            keys are correct.  This `3-2 merge' decreases the number
!            of values in the parent by one.  If the decrease does not
!            cause an underflow in the parent, the deletion is complete.
!
!            If the target has only one neighbor, and both the neighbor
!            and the neighbor's other neighbor (which always exists)
!            contain the minimum number of keys, these three nodes are
!            combined into two via a 3-2 merge.
!
!        4)  If the parent underflows as a result of a 3-2 merge, the
!            solution process is repeated at the parent's level.  The
!            process iterates until the underflow is resolved or a
!            child of the root underflows.
!
!        5)  If a child of the root underflows, the problem is solved
!            by balancing keys with a neighbor if possible.  Balancing
!            cannot be done only if the root has only two children, and
!            these contain the minimum number of keys.  In this case,
!            the contents of the two children of the root are moved
!            into the root and the children are eliminated.  The
!            children of the child nodes become children of the root.
!            This is the only case in which the tree grows shorter.
!
!            The process of collapsing two child nodes into the root is
!            called a `3-1 merge'.  After a 3-1 merge is performed, the
!            number of values in each node is within bounds.
!
!
!     An EK tree is always balanced after a deletion:  all leaf nodes
!     are at the same level.
!
! $ Examples
!
!     See EKDELR.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Other functions
!
      INTEGER               ZZEKTRNK

!
!     Local parameters
!
      INTEGER               TERM
      PARAMETER           ( TERM   = 1 )

      INTEGER               LCHECK
      PARAMETER           ( LCHECK = TERM   + 1 )

      INTEGER               RCHECK
      PARAMETER           ( RCHECK = LCHECK + 1 )

      INTEGER               BALNCE
      PARAMETER           ( BALNCE = RCHECK + 1 )

      INTEGER               MERG32
      PARAMETER           ( MERG32 = BALNCE + 1 )

      INTEGER               MERG31
      PARAMETER           ( MERG31 = MERG32 + 1 )

      INTEGER               LLCHCK
      PARAMETER           ( LLCHCK = MERG31 + 1 )

      INTEGER               RRCHCK
      PARAMETER           ( RRCHCK = LLCHCK + 1 )

!
!     Local variables
!
      INTEGER               IDX
      INTEGER               LEFT
      INTEGER               LEVEL
      INTEGER               LKEY
      INTEGER               LLKEY
      INTEGER               LLSIB
      INTEGER               LNKEY
      INTEGER               LNODE
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LRKEY
      INTEGER               LRSIB
      INTEGER               LSIB
      INTEGER               MNODE
      INTEGER               NKEYS
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               PTR
      INTEGER               RIGHT
      INTEGER               RKEY
      INTEGER               RLKEY
      INTEGER               RLSIB
      INTEGER               RNODE
      INTEGER               ROOT
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               RRKEY
      INTEGER               RRSIB
      INTEGER               RSIB
      INTEGER               STATE
      INTEGER               TRGKEY
      INTEGER               TRUST

      LOGICAL               UNDRFL


!
!     Use discovery check-in for speed.
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      ROOT  =  TREE

!
!     Work with a local copy of the input key.
!
      LKEY  =  KEY

!
!     The first step is to delete the key from the tree without
!     balancing.  This step may cause a node to underflow.  We'll
!     handle the underflow later.
!
      CALL ZZEKTRUD ( HANDLE, TREE, LKEY, TRGKEY, UNDRFL )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     If the deletion didn't result in an underflow, we're done.
!
      IF ( .NOT. UNDRFL ) THEN
         RETURN
      END IF

!
!     Handle node underflows, as required.  We describe our approach
!     below.  If any step fails, we try the next step.  We proceed
!     until we succeed in resolving the underflow.
!
!        1) If an immediate sibling can contribute a key, balance NODE
!           with that sibling.
!
!        2) If both left and right siblings exist, but neither can
!           contribute a key, execute a 3-2 merge.
!
!        3) If the left sibling has its own left sibling, and if that
!           second left sibling can contribute a key, rotate a key
!           from that sibling into NODE's left sibling.  Then execute
!           (1).
!
!        4) If the left sibling has its own left sibling, and if that
!           second left sibling cannot contribute a key, execute a 3-2
!           merge using NODE as the rightmost child.
!
!        5) Same as (3), except on the right side.
!
!        6) Same as (4), except on the right side.
!
!        7) Arrival at this step implies that NODE is a child of the
!           root and has one sibling.  Execute a 3-1 merge.
!

      STATE = LCHECK


      DO WHILE ( STATE .NE. TERM )


         IF ( STATE .EQ. LCHECK ) THEN
!
!           Look up the node containing the target key TRGKEY.  This
!           is where the underflow occurred; note that this node may
!           be different from the one that contained LKEY.
!
            CALL ZZEKTRLK (  HANDLE,  TREE,    TRGKEY,  IDX, &
                             NODE,    NOFFST,  LEVEL,   PTR  )

!
!           Look up the siblings of NODE.  If either sibling exists
!           and has a surplus of keys, we can remove the underflow
!           by balancing.
!
            CALL ZZEKTRSB (  HANDLE,  TREE,  TRGKEY, &
                             LSIB,    LKEY,  RSIB,    RKEY  )


            IF ( LSIB .GT. 0 ) THEN

               NKEYS = ZZEKTRNK ( HANDLE, TREE, LSIB )

               IF ( NKEYS .GT. MNKEYC ) THEN
!
!                 The left sibling can contribute a key.
!
                  LNKEY  =  LKEY
                  LNODE  =  LSIB
                  RNODE  =  NODE

                  STATE  =  BALNCE


               ELSE IF ( RSIB .GT. 0 ) THEN
!
!                 The left sibling cannot help with balancing, but
!                 the right sibling may be able to.
!
                  STATE  =  RCHECK


               ELSE
!
!                 The right sibling does not exist; the only chance
!                 of balancing will come from the left sibling of
!                 LSIB, if such a sibling exists.
!
                  STATE  =  LLCHCK

               END IF


            ELSE
!
!              There is no left sibling, so there must be a right
!              sibling.  Examine it.
!
               STATE = RCHECK

            END IF


         ELSE IF ( STATE .EQ. RCHECK ) THEN
!
!           See whether there's a node surplus in the right sibling
!           The left sibling has already been checked and found wanting,
!           or wasn't found at all.
!
            NKEYS = ZZEKTRNK ( HANDLE, TREE, RSIB )

            IF ( NKEYS .GT. MNKEYC ) THEN
!
!              The right sibling can contribute a key.
!
               LNKEY  =  TRGKEY
               LNODE  =  NODE
               RNODE  =  RSIB

               STATE  =  BALNCE


            ELSE IF ( LSIB .GT. 0 ) THEN
!
!              NODE has siblings on both sides, and each one contains
!              the minimum number of keys.  Execute a 3-2 merge.
!
               LNKEY   =  LKEY
               LNODE   =  LSIB
               MNODE   =  NODE
               RNODE   =  RSIB

               STATE   =  MERG32

            ELSE
!
!              Look for the right sibling of the right sibling.
!
               STATE  =  RRCHCK

            END IF


         ELSE IF ( STATE .EQ. LLCHCK ) THEN
!
!           See whether the left sibling has its own left sibling.
!
            CALL ZZEKTRSB (  HANDLE,  TREE,   LKEY, &
                             LLSIB,   LLKEY,  LRSIB,  LRKEY  )


            IF ( LLSIB .GT. 0 ) THEN

               NKEYS = ZZEKTRNK ( HANDLE, TREE, LLSIB )


               IF ( NKEYS .GT. MNKEYC ) THEN
!
!                 The left**2 sibling can contribute a key.  Rotate
!                 this key into the left sibling.  We'll need the
!                 parent and index of left parent key of LSIB in order
!                 to do this rotation.
!
                  CALL ZZEKTRPI ( HANDLE,  TREE,    LKEY,   PARENT, &
                                  PKEY,    POFFST,  LPIDX,  LPKEY, &
                                  LLSIB,   RPIDX,   RPKEY,  LRSIB  )

                  CALL ZZEKTRRK ( HANDLE,  TREE,    LLSIB,  LSIB, &
                                  PARENT,  LPIDX,   1              )

!
!                 Now LSIB has a one-key surplus, so we can balance
!                 LSIB and NODE.
!
                  LNKEY  =  LKEY
                  LNODE  =  LSIB
                  RNODE  =  NODE

                  STATE  =  BALNCE


               ELSE
!
!                 The left**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the right node.
!
                  LNKEY   =  LLKEY
                  LNODE   =  LLSIB
                  MNODE   =  LSIB
                  RNODE   =  NODE

                  STATE   =  MERG32

               END IF


            ELSE
!
!              LSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, LSIB and NODE
!              together contain the one less than twice the minimum
!              allowed number of keys.  Execute a 3-1 merge.
!
               LNODE  =  LSIB
               RNODE  =  NODE

               STATE  =  MERG31

            END IF



         ELSE IF ( STATE .EQ. RRCHCK ) THEN
!
!           See whether the right sibling has its own right sibling.
!
            CALL ZZEKTRSB (  HANDLE,  TREE,   RKEY, &
                             RLSIB,   RLKEY,  RRSIB,  RRKEY  )


            IF ( RRSIB .GT. 0 ) THEN

               NKEYS = ZZEKTRNK ( HANDLE, TREE, RRSIB )


               IF ( NKEYS .GT. MNKEYC ) THEN
!
!                 The right**2 sibling can contribute a key.  Rotate
!                 this key into the right sibling.  We'll need the
!                 parent and index of the right parent key of RSIB in
!                 order to do this rotation.
!
                  CALL ZZEKTRPI ( HANDLE,  TREE,    RKEY,   PARENT, &
                                  PKEY,    POFFST,  LPIDX,  LPKEY, &
                                  RLSIB,   RPIDX,   RPKEY,  RRSIB  )

                  CALL ZZEKTRRK ( HANDLE,  TREE,    RSIB,   RRSIB, &
                                  PARENT,  RPIDX,   -1             )

!
!                 Now RSIB has a one-key surplus, so we can balance
!                 RSIB and NODE.
!
                  LNKEY  =  TRGKEY
                  LNODE  =  NODE
                  RNODE  =  RSIB

                  STATE  =  BALNCE


               ELSE
!
!                 The right**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the left node.
!
                  LNKEY   =  TRGKEY
                  LNODE   =  NODE
                  MNODE   =  RSIB
                  RNODE   =  RRSIB

                  STATE   =  MERG32

               END IF


            ELSE
!
!              RSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, RSIB and NODE
!              together contain one less than twice the minimum allowed
!              number of keys.  Execute a 3-1 merge.
!
               LNODE  =  NODE
               RNODE  =  RSIB

               STATE  =  MERG31

            END IF


         ELSE IF ( STATE .EQ. BALNCE ) THEN
!
!           LNODE has a right sibling, and between the two nodes,
!           there are enough keys to accommodate the underflow.  After
!           balancing these nodes, we're done.
!
            CALL ZZEKTRPI ( HANDLE,  TREE,    LNKEY,   PARENT, &
                            PKEY,    POFFST,  LPIDX,   LPKEY, &
                            RLSIB,   RPIDX,   RPKEY,   RRSIB  )

!
!           The common parent of the nodes is PARENT.  The right parent
!           key of the left node is at location RPIDX.  We're ready to
!           balance the nodes.
!
            CALL ZZEKTRBN ( HANDLE, TREE, LNODE, RNODE, PARENT, RPIDX )

            STATE  =  TERM


         ELSE IF ( STATE .EQ. MERG32 ) THEN
!
!           LNODE, MNODE, and RNODE are siblings, and between the three
!           nodes, there's an underflow of one key.  Merge these three
!           nodes into two.  This merging process removes a key from the
!           parent; the parent may underflow as a result.
!
!           After executing the 3-2 merge, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.
!
!           To start with, we'll get a trusted key from the
!           leftmost node LNODE.  The first key of LNODE won't be
!           touched by the merge.
!
            CALL ZZEKTRKI ( HANDLE,  TREE,    LNKEY,   1,   TRUST )

            CALL ZZEKTRPI ( HANDLE,  TREE,    LNKEY,   PARENT, &
                            PKEY,    POFFST,  LPIDX,   LPKEY, &
                            RLSIB,   RPIDX,   RPKEY,   RRSIB  )

!
!           The right parent key of the left node is the left parent
!           key of the middle node.  The index of this key is required
!           by ZZEKTR32.
!
            CALL ZZEKTR32 ( HANDLE,  TREE,    LNODE,  MNODE, &
                            RNODE,   PARENT,  RPIDX,  UNDRFL  )


            IF ( UNDRFL ) THEN
!
!              We'll need to handle underflow in the parent.
!              The parent should be correctly identified by the
!              parent of TRUST.
!
!              Note that a 3-2 merge can't create an underflow in
!              the parent if the parent is the root:  the parent
!              contains at least one key after this merge.
!
               CALL ZZEKTRPI ( HANDLE, TREE,   TRUST,  PARENT, &
                               PKEY,   POFFST, LPIDX,  LPKEY, &
                               LEFT,   RPIDX,  RPKEY,  RIGHT  )

               TRGKEY =  PKEY
               STATE  =  LCHECK

            ELSE
               STATE  =  TERM
            END IF


         ELSE IF ( STATE .EQ. MERG31 ) THEN
!
!           We've got an underflow in the two children of the root.
!           Move all of the keys from these children into the root.
!           The root contains the maximum allowed number of keys
!           after this merge.
!
            CALL ZZEKTR31 ( HANDLE, TREE )
            STATE  =  TERM

         END IF

      END DO

      RETURN
      END
