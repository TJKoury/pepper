!$Procedure      ZZEKTRIN ( EK tree, insert value )

      SUBROUTINE ZZEKTRIN ( HANDLE, TREE, KEY, VALUE )

! $ Abstract
!
!     Insert a value into an EK tree at a specified location.
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
      INTEGER               VALUE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key at which to insert value.
!     VALUE      I   Value to insert.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key indicating the insertion
!                    location.  In EK trees, absolute keys are just
!                    ordinal positions relative to the leftmost element
!                    of the tree, with the leftmost element having
!                    position 1.  So setting KEY to 10, for example,
!                    indicates that the input VALUE is the 10th item in
!                    the tree.
!
!                    KEY must be in the range 1 : (NKEYS+1), where
!                    NKEYS is the number of keys in the tree prior to
!                    the insertion.
!
!     VALUE          is an integer value to be inserted into the
!                    specified tree at the ordinal position KEY.
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
!     This routine inserts a value into an EK tree at a specified
!     location.  If the location is occupied, the value previously at
!     that location and all higher-indexed values have their indexes
!     incremented.  Since keys are stored in subtree-relative form,
!     the only keys actually modified by the insertion itself are
!     higher-indexed keys in the node into which the insertion is done,
!     and higher-indexed keys in the chain of ancestors of this node.
!
!     The insertion is not the end of the story, however:  it's possible
!     that the node at which the insertion is done (the `target node')
!     will overflow.  If overflow occurs, this routine will restore the
!     tree to its normal form as follows:
!
!        1)  If a neighbor of the target node has room, data will be
!           `rotated' from the target node, through the target's parent,
!            and into the neighbor.  The insertion is complete at this
!            point.
!
!        2)  If no neighbor has room, then the target node and a
!            neighbor are split and recombined into three nodes:  this
!            is called a `2-3 split'.  The parent node is modified
!            appropriately so that all values are in the proper order
!            and all subtree-relative keys are correct.  This 2-3 split
!            increases the number of values in the parent by one.  If
!            the increase does not cause an overflow in the parent, the
!            insertion is complete.
!
!        3)  If the parent overflows as a result of a 2-3 split, the
!            solution process is repeated at the parent's level.  The
!            process iterates until the overflow is resolved or the
!            root overflows.
!
!        4)  If the root overflows, the root is split into two children
!            and a new root node; the new root contains a single value.
!            The children of the old root become children of the two
!            new child nodes of the new root.  This is the only
!            case in which the tree grows taller.
!
!            The process of splitting the root is called a `1-3 split'.
!            After a 1-3 split is performed, the number of values in
!            each node is within bounds.
!
!
!     An EK tree is always balanced after an insertion:  all leaf nodes
!     are at the same level.
!
! $ Examples
!
!     See EKINSR.
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
!-    Beta Version 1.0.0, 01-NOV-1995 (NJB)
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

      INTEGER               SPLT23
      PARAMETER           ( SPLT23 = BALNCE + 1 )

      INTEGER               SPLT13
      PARAMETER           ( SPLT13 = SPLT23 + 1 )


!
!     Local variables
!
      INTEGER               IDX
      INTEGER               LEFT
      INTEGER               LEVEL
      INTEGER               LKEY
      INTEGER               LNODE
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LVAL
      INTEGER               NKEYS
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               NSIZE
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               PKIDX
      INTEGER               POFFST
      INTEGER               RIGHT
      INTEGER               RNODE
      INTEGER               ROOT
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               STATE
      INTEGER               TRUST

      LOGICAL               OVERFL


!
!     Use discovery check-in for speed.
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      ROOT  =  TREE

!
!     Work with local copies of the input key and value.
!
      LKEY  =  KEY
      LVAL  =  VALUE

!
!     The first step is to insert the key into the tree without
!     balancing.  This step may cause a node to overflow.  We'll
!     handle the overflow later.  In general, the probability of
!     overflow is low:  each overflow creates at least one new node,
!     but the ratio of nodes to keys is very small.
!
      CALL ZZEKTRUI ( HANDLE, TREE, LKEY, LVAL, OVERFL )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     If the insertion didn't result in an overflow, we're done.
!
      IF ( .NOT. OVERFL ) THEN
         RETURN
      END IF

!
!     Handle node overflows, as required.
!
      STATE = LCHECK


      DO WHILE ( STATE .NE. TERM )


         IF ( STATE .EQ. LCHECK ) THEN
!
!           Look up the node containing LKEY.
!
            CALL ZZEKTRLK (  HANDLE,  TREE,    LKEY,   IDX, &
                             NODE,    NOFFST,  LEVEL,  LVAL  )

            IF ( NODE .EQ. ROOT ) THEN

               STATE  =  SPLT13

            ELSE
!
!              See if there's room in the left sibling.  Of course,
!              there must be a left sibling in order for there to be
!              room.
!
               CALL ZZEKTRPI ( HANDLE, TREE,   LKEY,   PARENT, &
                               PKEY,   POFFST, LPIDX,  LPKEY, &
                               LEFT,   RPIDX,  RPKEY,  RIGHT  )

               IF ( LEFT .GT. 0 ) THEN

                  NKEYS = ZZEKTRNK ( HANDLE, TREE, LEFT )

                  IF ( NKEYS .LT. MXKEYC ) THEN
                     LNODE  =  LEFT
                     RNODE  =  NODE
                     PKIDX  =  LPIDX
                     STATE  =  BALNCE
                  ELSE
                     STATE  =  RCHECK
                  END IF

               ELSE
                  STATE = RCHECK
               END IF


            END IF


         ELSE IF ( STATE .EQ. RCHECK ) THEN
!
!           See whether there's room in the right sibling, if there
!           is a right sibling.  The left sibling has already been
!           checked and found wanting.
!
            IF ( RIGHT .GT. 0 ) THEN

               NKEYS = ZZEKTRNK ( HANDLE, TREE, RIGHT )

               IF ( NKEYS .LT. MXKEYC ) THEN

                  LNODE  =  NODE
                  RNODE  =  RIGHT
                  PKIDX  =  RPIDX

                  STATE  =  BALNCE

               ELSE

                  LNODE  =  NODE
                  RNODE  =  RIGHT
                  PKIDX  =  RPIDX
                  STATE  =  SPLT23

               END IF

            ELSE
!
!              The left sibling is full, but at least it's there.
!
               LNODE  =  LEFT
               RNODE  =  NODE
               PKIDX  =  LPIDX
               STATE  =  SPLT23

            END IF


         ELSE IF ( STATE .EQ. BALNCE ) THEN
!
!           LNODE has a right sibling, and between the two nodes,
!           there's enough room to accommodate the overflow.  After
!           balancing these nodes, we're done.
!
            CALL ZZEKTRBN ( HANDLE, TREE, LNODE, RNODE, PARENT, PKIDX )

            STATE  =  TERM


         ELSE IF ( STATE .EQ. SPLT23 ) THEN
!
!           LNODE has a right sibling, and between the two nodes,
!           there's an overflow of one key.  Split these two nodes
!           into three.  This splitting process adds a key to the
!           parent; the parent may overflow as a result.
!
!           After executing the 2-3 split, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.  The old key PKEY may not be in the parent any more;
!           this key may have been rotated into the middle node created
!           by the 2-3 split.
!
!           To start with, we'll get a trusted key from the
!           original node NODE.  If NODE got mapped to LNODE,
!           then the first key in NODE will be unchanged by
!           the 2-3 split.  If NODE got mapped to RNODE, then
!           the last key in NODE will be unchanged.
!
            IF ( NODE .EQ. LNODE ) THEN
!
!              Save the first key from NODE.
!
               CALL ZZEKTRKI ( HANDLE, TREE, LKEY, 1, TRUST )

            ELSE
!
!              Save the last key from NODE.
!
               NSIZE  =  ZZEKTRNK ( HANDLE, TREE, NODE )

               CALL ZZEKTRKI ( HANDLE, TREE, LKEY, NSIZE, TRUST )

            END IF


            CALL ZZEKTR23 ( HANDLE, TREE,  LNODE, RNODE, &
                            PARENT, PKIDX, OVERFL       )


            IF ( OVERFL ) THEN

               IF ( PARENT .EQ. ROOT ) THEN

                  STATE   =  SPLT13

               ELSE
!
!                 We'll need to handle overflow in the parent.
!                 The parent should be correctly identified by the
!                 parent of TRUST.
!
                  CALL ZZEKTRPI ( HANDLE, TREE,   TRUST,  PARENT, &
                                  PKEY,   POFFST, LPIDX,  LPKEY, &
                                  LEFT,   RPIDX,  RPKEY,  RIGHT  )

                  LKEY    =  PKEY
                  STATE   =  LCHECK

               END IF


            ELSE
               STATE  =  TERM
            END IF


         ELSE IF ( STATE .EQ. SPLT13 ) THEN
!
!           We've got an overflow in the root.  Split the root,
!           creating two new children.  The root contains a single
!           key after this split.
!
            CALL ZZEKTR13 ( HANDLE, TREE )
            STATE  =  TERM

         END IF

      END DO

      RETURN
      END
