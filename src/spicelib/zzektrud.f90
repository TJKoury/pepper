!$Procedure      ZZEKTRUD ( EK tree, unbalanced deletion )

      SUBROUTINE ZZEKTRUD ( HANDLE, TREE, KEY, TRGKEY, UNDRFL )

! $ Abstract
!
!     Delete a value from a tree at a specified location without
!     balancing the tree.
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
      INTEGER               TRGKEY
      LOGICAL               UNDRFL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key to delete.
!     TRGKEY     O   Key identifying node from which deletion occurred.
!     UNDRFL     O   Underflow flag.
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
!     TRGKEY         is an absolute key identifying the node from which
!                    the deletion occurred.  This node may be different
!                    from the node that contained KEY before the
!                    deletion; see $Particulars for details.
!
!     UNDRFL         is a logical flag indicating whether the node
!                    at which VALUE was inserted underflowed as a
!                    result.  Child nodes must contain at least
!                    MNKEYC keys; this bound is declared in ektree.inc.
!                    The root node is permitted to become empty.
!
!                    When an underflow condition exists, the tree
!                    violates an invariant.  The underflow must be
!                    resolved before any other insertions or deletions
!                    are performed.
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
!     3)  If the input key is out of range, the error
!         SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine deletes a value from an EK tree at the ordinal
!     position indicated by KEY.  The deletion is always done from a
!     leaf node.  If KEY is not in a leaf node, the value corresponding
!     to KEY is swapped with that of an immediate neighbor, and the
!     neighbor is deleted. This is possible because every key is either
!     in a leaf or has the property that its predecessor and successor
!     are both located in leaf nodes.
!
!     After the deletion, the successor of location from which the
!     deletion actually was done is shifted to the next-lower-indexed
!     position.  The routine updates all affected key counts and key
!     values, both in the target node and all ancestors of the target.
!     Here the target node is the leaf from which the deletion was
!     actually done.
!
!     The caller must balance the tree when underflow occurs.
!
!     Deletion is not quite the opposite of insertion.  Note that the
!     output TRGKEY has no analog in the unbalanced insertion routine
!     ZZEKTRUI.
!
! $ Examples
!
!     See ZZEKTRDL.
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
!-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Fixed calls to CHKIN and CHKOUT so that the same name
!        is used throughout the routine.
!
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               DATPTR
      INTEGER               DEPTH
      INTEGER               I
      INTEGER               KEYIDX
      INTEGER               LEAF
      INTEGER               LEVEL
      INTEGER               LOFFST
      INTEGER               LPAGE  ( PGSIZI )
      INTEGER               LPIDX
      INTEGER               LPIDX2
      INTEGER               LPKEY
      INTEGER               LPKEY2
      INTEGER               LSIB
      INTEGER               LSIB2
      INTEGER               NKEYS
      INTEGER               NLKEYS
      INTEGER               NNODE
      INTEGER               PARENT
      INTEGER               PAREN2
      INTEGER               PKEY
      INTEGER               PKEY2
      INTEGER               POFFS2
      INTEGER               POFFST
      INTEGER               PREV
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               RPIDX
      INTEGER               RPIDX2
      INTEGER               RPKEY
      INTEGER               RPKEY2
      INTEGER               RSIB
      INTEGER               RSIB2
      INTEGER               TARGET
      INTEGER               TNKEYS
      INTEGER               TOFFST
      INTEGER               TOTKEY
      INTEGER               TPAGE  ( PGSIZI )

!
!     Use discovery check-in.
!
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      ROOT  =  TREE

!
!     We always need to update the root page, so read it now.
!
      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )

!
!     The allowed range of keys is 1 to TOTKEY, where TOTKEY is the
!     total number of keys already present.
!
      TOTKEY  =  RPAGE ( TRNKEY )

      IF (  ( KEY .LT. 1 ) .OR. ( KEY .GT. TOTKEY)  ) THEN

         CALL CHKIN  ( 'ZZEKTRUD'                                )
         CALL SETMSG ( 'Key = #. Valid range is 1:#.  File = #.' )
         CALL ERRINT ( '#',     KEY                              )
         CALL ERRINT ( '#',     TOTKEY                           )
         CALL ERRHAN ( '#',     HANDLE                           )
         CALL CHKOUT ( 'ZZEKTRUD'                                )
         RETURN

      END IF

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      NNODE  =  RPAGE ( TRNNOD )
      DEPTH  =  RPAGE ( TRDPTH )

!
!     Find the point at which the deletion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      IF ( NNODE .EQ. 1 ) THEN
!
!        This is the simplest case; all we need do is delete the
!        key from the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data pointer for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         NKEYS             =  TOTKEY
         RPAGE ( TRNKEY )  =  NKEYS  -  1
         RPAGE ( TRNKR  )  =  NKEYS  -  1

!
!        Shift the keys, data pointer, and child pointers to the left
!        of the deleted key.  Update the shifted keys.
!
         DO I = KEY, NKEYS-1
            RPAGE ( TRKEYR+I )  =  RPAGE ( TRKEYR+I+1 ) - 1
            RPAGE ( TRDATR+I )  =  RPAGE ( TRDATR+I+1 )
         END DO

         DO I = KEY, NKEYS
            RPAGE ( TRKIDR+I )  =  RPAGE ( TRKIDR+I+1 )
         END DO

!
!        Zero out the freed entries.
!
         RPAGE ( TRKEYR + NKEYS   )  =  0
         RPAGE ( TRDATR + NKEYS   )  =  0
         RPAGE ( TRKIDR + NKEYS+1 )  =  0

!
!        Update the key count.
!
         NKEYS   =  NKEYS - 1

!
!        Underflow never occurs in the root; the tree simply becomes
!        empty if no keys are left.
!
         UNDRFL  =  .FALSE.

!
!        The first key in the root will serve as the target key,
!        as long as the root isn't empty.
!
         IF ( NKEYS .GT. 0 ) THEN
            TRGKEY  =  RPAGE(TRKEYR+1)
         ELSE
            TRGKEY  =  0
         END IF

!
!        Write the page back out, and we're all set.
!
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )



      ELSE IF ( KEY .EQ. TOTKEY ) THEN
!
!        The deleted key is the last key in the tree.  This case
!        is simple, because no remaining keys change as a result of
!        this deletion.
!
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    KEYIDX, &
                         TARGET,  TOFFST,  LEVEL,  DATPTR  )

         IF ( FAILED() ) THEN
            RETURN
         END IF


         CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )

         NKEYS   =  TPAGE ( TRNKC )

!
!        Zero out the freed entries.
!
         TPAGE ( TRKEYC + NKEYS   )  =  0
         TPAGE ( TRDATC + NKEYS   )  =  0
         TPAGE ( TRKIDC + NKEYS+1 )  =  0

!
!        Update the key count for this node:
!
         TPAGE ( TRNKC  )  =  TPAGE ( TRNKC ) - 1

!
!        Since the key we deleted has no successors, there's no need
!        to adjust any other keys.  We must decrement the total
!        node count in the root, however.
!
         RPAGE ( TRNKEY ) =  TOTKEY - 1

!
!        Underflow occurs when the node started out at the minimum
!        key count.
!
         UNDRFL  =  NKEYS .EQ. MNKEYC

!
!        The first key in the target page is the target key.  Return
!        an absolute key.
!
         TRGKEY  =  TPAGE(TRKEYC+1)  +  TOFFST

!
!        Write the affected pages back out.
!
         CALL ZZEKPGWI ( HANDLE, ROOT,   RPAGE )
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )


      ELSE
!
!        Locate the item we wish to delete.
!
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    KEYIDX, &
                         TARGET,  TOFFST,  LEVEL,  DATPTR )


         IF ( LEVEL .EQ. DEPTH ) THEN
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Deletions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            CALL ZZEKTRPI ( HANDLE, TREE,  KEY,  PARENT, PKEY,  POFFST, &
                            LPIDX,  LPKEY, LSIB, RPIDX,  RPKEY, RSIB   )

            IF ( FAILED() ) THEN
               RETURN
            END IF

!
!           Read the target page.  Get the key count for this node.
!
            CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )

            TNKEYS = TPAGE ( TRNKC )

!
!           Each node is allowed to underflow by 1 element.  If there
!           is already a deficit, OK, that's it.
!
            IF ( TNKEYS .LT. MNKEYC ) THEN

               CALL CHKIN  ( 'ZZEKTRUD'                            )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRHAN ( '#',   HANDLE                         )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZEKTRUD'                            )
               RETURN

            END IF

!
!           Shift the keys, data pointers, and child pointers starting
!           at KEY to the left by 1 position.  Careful, move the
!           leftmost elements first.  Update the shifted key values
!           while we're at it.
!
            DO I = KEYIDX, TNKEYS-1
               TPAGE (TRKEYC+I)  =  TPAGE (TRKEYC+I+1) - 1
            END DO

            DO I = KEYIDX, TNKEYS-1
               TPAGE (TRDATC+I)  =  TPAGE (TRDATC+I+1)
            END DO

            DO I = KEYIDX, TNKEYS
               TPAGE (TRKIDC+I)  =  TPAGE (TRKIDC+I+1)
            END DO

!
!           Update the key count for the target node.
!
            TPAGE ( TRNKC ) =  TNKEYS - 1

!
!           Underflow occurs when the node started out at the minimum
!           count.
!
            UNDRFL  =  TNKEYS .EQ. MNKEYC

!
!           The first key in the target page is the target key.
!
            TRGKEY  =  TPAGE(TRKEYC+1)  +  TOFFST

!
!           Write the target page back out.
!
            CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )


         ELSE
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!           Find this predecessor.
!
            CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  PREV, &
                            LEAF,    LOFFST,  LEVEL,  DATPTR  )

            IF ( FAILED() ) THEN
               RETURN
            END IF

!
!           Since we'll have to update the ancestors of LEAF,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            CALL ZZEKTRPI ( HANDLE, TREE,  KEY-1, PARENT, PKEY,  POFFST, &
                            LPIDX,  LPKEY, LSIB,  RPIDX,  RPKEY, RSIB  )

            IF ( FAILED() ) THEN
               RETURN
            END IF

!
!           Since deletions are allowed only in leaf nodes, we'll
!           perform a little sleight-of-code:  We'll move the key's
!           predecessor into the key's location, then remove the
!           predecessor from its leaf node.  The order of the keys
!           is not disturbed by this re-arrangement.
!
!           Moving the key's predecessor into the key's location is
!           accomplished simply by transferring the data pointer.
!
            CALL ZZEKPGRI ( HANDLE, LEAF, LPAGE )


            IF ( TARGET .EQ. ROOT ) THEN
!
!              The root page has already been read into RPAGE.
!
               RPAGE (TRDATR+KEYIDX)  =  LPAGE (TRDATC+PREV)

            ELSE

               CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )

               TPAGE (TRDATC+KEYIDX)  =  LPAGE (TRDATC+PREV)

            END IF

!
!           The keys and data pointers in the leaf must be shifted
!           left to account for the deletion.  We'll zero out the
!           freed elements.  All child pointers are NIL and hence need
!           not be shifted.
!
            NLKEYS = LPAGE ( TRNKC )

            DO I = PREV, NLKEYS-1
               LPAGE (TRKEYC+I)  =  LPAGE (TRKEYC+I+1) - 1
               LPAGE (TRDATC+I)  =  LPAGE (TRDATC+I+1)
            END DO

!
!           Update the key count for the leaf node.
!
            LPAGE ( TRNKC ) =  NLKEYS - 1

!
!           Underflow occurs when the leaf node started out at the
!           minimum count.
!
            UNDRFL  =  NLKEYS .EQ. MNKEYC

!
!           The first key in the leaf page is the target key.
!
            TRGKEY  =  LPAGE(TRKEYC+1)  +  LOFFST

!
!           Write the leaf, and if necessary, the target page back out.
!
            CALL ZZEKPGWI ( HANDLE, LEAF, LPAGE )

            IF ( TARGET .NE. ROOT ) THEN
               CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )
            END IF

!
!           The next step will be to update the ancestors of LEAF.
!           For the purposes of this operation, LEAF is the target
!           node.
!
            TARGET  =  LEAF

         END IF

!
!        We must update the affected keys in every ancestor of TARGET.
!        We've already looked up information for the parent of
!        TARGET.  See the note at the prior call to ZZEKTRPI.
!
         DO WHILE ( PARENT .NE. ROOT )
!
!           Before going to work on the parent, get *its* parent's info.
!           This is the last chance to do so.
!
            CALL ZZEKTRPI ( HANDLE, TREE,   PKEY,   PAREN2, &
                            PKEY2,  POFFS2, LPIDX2, LPKEY2, &
                            LSIB2,  RPIDX2, RPKEY2, RSIB2  )

!
!           Read the parent node.  All keys from the right parent key
!           onward get decremented.  Remember that there may be no
!           right parent key.
!
            CALL ZZEKPGRI ( HANDLE, PARENT, TPAGE )

            TNKEYS  =  TPAGE ( TRNKC  )


            IF ( RPIDX .GT. 0 ) THEN

               DO I = RPIDX, TNKEYS
                  TPAGE(TRKEYC + I)  =  TPAGE(TRKEYC + I) - 1
               END DO

!
!              Write the updated page back out.
!
               CALL ZZEKPGWI ( HANDLE, PARENT, TPAGE )

            END IF

            PARENT = PAREN2
            PKEY   = PKEY2
            RPIDX  = RPIDX2

         END DO

!
!        Update the keys in the root.  Recall that the root page has
!        already been read into RPAGE.
!
         TNKEYS  =  RPAGE ( TRNKR )


         IF ( RPIDX .GT. 0 ) THEN

            DO I = RPIDX, TNKEYS
               RPAGE(TRKEYR + I)  =  RPAGE(TRKEYR + I) - 1
            END DO

         END IF

!
!        Update the total key count for the tree.
!
         RPAGE ( TRNKEY )  =  TOTKEY - 1

!
!        Write the updated root page back out.
!
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )

      END IF


      RETURN
      END
