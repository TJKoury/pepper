!$Procedure      ZZEKTRUI ( EK tree, unbalanced insertion )

      SUBROUTINE ZZEKTRUI ( HANDLE, TREE, KEY, VALUE, OVERFL )

! $ Abstract
!
!     Insert a value into a tree at a specified location without
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
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               VALUE
      LOGICAL               OVERFL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key to insert.
!     VALUE      I   Value to insert.
!     OVERFL     O   Overflow flag.
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
!     OVERFL         is a logical flag indicating whether the node
!                    at which VALUE was inserted overflowed as a result.
!                    Nodes contain extra space to temporarily
!                    accommodate an overflow of one value.
!
!                    When an overflow condition exists, the tree
!                    violates an invariant.  The overflow must be
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
!     4)  If the attempted insertion causes overflow in the target node
!         by more than 1 key, the error SPICE(NODETOOFULL) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine inserts a new value into an EK tree at the ordinal
!     position indicated by KEY.  The insertion is always done in a
!     leaf node.  This is possible because every key is either in a
!     leaf or has the property that its predecessor and successor are
!     both located in leaf nodes.
!
!     If the inserted value is not appended to the tree, the value
!     previously at location KEY is shifted to the next-higher-indexed
!     position.  The routine updates all affected key counts and key
!     values, both in the target node and all ancestors of the target.
!
!     The caller must balance the tree when overflow occurs.
!
! $ Examples
!
!     See ZZEKTRIN.
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
!        Removed redundant calls to CHKIN.
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
      INTEGER               DATLOC
      INTEGER               DATPTR
      INTEGER               DEPTH
      INTEGER               I
      INTEGER               IDX
      INTEGER               KEYLOC
      INTEGER               KIDLOC
      INTEGER               LEVEL
      INTEGER               LPIDX
      INTEGER               LPIDX2
      INTEGER               LPKEY
      INTEGER               LPKEY2
      INTEGER               LSIB
      INTEGER               LSIB2
      INTEGER               NEXT
      INTEGER               NKEYS
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
!     The allowed range of keys is 1 to (TOTKEY+1), where TOTKEY is the
!     total number of keys already present.
!
      TOTKEY  =  RPAGE ( TRNKEY )

      IF (  ( KEY .LT. 1 ) .OR. ( KEY .GT. TOTKEY+1 )  ) THEN

         CALL CHKIN  ( 'ZZEKTRUI'                                )
         CALL SETMSG ( 'Key = #. Valid range is 1:#.  File = #.' )
         CALL ERRINT ( '#',     KEY                              )
         CALL ERRINT ( '#',     TOTKEY+1                         )
         CALL ERRHAN ( '#',     HANDLE                           )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
         CALL CHKOUT ( 'ZZEKTRUI'                                )
         RETURN

      END IF

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      NNODE  =  RPAGE ( TRNNOD )
      DEPTH  =  RPAGE ( TRDPTH )

!
!     Find the point at which the insertion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      IF ( NNODE .EQ. 1 ) THEN
!
!        This is the simplest case; all we need do is set up the
!        key in the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data value for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         NKEYS             =  TOTKEY
         RPAGE ( TRNKEY )  =  NKEYS  +  1
         RPAGE ( TRNKR  )  =  NKEYS  +  1

!
!        Shift the keys, data value, and child pointers to the right
!        of the new key.  Update the shifted keys.
!
         DO I = NKEYS, KEY, - 1
            RPAGE ( TRKEYR+I+1 ) = RPAGE ( TRKEYR + I ) + 1
            RPAGE ( TRDATR+I+1 ) = RPAGE ( TRDATR + I )
         END DO

         DO I = NKEYS+1, KEY, - 1
            RPAGE ( TRKIDR+I+1 ) = RPAGE ( TRKIDR + I )
         END DO

         RPAGE ( TRKEYR + KEY )  =  KEY
         RPAGE ( TRDATR + KEY )  =  VALUE
         RPAGE ( TRKIDR + KEY )  =  0

!
!        Update the key count.
!
         NKEYS   =  NKEYS + 1

!
!        The node into which the key was inserted was the root.
!
         TARGET  =  ROOT

!
!        Overflow occurs when the root started out full.
!
         OVERFL  =  NKEYS .EQ. MXKEYR+1

!
!        Write the page back out, and we're all set.
!
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )



      ELSE IF ( KEY .EQ. TOTKEY+1 ) THEN
!
!        The new key will be the last key in the tree.  This case
!        is simple:  the key goes in the last node of the tree.
!        Since every child node contains more than one key, we can
!        find the node by looking up the last key already present.
!
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  IDX, &
                         TARGET,  TOFFST,  LEVEL,  DATPTR  )

         IF ( FAILED() ) THEN
            RETURN
         END IF

         CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )

         NKEYS   =  TPAGE ( TRNKC )

         KEYLOC  =  TRKEYC  +  NKEYS  +  1
         DATLOC  =  TRDATC  +  NKEYS  +  1
         KIDLOC  =  TRKIDC  +  NKEYS  +  1

!
!        The last node in the tree is always at the lowest level,
!        so the relative value of the new key can be computed from
!        that of its predecessor.
!
         TPAGE ( KEYLOC )  =  TPAGE ( KEYLOC-1 )  +  1
         TPAGE ( DATLOC )  =  VALUE
         TPAGE ( KIDLOC )  =  0

!
!        Update the key count for this node:
!
         TPAGE ( TRNKC  )  =  TPAGE ( TRNKC ) + 1

!
!        Since the key we inserted has no successors, there's no need
!        to adjust any other keys.  We must increment the total
!        node count in the root, however.
!
         RPAGE ( TRNKEY ) =  TOTKEY + 1

!
!        Overflow occurs when the node started out full.
!
         OVERFL  =  NKEYS .EQ. MXKEYC

!
!        Write the affected pages back out.
!
         CALL ZZEKPGWI ( HANDLE, ROOT,   RPAGE )
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )


      ELSE
!
!        The item we wish to insert will displace the item whose
!        ordinal position is KEY.  Locate this target item.
!
         CALL ZZEKTRLK ( HANDLE,  TREE,    KEY,    NEXT, &
                         TARGET,  TOFFST,  LEVEL,  DATPTR  )


         IF ( LEVEL .EQ. DEPTH ) THEN
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Insertions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
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
!           Each node is allowed to overflow by 1 element.  If there's
!           no more room, OK, that's it.
!
            IF ( TNKEYS .GT. MXKEYC ) THEN

               CALL CHKIN  ( 'ZZEKTRUI'                            )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRHAN ( '#',   HANDLE                         )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(NODETOOFULL)'                  )
               CALL CHKOUT ( 'ZZEKTRUI'                            )
               RETURN

            END IF

!
!           Shift the keys, data values, and child pointers starting
!           at NEXT over to the right by 1 position.  Careful, move the
!           rightmost elements first.  Update the shifted key values
!           while we're at it.
!
            DO I = TNKEYS, NEXT, -1
               TPAGE (TRKEYC + I + 1)  =  TPAGE (TRKEYC + I)  +  1
            END DO

            DO I = TNKEYS, NEXT, -1
               TPAGE (TRDATC + I + 1)  =  TPAGE (TRDATC + I)
            END DO

            DO I = TNKEYS+1, NEXT, -1
               TPAGE (TRKIDC + I + 1)  =  TPAGE (TRKIDC + I)
            END DO

!
!           The new key simply takes the value of the old one.  The
!           corresponding data value must be set, however.
!
            TPAGE ( TRDATC + NEXT ) = VALUE


         ELSE
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!
            CALL ZZEKTRLK ( HANDLE,  TREE,    KEY-1,  PREV, &
                            TARGET,  TOFFST,  LEVEL,  DATPTR  )

            IF ( FAILED() ) THEN
               RETURN
            END IF

!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
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
!           The predecessor of KEY will be the last key present in the
!           node TARGET.  Make sure there's room in the node.
!
            CALL ZZEKPGRI ( HANDLE, TARGET, TPAGE )

            TNKEYS = TPAGE ( TRNKC )


            IF ( TNKEYS .GT. MXKEYC + 1 ) THEN

               CALL CHKIN  ( 'ZZEKTRUI'                            )
               CALL SETMSG ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               CALL ERRINT ( '#',   TARGET                         )
               CALL ERRINT ( '#',   TREE                           )
               CALL ERRHAN ( '#',   HANDLE                         )
               CALL ERRINT ( '#',   TNKEYS                         )
               CALL ERRINT ( '#',   MXKEYC + 1                     )
               CALL SIGERR ( 'SPICE(NODETOOFULL)'                  )
               CALL CHKOUT ( 'ZZEKTRUI'                            )
               RETURN

            END IF

!
!           Set the new key and the corresponding data and child
!           pointers.
!
            TPAGE ( TRKEYC + PREV + 1 )   =   PREV + 1
            TPAGE ( TRDATC + PREV + 1   ) =   VALUE
            TPAGE ( TRKIDC + PREV + 2   ) =   0

         END IF

!
!        Update the key count for the target node.
!
         TPAGE ( TRNKC ) =  TNKEYS + 1

!
!        Overflow occurs when the node started out full.
!
         OVERFL  =  TNKEYS .EQ. MXKEYC

!
!        Write the target page back out.
!
         CALL ZZEKPGWI ( HANDLE, TARGET, TPAGE )

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
!           onward get incremented.  Remember that there may be no
!           right parent key.
!
            CALL ZZEKPGRI ( HANDLE, PARENT, TPAGE )

            TNKEYS  =  TPAGE ( TRNKC  )


            IF ( RPIDX .GT. 0 ) THEN

               DO I = RPIDX, TNKEYS
                  TPAGE(TRKEYC + I)  =  TPAGE(TRKEYC + I) + 1
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
               RPAGE(TRKEYR + I)  =  RPAGE(TRKEYR + I) + 1
            END DO

         END IF

!
!        Update the total key count for the tree.
!
         RPAGE ( TRNKEY )  =  TOTKEY + 1

!
!        Write the updated root page back out.
!
         CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )

      END IF


      RETURN
      END
