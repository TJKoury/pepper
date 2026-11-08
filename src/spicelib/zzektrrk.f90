!$Procedure      ZZEKTRRK ( EK tree, rotate keys )

      SUBROUTINE ZZEKTRRK ( HANDLE,  TREE,   LEFT,  RIGHT, &
                            PARENT,  PKIDX,  NROT         )

! $ Abstract
!
!     Rotate a specified number of keys from one node, through
!     the parent, into a neighboring sibling node.
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

      INCLUDE 'ektree.inc'
      INCLUDE 'ekpage.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               LEFT
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               PKIDX
      INTEGER               NROT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     LEFT       I   Left node of pair to participate in rotation.
!     RIGHT      I   Right node of pair to participate in rotation.
!     PARENT     I   Parent node of pair to participate in rotation.
!     PKIDX      I   Parent key index.
!     NROT       I   Number of keys to rotate.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     LEFT,
!     RIGHT          are the node numbers of a pair of nodes to
!                    be balanced.  LEFT and RIGHT must be neighboring
!                    subnodes of a common parent.
!
!     PARENT         is the node number of the common parent node of
!                    nodes LEFT, RIGHT.
!
!     PKIDX          is the `parent key index', that is, the
!                    node-relative index of the key in the parent that
!                    sits between PARENT's child node pointers to
!                    nodes LEFT and RIGHT.  The key at location PKIDX
!                    is the immediate successor of the greatest key in
!                    the subnode headed by LEFT.  It is the immediate
!                    predecessor of the least key in the subnode headed
!                    by RIGHT.
!
!     NROT           is the number of keys to rotate.  Positive counts
!                    indicate that keys are to be rotated from node
!                    LEFT to node RIGHT; negative counts indicate
!                    rotation in the reverse direction.
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
!     3)  If either LEFT or RIGHT are actually the root, the error
!         SPICE(BUG) is signaled.
!
!     4)  If LEFT and RIGHT are not neighboring sibling nodes, the
!         error will be diagnosed by routines called by this routine.
!
!     5)  The rotation is not allowed to create an overflow of more
!         than one key in the destination node, not an underflow of
!         more than one key in the source node.  If either restriction
!         is violated, the error SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Insertions into and deletions from EK trees can result in
!     overflows or underflows of keys in nodes affected by these
!     operations.  Many times key count invariants can be restored by
!     moving keys from one node into an adjacent sibling node.  This
!     maneuver is called `balancing' the nodes.  The process of moving
!     keys from one node, through the parent, into a neighboring
!     sibling node is called `rotating' the keys.
!
!     Key rotation affects the parent node of the neighboring children
!     because one key of the parent sits between the children.  This
!     `parent key' gets moved into one of the children as keys are
!     rotated.  If the rotation is to the right, the parent key is the
!     largest key of the rotated set; if the rotation is to the left,
!     the parent key is the least of the rotated set.
!
!     When keys are rotated, their data values move along with them.
!     In general, child pointers move along with keys, but there are
!     some tricky points:
!
!        - The left and right child pointers of the parent key don't
!          get updated; they continue to point to the two children
!          LEFT and RIGHT.
!
!        - On a right rotation, the right child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the first left child pointer of the right sibling.  The left
!          child pointer of this key doesn't get moved at all.
!
!        - On a left rotation, the left child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the last right child pointer of the left sibling.  The right
!          child pointer of this key becomes the left child pointer of
!          the first key of RIGHT.
!
! $ Examples
!
!     See ZZEKTRBN.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               DATBAS
      INTEGER               DPAR
      INTEGER               DROTAT
      INTEGER               DSHIFT
      INTEGER               FUTRPK
      INTEGER               I
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LNKEYS
      INTEGER               LNSIZE
      INTEGER               LPAGE  ( PGSIZI )
      INTEGER               LSIB
      INTEGER               NVOPAR
      INTEGER               PPAGE  ( PGSIZI )
      INTEGER               REMAIN
      INTEGER               RNKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               RSIB
      INTEGER               SCHLEP
      INTEGER               SUBSIZ


!
!     Use discovery check-in for speed.
!
      IF ( NROT .EQ. 0 ) THEN
         RETURN
      END IF


      ROOT  =  TREE

      IF ( ( LEFT .EQ. ROOT ) .OR. ( RIGHT .EQ. ROOT )  ) THEN

         CALL CHKIN  ( 'ZZEKTRRK'                                  )
         CALL SETMSG ( 'Input node is root; only children are '   // &
                       'eligible for key rotation.'                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZEKTRRK'                                  )

      END IF

!
!     Read in the input nodes.
!
      CALL ZZEKPGRI ( HANDLE, LEFT,   LPAGE )
      CALL ZZEKPGRI ( HANDLE, RIGHT,  RPAGE )
      CALL ZZEKPGRI ( HANDLE, PARENT, PPAGE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Set the base index of the parent keys.  This value depends on
!     whether the parent is the root.  Do the same for the pointer
!     bases.
!
      IF ( PARENT .EQ. TREE ) THEN
         KEYBAS = TRKEYR
         DATBAS = TRDATR
         KIDBAS = TRKIDR
      ELSE
         KEYBAS = TRKEYC
         DATBAS = TRDATC
         KIDBAS = TRKIDC
      END IF

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      LSIB  =  PPAGE ( KIDBAS+PKIDX   )
      RSIB  =  PPAGE ( KIDBAS+PKIDX+1 )

      IF (  ( LSIB .NE. LEFT ) .OR. ( RSIB .NE. RIGHT )  ) THEN

         CALL CHKIN  ( 'ZZEKTRRK'                               )
         CALL SETMSG ( 'LEFT, RIGHT, PARENT, and PKIDX are '   // &
                       'inconsistent. LEFT = #; RIGHT = #; '   // &
                       'PARENT = #; PKIDX = #; LSIB derived '  // &
                       'from PARENT = #; RSIB = #.'             )
         CALL ERRINT ( '#',   LEFT                              )
         CALL ERRINT ( '#',   RIGHT                             )
         CALL ERRINT ( '#',   PARENT                            )
         CALL ERRINT ( '#',   PKIDX                             )
         CALL ERRINT ( '#',   LSIB                              )
         CALL ERRINT ( '#',   RSIB                              )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'ZZEKTRRK'                               )
         RETURN

      END IF

!
!     Get the key counts for the left and right nodes.
!
      LNKEYS  =  LPAGE ( TRNKC )
      RNKEYS  =  RPAGE ( TRNKC )

!
!     The requested rotation will not be permitted to cause an
!     underflow of more than one key in the source node, nor an
!     overflow of more than one key in the destination node.
!
      IF ( NROT .GT. 0 ) THEN

         IF (      ( ( LNKEYS - NROT ) .LT. ( MNKEYC - 1 ) ) &
             .OR.  ( ( RNKEYS + NROT ) .GT. ( MXKEYC + 1 ) ) ) THEN

            CALL CHKIN  ( 'ZZEKTRRK'                                   )
            CALL SETMSG ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the right will violate the key count '  // &
                          'bounds of #:#.'                             )
            CALL ERRINT ( '#',  LEFT                                   )
            CALL ERRINT ( '#',  RIGHT                                  )
            CALL ERRINT ( '#',  LNKEYS                                 )
            CALL ERRINT ( '#',  RNKEYS                                 )
            CALL ERRINT ( '#',  NROT                                   )
            CALL ERRINT ( '#',  MNKEYC-1                               )
            CALL ERRINT ( '#',  MXKEYC+1                               )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRRK'                                   )
            RETURN

         END IF


      ELSE IF ( NROT .LT. 0 ) THEN

         IF (      ( ( LNKEYS - NROT ) .GT. ( MXKEYC + 1 ) ) &
             .OR.  ( ( RNKEYS + NROT ) .LT. ( MNKEYC - 1 ) ) ) THEN

            CALL CHKIN  ( 'ZZEKTRRK'                                   )
            CALL SETMSG ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the left will violate the key count '   // &
                          'bounds of #:#.'                             )
            CALL ERRINT ( '#',  LEFT                                   )
            CALL ERRINT ( '#',  RIGHT                                  )
            CALL ERRINT ( '#',  LNKEYS                                 )
            CALL ERRINT ( '#',  RNKEYS                                 )
            CALL ERRINT ( '#',  -NROT                                  )
            CALL ERRINT ( '#',  MNKEYC-1                               )
            CALL ERRINT ( '#',  MXKEYC+1                               )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRRK'                                   )
            RETURN

         END IF

      END IF

!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this later.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      IF ( PKIDX .EQ. 1 ) THEN
         LNSIZE  =  PPAGE ( KEYBAS + 1 ) -  1
      ELSE
         LNSIZE  =  PPAGE(KEYBAS+PKIDX)  -  PPAGE(KEYBAS+PKIDX-1)  -  1
      END IF

!
!     Now, the actions we take depend on whether we must schlep keys
!     to the right or left.
!
      IF ( NROT .GT. 0 ) THEN
!
!        We'll rotate keys to the right.  There are a bunch of numbers
!        to compute first:
!
!           -- The number of keys remaining in the input node:  REMAIN
!
!           -- The size of the subtree headed by the
!              rotated keys:  SUBSIZ
!
!           -- The offset delta to be applied to the rotated
!              keys:  DROTAT
!
!           -- The offset delta to be applied to the keys shifted
!              right in the sibling:  DSHIFT
!
!           -- The new value of the old right parent key,
!              which gets rotated into the sibling:  NVOPAR
!
!           -- The offset delta to apply to the new right parent key,
!              DPAR.  Note that the successors of this key in the
!              parent node remain unchanged.
!
!
         SCHLEP  =  NROT
         REMAIN  =  LNKEYS  -    SCHLEP

!
!        The size of the rotated subtree is the original size of the
!        subtree headed by LEFT, minus the value of the key preceding
!        the rotated subtree.  That key, which resides at location
!        REMAIN + 1, is the future right parent key; this key is also
!        the successor of the subtree left behind.
!
         FUTRPK  =  LPAGE  ( TRKEYC+REMAIN+1 )
         SUBSIZ  =  LNSIZE - FUTRPK

!
!        The rotated set of keys will no longer be preceded by the
!        set of keys of size NEWRPK that they originally followed.
!
         DROTAT  =  - FUTRPK

!
!        The shifted keys in the right sibling get SUBSIZ + 1 new
!        predecessors.
!
         DSHIFT  =   SUBSIZ + 1

!
!        The old right parent key will become the successor of the
!        shifted subtree.  Its value is just one greater than the
!        size of this subtree.
!
         NVOPAR  =   DSHIFT

!
!        The new parent key has DSHIFT fewer predecessors after
!        the rotation.
!
         DPAR    =   - DSHIFT

!
!        It's time for some action.  First of all, shift the keys
!        in the sibling to the right.  Their data pointers and child
!        pointers move along with them.  Update all the keys by
!        applying the shift delta to them.
!
!        Move the rightmost elements of each data component first.
!        Adjust the keys at the same time.  Note that the regions
!        allocated to keys, data pointers, and child pointers occupy
!        non-overlapping addresses, so the order in which we shift
!        these data sets is not important.  Within each data set, we
!        must be careful not to trash occupied addresses.
!
         DO I = RNKEYS, 1, -1
            RPAGE (TRKEYC + I + SCHLEP)  =  RPAGE (TRKEYC + I) + DSHIFT
         END DO

         DO I = RNKEYS, 1, -1
            RPAGE (TRDATC + I + SCHLEP)  =  RPAGE (TRDATC + I)
         END DO

         DO I = RNKEYS+1, 1, -1
            RPAGE (TRKIDC + I + SCHLEP)  =  RPAGE (TRKIDC + I)
         END DO

!
!        `Move' the old parent key to its target destination in the
!        sibling.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         RPAGE ( TRKEYC + SCHLEP )  =  NVOPAR
         RPAGE ( TRDATC + SCHLEP )  =  PPAGE ( DATBAS + PKIDX )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         PPAGE ( DATBAS + PKIDX )  =  LPAGE ( TRDATC + REMAIN  + 1    )
         PPAGE ( KEYBAS + PKIDX )  =  PPAGE ( KEYBAS + PKIDX ) + DPAR

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         DO I = 1, SCHLEP-1
            RPAGE(TRKEYC+I) = LPAGE(TRKEYC+REMAIN+1+I) + DROTAT
         END DO

         CALL MOVEI( LPAGE(TRDATC+REMAIN+2), SCHLEP-1, RPAGE(TRDATC+1) )
         CALL MOVEI( LPAGE(TRKIDC+REMAIN+2), SCHLEP,   RPAGE(TRKIDC+1) )


!
!        Update the key counts in both the input node and sibling.
!
         LPAGE ( TRNKC )  =  LPAGE ( TRNKC )  -  SCHLEP
         RPAGE ( TRNKC )  =  RPAGE ( TRNKC )  +  SCHLEP

!
!        Update the pages in the kernel.
!
         CALL ZZEKPGWI ( HANDLE, PARENT, PPAGE )
         CALL ZZEKPGWI ( HANDLE, LEFT,   LPAGE )
         CALL ZZEKPGWI ( HANDLE, RIGHT,  RPAGE )


      ELSE
!
!        Rotation to the left is almost, but not quite, a mirror image
!        of rotation to the right.
!
         SCHLEP  =  - NROT
         REMAIN  =    RNKEYS  -  SCHLEP

!
!        The size of the rotated subtree is one less than the value of
!        the future parent key.  This key resides at location
!        SCHLEP and is also the predecessor of the subtree
!        left behind.
!
         FUTRPK  =  RPAGE( TRKEYC + SCHLEP )
         SUBSIZ  =  FUTRPK - 1

!
!        The rotated set of keys will be preceded by the keys already
!        present in LEFT, as well as the key moved in from the parent
!        node.
!
         DROTAT  =  LNSIZE + 1

!
!        The shifted keys in the right sibling lose SUBSIZ + 1
!        predecessors.
!
         DSHIFT  =  - ( SUBSIZ + 1 )

!
!        The old parent key will become the successor of the
!        keys already in LEFT; it will be the predecessor of the
!        rotated subtree.
!
         NVOPAR  =   DROTAT

!
!        The new parent key has (-DSHIFT) more predecessors after
!        the rotation.
!
         DPAR    =  - DSHIFT

!
!        It's time for some action.
!
!        `Move' the old parent key to its target destination in the
!        input node.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         LPAGE ( TRKEYC + LNKEYS + 1 )  =  NVOPAR
         LPAGE ( TRDATC + LNKEYS + 1 )  =  PPAGE ( DATBAS + PKIDX )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         PPAGE ( DATBAS + PKIDX )  =  RPAGE ( TRDATC + SCHLEP )
         PPAGE ( KEYBAS + PKIDX )  =  PPAGE ( KEYBAS + PKIDX  ) + DPAR

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         CALL MOVEI( RPAGE(TRKEYC+1), SCHLEP-1, LPAGE(TRKEYC+LNKEYS+2) )
         CALL MOVEI( RPAGE(TRDATC+1), SCHLEP-1, LPAGE(TRDATC+LNKEYS+2) )
         CALL MOVEI( RPAGE(TRKIDC+1), SCHLEP,   LPAGE(TRKIDC+LNKEYS+2) )

         DO I = 1, SCHLEP-1
            LPAGE(TRKEYC+LNKEYS+1+I) = LPAGE(TRKEYC+LNKEYS+1+I) + DROTAT
         END DO

!
!        Shift the remaining elements of the sibling to the left.
!        Their data pointers and child pointers move along with them.
!        Update all the keys by applying the shift delta to them.
!
!        Move the leftmost elements of each data component first.
!        Adjust the keys at the same time.
!
         DO I = 1, REMAIN
            RPAGE (TRKEYC + I)  =  RPAGE (TRKEYC + I + SCHLEP) + DSHIFT
         END DO

         DO I = 1, REMAIN
            RPAGE (TRDATC + I)  =  RPAGE (TRDATC + I + SCHLEP)
         END DO

         DO I = 1, REMAIN+1
            RPAGE (TRKIDC + I)  =  RPAGE (TRKIDC + I + SCHLEP)
         END DO

!
!        Update the key counts in both the input node and sibling.
!
         LPAGE ( TRNKC )  =  LPAGE ( TRNKC )  +  SCHLEP
         RPAGE ( TRNKC )  =  RPAGE ( TRNKC )  -  SCHLEP

!
!        Update the pages in the kernel.
!
         CALL ZZEKPGWI ( HANDLE, PARENT, PPAGE )
         CALL ZZEKPGWI ( HANDLE, LEFT,   LPAGE )
         CALL ZZEKPGWI ( HANDLE, RIGHT,  RPAGE )


      END IF

      RETURN
      END
