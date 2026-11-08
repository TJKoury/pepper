!$Procedure      ZZEKTRPI ( EK tree, parent information )

      SUBROUTINE ZZEKTRPI ( HANDLE, TREE,  KEY,  PARENT, PKEY,  POFFST, &
                            LPIDX,  LPKEY, LSIB, RPIDX,  RPKEY, RSIB   )

! $ Abstract
!
!     Given a key, return general information pertaining to the key's
!     parent node.
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
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               LSIB
      INTEGER               RPIDX
      INTEGER               RPKEY
      INTEGER               RSIB

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key belonging to node of interest.
!     PARENT     O   Parent node of the node containing KEY.
!     PKEY       O   A key in the parent node.
!     POFFST     O   Key offset of the parent node.
!     LPIDX      O   Node-relative index of the left parent key.
!     LPKEY      O   Left parent key.
!     LSIB       O   Node number of left sibling.
!     RPIDX      O   Node-relative index of the right parent key.
!     RPKEY      O   Right parent key.
!     RSIB       O   Node number of right sibling.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node number of the tree of interest.
!
!     NODE           is the node number of interest.
!
! $ Detailed_Output
!
!     PARENT         is the number of the parent node of the node
!                    containing KEY.  If KEY is in the root, PARENT is
!                    set to zero.
!
!     PKEY           is a key in PARENT.  If PARENT is set to zero,
!                    PKEY is set to zero as well.  PKEY is used to
!                    traverse a chain of ancestors towards the to root.
!
!     POFFST         is the key offset of PARENT; this is the offset
!                    that must be added to the node-relative key
!                    values in PARENT to convert them to absolute keys.
!
!     LPIDX          is the index in PARENT of the key `to the left'
!                    of the node containing KEY.  This key is the
!                    immediate predecessor of the first key in the
!                    subtree headed by the node containing KEY.
!
!                    The key indices in PARENT start at 1.  If PARENT
!                    contains no keys that precede the node containing
!                    KEY, LPIDX is set to zero.
!
!     LPKEY          is the absolute key located in PARENT at index
!                    LPIDX.  If PARENT contains no keys that precede the
!                    node containing KEY, LPKEY is set to zero.
!
!     LSIB           is the number of the left sibling node of the node
!                    containing KEY.  If PARENT contains no keys that
!                    precede the node containing KEY, then the node
!                    containing KEY has no left sibling, and LSIB is
!                    set to zero.
!
!     RPIDX          is the index in PARENT of the key `to the right'
!                    of the node containing KEY.  This key is the
!                    immediate successor of the last key in the
!                    subtree headed by the node containing KEY.
!
!                    If PARENT contains no keys that succeed the node
!                    containing KEY, RPIDX is set to zero.
!
!     RPKEY          is the absolute key located in PARENT at index
!                    RPIDX.  If PARENT contains no keys that succeed the
!                    node containing KEY, RPKEY is set to zero.
!
!     RSIB           is the number of the right sibling node of the node
!                    containing KEY.  If PARENT contains no keys that
!                    succeed the node containing KEY, then the node
!                    containing KEY has no right sibling, and RSIB is
!                    set to zero.
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
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
!     3)  If the input key is out of range, the error
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
!     4)  If the input key is not found in the tree, the error
!         SPICE(ITEMNOTFOUND) is signaled.  This error most likely
!         indicates the presence of a serious bug in the EK software,
!         or that the input EK file has been corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine supports tree operations that involve identifying
!     the parent node of a specified node.  In particular, this
!     routine supports updating ancestors of a node when an insertion
!     or deletion occurs.
!
! $ Examples
!
!     See ZZEKTRUD, ZZEKTRUI.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LSTLEI

!
!     Local variables
!
      INTEGER               CHILD
      INTEGER               LKEY
      INTEGER               MAXKEY
      INTEGER               NEWKEY
      INTEGER               OFFSET
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PREV
      INTEGER               PRVKEY
      INTEGER               TOTKEY

      LOGICAL               FOUND

!
!     Use discovery check-in in this puppy.
!
!     Nothing found to begin with.
!
      FOUND  =  .FALSE.

!
!     Get a local copy of the input key.  We may overwrite the input
!     key when we set PKEY.
!
      LKEY   =   KEY

!
!     Start out by reading in the root page.  The node level starts
!     out at 1.
!
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )

      PARENT  =  0
      PKEY    =  0
      POFFST  =  0
      LPIDX   =  0
      LPKEY   =  0
      LSIB    =  0
      RPIDX   =  0
      RPKEY   =  0
      RSIB    =  0

!
!     Find out how many keys are in the tree.  If LKEY is outside
!     this range, we won't find it.
!
      TOTKEY  =  PAGE ( TRNKEY )

      IF (  ( LKEY .LT. 1 )  .OR.  ( LKEY .GT. TOTKEY )  ) THEN

         CALL CHKIN  ( 'ZZEKTRPI'                                      )
         CALL SETMSG ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         CALL ERRINT ( '#',     LKEY                                   )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRHAN ( '#',     HANDLE                                 )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'ZZEKTRPI'                                      )
         RETURN

      END IF

!
!     Find the last key at this level that is less than or equal to
!     the requested key.
!
      PREV   =  LSTLEI (  LKEY,  PAGE(TRNKR),  PAGE(TRKEYR+1)  )

      IF ( PREV .GT. 0 ) THEN
         PRVKEY =  PAGE(TRKEYR+PREV)
      ELSE
         PRVKEY =  0
      END IF

!
!     If we were lucky enough to get an exact match, we can quit now.
!     The root has no parent so the output values remain set to zero.
!
      IF ( PRVKEY .EQ. LKEY ) THEN
         RETURN
      END IF

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      OFFSET  =  PRVKEY
      PARENT  =  TREE
      PKEY    =  PAGE ( TRKEYR + 1 )
      MAXKEY  =  PAGE ( TRNKR      )

      IF ( PREV .GT. 0 ) THEN
         LPIDX  =  PREV
         LPKEY  =  PAGE ( TRKEYR + LPIDX )
         LSIB   =  PAGE ( TRKIDR + LPIDX )
      ELSE
         LPIDX  =  0
         LPKEY  =  0
         LSIB   =  0
      END IF

      IF ( PREV .LT. MAXKEY ) THEN
         RPIDX  =  PREV + 1
         RPKEY  =  PAGE ( TRKEYR + RPIDX     )
         RSIB   =  PAGE ( TRKIDR + RPIDX + 1 )
      ELSE
         RPIDX  =  0
         RPKEY  =  0
         RSIB   =  0
      END IF


      CHILD  =  PAGE ( TRKIDR + PREV + 1 )
      FOUND  =  .FALSE.

      DO WHILE (  ( CHILD .GT. 0 ) .AND. ( .NOT. FOUND )  )
!
!        Read in the child page.
!
         CALL ZZEKPGRI ( HANDLE, CHILD, PAGE )

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from LKEY the position of the
!        node preceding the first key of this subtree.
!
         NEWKEY  =  LKEY - OFFSET
         PREV    =  LSTLEI (  NEWKEY,  PAGE(TRNKC),  PAGE(TRKEYC+1)  )

         IF ( PREV .GT. 0 ) THEN
            PRVKEY =  PAGE(TRKEYC+PREV)
         ELSE
            PRVKEY =  0
         END IF

!
!        If we were lucky enough to get an exact match, we can quit.
!        The outputs are set.
!
         IF ( PRVKEY .EQ. NEWKEY ) THEN

            FOUND  =  .TRUE.

         ELSE
!
!           Record information from the current node before we read the
!           next child page.
!
            PARENT  =  CHILD
            POFFST  =  OFFSET
            PKEY    =  PAGE ( TRKEYC + 1 )  +  OFFSET
            MAXKEY  =  PAGE ( TRNKC      )

            IF ( PREV .GT. 0 ) THEN
               LPIDX  =  PREV
               LPKEY  =  PAGE ( TRKEYC + LPIDX )
               LSIB   =  PAGE ( TRKIDC + LPIDX )
            ELSE
               LPIDX  =  0
               LPKEY  =  0
               LSIB   =  0
            END IF

            IF ( PREV .LT. MAXKEY ) THEN
               RPIDX  =  PREV + 1
               RPKEY  =  PAGE ( TRKEYC + RPIDX     )
               RSIB   =  PAGE ( TRKIDC + RPIDX + 1 )
            ELSE
               RPIDX  =  0
               RPKEY  =  0
               RSIB   =  0
            END IF

!
!           Update the offset of the tree headed by CHILD, and set
!           the new child node.
!
            OFFSET  =  PRVKEY + OFFSET
            CHILD   =  PAGE ( TRKIDC + PREV + 1 )

         END IF


      END DO

!
!     If we found the key, our outputs are already set.  If not, we've
!     got trouble.
!
      IF ( .NOT. FOUND ) THEN

         CALL CHKIN  ( 'ZZEKTRPI'                                      )
         CALL SETMSG ( 'Key #; valid range = 1:#. Tree = #, file = #.'// &
                       '  Key was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         CALL ERRINT ( '#',     LKEY                                   )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRHAN ( '#',     HANDLE                                 )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                           )
         CALL CHKOUT ( 'ZZEKTRPI'                                      )
         RETURN

      END IF

      RETURN
      END
