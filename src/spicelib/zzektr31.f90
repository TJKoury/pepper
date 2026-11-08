!$Procedure      ZZEKTR31 ( EK tree, 3-1 merge )

      SUBROUTINE ZZEKTR31 ( HANDLE, TREE )

! $ Abstract
!
!     Execute a 3-1 merge:  move the contents of two children into
!     the root node and delete the children.
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

      INCLUDE 'ektree.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
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
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
!     3)  If there is not exactly 1 key in the root at the time this
!         routine is called, the error SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Deletions from an EK tree start at a leaf node.  If the node
!     underflows, the EK system attempts to shuffle keys at the leaf
!     level to resolve the underflow.  That attempt failing, the system
!     delegates the problem upward to the next higher level.  Underflow
!     may occur there as well; if it does, the problem gets passed
!     upward again.  If the root has only two children and one of these
!     underflows, the system reduces the height of the tree by
!     executing what's called a `3-1' merge:  the root loses its two
!     children, and all of the keys in the children are moved into
!     the root.  The former grandchildren of the root become
!     children of the root.
!
!     A tree is eligible for a 3-1 merge only if the root has exactly
!     two children, and the sum of the key counts of the children
!     constitutes an underflow of 1 key:  that is, the sum is
!
!        2*MNKEYC - 1
!
!     After the 3-1 merge, the tree is balanced and all invariants
!     relating to key counts are restored.
!
!     The tree grows shorter by one level as a result of a 3-1 merge;
!     this is the only circumstance under which the tree grows shorter.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 3-1 merge:
!
!
!        - The keys of the left child are moved into the root.  These
!          become the leftmost MNKEYC or MNKEYC-1 keys of the root,
!          depending on whether the underflow occurred in the left
!          child.
!
!        - The data values associated with the keys of the left child
!          of the root are moved into the root along with the keys.
!
!        - The left child pointers associated with the keys of the left
!          child of the root are moved into the root along with the
!          keys.
!
!        - The last right child pointer in the left child of the root
!          the root is moved to location NLEFT+1 in the child pointer
!          array of the root, where NLEFT is the number of keys in
!          the former left child.  This pointer overwrites the root's
!          pointer to the left child.
!
!        - The keys of the right child are moved into the root.  These
!          become the rightmost MNKEYC or MNKEYC-1 keys of the root,
!          depending on whether the underflow occurred in the right
!          child.
!
!        - The data values associated with the keys of the right child
!          of the root are moved into the root along with the keys.
!
!        - The left child pointers associated with the keys of the right
!          child of the root are moved into the root along with the
!          keys.  The first of these pointers overwrites the root's
!          pointer to the right child.
!
!        - The last right child pointer in the right child of the root
!          the root is moved to location 2*MNKEYC+1 in the child pointer
!          array of the root.
!
!        - The former children of the root are deleted.
!
!     As the above list shows, the root contains the maximum allowed
!     number of keys after a 3-1 merge.  This is because
!
!        MXKEYR  =  MXKIDR - 1
!
!                =  2 * ( (2*MXKIDC - 2)/3 )
!
!                =  2 * ( (2*MXKIDC + 1)/3  - 1 )
!
!                =  2 * ( MNKIDC - 1)
!
!                =  2 * MNKEYC
!
!     Our assumptions were that there was one key in the root and
!     that the sum of the key counts of the two children of the root
!     was
!
!        ( 2 * MNKEYC ) - 1
!
!     Thus the size constraints on the root node are met.
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
!     None.
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
!-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               CHILD  ( 2 )
      INTEGER               DELTA
      INTEGER               I
      INTEGER               MIDDLE
      INTEGER               NLKEYS
      INTEGER               NRKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )
      INTEGER               SUM

!
!     Use discovery check-in for speed.
!
      ROOT    =  TREE

      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )

      NRKEYS  =  RPAGE ( TRNKR )

!
!     There must be exactly 1 key in the root.
!
      IF ( NRKEYS .NE. 1 ) THEN

         CALL CHKIN  ( 'ZZEKTR31'                                 )
         CALL SETMSG ( 'Number of keys in root = #; should be 1.' )
         CALL ERRINT ( '#',  NRKEYS                               )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKTR31'                                 )
         RETURN

      END IF

!
!     Read in the child pages.  Get the key counts for these pages.
!
      CHILD(1)  =  RPAGE ( TRKIDR + 1 )
      CHILD(2)  =  RPAGE ( TRKIDR + 2 )

      CALL ZZEKPGRI ( HANDLE, CHILD(1), C1PAGE )
      CALL ZZEKPGRI ( HANDLE, CHILD(2), C2PAGE )

      NLKEYS    =  C1PAGE ( TRNKC )
      NRKEYS    =  C2PAGE ( TRNKC )
      SUM       =  NLKEYS + NRKEYS

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to value representing an underflow level of 1 key.
!
      IF (  SUM  .NE.  ( 2*MNKEYC - 1 )  ) THEN

         CALL CHKIN  ( 'ZZEKTR31'                                      )
         CALL SETMSG ( 'Number of keys in nodes LEFT = #; in RIGHT ' // &
                       '= #; counts summing to # were expected.'       )
         CALL ERRINT ( '#',  NLKEYS                                    )
         CALL ERRINT ( '#',  NRKEYS                                    )
         CALL ERRINT ( '#',  2*MNKEYC-1                                )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTR31'                                      )
         RETURN

      END IF

!
!     Shift the key and data pointer in the root to right to allow
!     insertion of NLKEYS new entries on the left.  The child pointers
!     need not be shifted; they'll be overwritten later.
!
      RPAGE (TRKEYR+NLKEYS+1)  =  RPAGE (TRKEYR+1)
      RPAGE (TRDATR+NLKEYS+1)  =  RPAGE (TRDATR+1)

!
!     Copy in the keys, data pointers, and child pointers from the
!     left child into the root.  The number of predecessors of the
!     new keys is unchanged by this operation.
!
      CALL MOVEI ( C1PAGE(TRKEYC+1), NLKEYS,     RPAGE(TRKEYR+1)  )
      CALL MOVEI ( C1PAGE(TRDATC+1), NLKEYS,     RPAGE(TRDATR+1)  )
      CALL MOVEI ( C1PAGE(TRKIDC+1), NLKEYS+1,   RPAGE(TRKIDR+1)  )

!
!     Copy in the keys, data pointers, and child pointers from the
!     right child into the root.  The number of predecessors of the
!     new keys is increased by the value of the last key already
!     present.
!
      MIDDLE  =  NLKEYS  +  1
      DELTA   =  RPAGE( TRKEYR + MIDDLE )

      DO I = 1, NRKEYS
         RPAGE( TRKEYR+MIDDLE+I )  =  C2PAGE(TRKEYC + I)  +  DELTA
      END DO

      CALL MOVEI ( C2PAGE(TRDATC+1), NRKEYS,    RPAGE(TRDATR+MIDDLE+1) )
      CALL MOVEI ( C2PAGE(TRKIDC+1), NRKEYS+1,  RPAGE(TRKIDR+MIDDLE+1) )

!
!     Now the root must be updated.  The root now contains
!     the maximum allowed number of keys.  The depth of the tree
!     has decreased, as well as the number of nodes in the tree.
!
      RPAGE ( TRNKR      )  =  MXKEYR
      RPAGE ( TRDPTH     )  =  RPAGE ( TRDPTH ) - 1
      RPAGE ( TRNNOD     )  =  RPAGE ( TRNNOD ) - 2

!
!     Write out the updated root.
!
      CALL ZZEKPGWI ( HANDLE, ROOT, RPAGE )

!
!     Free the pages occupied by the deleted children.
!
      DO I = 1, 2
         CALL ZZEKPGFR ( HANDLE, INT, CHILD(I)  )
      END DO

      RETURN
      END
