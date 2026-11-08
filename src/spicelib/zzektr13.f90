!$Procedure      ZZEKTR13 ( EK tree, 1-3 split )

      SUBROUTINE ZZEKTR13 ( HANDLE, TREE )

! $ Abstract
!
!     Execute a 1-3 split:  split the root node to create two new
!     children, leaving a single key in the root.
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
!     3)  If the number of keys in the root does not correspond to an
!         overflow of exactly 1 key, the error SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Insertions into an EK tree start at a leaf node.  If the node
!     overflows, the EK system attempts to shuffle keys at the leaf
!     level to resolve the overflow.  That attempt failing, the system
!     delegates the problem upward to the next higher level.  Overflow
!     may occur there as well; if it does, the problem gets passed
!     upward again.  If the root overflows, the system makes room by
!     executing what's called a `1-3' split:  the root gets two new
!     children, and all but one of the keys in the root are moved into
!     the new children.  The former children of the root become
!     children of the two new children of the root.
!
!     After the 1-3 split, the tree is balanced and all invariants
!     relating to key counts are restored.
!
!     The tree grows taller by one level as a result of a 1-3 split;
!     this is the only circumstance under which the tree grows taller.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 1-3 split:
!
!        - The leftmost MNKEYC keys of the root are moved into the
!          new left child.
!
!        - The data values associated with the first MNKEYC keys of the
!          root are moved along with the keys.
!
!        - The left child pointers associated with the first MNKEYC keys
!          of the root are moved along with the keys.
!
!        - The right child pointer of the key at location MNKEYC+1 in
!          the root is moved to location MYKEYC+1 in the child pointer
!          array of the left child.
!
!        - The rightmost MNKEYC keys of the root are moved into the
!          new right child.
!
!        - The data values associated with the last MNKEYC keys of the
!          root are moved along with the keys.
!
!        - The left child pointers associated with the last MNKEYC keys
!          of the root are moved along with the keys.
!
!        - The right child pointer of the last in the root is moved to
!          location MYKEYC+1 in the child pointer array of the right
!          child.
!
!        - The left child pointer of the one key left in the root
!          points to the new left child.
!
!        - The right child pointer of the one key left in the root
!          points to the new right child.
!
!     As the above list shows, each of the new children of the root
!     contains the minimum allowed number of keys that a child node
!     may have.  Thus the size constraints on child nodes are met.
!     The root must be non-empty unless the tree is empty; this
!     condition is also met.
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
!-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      INTEGER               BASE
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               CHILD  ( 2 )
      INTEGER               DELTA
      INTEGER               I
      INTEGER               MIDDLE
      INTEGER               NRKEYS
      INTEGER               ROOT
      INTEGER               RPAGE  ( PGSIZI )

!
!     Use discovery check-in for speed.
!
      ROOT    =  TREE

      CALL ZZEKPGRI ( HANDLE, ROOT, RPAGE )

      NRKEYS  =  RPAGE ( TRNKR )

!
!     The number of keys in the root must correspond exactly to an
!     overflow level of 1 key.
!
      IF (  NRKEYS  .NE.  ( MXKEYR + 1 )  ) THEN

         CALL CHKIN  ( 'ZZEKTR13'                                 )
         CALL SETMSG ( 'Number of keys in root = #; should be #.' )
         CALL ERRINT ( '#',  NRKEYS                               )
         CALL ERRINT ( '#',  MXKEYR+1                             )
         CALL SIGERR ( 'SPICE(BUG)'                               )
         CALL CHKOUT ( 'ZZEKTR13'                                 )
         RETURN

      END IF

!
!     Allocate two new pages; these will become children of the root.
!     Each one will be assigned MNKEYC keys.
!
      DO I = 1, 2
         CALL ZZEKPGAL ( HANDLE, INT, CHILD(I), BASE )
      END DO

!
!     Set the key count in the first child.
!
      CALL CLEARI ( PGSIZI, C1PAGE )

      C1PAGE ( TRNKC )  =  MNKEYC

!
!     Copy in the keys, data pointers, and child pointers from the
!     first MNKEYC locations in the root.  Also take the left child
!     pointer of the middle key.
!
      CALL MOVEI ( RPAGE(TRKEYR+1), MNKEYC,    C1PAGE(TRKEYC+1) )
      CALL MOVEI ( RPAGE(TRDATR+1), MNKEYC,    C1PAGE(TRDATC+1) )
      CALL MOVEI ( RPAGE(TRKIDR+1), MNKEYC+1,  C1PAGE(TRKIDC+1) )

!
!     Set up the key count in the second child.
!
      CALL CLEARI ( PGSIZI, C2PAGE )

      C2PAGE ( TRNKC  )  =  MNKEYC

!
!     Copy in the keys, data pointers, and child pointers from the
!     last MNKEYC locations in the root.  Also take the last right
!     child pointer.
!
      MIDDLE = MNKEYC + 1

      CALL MOVEI ( RPAGE(TRKEYR+MIDDLE+1), MNKEYC,    C2PAGE(TRKEYC+1) )
      CALL MOVEI ( RPAGE(TRDATR+MIDDLE+1), MNKEYC,    C2PAGE(TRDATC+1) )
      CALL MOVEI ( RPAGE(TRKIDR+MIDDLE+1), MNKEYC+1,  C2PAGE(TRKIDC+1) )

!
!     The keys in this second node must be adjusted to account for the
!     loss of the predecessors assigned to the subtree headed by the
!     left child, as well as of the middle key.
!
      DELTA  =  RPAGE ( TRKEYR + MIDDLE )

      DO I = 1, MNKEYC
         C2PAGE( TRKEYC + I )  =  C2PAGE( TRKEYC + I ) - DELTA
      END DO

!
!     Now the root must be updated.  The root now contains just 1
!     key; that key should be shifted left to the first key location.
!     There are two child pointers; these point to the children just
!     created.  The depth of the tree has increased, as well as the
!     number of nodes in the tree.
!
      RPAGE ( TRKEYR + 1 )  =  RPAGE ( TRKEYR + MIDDLE )
      RPAGE ( TRDATR + 1 )  =  RPAGE ( TRDATR + MIDDLE )
      RPAGE ( TRKIDR + 1 )  =  CHILD(1)
      RPAGE ( TRKIDR + 2 )  =  CHILD(2)
      RPAGE ( TRNKR      )  =  1
      RPAGE ( TRDPTH     )  =  RPAGE ( TRDPTH ) + 1
      RPAGE ( TRNNOD     )  =  RPAGE ( TRNNOD ) + 2

      CALL CLEARI ( MXKEYR, RPAGE(TRKEYR+2) )
      CALL CLEARI ( MXKEYR, RPAGE(TRDATR+2) )
      CALL CLEARI ( MXKEYR, RPAGE(TRKIDR+3) )

!
!     Write out our updates.
!
      CALL ZZEKPGWI ( HANDLE,  ROOT,      RPAGE  )
      CALL ZZEKPGWI ( HANDLE,  CHILD(1),  C1PAGE )
      CALL ZZEKPGWI ( HANDLE,  CHILD(2),  C2PAGE )

      RETURN
      END
