!$Procedure      ZZEKTRSB ( EK tree, identify siblings )

      SUBROUTINE ZZEKTRSB ( HANDLE, TREE, KEY, LSIB, LKEY, RSIB, RKEY )

! $ Abstract
!
!     Identify the immediate siblings of a node:  return a key in each
!     sibling and the siblings' node numbers.
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

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               LSIB
      INTEGER               LKEY
      INTEGER               RSIB
      INTEGER               RKEY

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key of interest.
!     LSIB       O   Left sibling node.
!     LKEY       O   Key in left sibling.
!     RSIB       O   Right sibling node.
!     RKEY       O   Key in right sibling.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is a key belonging to a node whose sibling nodes
!                    are sought.  KEY is expected to be an absolute,
!                    not node-relative, key.
!
! $ Detailed_Output
!
!     LSIB           is the number of the left sibling node of the node
!                    containing KEY.  If the node containing KEY has no
!                    left sibling, LSIB is set to zero.
!
!     LKEY           is an absolute key in node LSIB.
!
!     RSIB           is the number of the right sibling node of the node
!                    containing KEY.  If the node containing KEY has no
!                    right sibling, RSIB is set to zero.
!
!     RKEY           is an absolute key in node RSIB.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an error occurs while looking up the parent of the node
!         containing KEY, the error will be diagnosed by routines
!         called by this routine.  It is not an error for the node
!         containing KEY to have no parent, as long as KEY belongs to
!         the root.
!
!     3)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility intended for use by other routines in
!     the EKTRxx set.
!
!     The output keys LKEY and RKEY may be used to find the siblings
!     of the sibling nodes LSIB and RSIB.
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
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Other functions
!
      INTEGER               ZZEKTRBS

!
!     Local variables
!
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LOFFST
      INTEGER               LLPIDX
      INTEGER               LPIDX
      INTEGER               LPKEY
      INTEGER               NKBAS
      INTEGER               PARENT
      INTEGER               PKEY
      INTEGER               POFFST
      INTEGER               ROFFST
      INTEGER               RPIDX
      INTEGER               RPKEY

!
!     Start out by looking up the parent node.  We get LSIB
!     and RSIB for free.
!
      CALL ZZEKTRPI ( HANDLE, TREE,   KEY,  PARENT, PKEY,  POFFST, &
                      LPIDX,  LPKEY,  LSIB, RPIDX,  RPKEY, RSIB   )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Set the base addresses for the child pointers and keys,
!     based on whether the parent is the root.
!
      IF ( PARENT .EQ. TREE ) THEN
         KEYBAS = TRKEYR
         KIDBAS = TRKIDR
         NKBAS  = TRNKR
      ELSE
         KEYBAS = TRKEYC
         KIDBAS = TRKIDC
         NKBAS  = TRNKC
      END IF

!
!     We need to find absolute keys in each sibling that exists.
!     To do this, we need the node offset of each sibling node.
!     That offset is the value of the parent key preceding each node,
!     plus the parent's offset.
!
      IF ( LPIDX .GT. 1 ) THEN
!
!        The left parent key has a predecessor.  This predecessor is
!        the immediate predecessor of the left sibling node.
!
         LLPIDX  =  LPIDX  -  1
         BASE    =  ZZEKTRBS ( PARENT )
         ADDRSS  =  BASE   +  KEYBAS  +  LLPIDX

         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LOFFST )

         LOFFST  =  LOFFST + POFFST

!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         BASE    =  ZZEKTRBS ( LSIB )
         ADDRSS  =  BASE + TRKEYC + 1

         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LKEY )

         LKEY    =  LKEY + LOFFST


      ELSE IF ( LPIDX .EQ. 1 ) THEN
!
!        The left parent key is the first key.  The left sibling has
!        no predecessor.
!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         BASE    =  ZZEKTRBS ( LSIB )
         ADDRSS  =  BASE + TRKEYC + 1

         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, LKEY )

         LKEY    =  LKEY + POFFST


      ELSE
!
!        There's no left sibling.  Set the left sibling's key to a
!        value that won't be mistaken for a valid one.
!
         LKEY    =  0

      END IF

!
!     LKEY is set.  It's time to produce an absolute key for the
!     right sibling.
!
      IF ( RPIDX .GT. 0 ) THEN
!
!        The right parent key exists.  This key is the
!        immediate predecessor of the right sibling node.
!
         ROFFST  =  RPKEY  +  POFFST

!
!        Get the first key from the right sibling.  Convert the key
!        to an absolute key.
!
         BASE    =  ZZEKTRBS ( RSIB )
         ADDRSS  =  BASE + TRKEYC + 1

         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, RKEY )

         RKEY    =  RKEY + ROFFST


      ELSE
!
!        There's no right sibling.  Set the right sibling's key to a
!        value that won't be mistaken for a valid one.
!
         RKEY    =  0

      END IF
!
!     All outputs are set.
!

      RETURN
      END
