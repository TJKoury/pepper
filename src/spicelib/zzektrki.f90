!$Procedure      ZZEKTRKI ( EK tree, look up key by index )

      SUBROUTINE ZZEKTRKI ( HANDLE, TREE, NODKEY, N, KEY )

! $ Abstract
!
!     Get a key from a node by index:  return the key having a specified
!     index in a specified node.  The node of interest is identified
!     by a key in the node.
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
      INTEGER               NODKEY
      INTEGER               N
      INTEGER               KEY

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     NODKEY     I   Key identifying node containing key of interest.
!     N          I   Index of key of interest.
!     KEY        O   Key located at index N.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     NODKEY         is an absolute key belonging to the node
!                    containing the key of interest.
!
!                    NODKEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree.
!
!     N              is the node-relative index of the key of interest.
!                    Indices of keys start at 1.
!
! $ Detailed_Output
!
!     KEY            is the absolute key located at index N within the
!                    node containing NODKEY.
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
!     3)  If the input key is less than 1 or greater than the number
!         of keys in the specified tree, the error SPICE(INVALIDINDEX)
!         is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine allows lookup of keys by index.  It is frequently
!     used by other EK private routines to find the first key of a node.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


!
!     Functions
!
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRNK

!
!     Local variables
!
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               IDX
      INTEGER               LEVEL
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               PTR
      INTEGER               SIZE





      CALL ZZEKTRLK (  HANDLE,  TREE,    NODKEY,  IDX, &
                       NODE,    NOFFST,  LEVEL,   PTR   )

      SIZE  =  ZZEKTRNK ( HANDLE, TREE,  NODE )

!
!     Reject bad indices.
!
      IF (  ( N .LT. 0 ) .OR. ( N .GT. SIZE )  )  THEN

         CALL CHKIN  ( 'ZZEKTRKI'                                  )
         CALL SETMSG ( 'Key index = #; valid range in node # is ' // &
                       '1:#'                                       )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NODE                                  )
         CALL ERRINT ( '#',  SIZE                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKTRKI'                                  )
         RETURN

      END IF


      BASE  =  ZZEKTRBS ( NODE )


      IF ( LEVEL .EQ. 1 ) THEN
         ADDRSS = BASE + TRKEYR + N
      ELSE
         ADDRSS = BASE + TRKEYC + N
      END IF


      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, KEY )

!
!     Map the key from relative to absolute.
!
      KEY  =  KEY + NOFFST


      END
