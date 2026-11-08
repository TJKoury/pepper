!$Procedure      ZZEKTRLK ( EK tree, locate key )

      SUBROUTINE ZZEKTRLK ( HANDLE, TREE,   KEY,   IDX, &
                            NODE,   NOFFST, LEVEL, VALUE  )

! $ Abstract
!
!     Locate a specified key.  Return metadata describing the node
!     containing the key.
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
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               KEY
      INTEGER               IDX
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               LEVEL
      INTEGER               VALUE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key corresponding to value.
!     IDX        O   Node-relative index of KEY.
!     NODE       O   Node containing key.
!     NOFFST     O   Offset of NODE.
!     LEVEL      O   Level of NODE.
!     VALUE      O   Value associated with KEY.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key.  In EK trees, absolute keys are
!                    just ordinal positions relative to the leftmost
!                    element of the tree, with the leftmost element
!                    having position 1.  So setting KEY to 10, for
!                    example, indicates that the output VALUE is the
!                    10th item in the tree.
!
!                    KEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree.
!
! $ Detailed_Output
!
!     IDX            is the node-relative index of KEY:  this is the
!                    ordinal position of KEY relative to other keys
!                    in the same node.
!
!     NODE           is the number of the node containing KEY.
!
!     NOFFST         is the offset of NODE.  This is the count of the
!                    keys that precede every key in the subtree headed
!                    by NODE.  Adding NOFFST to any relative key stored
!                    in NODE will convert that key to an absolute key.
!
!     LEVEL          is the level of NODE in the tree.  The root is at
!                    level 1, children of the root are at level 2, and
!                    so on.
!
!     VALUE          is the integer value associated with the input key.
!                    Normally, this value is a data pointer.
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
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
!
!     4)  If the tree traversal fails to terminate at the leaf node
!         level, the error SPICE(BUG) is signaled.
!
!     5)  If the key is in range, but the key is not found, the error
!         SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine obtains the value associated with a key, and also
!     returns metadata describing the node containing the key and the
!     key's position in the node.
!
! $ Examples
!
!     See ZZEKTRUI.
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
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Added initializers for variables used to save old
!        values.
!
!-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               LSTLEI

!
!     Local parameters
!
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 15 )

!
!     Local variables
!
      CHARACTER*(ACCLEN)    ACCESS

      INTEGER               CHILD
      INTEGER               DATBAS
      INTEGER               DEPTH
      INTEGER               MINUS
      INTEGER               NEWKEY
      INTEGER               OLDHAN
      INTEGER               OLDIDX
      INTEGER               OLDLVL
      INTEGER               OLDMAX
      INTEGER               OLDNOD
      INTEGER               OLDNOF
      INTEGER               OLDTRE
      INTEGER               OLDKEY
      INTEGER               OLDVAL
      INTEGER               PAGE   ( PGSIZI )
      INTEGER               PLUS
      INTEGER               PREV
      INTEGER               PRVKEY
      INTEGER               TOTKEY

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               LEAF
      LOGICAL               SAMKEY
      LOGICAL               SAMTRE
      LOGICAL               RDONLY

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  FIRST  / .TRUE.   /

      DATA                  OLDHAN / 0        /
      DATA                  OLDIDX / 0        /
      DATA                  OLDKEY / 0        /
      DATA                  OLDLVL / 0        /
      DATA                  OLDMAX / 0        /
      DATA                  OLDNOD / 0        /
      DATA                  OLDNOF / 0        /
      DATA                  OLDTRE / 0        /
      DATA                  OLDVAL / 0        /
      DATA                  PAGE   / PGSIZI * 0 /

!
!     Use discovery check-in in this puppy.
!
!     Nothing found to begin with.
!
      FOUND  =  .FALSE.


      IF ( FIRST ) THEN
!
!        Find out the access method for the current file.
!
         CALL DASHAM ( HANDLE, ACCESS )

         RDONLY  =    ACCESS .EQ. 'READ'
         SAMKEY  =    .FALSE.
         SAMTRE  =    .FALSE.
         LEAF    =    .FALSE.
         FIRST   =    .FALSE.

      ELSE
!
!        See whether we're looking at the same key, or at least
!        the same tree, as last time.  Note that for the tree to
!        be guaranteed to be the same, it must belong to a file open
!        for read access only.
!
         IF ( HANDLE .NE. OLDHAN ) THEN

            CALL DASHAM ( HANDLE, ACCESS )

            RDONLY  =    ACCESS .EQ. 'READ'
            SAMTRE  =    .FALSE.
            SAMKEY  =    .FALSE.

         ELSE

            SAMTRE  =  ( TREE  .EQ.  OLDTRE )  .AND.   RDONLY
            SAMKEY  =  ( KEY   .EQ.  OLDKEY )  .AND.   SAMTRE

         END IF

      END IF

!
!     If we're lucky enough to be getting a request for the previously
!     returned key, we're set.  If we've been asked for a key that is
!     very close to the previously requested key, we still may make
!     out pretty well.
!
      IF ( SAMKEY ) THEN
!
!        It's the same key as last time.
!
         IDX     =  OLDIDX
         NODE    =  OLDNOD
         NOFFST  =  OLDNOF
         LEVEL   =  OLDLVL
         VALUE   =  OLDVAL

         RETURN


      ELSE IF ( SAMTRE .AND. LEAF ) THEN
!
!        Compute the margins around the old key.  Keys that fall within
!        the interval defined by the old key and these margins are on
!        the same page as the old key.
!
         PLUS   =  OLDMAX - OLDIDX
         MINUS  =  OLDIDX - 1


         IF (        (  KEY  .LE.  OLDKEY+PLUS   ) &
               .AND. (  KEY  .GE.  OLDKEY-MINUS  )  ) THEN
!
!           The requested key lies on the same page as the old key.
!
            LEVEL   =  OLDLVL

            IF ( LEVEL .EQ. 1 ) THEN
               DATBAS  =  TRDATR
            ELSE
               DATBAS  =  TRDATC
            END IF

            IDX     =  OLDIDX + ( KEY - OLDKEY )
            NODE    =  OLDNOD
            NOFFST  =  OLDNOF
            VALUE   =  PAGE ( DATBAS + IDX )

            OLDIDX  =  IDX
            OLDKEY  =  KEY
            OLDVAL  =  VALUE

            RETURN

         END IF

      END IF

!
!     If we arrived here, we have some actual work to do.
!     Start out by looking at the root page.  Save the tree depth;
!     we'll use this for error checking.
!
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )

      DEPTH  =  PAGE ( TRDPTH )
      LEVEL  =  1

!
!     Find out how many keys are in the tree.  If KEY is outside
!     this range, we won't find it.
!
      TOTKEY  =  PAGE ( TRNKEY )

      IF (  ( KEY .LT. 1 )  .OR.  ( KEY .GT. TOTKEY )  ) THEN

         CALL CHKIN  ( 'ZZEKTRLK'                                      )
         CALL SETMSG ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         CALL ERRINT ( '#',     KEY                                    )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRHAN ( '#',     HANDLE                                 )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                        )
         CALL CHKOUT ( 'ZZEKTRLK'                                      )
         RETURN

      END IF

!
!     Find the last key at this level that is less than or equal to
!     the requested key.
!
      PREV   =  LSTLEI (  KEY,  PAGE(TRNKR),  PAGE(TRKEYR+1)  )

      IF ( PREV .GT. 0 ) THEN
         PRVKEY =  PAGE(TRKEYR+PREV)
      ELSE
         PRVKEY =  0
      END IF

!
!     If we were lucky enough to get an exact match, set our outputs
!     and return.  The key offset in the root is zero.
!
      IF ( PRVKEY .EQ. KEY ) THEN

         NOFFST  =  0
         IDX     =  PREV
         NODE    =  TREE
         VALUE   =  PAGE ( TRDATR + IDX )

         OLDHAN  =  HANDLE
         OLDTRE  =  TREE
         OLDKEY  =  KEY
         OLDNOF  =  NOFFST
         OLDNOD  =  NODE
         OLDIDX  =  IDX
         OLDLVL  =  LEVEL
         OLDVAL  =  VALUE
         OLDMAX  =  PAGE ( TRNKR )

         LEAF    =  LEVEL  .EQ.  DEPTH

!
!        The root has no parent or siblings, so these values
!        remain set to zero.  The same is true of the parent keys.
!
         RETURN

      END IF

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      CHILD   =  PAGE ( TRKIDR + PREV + 1 )
      NOFFST  =  PRVKEY


      DO WHILE (  ( CHILD .GT. 0 ) .AND. ( .NOT. FOUND )  )
!
!        Look up the child node.
!
         CALL ZZEKPGRI ( HANDLE, CHILD, PAGE )

         LEVEL  =  LEVEL + 1


         IF ( LEVEL .GT. DEPTH ) THEN

            CALL CHKIN  ( 'ZZEKTRLK'                                   )
            CALL SETMSG ( 'Runaway node pointer chain.  Key = #; '// &
                          'valid range = 1:#. Tree = #, file = #'      )
            CALL ERRINT ( '#',     KEY                                 )
            CALL ERRINT ( '#',     TOTKEY                              )
            CALL ERRINT ( '#',     TREE                                )
            CALL ERRHAN ( '#',     HANDLE                              )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKTRLK'                                   )
            RETURN

         END IF

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from KEY the position of the
!        node preceding the first key of this subtree.
!
         NEWKEY  =  KEY - NOFFST
         PREV    =  LSTLEI (  NEWKEY,  PAGE(TRNKC),  PAGE(TRKEYC+1)  )

         IF ( PREV .GT. 0 ) THEN
            PRVKEY =  PAGE(TRKEYC+PREV)
         ELSE
            PRVKEY =  0
         END IF

!
!        If we were lucky enough to get an exact match, set our outputs
!        and return.  The key offset for the current node is stored
!        in NOFFST.
!
         IF ( PRVKEY .EQ. NEWKEY ) THEN

            FOUND   =  .TRUE.
            IDX     =  PREV
            NODE    =  CHILD
            VALUE   =  PAGE ( TRDATC + IDX )

            OLDHAN  =  HANDLE
            OLDTRE  =  TREE
            OLDKEY  =  KEY
            OLDNOF  =  NOFFST
            OLDNOD  =  NODE
            OLDIDX  =  IDX
            OLDLVL  =  LEVEL
            OLDVAL  =  VALUE
            OLDMAX  =  PAGE ( TRNKC )

            LEAF    =  LEVEL  .EQ.  DEPTH

         ELSE

            CHILD   =  PAGE ( TRKIDC + PREV + 1 )
            NOFFST  =  PRVKEY + NOFFST

         END IF

      END DO

!
!     If we found the key, our outputs are already set.  If not, we've
!     got trouble.
!
      IF ( .NOT. FOUND ) THEN

         CALL CHKIN  ( 'ZZEKTRLK'                                      )
         CALL SETMSG ( 'Key #; valid range = 1:#. Tree = #, file = #.'// &
                       '  Key was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         CALL ERRINT ( '#',     KEY                                    )
         CALL ERRINT ( '#',     TOTKEY                                 )
         CALL ERRINT ( '#',     TREE                                   )
         CALL ERRHAN ( '#',     HANDLE                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZEKTRLK'                                      )
         RETURN

      END IF


      RETURN
      END
