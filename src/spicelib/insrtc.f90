!$Procedure INSRTC ( Insert an item into a character set )

      SUBROUTINE INSRTC ( ITEM, A )

! $ Abstract
!
!     Insert an item into a character set.
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
!     SETS
!
! $ Keywords
!
!     CELLS
!     SETS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         ITEM
      CHARACTER*(*)         A        ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item to be inserted.
!     A         I-O  Insertion set.
!
! $ Detailed_Input
!
!     ITEM     is an item which is to be inserted into the specified
!              set. ITEM may or may not already be an element of the
!              set.
!
!              If ITEM is longer than the declared maximum length of the
!              set's elements, the string will be truncated on the right
!              when it is inserted. Trailing blanks in ITEM are not
!              significant.
!
!     A        is a SPICE set.
!
!              On input, A may or may not contain the input item as an
!              element.
!
! $ Detailed_Output
!
!     A        on output, contains the union of the input set and the
!              singleton set containing the input item, unless there was
!              not sufficient room in the set for the item to be
!              included, in which case the set is not changed and an
!              error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the insertion of the item into the set causes an excess
!         of elements, the error SPICE(SETEXCESS) is signaled.
!
!     2)  If the item to be inserted has greater length than the string
!         length of the elements of the set, the item will be truncated
!         on the right when it is inserted. The insertion point of the
!         element will be determined by the comparison of the truncated
!         item to members of the set. If, after truncation, the item to
!         be inserted matches an element already present in the set, no
!         insertion occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a set with all the original planets of the Solar
!        System and then remove Pluto from that set.
!
!
!        Example code begins here.
!
!
!              PROGRAM INSRTC_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 CARDC
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 LBCELL
!              PARAMETER             ( LBCELL = -5  )
!
!              INTEGER                 PNAMSZ
!              PARAMETER             ( PNAMSZ   = 7 )
!
!              INTEGER                 SETDIM
!              PARAMETER             ( SETDIM   = 9 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(PNAMSZ)      LIST   ( SETDIM        )
!              CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
!
!              INTEGER                 I
!
!        C
!        C     Create the original planets list.
!        C
!              DATA                    LIST  /
!             .                'MERCURY', 'VENUS',   'EARTH',
!             .                'MARS',    'JUPITER', 'SATURN',
!             .                'URANUS',  'NEPTUNE', 'PLUTO'   /
!
!        C
!        C     Initialize the empty set.
!        C
!              CALL VALIDC ( SETDIM, 0, PLNETS )
!
!        C
!        C     Insert the list of planets into the set. If the item is
!        C     an element of the set, the set is not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL INSRTC ( LIST(I), PLNETS )
!
!              END DO
!
!        C
!        C     Remove the Pluto from the set. If the Pluto is not an
!        C     element of the set, the set is not changed.
!        C
!              CALL REMOVC ( 'PLUTO', PLNETS )
!
!        C
!        C     Output the contents of PLNETS.
!        C
!              WRITE(*,*) 'Planets of the Solar System:'
!
!              DO I = 1, CARDC ( PLNETS )
!
!                 WRITE(*,*) '   ', PLNETS(I)
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Planets of the Solar System:
!            EARTH
!            JUPITER
!            MARS
!            MERCURY
!            NEPTUNE
!            SATURN
!            URANUS
!            VENUS
!
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
!     N.J. Bachman       (JPL)
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Improved the argument ITEM description in $Detailed_Input.
!
!-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
!
!        Bug fix: when the item to be inserted would, after
!        truncation to the set's string length, match an item
!        already in the set, no insertion is performed. Previously
!        the truncated string was inserted, corrupting the set.
!
!        Long error message was updated to include size of
!        set into which insertion was attempted.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     insert an item into a character set
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
!
!        Calling protocol of EXCESS changed. Call to SETMSG removed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               LSTLEC
      INTEGER               SIZEC

      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               CARD
      INTEGER               I
      INTEGER               LAST
      INTEGER               SIZE
      INTEGER               SLEN

      LOGICAL               IN

!
!     Set up the error processing.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'INSRTC' )

!
!     What are the size and cardinality of the set?
!
      SIZE = SIZEC ( A )
      CARD = CARDC ( A )

!
!     When we insert an item into the set, any trailing characters
!     that don't fit are truncated.  So in deciding where to insert
!     the item, we ignore any characters that won't remain after
!     insertion.
!
!     We're going to consider only the initial substring of ITEM
!     whose length doesn't exceed the string length of the set's
!     members.
!
      SLEN = MIN (  LEN(ITEM),  LEN( A(1) )  )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      LAST = LSTLEC ( ITEM(:SLEN),  CARD,  A(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      IF ( LAST .GT. 0 ) THEN
         IN  =  A(LAST) .EQ. ITEM(:SLEN)
      ELSE
         IN  =  .FALSE.
      END IF

      IF ( .NOT. IN ) THEN
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         IF ( CARD .LT. SIZE ) THEN

            DO I = CARD, LAST+1, -1
               A(I+1) = A(I)
            END DO

            A(LAST+1) = ITEM(:SLEN)

            CALL SCARDC ( CARD+1, A )

         ELSE

            CALL SETMSG ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            CALL ERRINT ( '#', SIZE                             )
            CALL SIGERR ( 'SPICE(SETEXCESS)'                    )

         END IF

      END IF

      CALL CHKOUT ( 'INSRTC' )
      RETURN
      END
