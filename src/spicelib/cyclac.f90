!$Procedure CYCLAC ( Cycle the elements of a character array )

      SUBROUTINE CYCLAC ( ARRAY, NELT, DIR, NCYCLE, OUT )

! $ Abstract
!
!     Cycle the elements of a character array forward or backward.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         ARRAY    ( * )
      INTEGER               NELT
      CHARACTER*1           DIR
      INTEGER               NCYCLE
      CHARACTER*(*)         OUT      ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I   Input array.
!     NELT       I   Number of elements.
!     DIR        I   Direction to cycle: 'F' or 'B'.
!     NCYCLE     I   Number of times to cycle.
!     OUT        O   Cycled array.
!
! $ Detailed_Input
!
!     ARRAY    is the array to be cycled.
!
!     NELT     is the number of elements in the input array.
!
!     DIR      is the direction in which the elements in the
!              array are to be cycled.
!
!                    'F' or 'f'  to cycle forward.
!                    'B' or 'b'  to cycle backward.
!
!     NCYCLE   is the number of times the elements in the array
!              are to be cycled.
!
! $ Detailed_Output
!
!     OUT      is the input array after it has been cycled.
!              OUT may overwrite ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of DIR is not recognized, the error
!         SPICE(INVALIDDIRECTION) is signaled.
!
!     2)  If NELT is less than 1, the output array is not modified.
!
!     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
!         the opposite direction of DIR.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     An array is cycled when its contents are shifted forward or
!     backward by one place. An element pushed off one end of the
!     array is brought around to the other end of the array instead
!     of disappearing.
!
! $ Examples
!
!     Let the integer array A contain the following elements.
!
!           A(1) = 'apple'
!           A(2) = 'bear'
!           A(3) = 'cake'
!           A(4) = 'dragon'
!
!     Cycling A forward once yields the array
!
!           A(1) = 'dragon'
!           A(2) = 'apple'
!           A(3) = 'bear'
!           A(4) = 'cake'
!
!     Cycling A backward once yields the array
!
!           A(1) = 'bear'
!           A(2) = 'cake'
!           A(3) = 'dragon'
!           A(4) = 'apple'
!
!     Cycling by any multiple of the number of elements in the array
!     yields the same array.
!
! $ Restrictions
!
!     1)  The memory used for the output array must be identical to or
!         disjoint from the memory used for the input array.
!
!         That is:
!
!            CALL CYCLAC ( ARRAY, NELT, DIR, NCYCLE, ARRAY )
!
!         will produce correct results, while
!
!            CALL CYCLAC ( ARRAY, NELT-3, DIR, NCYCLE, ARRAY(4) )
!
!         will produce garbage.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
!
!-&


! $ Index_Entries
!
!     cycle the elements of a character array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
!
!         Error handling was added to detect an invalid value for
!         the cycling direction. If the direction is not recognized
!         the error SPICE(INVALIDDIRECTION) is signaled and the
!         output array is not modified. (The routine used to copy the
!         input array into the output array if the direction was not
!         recognized.)
!
!         The "Exceptions" section was filled out in more detail.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL          RETURN
      INTEGER          GCD
      INTEGER          NBWID

!
!     Local variables
!
      INTEGER          OUTLEN
      INTEGER          WIDEST
      INTEGER          LIMIT

      CHARACTER*1      LAST
      CHARACTER*1      TEMP

      INTEGER          C
      INTEGER          G
      INTEGER          I
      INTEGER          J
      INTEGER          K
      INTEGER          L
      INTEGER          M



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CYCLAC' )
      END IF

!
!     Don't even screw around if there are no elements in the array.
!
      IF ( NELT .LT. 1 ) THEN
         CALL CHKOUT ( 'CYCLAC' )
         RETURN
      END IF

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!     If the cycling direction is not recognized, signal an error.
!
      IF (          ( DIR .EQ. 'B' ) &
               .OR. ( DIR .EQ. 'b' ) ) THEN

         K = MOD ( - NCYCLE, NELT )

      ELSE IF (      ( DIR .EQ. 'F' ) &
                .OR. ( DIR .EQ. 'f' ) ) THEN

         K = MOD (   NCYCLE, NELT )

      ELSE

         CALL SETMSG ( 'Cycling direction was *.' )
         CALL ERRCH  ( '*', DIR )
         CALL SIGERR ( 'SPICE(INVALIDDIRECTION)'  )
         CALL CHKOUT ( 'CYCLAC' )
         RETURN

      END IF



      IF ( K .LT. 0 ) THEN

         K = K + NELT

      ELSE IF ( K .EQ. 0 ) THEN

         CALL MOVEC  ( ARRAY, NELT, OUT )
         CALL CHKOUT ( 'CYCLAC' )
         RETURN

      END IF





!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
!     The character version of CYCLAx differs from the other
!     versions in that a single character is cycled at a time. That
!     is, the first trip through the outermost loop cycles the first
!     characters of the array elements; the second trip cycles the
!     second characters; and so on. This allows the same algorithm to
!     be used for all the routines. The local storage required is just
!     a couple of characters.
!

!
!     Don't swap the ends of strings if they're just blank padded.
!     And don't overwrite the elements of the output array, if they
!     happen to be shorter thAn those in the input array.
!
      OUTLEN = LEN   ( OUT(1)         )
      WIDEST = NBWID ( ARRAY,  NELT   )
      LIMIT  = MIN   ( OUTLEN, WIDEST )

!
!     The greatest common divisor need only be computed once.
!
      G = GCD ( K, NELT )
      M = NELT / G

!
!     To make this a non-character routine, remove all references to C.
!
      DO C = 1, LIMIT

         DO I = 1, G

            L    = I
            LAST = ARRAY(L)(C:C)

            DO J = 1, M

               L = L + K

               IF ( L .GT. NELT ) THEN
                  L = L - NELT
               END IF

               TEMP        = ARRAY(L)(C:C)
               OUT(L)(C:C) = LAST
               LAST        = TEMP

            END DO

         END DO

      END DO

!
!     If needed, pad the output array with blanks.
!
      IF ( OUTLEN .GT. LIMIT ) THEN

         DO I = 1, NELT
            OUT(I)(LIMIT+1: ) = ' '
         END DO

      END IF


      CALL CHKOUT ( 'CYCLAC' )
      RETURN
      END
