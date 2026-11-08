!$Procedure CYAIIP ( Cycle the elements of an integer array, in place )

      SUBROUTINE CYAIIP ( NELT, DIR, NCYCLE, ARRAY )

! $ Abstract
!
!     Cycle the elements of an integer array forward or backward
!     in place.
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
!     None.
!
! $ Keywords
!
!     ARRAY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NELT
      CHARACTER*1           DIR
      INTEGER               NCYCLE
      INTEGER               ARRAY    ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NELT       I   Number of elements.
!     DIR        I   Direction to cycle: 'F' or 'B'.
!     NCYCLE     I   Number of times to cycle.
!     ARRAY     I-O  Array to be cycled/cycled array.
!
! $ Detailed_Input
!
!     NELT     is the number of elements in the input array.
!
!     DIR      is the direction in which the elements in the
!              array are to be cycled.
!
!                 'F' or 'f'  to cycle forward.
!                 'B' or 'b'  to cycle backward.
!
!     NCYCLE   is the number of times the elements in the array
!              are to be cycled.
!
!     ARRAY    is the array to be cycled.
!
! $ Detailed_Output
!
!     ARRAY    is the input array after it has been cycled.
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
!     This routine cycles an integer array in place. To cycle
!     an array and store the result in a new array, use CYCLAI.
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
!        A(1) = 1
!        A(2) = 2
!        A(3) = 3
!        A(4) = 4
!
!     Cycling A forward once yields the array
!
!        A(1) = 4
!        A(2) = 1
!        A(3) = 2
!        A(4) = 3
!
!     Cycling A backward once yields the array
!
!        A(1) = 2
!        A(2) = 3
!        A(3) = 4
!        A(4) = 1
!
!     Cycling by any multiple of the number of elements in the array
!     yields the same array.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 09-SEP-2005 (NJB) (HAN) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     cycle the elements of an integer array in place
!
!-&


!
!     SPICELIB functions
!
      INTEGER          GCD
      LOGICAL          RETURN

!
!     Local variables
!
      INTEGER          LAST
      INTEGER          TEMP

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
         CALL CHKIN ( 'CYAIIP' )
      END IF

!
!     Don't even screw around if there are no elements in the array.
!
      IF ( NELT .LT. 1 ) THEN
         CALL CHKOUT ( 'CYAIIP' )
         RETURN
      END IF

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      IF (      ( DIR .EQ. 'B' ) &
           .OR. ( DIR .EQ. 'b' ) ) THEN

         K = MOD ( - NCYCLE, NELT )

      ELSE IF (      ( DIR .EQ. 'F' ) &
                .OR. ( DIR .EQ. 'F' ) ) THEN

         K = MOD (   NCYCLE, NELT )

      ELSE

         CALL SETMSG ( 'Cycling direction was *.' )
         CALL ERRCH  ( '*', DIR                   )
         CALL SIGERR ( 'SPICE(INVALIDDIRECTION)'  )
         CALL CHKOUT ( 'CYAIIP'                   )
         RETURN

      END IF


      IF ( K .LT. 0 ) THEN

         K = K + NELT

      ELSE IF ( K .EQ. 0 ) THEN

         CALL CHKOUT ( 'CYAIIP' )
         RETURN

      END IF


!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      G = GCD ( K, NELT )
      M = NELT / G

      DO I = 1, G

         L    = I
         LAST = ARRAY(L)

         DO J = 1, M

            L = L + K

            IF ( L .GT. NELT ) THEN
               L = L - NELT
            END IF

            TEMP     = ARRAY(L)
            ARRAY(L) = LAST
            LAST     = TEMP

         END DO

      END DO


      CALL CHKOUT ( 'CYAIIP' )
      RETURN
      END
