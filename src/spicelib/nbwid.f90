!$Procedure NBWID ( Non-blank width of a character array )

      INTEGER FUNCTION NBWID ( ARRAY, NELT )

! $ Abstract
!
!     Determine the non-blank width of a character array---that is,
!     the largest value of LASTNB for any element in the array.
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
!     CHARACTER
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         ARRAY   ( * )
      INTEGER               NELT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I   Input array.
!     NELT       I   Number of elements in the array.
!
!     The function returns the index of the rightmost non-blank
!     character in the entire array of strings ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is the input array.
!
!     NELT     is the number of elements in the input array.
!
! $ Detailed_Output
!
!     The function returns the index of the rightmost non-blank
!     character in the entire array. This is equivalent to the maximum
!     value of LASTNB for the array, but somewhat more efficient to
!     compute. If NELT is not greater than zero, NBWID is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Find the last non-blank character in the first element of the
!     array. Search the rest of the elements, starting at the end of
!     each string and moving back just far enough to determine if the
!     current string is wider than any of the previous ones. (This
!     makes NBWID somewhat more efficient than LASTNB.)
!
!     If any of the strings is found to contain no trailing blanks,
!     NBWID is just the length of the individual elements of the array,
!     and the search is terminated immediately.
!
! $ Examples
!
!     Let ARRAY contain the following strings.
!
!           ARRAY(1) = 'A string of medium length                      '
!           ARRAY(2) = 'A very long string, much longer than the rest  '
!           ARRAY(3) = 'Shorter                                        '
!           ARRAY(4) = 'Short                                          '
!
!     Then the value returned by
!
!           WIDEST = NBWID ( ARRAY, 4 )
!
!     is 45.
!
!     If the word 'rest' in the second element is changed to 'others',
!     the value returned is 47, and the search is terminated after the
!     second element.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     non-blank width of a character array
!
!-&


!
!     Local variables
!
      INTEGER               STRLEN
      INTEGER               I
      INTEGER               J


!
!     Nonsense case: no elements.
!
      IF ( NELT .LT. 1 ) THEN

         NBWID = 0

!
!     Get the length of the individual elements of the string.
!     So far, we have no maximum width, because we haven't examined
!     any elements.
!
      ELSE

         STRLEN = LEN ( ARRAY(1) )
         NBWID  = 0
         I      = 0

!
!        Continue until the end of the array is reached, or until
!        a string with no trailing blanks is found.
!
         DO WHILE ( I .LT. NELT  .AND.  NBWID .LT. STRLEN )

!
!           Search no further than the current value of NBWID.
!
            I = I + 1
            J = STRLEN

            DO WHILE ( J .GT. NBWID  .AND.  ARRAY(I)(J:J) .EQ. ' ' )
               J = J - 1
            END DO

!
!           NBWID only increases if this string was wider than all
!           previous strings.
!
            NBWID = MAX ( NBWID, J )

         END DO

      END IF

      RETURN
      END
