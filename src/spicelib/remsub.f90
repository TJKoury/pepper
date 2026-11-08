!$Procedure REMSUB ( Remove a substring )

      subroutine remsub ( in, left, right, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Remove the substring (LEFT:RIGHT) from a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     CHARACTER
!     STRING
!
! $ Declarations

      implicit none

      character(*)     in
      integer(ip)      left
      integer(ip)      right
      character(*)     out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     LEFT       I   Position of first character to be removed.
!     RIGHT      I   Position of last character to be removed.
!     OUT        O   Output string.
!
! $ Detailed_Input
!
!     IN       is an input character string, from which a substring
!              is to be removed.
!
!     LEFT,
!     RIGHT    are the ends of the substring to be removed.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is equivalent to the
!              string that would be created by the concatenation
!
!                    OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
!
!              If the string is too long to fit into OUT, it is
!              truncated on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If LEFT > RIGHT, RIGHT < 1, LEFT < 1, RIGHT > LEN(IN), or
!         LEFT > LEN(IN), the error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Move the characters, beginning with RIGHT, one at a time to the
!     positions immediately following LEFT. This has the same effect
!     as the concatenation
!
!           OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
!
!     Because this operation is not standard for strings of length (*),
!     this routine does not use concatenation.
!
! $ Examples
!
!     The following examples illustrate the use of REMSUB.
!
!     IN                 LEFT  RIGHT        OUT
!     -----------------  ----  -----        ------------------------
!     'ABCDEFGHIJ'          3      5        'ABFGHIJ'
!     'The best rabbit'     5      8        'The  rabbit'
!     'The other woman'     1      4        'other woman'
!     'An Apple a day'      2      2        'A apple a day'
!     'An Apple a day'      5      2         An error is signaled.
!     'An Apple a day'      0      0         An error is signaled.
!     'An Apple a day'     -3      3         An error is signaled.
!
!     Whenever an error has been signaled, the contents of OUT are
!     unpredictable.
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
!     H.A. Neilan        (JPL)
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
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     remove a substring
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 05-JAN-1989 (HAN)
!
!         Error handling was added to detect invalid character
!         positions. If LEFT > RIGHT, RIGHT < 1, LEFT < 1,
!         RIGHT > LEN(IN), or LEFT > LEN(IN), an error is signaled.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      lastnb
      logical          return

!
!     Other functions
!
      integer(ip)      len

!
!     Local variables
!
      integer(ip)      inlen
      integer(ip)      outlen

      integer(ip)      l
      integer(ip)      r

      integer(ip)      i
      integer(ip)      j




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'REMSUB' )
      end if

!
!     If a character position is out of range, signal an error.
!
      if ( ( left  > right    )   .or. &
           ( right < 1        )   .or. &
           ( left  < 1        )   .or. &
           ( right > len (in) )   .or. &
           ( left  > len (in) ) )       then

         call setmsg ( 'Left location was *. Right location was *.' )
         call errint ( '*', left             )
         call errint ( '*', right            )
         call sigerr ( 'SPICE(INVALIDINDEX)' )
         call chkout ( 'REMSUB' )
         return

      else

         l = left
         r = right

      end if


!
!     How much of the input string will we use? And how big is the
!     output string?
!
      inlen  = lastnb ( in  )
      outlen = len    ( out )

!
!     Copy the first part of the input string. (One character at a
!     time, in case this is being done in place.)
!
      do i = 1, min ( l-1, outlen )

         out(i:i) = in(i:i)

      end do

!
!     Now move the rest of the string over.
!
      i = l
      j = r + 1

      do while ( ( i <= outlen ) .and. ( j <= inlen ) )

         out(i:i) = in(j:j)
         i        = i + 1
         j        = j + 1

      end do

!
!     Pad with blanks, if necessary.
!
      if ( i <= outlen ) then

         out(i: ) = ' '

      end if


      call chkout ( 'REMSUB' )
      return
      end
