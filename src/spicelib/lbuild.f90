!$Procedure LBUILD ( Build a list in a character string )

      subroutine lbuild ( items, n, delim, list )

! $ Abstract
!
!     Build a list of items delimited by a character.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     LIST
!     STRING
!
! $ Declarations

      implicit none

      character*(*)    items ( * )
      integer          n
      character*(*)    delim
      character*(*)    list

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEMS      I   Items in the list.
!     N          I   Number of items in the list.
!     DELIM      I   String used to delimit items.
!     LIST       O   List of items delimited by DELIM.
!
! $ Detailed_Input
!
!     ITEMS    are the items to be combined to make the output
!              list. Leading and trailing blanks are ignored.
!              (Only the non-blank parts of the items are used.)
!
!     N        is the number of items.
!
!     DELIM    is the string used to delimit the items in the
!              output list. DELIM may contain any number of
!              characters, including blanks.
!
! $ Detailed_Output
!
!     LIST     is the output list, containing the N elements of
!              ITEMS delimited by DELIM. If LIST is not long enough
!              to contain the output list, it is truncated on the
!              right.
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
!     The non-blank parts of the elements of the ITEMS array are
!     appended to the list, one at a time, separated by DELIM.
!
! $ Examples
!
!     The following examples illustrate the operation of LBUILD.
!
!     1) Let
!              DELIM    = ' '
!
!              ITEMS(1) = 'A'
!              ITEMS(2) = '  number'
!              ITEMS(3) = 'of'
!              ITEMS(4) = ' words'
!              ITEMS(5) = 'separated'
!              ITEMS(6) = '  by'
!              ITEMS(7) = 'spaces'
!
!        Then
!              LIST  = 'A number of words separated by spaces'
!
!     2) Let
!              DELIM    = '/'
!
!              ITEMS(1) = ' '
!              ITEMS(2) = ' '
!              ITEMS(3) = 'option1'
!              ITEMS(4) = ' '
!              ITEMS(5) = 'option2'
!              ITEMS(6) = ' '
!              ITEMS(7) = ' '
!              ITEMS(8) = ' '
!
!        Then
!              LIST  = '//option1//option2///'
!
!     3) Let
!              DELIM    = ' and '
!
!              ITEMS(1) = 'Bob'
!              ITEMS(2) = 'Carol'
!              ITEMS(3) = 'Ted'
!              ITEMS(4) = 'Alice'
!
!        Then
!              LIST  = 'Bob and Carol and Ted and Alice'
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
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
!     build a list in a character_string
!
!-&


!
!     SPICELIB functions
!
      integer          frstnb
      integer          lastnb

!
!     Local variables
!
      integer          lpos
      integer          first
      integer          last
      integer          dlen
      integer          llen
      integer          ilen
      integer          i



!
!     Find the non-blank part of each item. Move it to the
!     end of the list, followed by a delimiter. If the item is
!     blank, don't move anything but the delimiter.
!
!     LPOS is the next position in the output list to be filled.
!     LLEN is the length of the output list.
!     DLEN is the length of DELIM.
!     ILEN is the length of the next item in the list.
!
      list = ' '
      lpos = 1
      llen = len ( list  )
      dlen = len ( delim )

      if ( n .gt. 0 ) then

         do i = 1, n

            if ( lpos .le. llen ) then

               if ( items(i) .eq. ' ' ) then
                  list(lpos: ) = delim
                  lpos         = lpos + dlen

               else
                  first = frstnb ( items(i) )
                  last  = lastnb ( items(i) )
                  ilen  = last - first + 1

                  list(lpos: ) = items(i)(first:last)
                  call suffix ( delim, 0, list )

                  lpos = lpos + ilen + dlen
               end if

            end if

         end do

!
!     We're at the end of the list. Right now, the list ends in
!     a delimiter. Drop it.
!
         if ( lpos-dlen .le. llen ) then
            list(lpos-dlen: ) = ' '
         end if

      end if

      return
      end
