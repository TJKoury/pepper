!$Procedure LPARSM ( Parse a list of items )

      subroutine lparsm ( list, delims, nmax, n, items )

! $ Abstract
!
!     Parse a list of items separated by multiple delimiters.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     LIST
!     PARSING
!     STRING
!
! $ Declarations

      implicit none

      character*(*)    list
      character*(*)    delims
      integer          nmax
      integer          n
      character*(*)    items ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I    List of items delimited by DELIMS.
!     DELIMS     I    Single characters which delimit items.
!     NMAX       I    Maximum number of items to return.
!     N          O    Number of items in the list.
!     ITEMS      O    Items in the list, left justified.
!
! $ Detailed_Input
!
!     LIST     is a list of items delimited by any one of the
!              characters in the string DELIMS. Consecutive
!              delimiters, and delimiters at the beginning and
!              end of the list, are considered to delimit blank
!              items. A blank list is considered to contain
!              a single (blank) item.
!
!     DELIMS   contains the individual characters which delimit
!              the items in the list. These may be any ASCII
!              characters, including blanks.
!
!              However, by definition, consecutive blanks are NOT
!              considered to be consecutive delimiters. Nor are
!              a blank and any other delimiter considered to be
!              consecutive delimiters. In addition, leading and
!              trailing blanks are ignored.
!
!     NMAX     is the maximum number of items to be returned from
!              the list. This allows the user to guard against
!              overflow from a list containing more items than
!              expected.
!
! $ Detailed_Output
!
!     N        is the number of items in the list. N may be
!              any number between one and NMAX. N is always the
!              number of delimiters plus one.
!
!     ITEMS    are the items in the list, left justified. Any item
!              in the list to long to fit into an element of ITEMS
!              is truncated on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the string length of ITEMS is too short to accommodate
!         an item, the item will be truncated on the right.
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
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Parse a character string to retrieve the words contained
!        within.
!
!
!        Example code begins here.
!
!
!              PROGRAM LPARSM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 DELMLN
!              PARAMETER             ( DELMLN = 1   )
!
!              INTEGER                 NMAX
!              PARAMETER             ( NMAX   = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(DELMLN)      DELIMS
!              CHARACTER*(STRLEN)      ITEMS  ( NMAX )
!              CHARACTER*(STRLEN)      LIST
!
!              INTEGER                 I
!              INTEGER                 N
!
!        C
!        C     Define the list of delimited items.
!        C
!        C     Think of a sentence as a list delimited by a space.
!        C     DELIMS is assigned to a space.
!        C
!              LIST   = 'Run and find out.'
!              DELIMS = ' '
!
!        C
!        C     Parse the items from LIST.
!        C
!              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
!
!        C
!        C     Output the ITEMS.
!        C
!              DO I = 1, N
!
!                 WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I)
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
!        Item  1: Run
!        Item  2: and
!        Item  3: find
!        Item  4: out.
!
!
!     2) Parse a character string to retrieve the items contained
!        within, when then items are separated by multiple delimiters.
!
!
!        Example code begins here.
!
!
!              PROGRAM LPARSM_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 RTRIM
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 DELMLN
!              PARAMETER             ( DELMLN = 5   )
!
!              INTEGER                 NMAX
!              PARAMETER             ( NMAX   = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(DELMLN)      DELIMS
!              CHARACTER*(STRLEN)      ITEMS  ( NMAX )
!              CHARACTER*(STRLEN)      LIST
!
!              INTEGER                 I
!              INTEGER                 N
!
!        C
!        C     Define the list of delimited items.
!        C
!        C     Think of a sentence as a list delimited by a space.
!        C     DELIMS is assigned to a space.
!        C
!              LIST   = '  1986-187// 13:15:12.184 '
!              DELIMS = ' ,/-:'
!
!        C
!        C     Parse the items from LIST.
!        C
!              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
!
!        C
!        C     Output the ITEMS.
!        C
!              DO I = 1, N
!
!                 WRITE(*,'(A,I3,3A)') 'Item', I, ': ''',
!             .                        ITEMS(I)(:RTRIM(ITEMS(I))), ''''
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
!        Item  1: '1986'
!        Item  2: '187'
!        Item  3: ' '
!        Item  4: '13'
!        Item  5: '15'
!        Item  6: '12.184'
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
!
!        Bug fix: code was modified to avoid out-of-range
!        substring bound conditions.
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
!     parse a list of items
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
!
!        Bug fix: code was modified to avoid out-of-range
!        substring bound conditions. The previous version
!        of this routine used DO WHILE statements of the form
!
!                  DO WHILE (      ( B         .LE. EOL   )
!           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
!
!        Such statements can cause index range violations when the
!        index B is greater than the length of the string LIST.
!        Whether or not such violations occur is platform-dependent.
!
!-&


!
!     Local parameters
!
      character*(1)         blank
      parameter           ( blank = ' ' )

      integer               ispace
      parameter           ( ispace = 32 )

!
!     Local variables
!
      character*(1)         bchr
      character*(1)         echr

      integer               b
      integer               e
      integer               eol

!
!     Because speed is essential in many list parsing applications,
!     LPARSM parses the input list in a single pass. What follows
!     is nearly identical to LPARSE, except the Fortran INDEX function
!     is used to test for delimiters, instead of testing each character
!     for simple equality.
!

!
!     Nothing yet.
!
      n = 0

!
!     Blank list contains a blank item.
!
      if ( list .eq. blank ) then

         n        = 1
         items(1) = blank

      else
!
!        Eliminate trailing blanks. EOL is the last non-blank
!        character in the list.
!
         eol = len ( list )

         do while ( ichar(list(eol:eol)) .eq. ispace )
            eol = eol - 1
         end do

!
!        As the King said to Alice: 'Begin at the beginning.
!        Continue until you reach the end. Then stop.'
!
!        When searching for items, B is the beginning of the current
!        item; E is the end.  E points to the next non-blank delimiter,
!        if any; otherwise E points to either the last character
!        preceding the next item, or to the last character of the list.
!
         b = 1

         do while ( b .le. eol )
!
!           Skip any blanks before the next item or delimiter.
!
!           At this point in the loop, we know
!
!              B <= EOL
!
            bchr = list(b:b)

            do while (       ( b           .le. eol    ) &
                       .and. ( ichar(bchr) .eq. ispace ) )

               b = b + 1

               if ( b .le. eol ) then
                  bchr = list(b:b)
               end if

            end do

!
!           At this point B is the index of the next non-blank
!           character BCHR, or else
!
!              B == EOL + 1
!
!           The item ends at the next delimiter.
!
            e = b

            if ( e .le. eol ) then
               echr = list(e:e)
            else
               echr = blank
            end if

            do while (       (  e                     .le. eol ) &
                       .and. (  index( delims, echr ) .eq. 0   )  )

               e = e + 1

               if ( e .le. eol ) then
                  echr = list(e:e)
               end if

            end do

!
!           (This is different from LPARSE. If the delimiter was
!           a blank, find the next non-blank character. If it's not
!           a delimiter, back up. This prevents constructions
!           like 'a , b', where the delimiters are blank and comma,
!           from being interpreted as three items instead of two.
!           By definition, consecutive blanks, or a blank and any
!           other delimiter, do not count as consecutive delimiters.)
!
            if (       ( e           .le. eol    ) &
                 .and. ( ichar(echr) .eq. ispace ) ) then
!
!              Find the next non-blank character.
!
               do while (       ( e           .le. eol    ) &
                          .and. ( ichar(echr) .eq. ispace )  )

                  e = e + 1

                  if ( e .le. eol ) then
                     echr = list(e:e)
                  end if

               end do

               if ( e .le. eol ) then

                  if (  index( delims, echr ) .eq. 0  ) then
!
!                    We're looking at a non-delimiter character.
!
!                    E is guaranteed to be > 1 if we're here, so the
!                    following subtraction is valid.
!
                     e = e - 1

                  end if

               end if

            end if

!
!           The item now lies between B and E. Unless, of course, B and
!           E are the same character; this can happen if the list
!           starts or ends with a non-blank delimiter, or if we have
!           stumbled upon consecutive delimiters.
!
            n = n + 1

            if ( e .gt. b ) then
               items(n) = list(b:e-1)
            else
               items(n) = blank
            end if

!
!           If there are more items to be found, continue with
!           character following E (which is a delimiter).
!
            if ( n .lt. nmax ) then
               b = e + 1
            else
               return
            end if

         end do

!
!        If the list ended with a (non-blank) delimiter, add a
!        blank item to the end.
!
         if (       ( index( delims, list(eol:eol) ) .ne. 0    ) &
              .and. (  n                             .lt. nmax )  ) then

            n        = n + 1
            items(n) = blank

         end if

      end if

      return
      end
