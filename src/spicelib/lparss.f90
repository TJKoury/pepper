!$Procedure LPARSS ( Parse a list of items; return a set. )

      subroutine lparss ( list, delims, set )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Parse a list of items delimited by multiple delimiters,
!     placing the resulting items into a set.
!
! $ Required_Reading
!
!     CELLS
!     SETS
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SETS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          list
      character(*)          delims
      character(*)          set  ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I   List of items delimited by DELIMS on input.
!     DELIMS     I   Single characters which delimit items.
!     SET        O   Items in the list, validated, left justified.
!
! $ Detailed_Input
!
!     LIST     is a list of items delimited by any one of the characters
!              in the string DELIMS. Consecutive delimiters, and
!              delimiters at the beginning and end of the list, are
!              considered to delimit blank items. A blank list is
!              considered to contain a single, blank item. Leading and
!              trailing blanks in list are ignored.
!
!     DELIMS   contains the individual characters which delimit the
!              items in the list. These may be any ASCII characters,
!              including blanks.
!
!              However, by definition, consecutive blanks are NOT
!              considered to be consecutive delimiters. Nor are a blank
!              and any other delimiter considered to be consecutive
!              delimiters. In addition, leading and trailing blanks are
!              ignored.
!
! $ Detailed_Output
!
!     SET      is a SPICE set containing the items in the list, left
!              justified. Any item in the list too long to fit into an
!              element of SET is truncated on the right.
!
!              The strings in SET will be sorted in increasing order,
!              and duplicates will be removed. Trailing blanks are
!              ignored in string comparisons.
!
!              The size of the set must be initialized prior to calling
!              LPARSS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the size of the set is not large enough to accommodate all
!         of the items in the set, an error is signaled by a routine in
!         the call tree of this routine.
!
!     2)  If the string length of SET is too short to accommodate an
!         item, the item will be truncated on the right.
!
!     3)  If the string length of SET is too short to permit encoding of
!         integers via the SPICELIB routine ENCHAR, an error is signaled
!         by a routine in the call tree of this routine.
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
!     The following examples illustrate the operation of LPARSS.
!
!     1) Let
!              LIST        = 'A number of words   separated   by
!                              spaces.'
!              DELIMS      = ' ,.'
!              SIZE (SET)  = 20
!
!        Then
!
!              CARDC (SET) = 8
!
!              SET (1)     = ' '
!              SET (2)     = 'A'
!              SET (3)     = 'by'
!              SET (4)     = 'number'
!              SET (5)     = 'of'
!              SET (6)     = 'separated'
!              SET (7)     = 'spaces'
!              SET (8)     = 'words'
!
!
!     2) Let
!
!              LIST        = '  1986-187// 13:15:12.184 '
!              DELIMS      = ' ,/-:'
!              SIZE (SET)  = 20
!
!        Then
!
!              CARDC (SET) = 6
!
!              SET (1)     = ' '
!              SET (2)     = '12.184'
!              SET (3)     = '13'
!              SET (4)     = '15'
!              SET (5)     = '187'
!              SET (6)     = '1986'
!
!
!     3) Let   LIST        = '  ,This,  is, ,an,, example, '
!              DELIMS      = ' ,'
!              SIZE (SET)  = 20
!
!        Then
!              CARDC (SET) = 5
!
!              SET (1)     = ' '
!              SET (2)     = 'This'
!              SET (3)     = 'an'
!              SET (4)     = 'example'
!              SET (5)     = 'is'
!
!
!     4) Let   LIST        = 'Mary had a little lamb, little lamb
!                             whose fleece was white      as snow.'
!              DELIMS      = ' ,.'
!              SIZE (SET)  = 6
!
!        An error would be signaled because the set is not
!        large enough to accommodate all of the items in the
!        list.
!
!
!     5) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
!              DELIMS      = ' .'
!              SIZE (SET)  = 10
!
!        An error would be signaled because the set is not
!        large enough to accommodate all of the items in the
!        list. Note that delimiters at the end (or beginning)
!        of list are considered to delimit blank items.
!
!
!     6) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
!              DELIMS      = '.'
!              SIZE (SET)  = 10
!
!        Then
!
!              CARDC (SET) = 2
!
!              SET (1)     = ' '
!              SET (2)     = '1 2 3 4 5 6 7 8 9 10'
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
!-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Improved
!        documentation of arguments LIST, DELIM and SET.
!
!        Updated entries #2 and #3 in $Exceptions section: changed
!        wrong argument name, and indicated that the routine used
!        for encoding is part of SPICELIB.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) (IMU)
!
!-&


! $ Index_Entries
!
!     parse a list of items and return a set
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
!
!-    Beta Version 2.0.0, 10-JAN-1989 (HAN)
!
!        Error handling was added, and old error flags and their
!        checks were removed. An error is signaled if the set
!        is not large enough to accommodate all of the items in
!        the list.
!
!        The header documentation was updated to reflect the error
!        handling changes, and more examples were added.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lastnb
      integer(ip)           sizec

      logical               failed
      logical               return

!
!     Local parameters
!
      character(1)          blank
      parameter           ( blank = ' ' )

      integer(ip)           ispace
      parameter           ( ispace = 32 )

!
!     Local variables
!
      character(1)          bchr
      character(1)          echr

      integer(ip)           b
      integer(ip)           e
      integer(ip)           eol
      integer(ip)           n
      integer(ip)           nmax

      logical               valid

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'LPARSS' )
      end if

!
!     Because speed is essential in many list parsing applications,
!     LPARSS, like LPARSE, parses the input list in a single pass.
!     What follows is nearly identical to LPARSE, except the FORTRAN
!     INDEX function is used to test for delimiters, instead of testing
!     each character for simple equality. Also, the items are inserted
!     into a set instead of simply placed at the end of an array.
!
!     No items yet.
!
      n = 0

!
!     What is the size of the set?
!
      nmax  = sizec ( set )

!
!     The array has not been validated yet.
!
      valid = .false.

!
!     Blank list contains a blank item.  No need to validate.
!
      if ( list == blank ) then

         call scardc ( 0,     set )
         call insrtc ( blank, set )

         valid = .true.

      else
!
!        Eliminate trailing blanks.  EOL is the last non-blank
!        character in the list.
!
         eol = lastnb ( list )

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

         do while ( b <= eol )
!
!           Skip any blanks before the next item or delimiter.
!
!           At this point in the loop, we know
!
!              B <= EOL
!
            bchr = list(b:b)

            do while (       ( b           <= eol    ) &
                       .and. ( ichar(bchr) == ispace ) )

               b = b + 1

               if ( b <= eol ) then
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

            if ( e <= eol ) then
               echr = list(e:e)
            else
               echr = blank
            end if

            do while (       (  e                     <= eol ) &
                       .and. (  index( delims, echr ) == 0   )  )

               e = e + 1

               if ( e <= eol ) then
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
            if (       ( e           <= eol    ) &
                 .and. ( ichar(echr) == ispace ) ) then
!
!              Find the next non-blank character.
!
               do while (       ( e           <= eol    ) &
                          .and. ( ichar(echr) == ispace )  )

                  e = e + 1

                  if ( e <= eol ) then
                     echr = list(e:e)
                  end if

               end do

               if ( e <= eol ) then

                  if (  index( delims, echr ) == 0  ) then
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
            if ( .not. valid ) then
!
!              If the array has not been validated, it's just an
!              array, and we can insert items directly into it.
!              Unless it's full, in which case we validate now and
!              insert later.
!
               if ( n < nmax ) then

                  n = n + 1

                  if ( e > b ) then
                     set(n) = list(b:e-1)
                  else
                     set(n) = blank
                  end if

               else

                  call validc ( nmax, nmax, set )
                  valid = .true.

               end if

            end if

!
!           Once the set has been validated, the strings are inserted
!           into the set if there's room. If there is not enough room
!           in the set, let INSRTC signal the error.
!
            if ( valid ) then

               if ( e > b ) then
                  call insrtc ( list ( b:e-1 ),   set )
               else
                  call insrtc ( blank,            set )
               end if

               if ( failed() ) then
                  call chkout ( 'LPARSS' )
                  return
               end if

            end if

!
!           If there are more items to be found, continue with the
!           character following E (which is a delimiter).
!
            b = e + 1

         end do

!
!        If the array has not yet been validated, validate it before
!        returning.
!
         if ( .not. valid ) then
            call validc ( nmax, n, set )
         end if

!
!        If the list ended with a (non-blank) delimiter, insert a
!        blank item into the set. If there isn't any room, signal
!        an error.
!
         if ( index( delims, list(eol:eol) ) /= 0 ) then

            call insrtc ( blank, set )
!
!           If INSRTC failed to insert the blank because the set
!           was already full, INSRTC will have signaled an error.
!           No action is necessary here.
!
         end if

      end if


      call chkout ( 'LPARSS' )
      return
      end
