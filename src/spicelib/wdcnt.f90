!$Procedure WDCNT ( Word Count )

      integer(ip) function wdcnt ( string )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the number of words in a string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     STRING
!     WORD
!
! $ Declarations

      implicit none

      character*(*)    string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!
!     The function returns the number of words in the input string
!     STRING.
!
! $ Detailed_Input
!
!     STRING   is the input string to be parsed. It contains some number
!              of words, where a word is any string of consecutive
!              non-blank characters delimited by a blank or by either
!              end of the string.
!
! $ Detailed_Output
!
!     The function returns the number of words in the input character
!     string STRING.
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
!     WDCNT, like NTHWD and NEXTWD, is useful primarily for parsing
!     input commands consisting of one or more words, where a word is
!     defined to be any sequence of consecutive non-blank characters
!     delimited by either a blank or by either end of the string.
!
! $ Examples
!
!     The following examples illustrate the use of WDCNT.
!
!           WDCNT ( 'Now is the time'  )   = 4
!           WDCNT ( '  for all  '      )   = 2
!           WDCNT ( 'good,men.to_come' )   = 1
!           WDCNT ( ' '                )   = 0
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 10-JAN-2005 (EDW)
!
!        Added logic to prevent the evaluation of STRING(LOC:LOC)
!        if LOC exceeds the length of string. Functionally, the
!        evaluation had no effect on WDCNT's output, but the NAG
!        F95 compiler flagged the evaluation as an array
!        overrun error. This occurred because given:
!
!            A .AND. B
!
!        NAG evaluates A then B then performs the logical
!        comparison.
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
!     word count
!
!-&


!
!     Local variables
!
      integer(ip)      n
      integer(ip)      loc
      integer(ip)      length
      logical          cont


!
!     This is just NTHWD, except that it keeps looking until
!     it finds the last word.
!

!
!     Trivial case first.
!
      if ( string .eq. ' ' ) then

         wdcnt = 0
         return

      else

         length = len ( string )

      end if

!
!     Skip leading blanks.
!
      loc = 1

      do while ( string(loc:loc) .eq. ' ' )
         loc = loc + 1
      end do

!
!     Keep stepping through STRING, counting words as we go.
!     (The current word is ended whenever a blank is encountered.)
!     Quit when the end of the string is reached.
!
!     N is the number of words found so far.
!     LOC is the current location in STRING.
!
      n = 1

      do while ( loc .lt. length )

         loc = loc + 1

!
!        Blank signals end of the current word.
!
         if ( string(loc:loc) .eq. ' ' ) then

!
!           Skip ahead to the next word. Ensure no
!           evaluation of STRING(LOC:LOC) when
!           LOC = LENGTH+1.
!
            cont = loc .le. length
            if( cont ) then
               cont = cont .and. string(loc:loc) .eq. ' '
            end if

            do while ( cont )
               loc = loc + 1

               cont = loc .le. length
               if( cont ) then
                  cont = cont .and. string(loc:loc) .eq. ' '
               end if

            end do

!
!           If not at the end of the string, we have another word.
!
            if ( loc .le. length ) then
               n = n + 1
            end if

         end if

      end do

!
!     Return the number of words found.
!
      wdcnt = n

      return
      end
