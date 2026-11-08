!$Procedure MATCHI ( Match string against wildcard template )

      logical function  matchi ( string, templ, wstr, wchr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether a string is matched by a template containing
!     wild cards. This routine is case-insensitive.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     COMPARE
!
! $ Declarations

      implicit none

      character(*)     string
      character(*)     templ
      character(1)     wstr
      character(1)     wchr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String to be tested.
!     TEMPL      I   Template (with wild cards) to test against STRING.
!     WSTR       I   Wild string token.
!     WCHR       I   Wild character token.
!
!     The function returns .TRUE. if STRING matches TEMPL and otherwise
!     returns .FALSE.
!
! $ Detailed_Input
!
!     STRING   is the input character string to be tested for a match
!              against the input template. Leading and trailing blanks
!              are ignored.
!
!     TEMPL    is the input template to be tested for a match against
!              the input string. TEMPL may contain wild cards. Leading
!              and trailing blanks are ignored.
!
!     WSTR     is the wild string token used in the input template. The
!              wild string token may represent from zero to any number
!              of characters.
!
!     WCHR     is the wild character token used in the input template.
!              The wild character token represents exactly one
!              character.
!
! $ Detailed_Output
!
!     The function returns .TRUE. when the input string matches the
!     input template, and .FALSE. otherwise. The string and template
!     match whenever the template can expand (through replacement of its
!     wild cards) to become the input string.
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
!     MATCHI ignores leading and trailing blanks in both the string
!     and the template. All of the following are equivalent (they
!     all return .TRUE.).
!
!        MATCHI ( 'ALCATRAZ',     'A*Z',      '*', '%' )
!        MATCHI ( '  ALCATRAZ  ', 'A*Z',      '*', '%' )
!        MATCHI ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' )
!        MATCHI ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' )
!
!     MATCHI is case-insensitive:  uppercase characters match
!     lowercase characters, and vice versa. Wild characters match
!     characters of both cases.
!
! $ Examples
!
!     Let
!
!        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
!        WSTR    = '*'
!        WCHR    = '%'
!
!     Then
!
!        if TEMPL is  '*A*'        MATCHI is   T
!                     'A%D*'                     F
!                     'A%C*'                   T
!                     '%A*'                      F
!                     '%%CD*Z'                 T
!                     '%%CD'                     F
!                     'A*MN*Y*Z'               T
!                     'A*MN*Y*%Z'                F
!                     '*BCD*Z*'                T
!                     '*bdc*z*'                  F
!                     ' *bcD*Z*  '             T
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
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB)
!
!        Corrected example calls in header; made other minor
!        edits to header.
!
!-    SPICELIB Version 1.1.0, 08-JUN-1999 (WLT)
!
!         Fixed comments in detailed output and example sections.
!
!-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     match string against wildcard template
!     test whether a string matches a wildcard template
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           frstnb
      integer(ip)           lastnb
      logical               nechr
      logical               samchi
      logical               samch


!
!     Local Variables
!
      integer(ip)           sfirst
      integer(ip)           tfirst
      integer(ip)           slast
      integer(ip)           tlast

      integer(ip)           slen
      integer(ip)           tlen

      integer(ip)           scur
      integer(ip)           tcur

      integer(ip)           left
      integer(ip)           right

      integer(ip)           i
      integer(ip)           j

      logical               nosubm

!
!     Give the function an initial value of .FALSE.
!
      matchi = .false.

!
!     First let's get everybody's measurements.
!
      sfirst = frstnb ( string )
      slast  = lastnb ( string )
      tfirst = frstnb ( templ  )
      tlast  = lastnb ( templ  )

      tlen   = tlast - tfirst + 1
      slen   = slast - sfirst + 1

      scur   = max ( 1, sfirst )
      tcur   = tfirst


!
!     A blank template matches a blank string, and nothing else.
!
      if ( ( tlast == 0 ) .and. ( slast == 0 ) ) then

         matchi = .true.
         return

      else if ( tlast == 0 ) then

         matchi = .false.
         return

      end if


!
!     The beginning of the string and template must be identical
!     up to the first occurrence of a wild string.
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( scur             <= slast ) &
                 .and. ( .not. samch( templ,tcur, wstr,1) ) )

         if ( nechr ( templ(tcur:tcur) ,    string(scur:scur) ) &
              .and. ( templ(tcur:tcur) /= wchr              ) ) then

            matchi = .false.
            return

         else

            tcur = tcur + 1
            scur = scur + 1

         end if

      end do

!
!     There are a three ways we could have finished the loop above
!     without hitting a wild string.
!
!     Case 1.  Both the string and template ran out of characters at
!     the same time without running into a wild string in the template.
!
      if (       ( tcur > tlast ) &
           .and. ( scur > slast ) ) then

         matchi = .true.
         return

      end if

!
!     Case 2. The template ran out of characters while there were still
!     characters remaining in the in the string.  No match.
!
      if (       ( tcur > tlast ) &
           .and. ( scur <= slast ) ) then

         matchi = .false.
         return

      end if

!
!     Case 3. The string ran out of characters while non-wild characters
!     remain in the template.
!
!     We have to check to see if any non-wild-string characters
!     remain.  If so, we DO NOT have a match.  On the other hand if
!     only wild string characters remain we DO have a match.
!
      if (       ( tcur <= tlast ) &
           .and. ( scur > slast ) ) then

         matchi = .true.

         do i = tcur, tlast
            matchi = matchi .and. ( templ(i:i) == wstr )
         end do

         return

      end if

!
!     OK. There is only one way that you can get to this point.
!     It must be the case that characters remain in both the template
!     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover,
!     to get out of the first loop you had to hit a wild string
!     character.  Find the first non-wild-string character in the
!     template. (If there isn't one, we have a match.)
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( samch ( templ,tcur, wstr,1 )  ) )

         tcur = tcur + 1

      end do

      if ( tcur > tlast ) then

         matchi = .true.
         return

      end if

!
!     Still here? Ok. We have a non-wild-string character at TCUR. Call
!     this position left and look for the right end of the maximum
!     length substring of TEMPL (starting at left) that does not have
!     a wild string character.
!
      left = tcur

      do while (       ( tcur             <=   tlast   ) &
                 .and. ( .not. samch(templ,tcur, wstr,1) ) )

         tcur = tcur + 1

      end do

      right = tcur - 1


      do while ( left <= tlast )

!
!        First see if there is enough room left in the string to
!        match TEMPL(LEFT:RIGHT)
!
         if ( slast - scur < right - left ) then

            matchi = .false.
            return

         end if

!
!        The substring TEMPL(LEFT:RIGHT) might be the end of the
!        string.  In such a case the ends of STRING must match
!        exactly with the end of TEMPL.
!
         if ( right == tlast ) then

            i = slast
            j = tlast

            do while ( j >= left )

               if (      samch (templ,j, wchr,  1 ) &
                    .or. samchi(templ,j, string,i )  ) then

                  j = j - 1
                  i = i - 1

               else

                  matchi = .false.
                  return

               end if

            end do

!
!           If we made it through the loop, we've got a match.
!
            matchi = .true.
            return
         else

!
!           In this case TEMPL(LEFT:RIGHT) is in between wild string
!           characters.  Try to find a substring at or to the right
!           of SCUR in STRING that matches TEMPL(LEFT:RIGHT)
!
            nosubm = .true.

            do while ( nosubm )

               i = scur
               j = left

               do while (     ( j           <= right           ) &
                         .and.(      samchi( string,i, templ,j ) &
                                .or. samch ( wchr,  1, templ,j ) ) )

                  i = i + 1
                  j = j + 1

               end do

!
!              If J made it past RIGHT, we have a substring match
!
               if ( j > right ) then

                  scur   = i
                  nosubm = .false.

!
!              Otherwise, try the substring starting 1 to the right
!              of where our last try began.
!
               else

                  scur = scur + 1

!
!                 Make sure there's room to even attempt a match.
!
                  if ( slast - scur < right - left ) then

                     matchi = .false.
                     return

                  end if

               end if
            end do

         end if

!
!        If you have reached this point there must be something left
!        in the template and that something must begin with a wild
!        string character.  Hunt for the next substring that doesn't
!        contain a wild string character.
!
         do while (       ( tcur             <= tlast ) &
                    .and.   samch ( templ,tcur,  wstr,1) )

            tcur = tcur + 1

         end do

         if ( tcur > tlast ) then

!
!           All that was left was wild string characters.  We've
!           got a match.
!
            matchi = .true.
            return

         end if

!
!        Still here? Ok. We have a non-wild-string character at TCUR.
!        Call this position left and look for the right end of the
!        maximum length substring of TEMPL (starting at left) that
!        does not have a wild string character.
!
         left = tcur

         do while (       ( tcur             <= tlast ) &
                    .and. ( .not. samch( templ,tcur, wstr,1 ) ) )

            tcur = tcur + 1

         end do

         right = tcur - 1

      end do

      end
