!$Procedure DPFMT ( Format a double precision number )

      subroutine dpfmt ( x, pictur, str )

! $ Abstract
!
!     Create a formatted string that represents a double precision
!     number, using a format picture.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     CONVERSION
!     UTILITY
!
! $ Declarations

      implicit none

      double precision      x
      character*(*)         pictur
      character*(*)         str

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   a double precision number.
!     PICTUR     I   a string describing the appearance of the output
!     STR        O   a string representing X as prescribed by PICTUR
!
! $ Detailed_Input
!
!     X        is any double precision number.
!
!     PICTUR   is a string used to describe the format of the
!              output string. There are four special characters
!              recognized by DPFMT --- a leading + or -, a leading
!              zero ( '0' ) or a zero that follows a leading + or -,
!              and the first decimal point of the string.
!
!              All other non-blank characters are regarded as
!              equivalent. The picture ends at the first blank
!              character. The effects associated with the various
!              characters in a picture are spelled out in the
!              description of the output STRING.
!
!              The following pictures are treated as errors.
!
!              ' ', '+', '-', '.', '+.', '-.'
!
! $ Detailed_Output
!
!     STRING   is a string representing X that matches the input
!              picture. The format of STRING is governed by PICTUR.
!              It will represent X rounded to the level of precision
!              specified by PICTUR.
!
!              If the first character of the picture is a minus sign,
!              the first character in the output string will be
!              a blank if the number is non-negative, a minus sign
!              if the number is negative.
!
!              If the first character of the picture is a plus sign,
!              the first character of the output string will be a
!              plus if the number is positive, a blank if the number
!              is zero, and a minus sign if the number is negative.
!
!              If the first character of the string is NOT a sign
!              (plus or minus) the first character of the output
!              string will be a minus sign if the number is negative
!              and will be the first character of the integer part
!              of the number otherwise.
!
!              The integer portion of STRING will contain the same
!              number of characters as appear before the decimal
!              point (or last character if there is no decimal
!              point) but after a leading + or -.
!
!              If the picture begins with any of the following
!
!                 '+0', '-0', or '0'
!
!              it is said to have a leading zero. If a picture has
!              a leading zero and the integer portion is not large
!              enough to fill up the integer space specified by
!              PICTUR, STRING will be zero padded from the sign (if
!              one is required) up to the first character of the
!              integer part of the number.
!
!              If picture does NOT have a leading zero and the
!              integer portion is not large enough to fill up the
!              space specified by PICTUR, STRING will be blank
!              padded on the left between the sign (if one is
!              required) and the first character of the integer part
!              of the number.
!
!              If a decimal point ( '.' ) is present in PICTUR it
!              will be present following the integer portion of
!              STRING. Moreover, the decimal portion of STRING will
!              contain the same number of digits as there are
!              non-blank characters following the decimal point in
!              PICTUR. However, only the first 14 digits starting
!              with the first non-zero digit are meaningful.
!
!              If the format specified by PICTUR does not provide
!              enough room for the integer portion of X, the routine
!              determines whether or not the number of characters
!              present in the picture is sufficient to create a
!              representation for X using scientific notation. If
!              so, the output is displayed using scientific notation
!              (leading signs, if they are present in PICTUR, will
!              also appear in STRING).   If the format specified by
!              PICTUR is too short to accommodate scientific
!              notation, the output string is filled with '*' to the
!              same length as the length of PICTUR. Leading signs
!              are not preserved in this overflow case.
!
!              STRING may overwrite PICTUR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If PICTUR begins with a blank, the error SPICE(NOPICTURE) is
!         signaled.
!
!     2)  If PICTUR consists only of '+', '-', '.', '+.' or '-.' are
!         regarded as invalid (there's no significant component to the
!         picture.) therefore, the error SPICE(BADPICTURE) is signaled.
!
!     3)  If the length of STR is less than the length of the first
!         non-blank portion of PICTUR, the error SPICE(OUTPUTTOOSHORT)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a mechanism for producing numeric strings
!     formatted according to a user supplied picture. We expect that
!     the string produced by this routine will be used to assist in
!     the construction of a string that can be read by people.
!
!     Note that the process of converting a double precision number
!     to a string, in not precisely invertible even if the string
!     contains all of the significant figures allowed by this
!     routine. You should not anticipate that the string produced
!     by this routine can be "read" into a double precision number
!     to reproduce the double precision number X. To the level of
!     accuracy implied by the string representation, they will be
!     the same. But, they are unlikely to have the same internal
!     binary representation.
!
! $ Examples
!
!     Suppose that X has the binary representation of PI. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!     PICTUR         |    STRING
!     -------------------------------
!     '0x.xxx'       |  '03.142'
!     'xx.xxx'       |  ' 3.142'
!     '+xxx.yyyy'    |  '+  3.1416'
!     '-.yyyy'       |  '******'
!     'xxxxxxxx'     |  '       3'
!     '00xx'         |  '0003'
!     '-00.0000000'  |  ' 03.1415927'
!     '00'           |  '03'
!     'x.'           |  '3.'
!     '.mynumber'    |  '3.142E+00'
!     'my dog spot'  |  ' 3'
!     'my.dog spot'  |  ' 3.142'
!     '+my.dog,spot' |  '+ 3.14159265'
!
!
!
!     Suppose that X has the binary representation of 2/3. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!     PICTUR         |    STRING
!     -------------------------------
!     '+x.xxx'       |  '+0.667'
!     '+xx.xxx'      |  '+ 0.667'
!     'xxx.yyyy'     |  '  0.6667'
!     '.yyyy'        |  '.6667'
!     'xxxxxxxx'     |  '       1'
!     '00xx'         |  '0001'
!     '-0.0000000'   |  ' 0.6666667'
!     '00'           |  '01'
!     'x.'           |  '1.'
!     'mynumber'     |  '       1'
!     'my dog spot'  |  ' 1'
!     'my.dog spot'  |  ' 0.667'
!     'my.dog,spot'  |  ' 0.66666667'
!
!     Suppose that X has the binary representation of -8/9. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!
!     PICTUR         |    STRING
!     -------------------------------
!     '+x.xxx'       |  '-0.889'
!     '-00.xxxx'     |  '-00.8889'
!     'xxx.xxx'      |  ' -0.889'
!     '000.000'      |  '-00.889'
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 27-OCT-2021 (JDR) (EDW) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
!
!        Removed non-standard end-of-declarations marker
!        'C%&END_DECLARATIONS' from comments.
!
!-    SPICELIB Version 1.0.1, 22-JUN-1998 (WLT)
!
!        A number of typographical and grammatical errors
!        were corrected in the header.
!
!-    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     format a string representing a d.p. number
!     string from a d.p. number and format picture
!
!-&


!
!     SPICELIB functions
!
      integer               pos
      integer               ncpos


      integer               wdsize
      parameter           ( wdsize = 32 )


      character*(1)         fill
      character*(1)         sign
      character*(wdsize)    mystr

      double precision      y

      integer               declen
      integer               dpat
      integer               exp
      integer               expsiz
      integer               frstch
      integer               i
      integer               intlen
      integer               lastch
      integer               firstb
      integer               sgnlen
      integer               sigdig
      integer               sprsiz
      integer               start


      logical               needsn
      logical               ovflow
      logical               shift


!
!     Initial values
!

!
!     Determine where the picture ends.
!
      firstb  = pos( pictur, ' ',  1 )

      if ( firstb .eq. 0 ) then
         lastch = len ( pictur )
      else
         lastch = firstb - 1
      end if

!
!     Make sure there is a picture to worry about.
!
      if ( lastch .eq. 0 ) then

         call chkin  ( 'DPFMT'                                      )
         call setmsg ( 'The format picture must begin with a '       // &
                       'non-blank character. The picture supplied '  // &
                       'was begun with a blank.'                    )
         call sigerr ( 'SPICE(NOPICTURE)'                           )
         call chkout ( 'DPFMT'                                      )
         return

      else if ( lastch .eq. 1 ) then

         if (     pictur .eq. '+' &
             .or. pictur .eq. '-' &
             .or. pictur .eq. '.' ) then

            call chkin ( 'DPFMT' )
            call setmsg ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            call errch  ( '#', pictur(1:1)                    )
            call sigerr ( 'SPICE(BADPICTURE)'                 )
            call chkout ( 'DPFMT'                             )
            return

         end if

      else if ( lastch .eq. 2 ) then

         if (     pictur .eq. '+.' &
             .or. pictur .eq. '-.' ) then

            call chkin ( 'DPFMT' )
            call setmsg ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            call errch  ( '#', pictur(1:2)                    )
            call sigerr ( 'SPICE(BADPICTURE)'                 )
            call chkout ( 'DPFMT'                             )
            return
         end if

      else if ( lastch .gt. len(str) ) then

         call chkin  ( 'DPFMT'                                      )
         call setmsg ( 'The output string is not long enough to ' &
         //            'accommodate a number formatted according ' &
         //            'to the supplied format picture. The ' &
         //            'output string has length #. The output ' &
         //            'picture ''#'' requires # characters. ' )

         call errint ( '#', len(str) )
         call errch  ( '#', pictur(1:lastch) )
         call errint ( '#', lastch )
         call sigerr ( 'SPICE(OUTPUTTOOSHORT)' )
         call chkout ( 'DPFMT'                                      )
         return

      end if

!
!     If we get this far, the routine can go ahead and do its business.
!     Determine the sign of X.  Also, determine how many characters
!     are needed to represent the sign if leading sign is suppressed for
!     positive numbers.
!
      if ( x .gt. 0 ) then
         sign   = '+'
         sprsiz =  0
      else if ( x .lt. 0 ) then
         sign   = '-'
         sprsiz = 1
      else
         sign   = ' '
         sprsiz =  0
      end if
!
!     Look at the picture and see if a leading sign is required and
!     if so whether the sign just determined should use a different
!     character and how many characters are needed for the sign.
!
      if      ( pictur(1:1) .eq. '+' ) then

         needsn = .true.
         sgnlen =  1

      else if ( pictur(1:1) .eq. '-' ) then

         needsn = .true.
         sgnlen = 1

         if ( x .gt. 0 ) then
            sign = ' '
         end if

      else

         if ( x .gt. 0 ) then
            sign = ' '
         end if

         needsn = .false.
         sgnlen =  sprsiz

      end if

!
!     If we need a leading sign. The numeric part of the string
!     will start at character 2.  Otherwise it starts at character 1.
!
      if ( needsn ) then
         start    = 2
      else
         start = 1
      end if

!
!     We can set the sign portion of the string now.
!
      str = sign

!
!     Determine what character should be use for leading characters
!     before the first significant character of the output string.
!
      if ( pictur(start:start) .eq. '0' ) then
         fill   = '0'
      else
         fill  = ' '
      end if

!
!     See if there is a decimal point.
!
      dpat   = pos( pictur, '.',  1 )
!
!     The integer part is the stuff to the left of the first
!     decimal point and that follows the sign (if there is one
!     that is explicitly required.  The length of the decimal
!     portion is the stuff to the right of the decimal point.
!
      if ( dpat .gt. 0 ) then

         intlen = dpat - start
         declen = lastch - dpat

      else

         intlen =  lastch - start + 1
         declen = -1

      end if
!
!     If a sign was not explicitly requested by placing it in
!     the first digit of the picture START will be 1.  If in
!     addition X is less than zero ( SGNLEN will be 1 in this
!     case) we have one fewer digits available for the integer
!     portion of the string than is currently set in INTLEN.
!     Adjust INTLEN to reflect the actual number of digits
!     available.
!
!     Also set the SHIFT flag to .TRUE. so that we know to swap
!     the sign and any blanks that might lie between the sign
!     and the first significant character of the output string.
!
      if ( start .eq. 1 .and. sgnlen .eq. 1 ) then
         intlen = intlen - 1
         shift  = .true.
!
!        If INTLEN has become negative (i.e. -1) the picture
!        must be of the form .xxxxx and the input number must
!        be negative. Add 1 back onto the INTLEN but take one
!        away from the decimal length DECLEN.
!
         if ( intlen .eq. -1 ) then
            intlen = 0
            declen = declen - 1

            if (       declen .eq. 0 &
                 .and. intlen .eq. 0 ) then
!
!              There is no room for anything other than a
!              decimal point.  We simply fill the output
!              string with the '*' character.
!
               do i = 1, lastch
                  str(i:i) = '*'
               end do
               return

            end if

         end if

      else
         shift  = .false.
      end if

!
!     Create the "virtual decimal string" associated with the
!     unsigned part of X.
!
      y   = dabs(x)

      call zzvststr ( y, fill, exp )

!
!     The actual number of digits required to print the unsigned integer
!     portion X is EXP + 1 (provided EXP is at least 0.) We have
!     INTLEN slots available.  So if EXP + 1 is more than INTLEN
!     ( which is equivalent to EXP being at least INTLEN) we don't
!     have enough room to print the unsigned integer portion of the
!     number.
!


      if ( exp .ge. intlen .and. y .ne. 0.0d0 ) then
!
!        See if we have room to print an exponential form.
!        First we need the number of characters for the
!        exponent which is always of the form 'E+dd...'
!
         expsiz = 4 + min(1, exp/1000 ) + min(1,exp/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         sigdig = lastch - sgnlen - expsiz - 1
!
!        If we don't have room for at least one significant digit,
!        there's not much we can do.  Fill the string with '*'.
!

         if ( sigdig .lt. 1 ) then
            do i = 1, lastch
               str(i:i) = '*'
            end do
         else

            call dpstr ( x, sigdig, mystr )
            mystr(1:1) = sign
            call ljust ( mystr, str )
            call rjust ( str(1:lastch), str(1:lastch) )

         end if


         return

      end if
!
!     One more check.  If -INTLEN is greater than DECLEN, or if
!     both are zero, we don't have room to create an output string.
!
      if (  (       intlen .eq. 0 &
              .and. declen .eq. 0 ) &
              .or. -intlen .gt. declen ) then

         do i = 1, lastch
            str(i:i) = '*'
         end do

         return
      end if
!
!     We have a reasonable chance of successfully constructing
!     the string without overflow.
!
      start = sgnlen + 1
      call zzvsbstr ( -intlen, declen, .true., str(start:), ovflow )

!
!     We might be done at this point.  The IF-THEN block below
!     handles the one snag that could arise.
!
!     If the first digit is a zero as a result of rounding it up
!     OVFLOW will be true.  This means we don't have enough room
!     in the picture for the integer portion of the string.  We try
!     to make an exponential picture.
!
      if ( ovflow ) then
!
!        See if we have room to print an exponential form.
!
         expsiz = 4 + min(1, exp/1000 ) + min(1,exp/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         sigdig = lastch - sgnlen - expsiz - 1


         if ( sigdig .lt. 1 ) then
            do i = 1, lastch
               str(i:i) = '*'
            end do
         else

            call dpstr ( x, sigdig, mystr )
            mystr(1:1) = sign
            call ljust ( mystr, str )
            call rjust ( str(1:lastch), str(1:lastch) )
            return

         end if

      else if ( shift ) then
!
!        We need to move the sign right until, there are no
!        blanks between it and the next character.
!
         frstch = ncpos ( str, ' -', 1 )

         if ( frstch .gt. 2 ) then
            str(frstch-1:frstch-1) = str(1:1)
            str(1:1)               = ' '
         end if

      end if



      return
      end
