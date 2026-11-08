!$Procedure NPARSD ( Double Precision parsing of a string )

      subroutine nparsd ( string, x, error, ptr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Parse a character string that represents a number and return
!     a double precision value.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     CONVERSION
!     PARSING
!
! $ Declarations

      implicit none

      character(*)     string
      real(wp) x
      character(*)     error
      integer(ip)      ptr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     STRING     I   Character string representing a numeric value.
!     X          O   Double precision value parsed from STRING.
!     ERROR      O   Message indicating whether errors have occurred.
!     PTR        O   Position in string where an error occurred.
!
! $ Detailed_Input
!
!     STRING   is a character string that represents a numeric value.
!              Commas and spaces may be used in this string for
!              ease of reading and writing the number. They
!              are treated as insignificant but non-error-producing
!              characters.
!
!              For exponential representation the characters
!              'E','D','e','d' may be used.
!
!              The following are legitimate numeric expressions
!
!               +12.2 e-1
!               -3. 1415 9276
!               1e12
!               E10
!
!              The program also recognizes the following  mnemonics
!              'PI', 'pi', 'Pi', 'pI'
!              '+PI', '+pi', '+Pi', '+pI'
!              '-PI', '-pi', '-Pi', '-pI'
!              and returns the value
!              ( + OR - ) 3.1415 9265 3589 7932 3846 2600 D0 as
!              appropriate.
!
! $ Detailed_Output
!
!     X        double precision parsed value of input string. If an
!              error is encountered, X is not changed.
!
!     ERROR    is a message indicating that the string could
!              not be parsed due to use of an unexpected or misplaced
!              character or due to a string representing a number
!              too large for double precision. If the number was
!              successfully parsed, ERROR will be returned as a blank.
!
!              In particular, blank strings, or strings that do not
!              contain either a digit or exponent character will
!              be regarded as errors.
!
!     PTR      this indicates which character was being used when
!              the error occurred. If no error occurs, PTR is
!              returned as 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the string is non-numeric, PTR indicates the location in
!         the string where the error occurred, and ERROR contains a
!         descriptive error message.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine parses an input character string that represents a
!     number, checks for overflow, unexpected or misplaced
!     characters. It returns the double precision number or an error
!     message.
!
! $ Examples
!
!     Let   LINE = 'DELTA_T_A       =   32.184'
!
!     The following code fragment parses the line and obtains the
!     double precision value.
!
!
!        CALL NEXTWD ( LINE,  FIRST,  REST )
!        CALL NEXTWD ( REST, SECOND,  REST )
!        CALL NEXTWD ( REST,  THIRD,  REST )
!
!        CALL NPARSD (  THIRD,  VALUE, ERROR, PTR    )
!
! $ Restrictions
!
!     1)  Due to rounding errors this routine may not be able to parse
!         the decimal character string representation of the largest
!         and smallest double precision numbers.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.6.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 3.5.0, 15-AUG-2002 (WLT)
!
!        Replaced the call to INSSUB with a call to ZZINSSUB so
!        that this routine can legitimately call itself Error Free
!
!-    SPICELIB Version 3.4.0, 03-DEC-2001 (NJB)
!
!        Added an extra check to make sure that ICHAR of any character
!        of the input string is positive.
!
!-    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!        Removed the error message and storage for the unexpected
!        comma error message. This variable was set but never used,
!        and according to the spec for this routine a comma is a valid
!        delimiter, treated like a space, within numbers.
!
!-    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT)
!
!        Changed error strings from parameters to assignments to
!        compensate for shortcomings of the Absoft FORTRAN compiler
!        on the NeXT.
!
!-    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT)
!
!        The previous version of the routine assumed that the range
!        of values of ICHAR was 0 to 128. That turns out not to be
!        true on some machines. If a character whose ICHAR value is
!        outside this range is detected, it is now handled properly
!        as an unexpected character.
!
!-    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT)
!
!        The previous version of the algorithm interpreted P or p as 1.
!        This was not the intent of the routine and was corrected.
!
!-    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG)
!
!        The basic algorithm was completely re-written. As a result
!        the routine now runs an order of magnitude faster than
!        it did before. In addition, strings that do not contain
!        enough information to assign a value to the string are now
!        regarded as errors. These include blank strings or strings
!        that contain only a sign characters, blanks and commas.
!
!        In addition the error diagnosis and checking for overflow
!        was greatly enhanced.
!
!        Note: strings may now parse with slightly different values
!        from the previous version of NPARSD. The current
!        implementation is more accurate in converting strings to
!        double precision numbers.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 17-APR-1990 (WLT)
!
!        Bug fix. The subscript used to reference individual characters
!        of the input string could sometimes step out of bounds. This
!        went unnoticed until NAIF began compiling with the CHECK=BOUNDS
!        option of the DEC Fortran compiler.
!
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     parse a character_string to a d.p. number
!
!-&


!
!     SPICELIB functions.
!

      real(wp)              dpmax
      external              pi
      real(wp)              pi

!
!     Local Parameters.
!

      real(wp)              base
      parameter           ( base   = 10.0_wp )

      integer(ip)           digit
      parameter           ( digit  = 1   )

      integer(ip)           dpoint
      parameter           ( dpoint = 2   )

      integer(ip)           expont
      parameter           ( expont = 3   )

      integer(ip)           ignore
      parameter           ( ignore = 4   )

      integer(ip)           bpistr
      parameter           ( bpistr = 5   )

      integer(ip)           epistr
      parameter           ( epistr = 6   )

      integer(ip)           sign
      parameter           ( sign   = 7   )

      integer(ip)           ibase
      parameter           ( ibase  = 10  )

      integer(ip)           nknown
      parameter           ( nknown = 128 )

      integer(ip)           test
      parameter           ( test = 16 )

      integer(ip)           errsiz
      parameter           ( errsiz = 160 )

      character(errsiz)     blnkst
      character(errsiz)     toobig
      character(errsiz)     unrcst
      character(errsiz)     unxpch
      character(errsiz)     unxppt
      character(errsiz)     unxpsn

      real(wp)              decval
      real(wp)              divisr
      real(wp)              dpsign (     2     )
      real(wp)              ecount
      real(wp)              expval
      real(wp)              factor
      real(wp)              intbnd
      real(wp)              intval
      real(wp)              lookup ( 0 : ibase )
      real(wp)              maxexp
      real(wp)              minexp
      real(wp)              next
      real(wp)              smlbnd
      real(wp)              value
      real(wp)              values (     nknown )

      integer(ip)           class  ( 0 : nknown )
      integer(ip)           exp
      integer(ip)           i
      integer(ip)           id
      integer(ip)           signdx

      integer(ip)           b
      integer(ip)           blank
      integer(ip)           l
      integer(ip)           m
      integer(ip)           nl
      integer(ip)           nexti
      integer(ip)           thisi


      logical               dodec
      logical               expok
      logical               doexp
      logical               first
      logical               doint
      logical               mantsa
      logical               bpiok
      logical               epiok
      logical               pntok
      logical               roundi
      logical               roundd
      logical               sigchr
      logical               signok
      logical               zeroi
!
!     Save everything.  It's easier than tracking down every
!     little variable that might need to be saved.
!
      save

      data                  lookup  /               1.0_wp, &
                                                   10.0_wp, &
                                                  100.0_wp, &
                                                 1000.0_wp, &
                                                10000.0_wp, &
                                               100000.0_wp, &
                                              1000000.0_wp, &
                                             10000000.0_wp, &
                                            100000000.0_wp, &
                                           1000000000.0_wp, &
                                          10000000000.0_wp  /

      data                  first  / .true. /

      data                  values  / nknown * 0.0_wp /

      data                  class   / 0, nknown * 0 /


      if ( first ) then

         first = .false.
!
!        Set up the error messages
!
         toobig = 'The number represented by the input string is ' &
         //       'too large to be stored as a double precision ' &
         //       'number. '


         unxpch = 'An unexpected character was found while ' &
         //       'attempting to parse the input string. '


         unxppt = 'An unexpected decimal point was found in the ' &
         //       'input string. '


         unxpsn = 'An unexpected sign character was found in the ' &
         //       'input string. '


         blnkst = 'The input string is blank. Blank strings are ' &
         //       'not considered to be numbers. '


         unrcst = 'The input string could not be recognized as a ' &
         //       'number. '

         blank = ichar ( ' ' )

         values( ichar ( '0' ) ) =  0.0_wp
         values( ichar ( '1' ) ) =  1.0_wp
         values( ichar ( '2' ) ) =  2.0_wp
         values( ichar ( '3' ) ) =  3.0_wp
         values( ichar ( '4' ) ) =  4.0_wp
         values( ichar ( '5' ) ) =  5.0_wp
         values( ichar ( '6' ) ) =  6.0_wp
         values( ichar ( '7' ) ) =  7.0_wp
         values( ichar ( '8' ) ) =  8.0_wp
         values( ichar ( '9' ) ) =  9.0_wp
         values( ichar ( '-' ) ) = -1.0_wp
         values( ichar ( '+' ) ) =  1.0_wp

         class( ichar(' ') ) = ignore
         class( ichar(',') ) = ignore

         class( ichar('.') ) = dpoint

         class( ichar('E') ) = expont
         class( ichar('D') ) = expont
         class( ichar('e') ) = expont
         class( ichar('d') ) = expont

         class( ichar('+') ) = sign
         class( ichar('-') ) = sign

         class( ichar('1') ) = digit
         class( ichar('2') ) = digit
         class( ichar('3') ) = digit
         class( ichar('4') ) = digit
         class( ichar('5') ) = digit
         class( ichar('6') ) = digit
         class( ichar('7') ) = digit
         class( ichar('8') ) = digit
         class( ichar('9') ) = digit
         class( ichar('0') ) = digit

         class( ichar('p') ) = bpistr
         class( ichar('P') ) = bpistr
         class( ichar('i') ) = epistr
         class( ichar('I') ) = epistr
!
!        Finally create the numbers that will be used for checking
!        for floating point overflow.
!
!        NOTE: The value for MINEXP may be too small by one, but it
!              really doesn't make any difference, as you're going to
!              underflow anyway, and dividing zero by a number (BASE)
!              still gives you zero.
!
         maxexp = dint ( dlog10( dpmax() ) )
         minexp =-( maxexp + 1 )
         smlbnd = dpmax()/lookup(ibase)

         intbnd = base
         next   = intbnd + 1.0_wp

         do while ( intbnd /= next )

            intbnd = intbnd *  base
            next   = intbnd +  1.0_wp

         end do

         intbnd = intbnd / base
!
!        That takes care of the first pass initializations.
!
      end if
!
!     Here's what's true right now.
!
!     There are no errors.
!     The error pointer doesn't need to point anywhere.
!     It's ok for the next token to be a decimal point.
!     It's ok for the next token to be a sign character.
!     It's ok for the next token to be an exponent marker.
!     It's ok for the next character to be the start of pi.
!
!     We expect to be constructing the integer part of the
!     numeric string.
!
      error     = ' '
      ptr       = 0

      pntok     = .true.
      signok    = .true.
      expok     = .true.
      bpiok     = .true.
      doint     = .true.
      roundd    = .true.
      roundi    = .true.
!
!     Here's some other facts.
!
!     We are not parsing the decimal part of the string.
!     We are not parsing the exponent part of the string.
!     We have not encountered any digits in the mantissa.
!     We have not encountered any significant characters.
!     It's not ok for the next character to be the end of pi (i).
!
      dodec     = .false.
      doexp     = .false.
      mantsa    = .false.
      sigchr    = .false.
      epiok     = .false.
!
!     So far there is no integer, decimal or exponent part to this
!     string.
!
      intval    = 0.0_wp
      decval    = 0.0_wp
      expval    = 0.0_wp
      divisr    = 1.0_wp
      factor    = 1.0_wp
      ecount    = 0.0_wp
!
!     Right now if we encounter a sign, it's part of the mantissa.
!     And until we know better the sign of both the mantissa and
!     exponent are +1 (as opposed to -1).
!
      signdx    = 1
      dpsign(1) = 1.0_wp
      dpsign(2) = 1.0_wp
!
!     Before doing anything else we determine whether or not
!     the input string is empty.
!
      if ( string == ' ' ) then

         error = blnkst
         ptr   = 1
         return

      end if

!
!     We need to find the last non-blank character of the input
!     string.  We shall use the idea of binary searching to locate
!     this character.  At first this may appear to be a bit convoluted
!     when compared to the obvious thing to do (start at the end of
!     the string and step backward until a non-blank character is
!     located).  However, on every machine we've looked at this method
!     locates the last non-blank character much more quickly on average
!     than the obvious method.
!
!     L and B denote the last and beginning characters
!     of the substring we are searching.  NL is the next to last
!     character that we are concerned with and M is the middle of
!     the current search interval ( from B to NL ).
!
      l     =  len(string)
      b     =  1
      nl    =  l - 1

!
!     We want M to be ( B + NL ) / 2   but right now that's L/2
!
      m     =  l / 2

      do while ( l-b > test )
!
!        What is true right now?  The string from L+1 on out
!        is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
!        and M >= B,  B is at least one and if greater than 1
!        there must be a non-blank character between B and the
!        end of the string.
!
         if ( ichar(string(l:l)) /= blank ) then

            b = l

         else if ( string(m:nl) == ' ' ) then

!
!           If you got here, the STRING(L:L) is a blank.
!           The string from L+1 on out is blank.
!           The string from M to NL (=L-1) is blank.  Thus the
!           string from M out is blank.
!
!           M is greater than or equal to B.
!           If M  is less than B + 2, then L will become
!           B or less and there will not be a
!           next pass through the loop.  That means that
!           we will never get to this point again and don't
!           have to worry about the reference STRING(M:NL)
!           giving us an access violation.
!
            l  = m - 1
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         else
!
!           If you get to this point all of the string from
!           L out is blank and L is greater than M.
!           There is a non-blank character between M and NL.
!           If L should get within 16 of B, then the loop
!           will not be executed again.  That means again that
!           we don't have to worry about STRING(M:NL) being
!           an ill formed string.
!
            l  =  nl
            b  =  m
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         end if


!
!        Finally compute NL,the index of the character that precedes
!        L and the new midpoint of the stuff from B to NL.
!
         nl =  l - 1
         m  = (b + nl) / 2

!
!        What's true now?  The string from L+1 on out is blank.
!
      end do

!
!     L is now within 16 characters of the last non-blank character
!     of the input string.  We simply search backward from L to
!     locate this last non-blank.
!
      do while ( ichar(string(l:l)) == blank )
         l = l - 1
      end do


!
!     Begin to collect the number in its various parts: an integer
!     portion, a fractional portion, and an exponent.
!
      do i = 1, l



         id    = ichar(string(i:i))

         if ( id > nknown .or. id < 0 ) then

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            nexti = i + 1
            thisi = i

            call zzinssub ( string, ']', nexti, error )
            call zzinssub ( error,  '[', thisi, error )
            call prefix ( unxpch,  1,         error )

            ptr   = i
            return

!
!        The action taken depends upon the class of the token.
!
         else if ( class(id) == digit ) then
!
!           Once a digit has been encountered, we can no longer
!           allow the string 'PI' or a sign until an exponent
!           character is hit and resets the SIGNOK flag.
!
            bpiok  = .false.
            epiok  = .false.
            signok = .false.
            sigchr = .true.
!
!           If we are constructing the integer part ...
!
            if ( doint  ) then

               mantsa = .true.
!
!              Check the current value of the integer part to
!              make sure we don't overflow.
!
               if ( intval < intbnd ) then

                  intval = intval * base + values(id)

               else
!
!                 Once the integer exceeds a given bound,
!                 we add the rest on as fractional part and
!                 keep track of the factor we will need to
!                 multiply the decimal part by to scale things
!                 appropriately.  We also keep track of the number
!                 we will need to add to the exponent part.
!
                  ecount = ecount + 1
                  factor = factor / base

                  if ( roundi ) then

                     roundi = .false.

                     if ( values(id) > 0.5_wp*base ) then
                        intval = intval + 1.0_wp
                     end if

                  end if

               end if
!
!           ... or the decimal part ...
!
            else if ( dodec ) then

               mantsa = .true.
!
!              There are two cases to consider.  The case in which
!              the integer portion of the string has value 0...
!
               if ( zeroi ) then
!
!                 We can just keep accumulating the decimal part
!                 as an integer.  But we keep track of how many
!                 places past the decimal point the first non-zero
!                 digit occurs.  Note that once the decimal part
!                 exceeds the integer bound, we don't need to do
!                 anything.  The remaining digits cannot contribute
!                 to the value of the decimal part.
!
                  if  ( decval < intbnd ) then

                     decval = decval * base + values(id)
                     ecount = ecount - 1

                  else if ( roundd ) then

                     roundd = .false.

                     if ( values(id) >= 0.5_wp*base ) then
                        decval =  decval + 1.0_wp
                     end if

                  end if
!
!              ...and the case in which the integer portion is not
!              zero.
!
               else
!
!                 In this case, we know there is at least _something_
!                 to the integer part of this string.  We can
!                 stop accumulating the decimal part when the divisor
!                 portion exceeds the integer barrier.  After that
!                 the extra digits can't make any contribution to
!                 the double precision value given to the string.
!
                  if ( divisr < intbnd ) then

                     decval = decval * base + values(id)
                     divisr = divisr * base

                  end if

               end if
!
!           ...or the exponent part of the string.
!
            else if ( doexp  ) then

               if ( expval + ecount > maxexp ) then
!
!                 This number is too big to put into a double
!                 precision number. The marginal case where
!                 EXPVAL + ECOUNT .EQ. MAXEXP will be dealt
!                 with when the integer and fractional parts
!                 of the double precision number are built
!                 at the end of this routine.
!
                  error = toobig
                  ptr   = i
                  return

               else if ( expval + ecount < minexp ) then
!
!                 This number is going to underflow, we can
!                 just stop accumulating exponent. But we don't
!                 stop parsing the string yet. There might be
!                 a bad character lurking somewhere later in the
!                 string.
!
!                 NOTE: It is also possible to underflow when the
!                       value of EXPVAL + ECOUNT is equal to MINEXP,
!                       since an entire 'BASE' scale is not supported
!                       for this particular exponent.
!
               else
!
!                 This is the case we expect.  Just add on the
!                 next part of the exponent.
!
                  expval = expval*base + dpsign(2)*values(id)

               end if
!
!           Even though this character is a digit, its not expected
!           for some reason.  Set the error flag and return.
!
            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == dpoint ) then

            if ( pntok ) then

               bpiok  = .false.
               epiok  = .false.
               pntok  = .false.
               signok = .false.
               dodec  = .true.
               doint  = .false.
               doexp  = .false.
               zeroi  = intval == 0.0_wp

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxppt,  1,         error )

               ptr    = i
               return

            end if

         else if ( class(id) == expont ) then

            sigchr = .true.

            if ( expok ) then

               bpiok  = .false.
               epiok  = .false.
               expok  = .false.
               pntok  = .false.
               dodec  = .false.
               doint  = .false.
               doexp  = .true.
               signok = .true.
               signdx =  2

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxpch,  1,         error )

               ptr   =  i
               return

            end if

         else if ( class(id) == sign ) then

            if ( signok ) then

               dpsign(signdx) = values(id)
               signok         = .false.

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxpsn,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == bpistr ) then

            sigchr = .true.

            if ( bpiok ) then

               doint  = .false.
               dodec  = .false.
               doexp  = .false.
               expok  = .false.
               pntok  = .false.
               bpiok  = .false.
               signok = .false.
               epiok  = .true.

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == epistr ) then

            if ( epiok ) then

               doint  = .false.
               dodec  = .false.
               doexp  = .false.
               expok  = .false.
               pntok  = .false.
               bpiok  = .false.
               signok = .false.
               epiok  = .false.
               mantsa = .true.

               intval = pi()

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub ( error,  '[', thisi, error )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == ignore ) then

!
!           We don't do anything.
!

         else

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            nexti = i + 1
            thisi = i

            call zzinssub ( string, ']', nexti, error )
            call zzinssub ( error,  '[', thisi, error )
            call prefix ( unxpch,  1,         error )

            ptr   = i
            return

         end if

      end do


!
!     If we got through the loop and it's OK to end PI, then we started
!     it but never finished.  This is an error.
!
      if ( epiok ) then
         error = unrcst
         ptr   = l
         return
      end if

!
!     Put together the portion that does not involve an exponent.
!
!     If
!        (1) MANTSA = .TRUE., then we had some explicit part of a
!            number, an  integer part, a fractional part, or both.
!
!        (2) SIGCHR = .TRUE, then we had either:
!
!            (a) MANTSA = .TRUE.
!
!         or
!
!            (b) there was an implicit value associated with the input
!                string. For example, an exponent character followed
!                by an optional exponent would produce a valid number:
!                E+10 --> 1.0d+10. This is due to the fact that this
!                routine emulates an RPN calculator of popular repute,
!                not because it is inherently a good idea.
!
      if ( mantsa ) then
!
!        We had an integer part of the number, a fractional part, or
!        both, so we need to put them together in an appropriate
!        fashion.
!
         value = intval + ( decval / divisr ) * factor

      else if ( sigchr ) then
!
!        We do not have a mantissa, so we had an  implicit mantissa,
!        see above, so we need to set the value to one.
!
         value = 1.0_wp

      else
!
!        We have an error. There were no significant characters in the
!        input character string, and hence we could not parse it into
!        a number. An example of such a string would be: '+  ,,.,,'.
!        So, we will set an appropriate error message and return.
!
         error = unrcst
         ptr   = len(string) + 1
         return

      end if
!
!     Adjust the entered part of the exponent by the amount
!     we "shifted" the decimal point when we were computing
!     the integer and decimal values.
!
      expval = expval + ecount
!
!     Now take care of the exponent contribution to the answer.
!
!     If the exponent is negative ...
!
      if ( expval < 0 ) then

         do while ( expval < -base )

            value  = value  / lookup(ibase)
            expval = expval + base

         end do

         value  = value / lookup( -int(expval) )
!
!     If the exponent is positive ...
!
      else if ( expval > 0 ) then

         do while ( expval > base )
!
!           Make sure that a multiply isn't going to create
!           a number that overflows.
!
            if ( value >= smlbnd ) then

               error = toobig
               ptr   = len(string)+1
               return

            else

               value  = value  * lookup(ibase)
               expval = expval - base

            end if

         end do



         exp = nint(expval)
!
!        Again, make sure that a floating point overflow isn't
!        going to happen.
!
         if ( value < dpmax()/lookup(exp) ) then

            value = value * lookup(exp)

         else

               error = toobig
               ptr   = len(string)+1
               return

         end if

      end if

      x = dpsign(1)*value

      return
      end
