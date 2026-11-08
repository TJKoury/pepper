!$Procedure DPSTR ( Double Precision Number to Character )

      subroutine dpstr ( x, sigdig, string )

! $ Abstract
!
!     Take a double precision number and convert it to
!     an equivalent character string representation (base 10).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     CONVERSION
!     PARSING
!
! $ Declarations

      implicit none

      double precision x
      integer          sigdig
      character*(*)    string

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   A double precision number
!     SIGDIG     I   The number of significant digits placed in output
!     STRING     O   A character string representation of X
!
! $ Detailed_Input
!
!     X        is a double precision number.
!
!     SIGDIG   is the number of significant digits that are desired
!              for the output string.
!
! $ Detailed_Output
!
!     STRING   is a character representation of X to the number of
!              significant digits specified by SIGDIG. The number of
!              spaces required to return the requested character
!              string is SIGDIG + 6. If STRING is not declared to
!              have adequate length, the number returned will be
!              truncated on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If SIGDIG is less than one, this routine returns one
!         significant digit in the output string.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes an approximate character representation
!     of the input string X. The maximum number of significant
!     digits returned is 14. The representation returned will be
!     the same as that given by the FORTRAN write statement
!
!         WRITE ( STRING, FMT=(P1E23.xx)
!
!     where xx is a two digit number that represents MIN(14,SIGDIG).
!     The last decimal place is rounded. The output string is left
!     justified.
!
!     This routine has the advantage that it does not use an internal
!     file and is about 2.3 times as fast as an internal write. It can
!     be used as part of character function without fear of introducing
!     recursive I/O conflicts. It is intended to be an approximate
!     inverse to the subroutine NPARSD.
!
!     There is of course no formatting of the output string. All
!     outputs are written in scientific notation.
!
!     IF you want the character string representation of a double
!     precision number to be the same as that produced by a formatted
!     write statement use a FORTRAN write statement.
!
!     For example the number represented by the string
!
!           1.245454545454545454545E+01
!
!     when read (via a FORTRAN READ statement) into the DP variable X
!     and converted back to a character string having 14 significant
!     digits by this routine yields
!
!           1.2454545454545E+01
!
!     The FORTRAN write statement
!
!           WRITE ( 6, FMT='(P1E)' ) X
!
!     yields
!
!           1.2454545454545454E+01
!
!     If this is too much error for your application DO NOT use this
!     routine. You should be aware however, that a character string
!     read into a double precision number may not WRITE out with an
!     equivalent character representation as was input.
!
!     For example on a VAX 11/780 if you
!
!           READ  (5,*)         X
!           WRITE (6,FMT='(E)') X
!
!     and enter a value of 7.00000001 for the read statement
!     the output written will be 0.7000000010000001E+01
!
! $ Examples
!
!     This routine is intended for use by routines that manipulate
!     character strings. For example, it may be desirable for a
!     routine to be able to take a character string input such as
!
!           12 miles
!
!     and convert it to the string
!
!           1.932E+02 km
!
!     or to simply
!
!           1.932E+02
!
!     The arithmetic is of course most easily handled using numeric
!     variables. However, it may be that a string is required for
!     subsequent processing of the input.  A SPICELIB routine NPARSD
!     exists that will take a character representation of a number
!     and convert it to a DOUBLE PRECISION number. The 12 above
!     can be converted to double precision using NPARSD,  the d.p.
!     number can then be multiplied by the 1.61... and the result
!     converted back to a string using this routine.
!
!     Suppose the following declarations are made
!
!           CHARACTER*(80)     TEXT
!           CHARACTER*(80)     NUMBER
!           CHARACTER*(80)     SCRATCH
!
!           DOUBLE PRECISION   X
!           INTEGER            I
!
!     and that TEXT contains the string '12 mi'.  Then the following
!     code would produce a character string  '1.932E+01 KM'
!
!           CALL NEXTWD (  TEXT,   NUMBER, SCRATCH   )
!           CALL NPARSD (  NUMBER, X,      ERROR,  I )
!
!           IF ( ERROR .EQ. ' ' ) THEN
!
!              X    = X * 1.61D0
!              CALL DPSTR ( X, 5, NUMBER )
!              TEXT = NUMBER(1:10) // 'KM'
!
!           ELSE
!              .
!              .
!              create an error message, try again, etc.
!              .
!              .
!           END IF
!
! $ Restrictions
!
!     1)  The format of the string returned by this routine is used in
!         DPSTRF which is in the call tree to DPFMT. Changes to the
!         format of the output string may have unexpected consequences
!         for these SPICE routines. Please check those routines before
!         modifying this routine.
!
!     2)  The maximum number of significant digits returned is 14.
!
!     3)  If the output string is not declared to be adequately large
!         (at least SIGDIG + 6), the numeric string will be truncated to
!         the side opposite its justification.
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
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 09-SEP-1996 (WLT)
!
!        Added a reference to the header concerning the dependency
!        of the SPICE routines DPSTRF and DPFMT on the format of
!        the string produced by this routine.
!
!-    SPICELIB Version 1.1.0, 11-JUN-1992 (WLT)
!
!        A bug that caused this routine to have a floating point
!        overflow for values of X close to zero was corrected. In
!        addition the restriction on range of exponents supported
!        has been removed.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     d.p. number to character
!
!-&


! $ Revisions
!
!-     SPICELIB Version 1.1.0, 14-OCT-1992 (WLT)
!
!         A bug that caused this routine to have a floating point
!         overflow for values of X close to zero was corrected. In
!         addition the restriction on range of exponents supported
!         has been removed.
!
!-     Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
!
!         Header was changed to reflect the "error free" status
!         of the module, and a comment was added stating what the
!         routine does if SIGIDG is less than one.
!
!         Declaration of the unused variable FRAC removed.
!
!-&


!
!     Maximum number of allowed significant digits.
!
      integer               mostdg
      parameter           ( mostdg = 14 )

!
!     Local variables
!
      integer               maxpow
      parameter           ( maxpow = 17 )

      double precision      power (0:maxpow)
      double precision      ipower(0:maxpow)

      character*(1)         digits(0:9)
      double precision      values(0:9)

      integer               maxexp
      parameter           ( maxexp = 40 )

      character*(2)         vaxexp(0:40)



      double precision      copy
      integer               postn
      integer               expont
      integer               maxsig

      double precision      exp10

      integer               i
      integer               k
      integer               last

      character*(20)        expc
      character*(28)        zero


      character*(32)        numstr

      save                  power
      save                  ipower
      save                  digits
      save                  values
      save                  vaxexp

      data             power        /  1.0d0, &
                                       1.0d1,  1.0d2,  1.0d3,  1.0d4, &
                                       1.0d5,  1.0d6,  1.0d7,  1.0d8, &
                                       1.0d9,  1.0d10, 1.0d11, 1.0d12, &
                                       1.0d13, 1.0d14, 1.0d15, 1.0d16, &
                                       1.0d17                          /

      data             ipower       /1.0d0, &
                                     1.0d-1,  1.0d-2,  1.0d-3,  1.0d-4, &
                                     1.0d-5,  1.0d-6,  1.0d-7,  1.0d-8, &
                                     1.0d-9,  1.0d-10, 1.0d-11, 1.0d-12, &
                                     1.0d-13, 1.0d-14, 1.0d-15, 1.0d-16, &
                                     1.0d-17                          /

      data             digits       /  '0', '1', '2', '3', '4', &
                                       '5', '6', '7', '8', '9'    /

      data             values       /   0.0d0 ,  1.0d0 ,  2.0d0 , &
                                        3.0d0 ,  4.0d0 ,  5.0d0 , &
                                        6.0d0 ,  7.0d0 ,  8.0d0 , &
                                        9.0d0                     /

      data             vaxexp       / '00', '01', '02', '03', '04', &
                                      '05', '06', '07', '08', '09', &
                                      '10', '11', '12', '13', '14', &
                                      '15', '16', '17', '18', '19', &
                                      '20', '21', '22', '23', '24', &
                                      '25', '26', '27', '28', '29', &
                                      '30', '31', '32', '33', '34', &
                                      '35', '36', '37', '38', '39', &
                                      '40'                          /

!
!     Transfer X to the local variable COPY and leave X alone for the
!     rest of the routine.
!
      copy   = x

!
!     Wipe out anything sitting in NUMSTR
!
      numstr = ' '

!
!     At least 1 significant digit is required.  The most allowed is 14.
!     MAXSIG is the integer in this range that is closest to SIGDIG.
!
      maxsig = min ( mostdg, max( 1, sigdig ) )

!
!     Examine COPY to see if its positive, zero, or negative.
!     This determines whether we need a minus sign and where the
!     decimal point needs to go in the output string.
!
      if      ( copy .lt. 0 ) then

         numstr(1:1) = '-'
         copy        =  - copy
         postn       =  2
         numstr(3:3) = '.'

      else if ( copy .gt. 0 ) then

         numstr(1:1) = ' '
         postn       =  2
         numstr(3:3) = '.'

      else
         zero        = ' 0.0000000000000000000000000'
         numstr      = zero(1:maxsig+2) // 'E+00'

         string = numstr
         return

      end if


!
!     We need a first guess at the exponent string.  Compute the LOG
!     base 10 of COPY
!
      exp10  = dlog10(copy)


!
!     Scale our copy of the input into the range 1 to 10.
!
      if ( exp10 .lt. 0 ) then

!
!        In this case the exponent will be negative.  We want the
!        largest integer exponent less than EXP10,  but the FORTRAN
!        INT function gives the INTEGER closest to EXP10 between EXP10
!        and zero.  As a result we have to subtract 1 from INT(EXP10).
!
         expont =  int(exp10) - 1
         k      = -expont

         do while ( k .gt. 16 )
            copy = copy * 1.0d16
            k    = k - 16
         end do

         if ( k .ne. 0 ) then
            copy = copy * power(k)
         end if


      else

         expont = int ( exp10 )
         k      = expont

         do while ( k .gt. 16 )
            copy = copy * 1.0d-16
            k    = k - 16
         end do

         if ( k .ne. 0 ) then
            copy = copy * ipower(k)
         end if


      end if



!
!     Round off the last significant digit.
!
      copy   = (dnint( copy*power(maxsig-1)) + 0.125d0 ) &
             * ipower(maxsig-1)

!
!     We might have accidentally made copy as big as 10 by the
!     round off process.  If we did we need to divide by 10 and add 1
!     to the exponent value.  (COPY must always remain between 0 and 10)
!
      if ( copy .ge. 10.0d0 ) then
         copy   = copy * 1.0d-1
         expont = expont + 1
      end if
!
!     Get the first digit of the decimal expansion of X.
!
      i                   = int ( copy )
      numstr(postn:postn) = digits(i)

      copy                = ( copy - values(i) ) * 10.0d0

!
!     Set the string pointer to the next position and compute the
!     position of the last significant digit
!
      postn               = postn + 2
      last                = postn + maxsig - 1

!
!     Fetch digits until we fill in the last available slot for
!     significant digits.
!
      do while ( postn  .lt. last )

         i                   = int ( copy )
         numstr(postn:postn) = digits(i)
         copy                = ( copy - values(i) ) * 10.0d0
         postn               = postn + 1

      end do

!
!     Tack on the exponent to the output. Note that the rather odd
!     if, else if, else construction below is done to maintain backward
!     compatibility of the "look" of the output.
!
!     First get the exponent symbol and sign of the exponent.
!
      if   ( expont .ge. 0  ) then

         numstr(postn:) = 'E+'

      else

         expont         = - expont
         numstr(postn:) = 'E-'

      end if

      postn = postn + 2

!
!     Now get the numeric representation.
!
      if ( expont .le. 40 ) then
         expc  = vaxexp ( expont )
      else
         call intstr ( expont, expc )
      end if

      numstr(postn:) = expc
      string         = numstr

!
!     That's all folks.
!
      return

      end
