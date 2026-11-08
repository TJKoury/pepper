!$Procedure ZZXLATED ( Private --- Translate Double Precision Numbers )

      subroutine zzxlated ( inbff, input, space, output )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Convert double precision values from one binary file format
!     to another.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'zzddhman.inc'

      integer(ip)           inbff
      character(*)          input
      integer(ip)           space
      real(wp)              output ( * )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INBFF      I   Binary file format of d.p. values in INPUT.
!     INPUT      I   String containing d.p. values read as characters.
!     SPACE      I   Number of d.p. values that can be placed in OUTPUT.
!     OUTPUT     O   Translated d.p. values.
!
! $ Detailed_Input
!
!     INBFF      is an integer code that indicates the binary file
!                format of INPUT.  Acceptable values are the
!                parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
!     INPUT      is a string containing a group of d.p. values read
!                from a file as a character string.  The length of
!                this string must be a multiple of the number of
!                bytes used to store a d.p. value in a file utilizing
!                INBFF.
!
!     SPACE      is the number of d.p. values that OUTPUT has room to
!                store.
!
! $ Detailed_Output
!
!     OUTPUT     is an array of double precision values containing
!                the translated values from INPUT into the native
!                binary format.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     This routine signals several SPICE(BUG) exceptions.  They are
!     signaled when improperly specified inputs are passed into the
!     routine or if the module or modules in its calling tree are
!     improperly configured to run on this platform.  Callers that
!     prevent invalid inputs from being passed into this routine
!     need not check in.  See the $Restrictions section for a
!     discussion of input argument restrictions.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine translates double precision values from a non-native
!     binary format read from a file as a sequence of characters to the
!     native format.
!
! $ Examples
!
!     See ZZDAFGSR or ZZDAFGDR.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserves
!        the bit patterns present in the file in memory.
!
!     2) The intrinsic ICHAR preserves the bit pattern of the character
!        byte read from a file.  Namely if one examines the integer
!        created the 8 least significant bits will be precisely those
!        found in the character.
!
!     3) The size of double precision values on the target environment
!        are a multiple of some number of bytes.
!
!     4) The length of the INPUT string is a multiple of the number
!        of bytes for a double precision value in the INBFF format.
!
!     5) INBFF is supported for reading on this platform, and not
!        equivalent to NATBFF on this platform.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.26.1, 01-OCT-2021 (NJB)
!
!        Fixed typo in comments.
!
!-    SPICELIB Version 1.26.0, 15-NOV-2015 (EDW)
!
!        Recast J = 8*(-1+I) + 1 as J = -7+8*I to accommodate
!        f2c processing.
!
!-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           intmin
      integer(ip)           isrchc

      logical               return

!
!     Local Parameters
!

!
!     Length of the double precision and integer buffers that
!     are equivalenced.
!
      integer(ip), parameter :: dpblen = 128

      integer(ip), parameter :: inblen = 2 * dpblen

!
!     These parameters are used for arithmetic shifting.
!
      integer(ip), parameter :: shft8 = 2**8

      integer(ip), parameter :: shft16 = 2**16

      integer(ip), parameter :: shft24 = 2**24

!
!     Local Variables
!
      character(1)          carg
      character(strsiz)     strbff ( numbff )
      character(strsiz)     tmpstr

      real(wp)              dpbufr ( dpblen )

      integer(ip)           bigint
      integer(ip)           i
      integer(ip)           inbufr ( inblen )
      integer(ip)           j
      integer(ip)           k
      integer(ip)           lenipt
      integer(ip)           natbff
      integer(ip)           numdp
      integer(ip)           osign
      integer(ip)           outpos
      integer(ip)           smlint
      integer(ip)           value

      logical               first

!
!     Equivalence DPBUFR to INBUFR.
!
      equivalence         ( dpbufr, inbufr )

!
!     Statement Functions
!
      integer(ip)           zzichr

!
!     Saved Variables
!
      save                  bigint
      save                  smlint
      save                  strbff
      save                  natbff
      save                  first

!
!     Data Statements
!
      data                  first  / .true. /
      data                  natbff / 0      /

!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
#if defined(SPICE_COMPILER_INTEL)
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This, however, is not the case
!     with this particular environment.
#else
!     Some versions of the g77 implement ICHAR with a signed integer.
!     This function computes the value of ICHAR that this code requires
!     on any version of g77 for x86 Linux.
#endif
!
#if defined(SPICE_COMPILER_INTEL)
      zzichr(carg) = ichar(carg)
#else
      zzichr(carg) = ichar(carg) - max( -1, min(0,ichar(carg)) )*256
#endif

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Perform some initialization tasks.
!
      if ( first ) then

!
!        Populate STRBFF.
!
         do i = 1, numbff
            call zzddhgsd ( 'BFF', i, strbff(i) )
         end do

!
!        Fetch the native binary file format.
!
         call zzplatfm ( 'FILE_FORMAT', tmpstr )
         call ucase    ( tmpstr,        tmpstr )

         natbff = isrchc ( tmpstr, numbff, strbff )

         if ( natbff == 0 ) then

            call setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call errch  ( '#', tmpstr                                  )
            call sigerr ( 'SPICE(BUG)'                                 )
            return

         end if


!
!        Store the largest value a 32-bit integer can actually
!        hold.
!
         bigint = 2147483647

!
!        Prepare the smallest value a 32-bit integer can actually
!        store, regardless of what INTMIN returns.
!
         smlint = intmin()

!
!        Set SMLINT to the appropriate value if INTMIN is too large.
!
         if ( smlint == -2147483647 ) then
            smlint = smlint - 1
         end if

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Check to see if INBFF makes sense.
!
      if ( ( inbff < 1 ) .or. ( inbff > numbff ) ) then

         call setmsg ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         call errint ( '#', inbff                                   )
         call sigerr ( 'SPICE(BUG)'                                 )
         return

      end if

!
!     Retrieve the length of the input string, and set the position
!     into the output buffer to the beginning.
!
      lenipt = len(input)
      outpos = 1

!
!     Now branch based on NATBFF.
!
      if ( natbff == bigi3e ) then

         if ( inbff == ltli3e ) then

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing LTL-IEEE
!           d.p. values, and this is a BIG-IEEE machine characters
!           are 1-byte and d.p. values are 8-bytes.  So the length
!           of INPUT must be a multiple of 8.
!
            numdp = lenipt / 8

            if ( lenipt - ( numdp*8 ) /= 0 ) then

               call setmsg ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               call errch  ( '#', strbff(inbff)                    )
               call errch  ( '#', strbff(natbff)                   )
               call sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Verify there is enough room to store the results of
!           the translation.
!
            if ( numdp > space ) then

               call setmsg ( 'The caller specified that # double ' &
               //            'precision numbers are to be ' &
               //            'translated from binary format # to #. ' &
               //            ' However there is only room to hold # ' &
               //            'integers in the output array.  This ' &
               //            'error should never occur.'              )
               call errint ( '#', numdp                               )
               call errch  ( '#', strbff(inbff)                       )
               call errch  ( '#', strbff(natbff)                      )
               call errint ( '#', space                               )
               call sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           The remainder of this branch is devoted to translating
!           and copying blocks of DPBLEN double precision numbers
!           into OUTPUT.  Initialize K, the integer index into the
!           buffer equivalenced to DPBUFR.
!
            k = 1

!
!           Start looping over each 8 character package in INPUT and
!           converting it to double precision numbers.
!
            do i = 1, numdp

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -7 + 8*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing LTL-IEEE:
!
!                      .
!                      .
!                      .
!                   -------
!                  |   J   |  - Least Significant Byte of Mantissa
!                   -------
!                  |  J+1  |  - Sixth Most Significant Mantissa Byte
!                   -------
!                  |  J+2  |  - Fifth Most Significant Mantissa Byte
!                   -------
!                  |  J+3  |  - Fourth Most Significant Mantissa Byte
!                   -------
!                  |  J+4  |  - Third Most Significant Mantissa Byte
!                   -------
!                  |  J+5  |  - Second Most Significant Mantissa Byte
!                   -------
!                  |  J+6  |  - Tail of Exponent, Most Significant
!                   -------     Bits of the Mantissa
!                  |  J+7  |  - Sign Bit, Head of Exponent
!                   -------
!                      .
!                      .
!                      .
!
!              Now rearrange the bytes to place them in the
!              proper order for d.p. values on BIG-IEEE machines.
!              This is accomplished in the following manner:
!
!                     INPUT(J+4:J+4)
!                     INPUT(J+5:J+5)*SHFT8
!                     INPUT(J+6:J+6)*SHFT16
!                  +  INPUT(J+7:J+7)*SHFT24
!                  -------------------------
!                     INBUFR(K)
!
!                     INPUT(J:J)
!                     INPUT(J+1:J+1)*SHFT8
!                     INPUT(J+2:J+2)*SHFT16
!                  +  INPUT(J+3:J+3)*SHFT24
!                  -------------------------
!                     INBUFR(K+1)
!
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value       = zzichr( input((j+4):(j+4)) )
               inbufr(k)   = value

               value       = zzichr( input((j+5):(j+5)) )
               value       = value*shft8
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+6):(j+6)) )
               value       = value*shft16
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+7):(j+7)) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with INBUFR(K).
!
               value     = value*shft24
               inbufr(k) = inbufr(k)+value

!
!              Install the sign bit.  At the moment in INBUFR(K)
!              the bits are precisely as they need to be arranged.
!              Perform the following computations:
!
!                 INBUFR(K) = (2**31-1) - INBUFR(K) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 INBUFR(K) = -INBUFR(K)
!
!              But this negation is effectively:
!
!                 INBUFR(K) = 2**32 - INBUFR(K)
!
!              Which yields:
!
!                 2**32 - (2**31) + INBUFR(K)
!
!              or
!
!                 2**31 + INBUFR(K)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( inbufr(k) == 0 ) then
                     inbufr(k) = smlint
                  else
                     inbufr(k) = bigint-inbufr(k)
                     inbufr(k) = inbufr(k) + 1
                     inbufr(k) = -inbufr(k)
                  end if
               end if

               value       = zzichr( input(j:j) )
               inbufr(k+1) = value

               value       = zzichr( input((j+1):(j+1)) )
               value       = value*shft8
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+2):(j+2)) )
               value       = value*shft16
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+3):(j+3)) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with INBUFR(K).
!
               value       = value*shft24
               inbufr(k+1) = inbufr(k+1)+value

!
!              Install the sign bit.  At the moment in INBUFR(K+1)
!              we have the bits precisely as they need to be
!              arranged.  Perform the computations as with the
!              previous integer.
!
               if ( osign == 1 ) then
                  if ( inbufr(k+1) == 0 ) then
                     inbufr(k+1) = smlint
                  else
                     inbufr(k+1) = bigint-inbufr(k+1)
                     inbufr(k+1) = inbufr(k+1) + 1
                     inbufr(k+1) = -inbufr(k+1)
                  end if
               end if


!
!              Check to see if the local buffer is full and the
!              double precision numbers need to be moved into the
!              next block of OUTPUT.
!
               if ( k == inblen-1 ) then

                  call moved ( dpbufr, dpblen, output(outpos) )
                  outpos = outpos + dpblen
                  k = 1

!
!              Otherwise, increment K.
!
               else
                  k = k + 2
               end if

            end do

!
!           Copy any remaining double precision numbers from DPBUFR
!           into OUTPUT.
!
            if ( k /= 1 ) then
               call moved ( dpbufr, k/2, output(outpos) )
            end if

         else

            call setmsg ( 'Unable to translate double precision ' &
            //            'values from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'     )
            call errch  ( '#', strbff(inbff)                        )
            call errch  ( '#', strbff(natbff)                       )
            call sigerr ( 'SPICE(BUG)'                              )
            return

         end if

      else if ( natbff == ltli3e ) then

         if ( inbff == bigi3e ) then

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing BIG-IEEE
!           d.p. values, and this is a LTL-IEEE machine characters
!           are 1-byte and d.p. values are 8-bytes.  So the length
!           of INPUT must be a multiple of 8.
!
            numdp = lenipt / 8

            if ( lenipt - ( numdp*8 ) /= 0 ) then

               call setmsg ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               call errch  ( '#', strbff(inbff)                    )
               call errch  ( '#', strbff(natbff)                   )
               call sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Verify there is enough room to store the results of
!           the translation.
!
            if ( numdp > space ) then

               call setmsg ( 'The caller specified that # double ' &
               //            'precision numbers are to be ' &
               //            'translated from binary format # to #. ' &
               //            ' However there is only room to hold # ' &
               //            'integers in the output array.  This ' &
               //            'error should never occur.'              )
               call errint ( '#', numdp                               )
               call errch  ( '#', strbff(inbff)                       )
               call errch  ( '#', strbff(natbff)                      )
               call errint ( '#', space                               )
               call sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           The remainder of this branch is devoted to translating
!           and copying blocks of DPBLEN double precision numbers
!           into OUTPUT.  Initialize K, the integer index into the
!           buffer equivalenced to DPBUFR.
!
            k = 1

!
!           Start looping over each 8 character package in INPUT and
!           converting them to double precision numbers.
!
            do i = 1, numdp

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -7 + 8*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing BIG-IEEE:
!
!                      .
!                      .
!                      .
!                   -------
!                  |   J   |  - Sign Bit, Head of Exponent
!                   -------
!                  |  J+1  |  - Tail of Exponent, Most Significant
!                   -------     Bits of the Mantissa
!                  |  J+2  |  - Second Most Significant Mantissa Byte
!                   -------
!                  |  J+3  |  - Third Most Significant Mantissa Byte
!                   -------
!                  |  J+4  |  - Fourth Most Significant Mantissa Byte
!                   -------
!                  |  J+5  |  - Fifth Most Significant Mantissa Byte
!                   -------
!                  |  J+6  |  - Sixth Most Significant Mantissa Byte
!                   -------
!                  |  J+7  |  - Least Significant Byte of Mantissa
!                   -------
!                      .
!                      .
!                      .
!
!              Now rearrange the bytes to place them in the
!              proper order for d.p. values on LTL-IEEE machines.
!              This is accomplished in the following manner:
!
!                     INPUT(J+7:J+7)
!                     INPUT(J+6:J+6)*SHFT8
!                     INPUT(J+5:J+5)*SHFT16
!                  +  INPUT(J+4:J+4)*SHFT24
!                  -------------------------
!                     INBUFR(K)
!
!                     INPUT(J+3:J+3)
!                     INPUT(J+2:J+2)*SHFT8
!                     INPUT(J+1:J+1)*SHFT16
!                  +  INPUT(J:J)*SHFT24
!                  -------------------------
!                     INBUFR(K+1)
!
!
!              Perform the necessary computations. What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value       = zzichr( input((j+7):(j+7)) )
               inbufr(k)   = value

               value       = zzichr( input((j+6):(j+6)) )
               value       = value*shft8
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+5):(j+5)) )
               value       = value*shft16
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+4):(j+4)) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with INBUFR(K).
!
               value     = value*shft24
               inbufr(k) = inbufr(k)+value

!
!              Install the sign bit.  At the moment in INBUFR(K)
!              the bits are precisely as they need to be arranged.
!              Perform the following computations:
!
!                 INBUFR(K) = (2**31-1) - INBUFR(K) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 INBUFR(K) = -INBUFR(K)
!
!              But this negation is effectively:
!
!                 INBUFR(K) = 2**32 - INBUFR(K)
!
!              Which yields:
!
!                 2**32 - (2**31) + INBUFR(K)
!
!              or
!
!                 2**31 + INBUFR(K)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( inbufr(k) == 0 ) then
                     inbufr(k) = smlint
                  else
                     inbufr(k) = bigint-inbufr(k)
                     inbufr(k) = inbufr(k) + 1
                     inbufr(k) = -inbufr(k)
                  end if
               end if

               value       = zzichr( input((j+3):(j+3)) )
               inbufr(k+1) = value

               value       = zzichr( input((j+2):(j+2)) )
               value       = value*shft8
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+1):(j+1)) )
               value       = value*shft16
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input(j:j) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with INBUFR(K).
!
               value       = value*shft24
               inbufr(k+1) = inbufr(k+1)+value

!
!              Install the sign bit.  At the moment in INBUFR(K+1)
!              we have the bits precisely as they need to be
!              arranged.  Perform the computations as with the
!              previous integer.
!
               if ( osign == 1 ) then
                  if ( inbufr(k+1) == 0 ) then
                     inbufr(k+1) = smlint
                  else
                     inbufr(k+1) = bigint-inbufr(k+1)
                     inbufr(k+1) = inbufr(k+1) + 1
                     inbufr(k+1) = -inbufr(k+1)
                  end if
               end if


!
!              Check to see if the local buffer is full and the
!              double precision numbers need to be moved into the
!              next block of OUTPUT.
!
               if ( k == inblen-1 ) then

                  call moved ( dpbufr, dpblen, output(outpos) )
                  outpos = outpos + dpblen
                  k = 1

!
!              Otherwise, increment K.
!
               else
                  k = k + 2
               end if

            end do

!
!           Copy any remaining double precision numbers from DPBUFR
!           into OUTPUT.
!
            if ( k /= 1 ) then
               call moved ( dpbufr, k/2, output(outpos) )
            end if

         else

            call setmsg ( 'Unable to translate double precision ' &
            //            'values from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'     )
            call errch  ( '#', strbff(inbff)                        )
            call errch  ( '#', strbff(natbff)                       )
            call sigerr ( 'SPICE(BUG)'                              )
            return

         end if

!
!     The native binary file format on this platform is not supported
!     for the conversion of integers.  This is a bug, as this branch
!     of code should never be reached in normal operation.
!
      else

         call setmsg ( 'The native binary file format of this ' &
         //            'toolkit build, #, is not currently supported ' &
         //            'for translation of double precision numbers ' &
         //            'from non-native formats.'                      )
         call errch  ( '#', strbff(natbff)                             )
         call sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

      return

      end
