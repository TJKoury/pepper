!$Procedure ZZDDHIVF ( Private --- DDH Identify VAX DAF File Format )

      subroutine zzddhivf ( nsum, bff, found )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Identify VAX DAF file format.
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

      character*(*)         nsum
      integer(ip)           bff
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NSUM       I   String storing the 8 bytes of the FDREC NSUM DP.
!     BFF        O   Integer code indicating the binary file format.
!     FOUND      O   Logical indicating that BFF was determined.
!
! $ Detailed_Input
!
!     NSUM       is a string whose first 8 bytes contain NSUM (the third
!                double precision number) from the first descriptor
!                record of a non-empty DAF suspected to be in one of
!                the VAX binary file formats.
!
! $ Detailed_Output
!
!     BFF        is an integer that signals whether NSUM indicates the
!                DAF is VAX-DFLT or VAX-GFLT.  Possible values are:
!
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.  See it
!                for details.
!
!     FOUND      is a logical that indicates whether the ZZDDHVFF check
!                was successful.  If TRUE, BFF contains the code for
!                VAX-DFLT or VAX-GFLT binary file format.  If FALSE,
!                then BFF is untouched.
!
! $ Parameters
!
!     See the include file 'zzddhman.inc'.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine examines a series of bytes from a potential pre-N0050
!     DAF to determine its architecture, but does not access the file
!     itself.
!
! $ Particulars
!
!     This routine examines the bit patterns stored in NSUM to determine
!     which of the two VAX binary file formats are used to store the
!     double precision values in the DAF file.
!
! $ Examples
!
!     See ZZDDHPPF for sample usage.
!
! $ Restrictions
!
!     1) The first 8 bytes of NSUM must contain the third double
!        precision value from the first descriptor record in a DAF
!        file not in BIG-IEEE binary file format.
!
!     2) The DAF file from which NSUM is extracted must be correct
!        or this routine may produce incorrect results.
!
!     3) Assumes CHARACTER*(1) is byte sized.
!
!     4) Assumes that ICHAR(CHAR(CHARACTER)) yields an integer with
!        the same bit pattern as the source character.
!
! $ Literature_References
!
!     1) Binary File Formats and Code Relying on Function Not Addressed
!        by the ANSI '77 Fortran Standard.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.25.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
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
!-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      character*(1)         carg

      integer(ip)           leader
      integer(ip)           trailr

!
!     Statement Functions
!
      integer(ip)           zzichr

!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
#if defined(SPICE_COMPILER_INTEL)
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This however, is not the case
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
!     Before diving right into the code that examines the bit patterns
!     stored in NSUM, review exactly what checks require completion and
!     why they function.
!
!     When this module is invoked, we already know that the DAF from
!     which NSUM was extracted is little endian, and that it is not
!     a LTL-IEEE file.  This leaves us with one of 3 sources for
!     NSUM:
!
!       (a) A VAX D-Floating file
!       (b) A VAX G-Floating file
!       (c) A damaged file
!
!     In the case of (c) the algorithm outlined below is not guaranteed
!     to produce correct results.  If the case is either (a) or (b),
!     then the routine will correctly determine the source binary file
!     format.  Here's why:
!
!        NSUM is the third double precision number from the first
!        descriptor record of a non-empty DAF file.  This number is
!        an integral valued DP bounded between 1 and 125 inclusive.
!
!        An examination of a binary file created with the following
!        code fragment:
!
!           INCLUDE              'zzddhman.inc'
!
!           DOUBLE PRECISION      DPDATA ( 125 )
!           INTEGER               I
!           INTEGER               LUN
!             .
!             .
!             .
!           CALL GETLUN( LUN )
!
!           DO I = 1, 125
!              DPDATA (I) = DBLE (I)
!           END DO
!
!           OPEN ( UNIT   = LUN,
!          .       FILE   = FNAME,
!          .       STATUS = 'NEW',
!          .       ACCESS = 'DIRECT',
!          .       RECL   = RECL      )
!
!           WRITE ( UNIT = LUN, REC = 1 ) ( DPDATA(I), I = 1, 125 )
!
!           END
!
!        This source file was compiled on a VMS VAX system both with
!        G-Floating and D-Floating options, and executed to produce
!        the binary file of interest.  The bit patterns for each of
!        the 125 entries were compared using the UNIX command 'od'.
!
!        This comparison yielded the fact that these two sets of 125
!        bit patterns did not intersect, and all that remained was to
!        uncover an efficient means of identifying which set a
!        particular member belonged to.
!
!        The following was observed:
!
!           With the exception of the first entry representing the
!           number 1.0D0 in the D-Floating case, all entries
!           appeared as: (hexadecimal byte dump from 'od' output)
!
!              0041 0000 0000 0000
!              4041 0000 0000 0000
!              8041 0000 0000 0000
!                       .
!                       .
!                       .
!              f643 0000 0000 0000
!              f843 0000 0000 0000
!              fa43 0000 0000 0000
!
!           While the G-Floating case:
!
!              1040 0000 0000 0000
!              2040 0000 0000 0000
!              2840 0000 0000 0000
!                       .
!                       .
!                       .
!              7e40 00c0 0000 0000
!              7f40 0000 0000 0000
!              7f40 0040 0000 0000
!
!           The important thing to note is that the fourth entry in
!           G-Floating bit patterns is always '0', and in the
!           D-Floating case (with the exception of the first entry)
!           is always non-zero.  The first entry in the D-Floating
!           table is:
!
!              8040 0000 0000 0000
!
!           It also happens to be the case that the leading value
!           of all G-Floating cases are numbers less than 8.
!           Constructing a series of tests around these observations
!           will produce correct results.  When the input file meets
!           the restrictions non-empty and correct.
!
!     So now all that remains is to lay out the specifics of the test.
!     First extract the leading 4 bits from NSUM(1:1) and the trailing
!     four bits from NSUM(2:2).  Then enter this IF/ELSE IF block:
!
!        If the value of the leading 4 bits from NSUM(1:1) is 8 and
!        the trailing 4 bits from NSUM(2:2) are 0, then the file is
!        of the D-Floating binary format.
!
!        Else if the value of the trailing 4 bits of NSUM(2:2) is
!        non-zero, then the file is also of the D-Floating binary
!        format.
!
!        Else if the value of the leading 4 bits of NSUM(1:1) is
!        strictly less than 8 and the trailing bits of NSUM(2:2)
!        are 0, then the file is of the G-Floating binary format.
!
!        Else the file is not of VAX type.
!
!     This routine could be reimplemented to examine all 8 bytes of
!     each double precision number and compare it to two tables of
!     values.  In the interest of simplicity the preceding option
!     was selected.
!
!
!
!     Convert the first and second characters in NSUM to integers.
!
      leader = zzichr( nsum(1:1) )
      trailr = zzichr( nsum(2:2) )

!
!     Shift the trailing 4 bits off LEADER.
!
      leader = leader / 16

!
!     Subtract the leading bits off TRAILR.
!
      trailr = trailr - ( 16 * (trailr / 16) )

!
!     Now determine what file we are looking at.
!
      if ( ( leader .eq. 8 ) .and. ( trailr .eq. 0 ) ) then

         found = .true.
         bff   = vaxdfl

      else if ( trailr .ne. 0 ) then

         found = .true.
         bff   = vaxdfl

      else if ( ( leader .lt. 8 ) .and. ( trailr .eq. 0 ) ) then

         found = .true.
         bff   = vaxgfl

      else

         found = .false.

      end if

      return
      end
