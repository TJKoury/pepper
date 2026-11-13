!> The legacy interface's routines of the subsystem constants (19 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_constants

   implicit none

   private
   public :: b1900, b1950, clight, dpmax, dpmin, dpr, halfpi, intmax, intmin, j1900, j1950, &
      j2000, j2100, jyear, pi, rpd, spd, twopi, tyear

contains

      !> Legacy interface: the constant b1900 of module pepper_constants.
      real(wp) function b1900 ( )

! $ Abstract
!
!     Return the Julian Date corresponding to Besselian Date 1900.0.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2415020.31352, the Julian Date corresponding
!     to Besselian Date 1900.0.
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of B1900.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (B1900).
!        C
!              SPREF = ( JD - B1900() ) * SPD()
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
!          Astronomical Constants," Astron. Astrophys. 73, 282-284,
!          1979.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 13-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added entry [1]
!        to $Literature_References section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     besselian date 1900.0
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_b1900 => b1900
      implicit none
      b1900 = pepper_b1900
      end function b1900

      !> Legacy interface: the constant b1950 of module pepper_constants.
      real(wp) function b1950 ( )

! $ Abstract
!
!     Return the Julian Date corresponding to Besselian Date 1950.0.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2433282.42345905, the Julian Date
!     corresponding to Besselian Date 1950.0.
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of B1950.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (B1950).
!        C
!              SPREF = ( JD - B1950() ) * SPD()
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of
!          Astronomical Constants," Astron. Astrophys. 73, 282-284,
!          1979.
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
!-    SPICELIB Version 2.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 18-AUG-2008 (EDW)
!
!        Edited the value stated in $Detailed_Output to match the
!        current return value. The edit changed:
!
!           2433282.423
!
!        to
!
!           2433282.42345905
!
!-    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT)
!
!        The value of B1950 was updated to reflect the value given
!        by Lieske in [1]
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     besselian date 1950.0
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_b1950 => b1950
      implicit none
      b1950 = pepper_b1950
      end function b1950

      !> Legacy interface: the constant clight of module pepper_constants.
      real(wp) function clight ( )

! $ Abstract
!
!     Return the speed of light in a vacuum (IAU official
!     value, in km/sec).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the IAU official value for the speed of light
!     in vacuum: 299792.458 km/sec.
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     Find the light time corresponding to the length of a given
!     3-dimensional position vector. Length units are km.
!
!     To use CLIGHT, declare it as having double precision type:
!
!        DOUBLE PRECISION      CLIGHT
!
!     Let POS be a 3-vector of interest; let TAU be the light time.
!     VNORM is the SPICELIB function that returns the norm of a
!     3-vector.
!
!        DOUBLE PRECISION      VNORM
!        DOUBLE PRECISION      TAU
!        DOUBLE PRECISION      POS (3 )
!
!     Find the light time:
!
!        TAU = VNORM ( POS ) / CLIGHT ()
!
!     Note that the SPK readers
!
!        SPKEZR
!        SPKEZ
!        SPKPOS
!        SPKEZP
!
!     return the one-way light time between target and observer
!     as an output.
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
!-    SPICELIB Version 1.0.3, 25-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 08-JAN-2008 (NJB)
!
!        $Examples section was updated to remove references to SPKAPP
!        and BODMAT.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     c speed of light in a vacuum
!
!-&

!
!     Just like it says.
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_clight => clight
      implicit none
      clight = pepper_clight
      end function clight

      !> Legacy interface: the constant dpmax of module pepper_constants.
      real(wp) function dpmax ( )

! $ Abstract
!
!     Return the value of the largest (positive) number representable
!     in a double precision variable.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of the largest (positive) number
!     that can be represented in a double precision variable.
!
!     This varies from machine to machine. Values for several popular
!     machines are shown below.
!
!     Environment: VAX/VMS, VAX FORTRAN
!     Value:       1.70141183460469229D+38
!
!     Environment: Sun, Sun FORTRAN
!     Value:       1.7976931348623D+308
!
!     Environment: PC, MS (Microsoft) FORTRAN
!     Value:       1.79769313486231D+308
!
!     Environment: Macintosh, Language Systems FORTRAN
!     Value:       1.7D+308
!
!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Value:       1.79769313486231D+308
!
!     Environment: PC/Linux, Fort77
!     Value:       1.79769313486231D+308
!
!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Value:       1.79769313486231D+308
!
!     Environment: Silicon Graphics, IRIX OS, SGI FORTRAN 77
!     Value:       1.7976931348623158D+308
!
!     Environment: DEC Alpha, OSF/1, DEC FORTRAN-77
!     Value:       1.70141183460469229D+38
!
!     Environment: NeXT, Mach OS, Absoft Fortran 77
!     Value:       (refer to the code below)
!
!     References for the values above are listed in the
!     $Literature_References section of the header.
!
!     IBM PC Note:
!
!        Although the Microsoft FORTRAN documentation lists
!        1.797693134862316D+308 as the maximum double precision number,
!        it produced a compilation error. As a result, we changed the
!        number to 1.79769313486231D+308, which did not produce a
!        compilation error.
!
!     DEC Alpha OSF/1 Note:
!
!        NAIF does not own an Alpha nor its accompanying documentation.
!        The value in this routine was supplied by a SPICELIB user who
!        looked the value up in his manual.
!
!     PC/Linux Fort77 Note:
!
!        Value was validated by experiment. The value is identical to
!        that for the PC/MS Fortran and HP-UX 9000/750, FORTRAN/9000
!        platforms.
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
!     The function always returns a constant value, set by the user
!     prior to compilation.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Return the value of the largest positive number representable
!        in a double precision variable.
!
!        Example code begins here.
!
!              PROGRAM DPMAX_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      DPMAX
!
!        C
!        C     Display the largest positive number representable
!        C     in a double precision variable.
!        C
!              WRITE(*,'(A,E22.14)')
!             .                'Maximum Double Precision Number: ',
!             .                 DPMAX()
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!        Maximum Double Precision Number:   0.17976931348623+309
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation,
!          September 1984, Appendix C, FORTRAN Data Representation,
!          page C-3.
!
!     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation
!          1989, Section 1.3.1, page 13.
!
!     [3]  "Sun FORTRAN Programmer's Guide", Sun Microsystems,
!          Revision A of 6 May 1988, Appendix F, Manual Pages for
!          FORTRAN, page 306 (LIBM_DOUBLE). This routine includes
!          the function D_MAX_NORMAL.
!
!     [4]  "FORTRAN Programmer's Guide for the Sun Workstation",
!          Sun Microsystems, Revision E of 17 February 1986,
!          Appendix E, Manual Pages for FORTRAN, page 184 (RANGE).
!          This routine includes the function DFLMAX.
!
!     [5]  "Language Systems FORTRAN Reference Manual", Language Systems
!          Corporation, version 1.2.1, page 3-12.
!
!     [6]  "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
!          page 95.
!
!     [7]  "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
!          First Edition, June 1991, Hewlett Packard Company, page 4-5.
!
!     [8]  "SGI Fortran 77 Programmer's Guide", Document number
!          007-0711-030, page 2-2.
!
!     [9]  "Language Reference Manual", Absoft Fortran V3.2, 1993,
!          page 3-16, section 3.6.1.5. (for the NeXT)
!
!     [10] "Unix/VMS Compatibility Libraries", Absoft Fortran V3.2,
!          1993; Chapter 3, Support Libraries, page 3-5, dflmax. (for
!          the NeXT)
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.15.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.12.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.7.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.5.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.4.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.2.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.0.4, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.7.0, 07-AUG-1996 (WLT)
!
!        The master program was updated to SAVE the maximum
!        d.p. for the NeXT edition of the toolkit.
!
!-    SPICELIB Version 1.6.0, 05-JAN-1995 (HAN)
!
!        Module was revised to include one assigned value for all
!        Sun operating systems instead of calling different Sun
!        functions to return the value.
!
!-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN) (NJB)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
!
!        Module was updated to include the value of DPMAX for the
!        Hewlett Packard UX 9000/750 environment.
!
!-    SPICELIB Version 1.3.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of DPMAX
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
!
!        Module was updated to include the value of DPMAX
!        for the Macintosh.
!
!-    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN)
!
!        Module was changed to include the function value for
!        the Sun, IBM PC value was changed, and references
!        were added to specify the sources of the function
!        values on different machines.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     largest d.p. number
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.6.0, 05-JAN-1995 (HAN)
!
!        Module was revised to include one assigned value for all
!        Sun operating systems instead of calling different Sun
!        functions to return the value.
!
!-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN) (NJB)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
!
!        Module was updated to include the value of DPMAX for the
!        Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program
!        can create the source file for a specific environment
!        given a master source file.
!
!-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of DPMAX
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
!
!        Module was updated to include the value of DPMAX
!        for the Macintosh.
!
!-    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN)
!
!        Code was changed to include the function value
!        for the Sun. Documentation in the $Particulars
!        section was changed to include the value, the
!        example was updated and corrected.
!
!        Code was changed to update the function value for
!        the IBM PC. The previous value did not compile
!        under the Microsoft FORTRAN Version 5.0. The last
!        digit was deleted from the value, and the module then
!        compiled.
!
!        All sources for the values contained in this module
!        are now specified in the $Literature_References section.
!
!-&

!
!     Numbers are provided in a variety of formats: decimal, hex,
!     and binary. These last two formats are not portable; but then,
!     neither are the values.
!

!
!     IBM PC, Microsoft FORTRAN
!     Lahey F77L EM/32 FORTRAN V 4.0
!     HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     PC/Linux, Fort77
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_dpmax => dpmax
      implicit none
      dpmax = pepper_dpmax
      end function dpmax

      !> Legacy interface: the constant dpmin of module pepper_constants.
      real(wp) function dpmin ( )

! $ Abstract
!
!     Return the value of the smallest (negative) number representable
!     in a double precision variable.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of the smallest (negative) number
!     that can be represented in a double precision variable.
!
!     This varies from machine to machine. Values for several popular
!     machines are shown below.
!
!     Environment: VAX/VMS, VAX FORTRAN
!     Value:       -1.70141183460469229D+38
!
!     Environment: Sun, Sun FORTRAN
!     Value:       -1.7976931348623D+308
!
!     Environment: IBM PC, MS (Microsoft) FORTRAN 5.0
!     Value:       -1.79769313486231D+308
!
!     Environment: Macintosh, Language Systems FORTRAN 1.2.1
!     Value:       -1.7D+308
!
!     Environment: PC/Linux, Fort77
!     Value:       -1.79769313486231D+308
!
!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Value:       -1.79769313486231D+308
!
!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Value:       -1.79769313486231D+308
!
!     Environment: Silicon Graphics, IRIX OS, SGI FORTRAN 77
!     Value:       -1.7976931348623158D+308
!
!     Environment: DEC Alpha, OSF/1, DEC FORTRAN-77
!     Value:       -1.70141183460469229D+38
!
!     Environment: NeXT, Mach OS, Absoft Fortran 77
!     Value:       (refer to the code below)
!
!     References for the values above are listed in the
!     $Literature_References section of the header.
!
!     IBM PC Note:
!
!        Although the Microsoft FORTRAN documentation lists
!        -1.797693134862316D+308 as the minimum negative double
!        precision number, it produced a compilation error. As a result,
!        we changed the number to -1.79769313486231D+308, which did not
!        produce a compilation error.
!
!     DEC Alpha OSF/1 Note:
!
!        NAIF does not own an Alpha nor its accompanying documentation.
!        The value in this routine was supplied by a SPICELIB user who
!        looked the value up in his manual.
!
!     PC/Linux Fort77 Note:
!
!        Value was validated by experiment. The value is identical to
!        that for the PC/MS Fortran and HP-UX 9000/750, FORTRAN/9000
!        platforms.
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
!     The function always returns a constant value, set by the user
!     prior to compilation.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Return the value of the smallest negative number representable
!        in a double precision variable.
!
!        Example code begins here.
!
!              PROGRAM DPMIN_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      DPMIN
!
!        C
!        C     Display the smallest negative number representable
!        C     in a double precision variable.
!        C
!              WRITE(*,'(A,E22.14)')
!             .               'Minimum Double Precision Number: ',
!             .               DPMIN()
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!        Minimum Double Precision Number:  -0.17976931348623+309
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation,
!          September 1984, Appendix C, FORTRAN Data Representation,
!          page C-3.
!
!     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation
!          1989, Section 1.3.1, page 13.
!
!     [3]  "Sun FORTRAN Programmer's Guide", Sun Microsystems,
!          Revision A of 6 May 1988, Appendix F, Manual Pages for
!          FORTRAN, page 306 (LIBM_DOUBLE). This routine includes
!          the function D_MAX_NORMAL.
!
!     [4]  "FORTRAN Programmer's Guide for the Sun Workstation",
!          Sun Microsystems, Revision E of 17 February 1986,
!          Appendix E, Manual Pages for FORTRAN, page 184 (RANGE).
!          This routine includes the function DFLMAX.
!
!     [5]  "Language Systems FORTRAN Reference Manual", Language Systems
!          Corporation, version 1.2.1, page 3-12.
!
!     [6]  "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
!          page 95.
!
!     [7]  "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
!          First Edition, June 1991, Hewlett Packard Company, page 4-5.
!
!     [8]  "SGI Fortran 77 Programmer's Guide", Document number
!          007-0711-030, page 2-2.
!
!     [9]  "Language Reference Manual", Absoft Fortran V3.2, 1993,
!          page 3-16, section 3.6.1.5. (for the NeXT)
!
!     [10] "Unix/VMS Compatibility Libraries", Absoft Fortran V3.2,
!          1993; Chapter 3, Support Libraries, page 3-5, dflmax. (for
!          the NeXT)
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.15.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.12.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.7.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.5.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.4.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.2.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.0.4, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.7.0, 07-AUG-1996 (WLT)
!
!        The master program was updated to SAVE the minimum
!        d.p. for the NeXT edition of the toolkit.
!
!-    SPICELIB Version 1.6.0, 05-JAN-1995 (HAN)
!
!        Module was revised to include one assigned value for all
!        Sun operating systems instead of calling different Sun
!        functions to return the value.
!
!-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN) (NJB)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
!
!        Module was updated to include the value of DPMIN for the
!        Hewlett Packard UX 9000/750 environment.
!
!-    SPICELIB Version 1.3.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of DPMIN
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
!
!        Module was updated to include the value of DPMIN for
!        the Macintosh.
!
!-    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN)
!
!        Module was changed to include the function value for
!        the Sun, IBM PC value was changed, and references
!        were added to specify the sources of the function
!        values on different machines.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     smallest d.p. number
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.6.0, 5-JAN-1995 (HAN)
!
!        Module was revised to include one assigned value for all
!        Sun operating systems instead of calling different Sun
!        functions to return the value.
!
!-    SPICELIB Version 1.5.0, 3-NOV-1993 (HAN) (NJB)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 6-OCT-1992 (HAN)
!
!        Module was updated to include the value of DPMIN for the
!        Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program
!        can create the source file for a specific environment
!        given a master source file.
!
!-    SPICELIB Version 1.2.0, 7-DEC-1990 (MJS)
!
!        Module was updated to include the value of DPMIN for
!        the Macintosh.
!
!-    SPICELIB Version 1.1.0, 9-MAR-1990 (HAN)
!
!        Code was changed to include the function value
!        for Sun. Documentation in the $Particulars
!        section was changed to include the value, the
!        example was updated and corrected.
!
!        Code was changed to update the function value for
!        the IBM PC. The previous value did not compile
!        under the Microsoft FORTRAN Version 5.0. The last
!        digit was deleted from the value, and the module then
!        compiled.
!
!        All sources for the values contained in this module
!        are now specified in the $Literature_References section.
!
!-&

!
!     Numbers are provided in a variety of formats: decimal, hex,
!     and binary. These last two formats are not portable; but then,
!     neither are the values.
!

!
!     IBM PC, Microsoft FORTRAN
!     PC,     Lahey F77 EM/32 FORTRAN
!     HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     PC/Linux, Fort77
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_dpmin => dpmin
      implicit none
      dpmin = pepper_dpmin
      end function dpmin

      !> Legacy interface: the constant dpr of module pepper_constants.
      real(wp) function dpr ( )

! $ Abstract
!
!     Return the number of degrees per radian.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of degrees per radian: 180/pi.
!     The value of pi is determined by the ACOS function. That is,
!
!           DPR = 180.D0 / ACOS ( -1.D0 )
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The code fragment below illustrates the use of DPR.
!
!        C
!        C     Convert all angles to degrees for output.
!        C
!              CLOCK = CLOCK * DPR()
!              CONE  = CONE  * DPR()
!              TWIST = TWIST * DPR()
!
!     or equivalently,
!
!        C
!        C     Convert all input angles to radians.
!        C
!              CALL VPACK  ( CLOCK, CONE, CCTWIST, ALBTGAM )
!              CALL VSCL   ( DPR(), ALBTGAM, ALBTGAM )
!              CALL VUPACK ( ALBTGAM, CLOCK, CONE, CCTWIST )
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     degrees per radian
!
!-&

!
!     Local variables
!

!
!     Initial values
!

!
!     What is there to say?
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_dpr => dpr
      implicit none
      dpr = pepper_dpr
      end function dpr

      !> Legacy interface: the constant halfpi of module pepper_constants.
      real(wp) function halfpi ( )

! $ Abstract
!
!     Return half the value of pi (the ratio of the circumference of
!     a circle to its diameter).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns half the value of pi (the ratio of
!     a circle's circumference to its diameter), determined by
!     the ACOS function. That is,
!
!           HALFPI = ACOS ( -1.D0 ) * 0.5D0
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The subroutine shown below illustrates the use of HALFPI.
!
!                 SUBROUTINE BFTRAN ( RA, DEC, W, TIPM )
!
!           C
!           C     Compute the transformation from inertial to body
!           C     fixed coordinates, given the directions of the north
!           C     pole and prime meridian of the body.
!           C
!                 DOUBLE PRECISION    RA
!                 DOUBLE PRECISION    DEC
!                 DOUBLE PRECISION    W
!                 DOUBLE PRECISION    TIPM ( 3,3 )
!
!           C
!           C     SPICELIB functions
!           C
!                 DOUBLE PRECISION    HALFPI
!
!           C
!           C     The transformation is defined by the compound
!           C     rotation
!           C
!           C        [W] [pi/2 - Dec] [RA + pi/2]
!           C           3            1           3
!           C
!                 CALL ROTATE (       RA + HALFPI(),  3, TIPM)
!                 CALL ROTMAT (TIPM,  HALFPI() - DEC, 1, TIPM)
!                 CALL ROTMAT (TIPM,  W,              3, TIPM)
!
!                 RETURN
!                 END
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
!-    SPICELIB Version 1.0.3, 09-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 08-APR-2015 (JDR)
!
!        Minor edit to example comments eliminating typos.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     half the value of pi
!
!-&

!
!     Local variables
!

!
!     Initial values
!

!
!     What is there to say?
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_halfpi => halfpi
      implicit none
      halfpi = pepper_halfpi
      end function halfpi

      !> Legacy interface: the constant intmax of module pepper_constants.
      integer(ip) function intmax ( )

! $ Abstract
!
!     Return the value of the largest (positive) number representable
!     in an integer variable.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations

! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of the largest (positive) number
!     that can be represented in an integer variable.
!
!     This varies from machine to machine. Values for several popular
!     machines are shown below.
!
!     Environment: VAX/VMS, VAX FORTRAN
!     Value:       2147483647
!
!     Environment: Sun, Sun FORTRAN
!     Value:       INMAX()
!
!     Environment: PC, MS (Microsoft) FORTRAN
!     Value:       2147483647
!
!     Environment: Macintosh, Language Systems FORTRAN
!     Value:       2147483647
!
!     Environment: PC/Linux, Fort77
!     Value:       2147483647
!
!     Environment: PC, Lahey F77 EM/32 Version 4.0
!     Value:       2147483647
!
!     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
!     Value:       2147483647
!
!     Environment: Silicon Graphics, IRIX OS, SGI FORTRAN 77
!     Value:       2147483647
!
!     Environment: DEC Alpha, OSF/1, DEC FORTRAN-77
!     Value:       2147483647
!
!     Environment: NeXT, Mach OS, Absoft Fortran 77
!     Value:       2147483647
!
!     References for the values above are listed in the
!     $Literature_References section of the header.
!
!     PC/Linux Fort77 Note:
!
!      Value was validated by experiment. The value is identical to
!      that for the PC/MS Fortran and HP-UX 9000/750, FORTRAN/9000
!      platforms.
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
!     The function always returns a constant value, set by the user
!     prior to compilation.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Obtain the integer part of a double precision number. If the
!        integer component of the number is out of range, avoid
!        overflow by making it as large or small as possible.
!
!        Example code begins here.
!
!              PROGRAM INTMAX_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               INTMAX
!              INTEGER               INTMIN
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      NUMBER  ( 3 )
!
!              INTEGER               I
!              INTEGER               IVALUE
!
!        C
!        C     Define a set of three numbers, two of them having an
!        C     integer component that is out of range.
!        C
!              DATA                  NUMBER  / 2.D40, -1.5D35, 1.6D0 /
!
!              DO I = 1, 3
!
!                 WRITE(*,*) 'Double precision number: ', NUMBER(I)
!
!        C
!        C        If the integer component is out of range, avoid
!        C        overflow by making it as large as possible.
!        C
!                 IF ( NUMBER(I) .GT. DBLE( INTMAX() ) ) THEN
!
!                    WRITE(*,*) '   Overflow! Greater than INTMAX.'
!                    IVALUE = INTMAX()
!
!                 ELSE IF ( NUMBER(I) .LT. DBLE( INTMIN() ) ) THEN
!
!                    WRITE(*,*) '   Overflow! Smaller than INTMIN.'
!                    IVALUE = INTMIN()
!
!                 ELSE
!
!                    IVALUE = INT( NUMBER(I) )
!
!                 END IF
!
!                 WRITE(*,*) '   Integer part        : ', IVALUE
!                 WRITE(*,*)
!
!              END DO
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Double precision number:    2.0000000000000001E+040
!            Overflow! Greater than INTMAX.
!            Integer part        :   2147483647
!
!         Double precision number:   -1.5000000000000000E+035
!            Overflow! Smaller than INTMIN.
!            Integer part        :  -2147483648
!
!         Double precision number:    1.6000000000000001
!            Integer part        :            1
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation,
!          September 1984, Appendix C, FORTRAN Data Representation,
!          page C-2.
!
!     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation
!          1989, Section 1.3.1, page 10.
!
!     [3]  "Sun FORTRAN Programmer's Guide, Sun Microsystems,
!          Revision A of 6 May 1988, Appendix F, Manual Pages for
!          FORTRAN, page 306 (RANGE).
!
!     [4]  "Language Systems FORTRAN Reference Manual", Language
!          Systems Corporation, version 1.2.1, page 3-2.
!
!     [5]  "Lahey F77L EM/32 Programmers Reference Manual",
!          version 4.0, page 95.
!
!     [6]  "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
!          First Edition, June 1991, Hewlett Packard Company, page 4-4.
!
!     [7]  "SGI Fortran 77 Programmer's Guide", Document number
!          007-0711-030, page 2-2.
!
!     [8]  "Language Reference Manual", Absoft Fortran V3.2, 1993,
!          page 3-14, section 3.6.1.5. (for the NeXT)
!
!     [9]  "Unix/VMS Compatibility Libraries", Absoft Fortran V3.2,
!          1993; Chapter 3, Support Libraries, page 3-14, inmax.
!          (for the NeXT)
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.16.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example from existing code fragment.
!
!-    SPICELIB Version 2.16.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.7.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.5.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.1.0, 30-JUN-2002 (EDW)
!
!        Added environments for Apple OS X G77 (MAC-OSX-G77),
!        and Absoft F77 (MAC-OSX-F77)
!
!-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 09-OCT-1992 (HAN)
!
!        Module was updated to include the value of INTMAX for the
!        Hewlett Packard UX 9000/750 environment.
!
!-    SPICELIB Version 1.3.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of INTMAX
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
!
!        Module was updated to include the value of INTMAX for
!        the Macintosh.
!
!-    SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
!
!        Module was changed to include the function value for
!        the Sun. References were added to specify the sources
!        of the function values on different machines.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     largest integer number
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        Added references to the PC-LINUX environment.
!
!-    SPICELIB Version 1.5.0, 3-NOV-1993 (HAN)
!
!        Module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.4.0, 9-OCT-1992 (HAN)
!
!        Module was updated to include the value of INTMAX for the
!        Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of INTMAX
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.2.0, 7-DEC-1990 (MJS)
!
!        Module was updated to include the value of INTMAX for
!        the Macintosh. $Literature_References section was
!        updated.
!
!-    SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
!
!        Code was changed to include the function value
!        for the Sun. Documentation in the $Particulars
!        section was changed to include the value, and the
!        example was updated and corrected.
!
!        All sources for the values contained in this module
!        are now specified in the $Literature_References section.
!
!-    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
!
!        Contents of the $Exceptions section was changed
!        to "error free" to reflect the decision that the
!        module will never participate in error handling.
!
!        Parentheses added to INTMAX declaration.
!
!-&

!
!     Numbers are provided in a variety of formats: decimal, hex,
!     and binary. These last two formats are not portable; but then,
!     neither are the values.
!

!
!     VAX/VMS,          VAX FORTRAN
!     IBM PC,           Microsoft FORTRAN
!     PC,               Lahey F77L EM/32 FORTRAN
!     Macintosh,        Language Systems FORTRAN
!     HP-UX 9000/750,   FORTRAN/9000 Series 700 computers
!     Silicon Graphics, Silicon Graphics Fortran
!     DEC Alpha-OSF/1,  DEC Fortran
!     NeXT Workstation, Absoft Fortran
!     PC/Linux,         Fort77
!     Apple OS X G77,   Absoft OS X F77
      use pepper_kinds, only: ip
      use pepper_constants, only: pepper_intmax => intmax
      implicit none
      intmax = pepper_intmax
      end function intmax

      !> Legacy interface: the constant intmin of module pepper_constants.
      integer(ip) function intmin ( )

! $ Abstract
!
!     Return the value of the smallest (negative) number representable
!     in an integer variable.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations

! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of the smallest (negative) number
!     that can be represented in an integer variable.
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
!     The function always returns a constant value, set by the user
!     prior to compilation.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Obtain the integer part of a double precision number. If the
!        integer component of the number is out of range, avoid
!        overflow by making it as large or small as possible.
!
!        Example code begins here.
!
!              PROGRAM INTMIN_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               INTMAX
!              INTEGER               INTMIN
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      NUMBER  ( 3 )
!
!              INTEGER               I
!              INTEGER               IVALUE
!
!        C
!        C     Define a set of three numbers, two of them having an
!        C     integer component that is out of range.
!        C
!              DATA                  NUMBER  / 2.D40, -1.5D35, 1.6D0 /
!
!              DO I = 1, 3
!
!                 WRITE(*,*) 'Double precision number: ', NUMBER(I)
!
!        C
!        C        If the integer component is out of range, avoid
!        C        overflow by making it as large as possible.
!        C
!                 IF ( NUMBER(I) .GT. DBLE( INTMAX() ) ) THEN
!
!                    WRITE(*,*) '   Overflow! Greater than INTMAX.'
!                    IVALUE = INTMAX()
!
!                 ELSE IF ( NUMBER(I) .LT. DBLE( INTMIN() ) ) THEN
!
!                    WRITE(*,*) '   Overflow! Smaller than INTMIN.'
!                    IVALUE = INTMIN()
!
!                 ELSE
!
!                    IVALUE = INT( NUMBER(I) )
!
!                 END IF
!
!                 WRITE(*,*) '   Integer part        : ', IVALUE
!                 WRITE(*,*)
!
!              END DO
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Double precision number:    2.0000000000000001E+040
!            Overflow! Greater than INTMAX.
!            Integer part        :   2147483647
!
!         Double precision number:   -1.5000000000000000E+035
!            Overflow! Smaller than INTMIN.
!            Integer part        :  -2147483648
!
!         Double precision number:    1.6000000000000001
!            Integer part        :            1
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation,
!          September 1984, Appendix C, FORTRAN Data Representation,
!          page C-2.
!
!     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation,
!          1989, Section 1.3.1, page 10.
!
!     [3]  "Sun FORTRAN Programmer's Guide", Sun Microsystems,
!          Revision A of 6 May 1988, Appendix F, Manual Pages for
!          FORTRAN, page 306 (RANGE).
!
!     [4]  "Language Systems FORTRAN Reference Manual", Language
!          Systems Corporation, version 1.2.1, page 3-2.
!
!     [5]  "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
!          page 95.
!
!     [6]  "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
!          First Edition, June 1991, Hewlett Packard Company, page 4-4.
!
!     [7]  "SGI Fortran 77 Programmer's Guide", Document number
!          007-0711-030, page 2-2.
!
!     [8]  "Language Reference Manual", Absoft Fortran V3.2, 1993,
!          page 3-14, section 3.6.1.5. (for the NeXT)
!
!     [9]  "Unix/VMS Compatibility Libraries", Absoft Fortran V3.2,
!          1993; Chapter 3, Support Libraries, page 3-14, inmax.
!          (for the NeXT)
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.17.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example from existing code fragment.
!
!-    SPICELIB Version 3.17.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 3.16.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 3.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 3.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.10.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 3.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 3.8.0, 12-DEC-2008 (EDW)
!
!        Updated for PC-LINUX-GFORTRAN and MAC-OSX-GFORTRAN. Eliminated
!        environment descriptions. Most were out-of-date or wrong.
!        IMPLICIT NONE now included in all environments.
!
!        Corrected the PC-CYGWIN expression from
!
!           -2147483647
!
!        to
!
!           -2147483647 -1
!
!-    SPICELIB Version 3.7.0, 14-MAR-2008 (EDW)
!
!        Changed the expression for
!
!           -2147483648
!
!        to
!
!           -2147483647 -1
!
!        to support those compilers that implement "-" as an operator.
!
!-    SPICELIB Version 3.6.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 3.3.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 3.2.1, 31-AUG-2002 (BVS)
!
!        Changed value for SGI compiled with "-n32" from -2147483648 to
!        -2147483647.
!
!-    SPICELIB Version 3.2.0, 30-JUN-2002 (EDW)
!
!        Added environments for Apple OS X G77 (MAC-OSX-G77),
!        and Absoft F77 (MAC-OSX-F77)
!
!-    SPICELIB Version 3.1.3, 18-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 3.1.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 3.1.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 3.1.0, 03-AUG-1998 (FST)
!
!        The value for the PC under Microsoft Powerstation was
!        changed from -2147483647 to -2147483648. The Macintosh
!        value was changed back to -2147483648 as well.
!
!-    SPICELIB Version 3.0.0, 05-APR-1998 (NJB)
!
!        Added reference to the PC-LINUX environment.
!
!-    SPICELIB Version 2.0.0, 09-NOV-1993 (HAN) (MJS)
!
!        The value for the Macintosh has been changed from
!        -2147483648 to -2147483647.
!        Also, the module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.5.0, 09-OCT-1992 (HAN)
!
!        Module was updated to include the value of INTMIN for the
!        Hewlett Packard UX 9000/750 environment.
!
!-    SPICELIB Version 1.4.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.4.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of INTMIN
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.3.0, 19-SEP-1991 (HAN) (NJB)
!
!        The minimum integer is -2147483648. Because the compiler
!        (version 1.3.1) does not accept this value in a direct
!        assignment, the value of INTMIN is specified as
!        ( -INMAX() ) - 1.
!
!-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
!
!        Module was updated to include the value of INTMIN for
!        the Macintosh.
!
!-    SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
!
!        Module was changed to include the function value for
!        the Sun. References were added to specify the sources
!        of the function values on different machines.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     smallest integer number
!
!-&

! $ Revisions
!
!-    SPICELIB Version 3.1.0, 03-AUG-1998 (FST)
!
!        The value for the PC under Microsoft Powerstation was
!        changed from -2147483647 to -2147483648. The Macintosh
!        value was changed back to -2147483648 as well.
!
!-    SPICELIB Version 3.0.0, 05-APR-1998 (NJB)
!
!        Added reference to the PC-LINUX environment.
!
!-    SPICELIB Version 2.0.0, 9-NOV-1993 (HAN) (MJS)
!
!        The value for the Macintosh has been changed from
!        -2147483648 to -2147483647.
!        Also, the module was updated to include the function value
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms.
!
!-    SPICELIB Version 1.5.0, 6-OCT-1992 (HAN)
!
!        Module was updated to include the value of INTMIN for the
!        Hewlett Packard UX 9000/750 environment.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.4.0, 13-NOV-1991 (MJS)
!
!        Module was updated to include the value of INTMIN
!        for the Lahey F77L EM/32 environment (PC).
!
!-    SPICELIB Version 1.3.0, 19-SEP-1991 (HAN) (NJB)
!
!        The minimum integer is -2147483648. Because the compiler
!        (version 1.3.1) does not accept this value in a direct
!        assignment, the value of INTMIN is specified as
!        ( -INMAX() ) - 1.
!
!-    SPICELIB Version 1.2.0, 7-DEC-1990 (MJS)
!
!        Module was updated to include the value of INTMIN for
!        the Macintosh. $Literature_References section was updated.
!
!-    SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
!
!        Code was changed to include the function value
!        for the Sun. Documentation in the $Particulars
!        section was changed to include the value, and the
!        example was updated and corrected.
!
!        All sources for the values contained in this module
!        are now specified in the $Literature_References section.
!
!-    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
!
!        Contents of the $Exceptions section was changed
!        to "error free" to reflect the decision that the
!        module will never participate in error handling.
!
!        Parentheses added to function declaration.
!
!-&

!
!     Numbers are provided in a variety of formats: decimal, hex, sand
!     binary. These last two formats are not portable; but then,
!     neither are the values.
      use pepper_kinds, only: ip
      use pepper_constants, only: pepper_intmin => intmin
      implicit none
      intmin = pepper_intmin
      end function intmin

      !> Legacy interface: the constant j1900 of module pepper_constants.
      real(wp) function j1900 ( )

! $ Abstract
!
!     Return the Julian Date of 1899 DEC 31 12:00:00 (1900 JAN 0.5).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2415020.0, the Julian Date corresponding
!     to 1899 DEC 31 12:00:00 (1900 JAN 0.5).
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of J1900.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J1900).
!        C
!              SPREF = ( JD - J1900() ) * SPD()
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     julian date of 1900.0 jan 0.5
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_j1900 => j1900
      implicit none
      j1900 = pepper_j1900
      end function j1900

      !> Legacy interface: the constant j1950 of module pepper_constants.
      real(wp) function j1950 ( )

! $ Abstract
!
!     Return the Julian Date of 1950 JAN 01 00:00:00 (1950 JAN 1.0).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2433282.5, the Julian Date corresponding
!     to 1950 JAN 01 00:00:00 (1950 JAN 1.0).
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of J1950.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J1950).
!        C
!              SPREF = ( JD - J1950() ) * SPD()
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     julian date of 1950.0 jan 1.0
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_j1950 => j1950
      implicit none
      j1950 = pepper_j1950
      end function j1950

      !> Legacy interface: the constant j2000 of module pepper_constants.
      real(wp) function j2000 ( )

! $ Abstract
!
!     Return the Julian Date of 2000 JAN 01 12:00:00 (2000 JAN 1.5).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2451545.0, the Julian Date corresponding
!     to 2000 JAN 01 12:00:00 (2000 JAN 1.5).
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of J2000.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J2000).
!        C
!              SPREF = ( JD - J2000() ) * SPD()
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     julian date of 2000 jan 1.5
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_j2000 => j2000
      implicit none
      j2000 = pepper_j2000
      end function j2000

      !> Legacy interface: the constant j2100 of module pepper_constants.
      real(wp) function j2100 ( )

! $ Abstract
!
!     Return the Julian Date of 2100 JAN 01 12:00:00 (2100 JAN 1.5).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns 2488070.0, the Julian Date corresponding
!     to 2100 JAN 01 12:00:00 (2100 JAN 1.5).
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of J2100.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J2100).
!        C
!              SPREF = ( JD - J2100() ) * SPD()
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     julian date of 2100 jan 1.5
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_j2100 => j2100
      implicit none
      j2100 = pepper_j2100
      end function j2100

      !> Legacy interface: the constant jyear of module pepper_constants.
      real(wp) function jyear ( )

! $ Abstract
!
!     Return the number of seconds in a julian year.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds per julian year.
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
!     The julian year is often used as a fundamental unit of time when
!     dealing with ephemeris data. For this reason its value in terms of
!     ephemeris seconds is recorded in this function.
!
! $ Examples
!
!     Suppose you wish to compute the number of julian centuries
!     that have elapsed since the ephemeris epoch J1950 (beginning
!     of the julian year 1950) at a particular ET epoch. The
!     following line of code will do the trick.
!
!        CENTRY = ( ET - UNITIM ( J1950(), 'JED', 'ET' ) )
!       .       / ( 100.0D0 * JYEAR() )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
!          Astronomical Almanac," Page 8, University Science Books,
!          1992.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
!
!-&

! $ Index_Entries
!
!     Number of seconds per julian year
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_jyear => jyear
      implicit none
      jyear = pepper_jyear
      end function jyear

      !> Legacy interface: the constant pi of module pepper_constants.
      real(wp) function pi ( )

! $ Abstract
!
!     Return the value of pi (the ratio of the circumference of
!     a circle to its diameter).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the value of pi (the ratio of a circle's
!     circumference to its diameter), determined by the ACOS function.
!     That is,
!
!           PI = ACOS ( -1.D0 )
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The code fragment below illustrates the use of PI.
!
!        C
!        C     Compute the polar radius,
!        C
!        C                 p
!        C          ----------------
!        C          1 + e cos(theta)
!        C
!        C     at evenly spaced values of the polar angle, theta.
!        C
!              DELTA = PI() / N
!
!              DO I = 0, N
!                 R(I) = P / (1.D0 + ECC * COS(I*DELTA))
!              END DO
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     value of pi
!
!-&

!
!     Local variables
!

!
!     Initial values
!

!
!     What is there to say?
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_pi => pi
      implicit none
      pi = pepper_pi
      end function pi

      !> Legacy interface: the constant rpd of module pepper_constants.
      real(wp) function rpd ( )

! $ Abstract
!
!     Return the number of radians per degree.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of radians per degree: pi/180.
!     The value of pi is determined by the ACOS function. That is,
!
!           RPD = ACOS ( -1.D0 ) / 180.D0
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The code fragment below illustrates the use of RPD.
!
!        C
!        C     Convert all input angles to radians.
!        C
!              CLOCK = CLOCK * RPD()
!              CONE  = CONE  * RPD()
!              TWIST = TWIST * RPD()
!
!     or equivalently,
!
!        C
!        C     Convert all input angles to radians.
!        C
!              CALL VPACK  ( CLOCK, CONE, CCTWIST, ALBTGAM )
!              CALL VSCL   ( RPD(), ALBTGAM, ALBTGAM )
!              CALL VUPACK ( ALBTGAM, CLOCK, CONE, CCTWIST )
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
!-    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     radians per degree
!
!-&

!
!     Local variables
!

!
!     Initial values
!

!
!     What is there to say?
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_rpd => rpd
      implicit none
      rpd = pepper_rpd
      end function rpd

      !> Legacy interface: the constant spd of module pepper_constants.
      real(wp) function spd ( )

! $ Abstract
!
!     Return the number of seconds in a day.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds in a day: 86400.
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
!     The function always returns the constant value shown above.
!
! $ Examples
!
!     The following code fragment illustrates the use of SPD.
!
!        C
!        C     Convert Julian Date to UTC seconds past the reference
!        C     epoch (J2000).
!        C
!              SPREF = ( JD - J2000() ) * SPD()
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
!-    SPICELIB Version 1.0.2, 07-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     seconds per day
!
!-&

!
!     Just like it says.
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_spd => spd
      implicit none
      spd = pepper_spd
      end function spd

      !> Legacy interface: the constant twopi of module pepper_constants.
      real(wp) function twopi ( )

! $ Abstract
!
!     Return twice the value of pi (the ratio of the circumference of
!     a circle to its diameter).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns twice the value of pi (the ratio of
!     a circle's circumference to its diameter), determined by
!     the ACOS function. That is,
!
!           TWOPI = ACOS ( -1.D0 ) * 2.D0
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
!     The first time the function is referenced, the value is computed
!     as shown above. The value is saved, and returned directly upon
!     subsequent reference.
!
! $ Examples
!
!     The code fragment below illustrates the use of TWOPI.
!
!        C
!        C      The longitude of the ascending node is the angle
!        C      between the x-axis and the node vector, n.
!        C                                              -
!        C
!               NODE = ACOS ( N(1) / VNORM(N) )
!
!               IF ( NODE .LT. 0.D0 ) THEN
!                  NODE = NODE + TWOPI()
!               END IF
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
!-    SPICELIB Version 1.0.3, 07-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 28-AUG-1997 (WLT)
!
!        Fixed the description in the detailed output section
!        of the header.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     twice the value of pi
!
!-&

!
!     Local variables
!

!
!     Initial values
!

!
!     What is there to say?
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_twopi => twopi
      implicit none
      twopi = pepper_twopi
      end function twopi

      !> Legacy interface: the constant tyear of module pepper_constants.
      real(wp) function tyear ( )

! $ Abstract
!
!     Return the number of seconds in a tropical year.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns the number of seconds per tropical
!     year. This value is taken from the 1992 Explanatory Supplement
!     to the Astronomical Almanac (see [1]).
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
!     The tropical year is often used as a fundamental unit
!     of time when dealing with older ephemeris data. For this
!     reason its value in terms of ephemeris seconds is
!     recorded in this function.
!
! $ Examples
!
!     Suppose you wish to compute the number of tropical centuries
!     that have elapsed since the ephemeris epoch B1950 (beginning
!     of the Besselian year 1950) at a particular ET epoch. The
!     following line of code will do the trick.
!
!        CENTRY = ( ET - UNITIM ( B1950(), 'JED', 'ET' ) )
!       .       / ( 100.0D0 * TYEAR() )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
!          Astronomical Almanac," p 80, University Science Books, 1992.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 13-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
!
!-&

! $ Index_Entries
!
!     Number of seconds per tropical year
!
!-&
      use pepper_kinds, only: wp
      use pepper_constants, only: pepper_tyear => tyear
      implicit none
      tyear = pepper_tyear
      end function tyear

end module pepper_legacy_constants
