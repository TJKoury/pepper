!$Procedure INTMAX ( Largest integer number )

      INTEGER FUNCTION INTMAX ()

! $ Abstract
!
!     Return the value of the largest (positive) number representable
!     in an integer variable.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

! $ Brief_I/O
!
!     The function returns the value of the largest (positive) number
!     that can be represented in an integer variable.
!
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
!
!        Example code begins here.
!
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
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
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
!
      INTMAX = 2147483647

      RETURN
      END
