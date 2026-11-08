!$Procedure   ZZGFDSPS  ( GF, display string  )

      subroutine zzgfdsps ( nlead, string, fmt, ntrail )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Display a character string at a position at the first column on
!     the previous line on the screen.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     STRING
!     DISPLAY
!     CURSOR
!     POSITION
!
! $ Declarations

      implicit none

      integer(ip)           nlead
      character(*)          string
      character(*)          fmt
      integer(ip)           ntrail

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NLEAD      I   Number of leading blank lines to write.
!     STRING     I   The string to display.
!     FMT        I   Format in which the string is to be written.
!     NTRAIL     I   Number of trailing blank lines to write.
!
! $ Detailed_Input
!
!     NLEAD          is the number of blank lines to write before
!                    writing the output text string.
!
!     STRING         is a message to be displayed on the standard
!                    output stream.
!
!     FMT            is a Fortran format specification used to write
!                    the output string.
!
!                    FMT may be left to default ('A'), or may be used
!                    to control the length of the string ('A10').
!
!     NTRAIL         is the number of blank lines to write after
!                    writing the output text string.
!
! $ Detailed_Output
!
!     None. This program has no output arguments but writes to the
!     standard output stream.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If an error occurs when this routine attempts to
!        write its output, the message SPICE(WRITEERROR)
!        will be signaled.
!
!     2) If the either of the input arguments NLEAD or NTRAIL
!        is non-positive, then no leading or trailing blank
!        lines will be written, respectively. This case is not
!        considered an error.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the default GF progress report display.
!     Output is written to the standard output stream; normally this
!     results in output on a terminal window.
!
!     After the output line is written, this routine moves the cursor
!     up and to the first column, so a subsequent call will overwrite
!     output from the current call.
!
! $ Examples
!
!     See calls made to this routine by the entry points of
!     ZZGFRPWK.
!
! $ Restrictions
!
!     This routine relies on platform support for ANSI cursor control
!     character sequences.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!     L.S. Elson       (JPL)
!     I.M. Underwood   (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.17.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.16.0, 14-NOV-2021 (NJB)
!
!        Bug fix: for the set of platforms shown below, changed loop
!        upper bound for output of trailing lines to NTRAIL; value
!        previously was NLEAD.
!
!           PC-WINDOWS-64BIT-IFORT
!           PC-MS PC-LAHEY WIN-NT PC-DIGITAL PC-WINDOWS-IFORT
!
!-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.13.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.12.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.11.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.8.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.7.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.6.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.5.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.4.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.3.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.2.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.1.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.0.0, 15-APR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     GF output progress report string
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           rtrim

!
!     Local parameters
!
      integer(ip), parameter :: lnsize = 80

!
!     Local Variables
!
      character(lnsize)     tmpstr

      integer(ip)           i
      integer(ip)           iostat
      integer(ip)           l

!
!     Get a local copy of the input string.
!
      tmpstr = string
      l      = rtrim(tmpstr)
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
!     Write leading blank lines, if any.
#else
!     Write leading blank lines, if any. We use cursor control
!     here so as to avoid overwriting the last status line.
#endif
      do i = 1, nlead
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         call tostdo ( ' ' )
#else
         write (*, '(A)' ) char(27)//'[B'
#endif
      end do

#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
!     Write the status line without special cursor control.
#else
!     Write the status line, then move the cursor up one line.
!     This places the cursor at the start of the line that
!     was just written.
#endif
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      call tostdo (   tmpstr( : rtrim(tmpstr) )   )
#else
      call suffix (  char(27)//'[A', 0, tmpstr )

      write ( *, '(A)', iostat = iostat ) tmpstr( : rtrim(tmpstr) )

      if ( iostat /= 0  ) then

         call setmsg ( 'Error writing to standard output: IOSTAT = #.' )
         call errint ( '#', iostat                                     )
         call sigerr ( 'SPICE(WRITEFAILED)'                            )
         return

      end if
#endif

#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
!     Write trailing blank lines, if any.
#else
!     Write trailing blank lines, if any. We use cursor control
!     here so as to avoid overwriting the last status line.
#endif
      do i = 1, ntrail
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
         call tostdo ( ' ' )
#else
         write (*, '(A)' ) char(27)//'[B'
#endif
      end do

      return
      end
