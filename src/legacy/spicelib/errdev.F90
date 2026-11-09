!$Procedure ERRDEV ( Get/Set Error Output Device Name )

      subroutine errdev ( op, device )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Retrieve or set the name of the current output
!     device for error messages.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      implicit none

      character(*)           op
      character(*)           device

      integer(ip)            filen
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      parameter            ( filen = 128 )
#else
      parameter            ( filen = 255 )
#endif

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OP         I   The operation:  'GET' or 'SET'.
!     DEVICE    I-O  The device name.
!     FILEN      P   Maximum length of file name.
!
! $ Detailed_Input
!
!     OP       indicates the operation to be performed. Possible
!              values are 'GET' and 'SET'. 'GET' means, "set
!              DEVICE equal to the name of the current error
!              output device"  'SET' means, "set the name of the
!              current error output device equal to the value of
!              DEVICE."
!
!     DEVICE   is an input when OP has the value, 'SET'. It
!              indicates an output device to which error messages
!              are to be sent. Possible values for DEVICE are:
!
!                 A file name     Note that the name must not
!                                 be any of the reserved strings below.
!
!                 'SCREEN'        The output will go to the
!                                 screen. This is the default device.
!
!                 'NULL'          The data will not be output.
!
!               'SCREEN' and 'NULL' can be written in mixed
!               case. For example, the following call will work:
!
!               CALL ERRDEV ( 'SET' , 'screEn' )
!
! $ Detailed_Output
!
!     DEVICE   is an output when OP is 'GET'. It is the
!              current error output device. See "Detailed
!              Input" for possible values and meanings.
!
! $ Parameters
!
!     FILEN    is the maximum length of a file name that can be
!              processed by this routine. FILEN is currently set to 255
!              characters.
!
! $ Exceptions
!
!     1)  If an invalid value of the argument OP is supplied, the error
!         SPICE(INVALIDOPERATION) is signaled.
!
!     2)  If OP is 'SET' and the device name DEVICE exceeds the maximum
!         length FILEN, the error SPICE(DEVICENAMETOOLONG) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling mechanism.
!
!     Please read the "required reading"!
!
!     This routine can't tell whether the name supplied
!     to indicate the output device is valid. Be careful!
!
! $ Examples
!
!     1. In this example, we select as the output device
!         the file, SPUD.DAT.
!
!      C
!      C      Set the error output device to SPUD.DAT:
!      C
!
!             CALL ERRDEV (  'SET',  'SPUD.DAT'  )
!
! $ Restrictions
!
!     1)  This routine has no capability of determining the validity of
!         the name of an output device. Care must be taken to ensure
!         that the file named is the correct one.
!
!         The device name is assumed to be no longer than FILEN
!         characters.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 2.26.0, 07-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. $Exceptions
!        section has been completely updated to provide only the list
!        of exceptions. Additional information provided there has been
!        moved to $Particulars.
!
!-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 2.0.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
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
!        References to the PC-LINUX environment were added.
!
!-    SPICELIB Version 1.2.0, 03-NOV-1993 (HAN)
!
!        Module was updated to include the value for FILEN
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms. Also, the previous value of 256 for
!        Unix platforms was changed to 255.
!
!-    SPICELIB Version 1.1.0, 09-OCT-1992 (HAN)
!
!        Updated module for multiple environments.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&

! $ Index_Entries
!
!     get/set error output device name
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
!
!        References to the PC-LINUX environment were added.
!
!-     SPICELIB Version 1.2.0, 03-NOV-1993 (HAN)
!
!         Module was updated to include the value for FILEN
!         for the Silicon Graphics, DEC Alpha-OSF/1, and
!         NeXT platforms. Also, the previous value of 256 for
!         Unix platforms was changed to 255.
!
!-     SPICELIB Version 1.1.0, 09-OCT-1992 (HAN)
!
!         Updated module for multiple environments.
!
!         The code was also reformatted so that a utility program can
!         create the source file for a specific environment given a
!         master source file.
!
!-     Beta Version 1.1.0, 16-FEB-1989 (NJB)
!
!        File name length parameter added to $Parameters section.
!        Declaration of the unused function FRSTNB removed.
!        Trace participation added. This routine now checks in
!        and checks out. However, it does not test RETURN,
!        because it should be able to execute in RETURN mode when
!        an error condition exists.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           lastnb

!
!     Local Variables:
!

      character(filen)      upnam
      character(filen)      locnam

      integer(ip), parameter :: oplen = 3

      character(oplen)      upop
      character(oplen)      locop

!
!     Initial Values:
!

!
!     Executable Code:
!

!
!     We save the operation string as input, and get
!     an upper case version for our own use:
!

      call ljust ( op,   upop )
      call ucase ( upop, upop )

      if ( upop  == 'GET' ) then

         call getdev ( device )

      else if ( upop  == 'SET' ) then

!
!        We want the reserved words to be in upper
!        case for our own use.  So, save the input value
!        and get an upper case version:
!

         call ljust ( device, upnam )
         call ucase ( upnam,  upnam )

         if (   lastnb( upnam )   >   filen    )  then

            locnam = device

            call setmsg ( 'ERRDEV:  Device name exceeds FILEN'        // &
                          ' characters; device selection not updated.'// &
                          ' The first FILEN characters of the name'   // &
                          ' were:  '//locnam &
                        )

            call sigerr ( 'SPICE(DEVICENAMETOOLONG)' )

            return

         end if

         if (  ( upnam  == 'SCREEN' ) .or. &
               ( upnam  == 'NULL'   )        )  then

!
!           Store upper case version of DEVICE:
!

            call putdev ( upnam )

         else

!
!           We assume we've got a file name...
!           Store it as it was input.
!
            call putdev ( device )

         end if

      else

!
!        An invalid value of OP was supplied.
!

         locop = op

         call setmsg ( 'ERRDEV:  An invalid value of OP was supplied.'// &
                       '  The value was: '  // locop   )

         call sigerr ( 'SPICE(INVALIDOPERATION)' )

      end if

      return
      end
