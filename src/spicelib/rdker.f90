!$Procedure RDKER ( Read a kernel file )

      subroutine rdker ( kernel, line, number, eof )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Open and read the contents of a SPICE ASCII kernel file.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      character*(*)         kernel
      character*(*)         line
      integer(ip)           number
      logical               eof

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     KERNEL     I   RDKNEW
!     LINE       O   RDKDAT
!     NUMBER     O   RDKLIN
!     EOF        O   RDKDAT
!
! $ Detailed_Input
!
!     All input is through entry RDKNEW.
!
! $ Detailed_Output
!
!     All output is through entry RDKDAT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If RDKER is called directly, the error SPICE(BOGUSENTRY) is
!         signaled.
!
! $ Files
!
!     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
!     by RDKDAT. The entry point RDKLIN is available for reporting
!     the name of the open file and the number of the last line that
!     was read from that file.
!
! $ Particulars
!
!     RDKER should never be called directly, but should instead be
!     accessed only through its entry points, RDKNEW, RDKDAT and
!     RDKLIN.
!
! $ Examples
!
!     In the following example, RDKNEW and RDKDAT are used to read
!     the contents of a kernel file.
!
!     Let the file KERNEL contain the following lines.
!
!        =============================================================
!
!        DELTA_T_A is defined to be 32.184 seconds, and should not
!        be changed except under the most unusual circumstances.
!
!        \begindata
!
!        DELTA_T_A       =   32.184
!
!        \begintext
!
!        The next three items determine the relativistic correction
!        in the difference ET - TAI. To turn the correction off,
!        just set K to zero.
!
!        \begindata
!
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!
!        =============================================================
!
!     Then the code fragment
!
!        CALL RDKNEW ( KERNEL )
!        CALL RDKDAT ( LINE, EOF )
!
!        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
!           WRITE (6,*) LINE
!           CALL RDKDAT ( LINE, EOF )
!        END DO
!
!     prints the following lines.
!
!        =============================================================
!        DELTA_T_A       =   32.184
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!        =============================================================
!
! $ Restrictions
!
!     1)  The input file must be opened and initialized by RDKNEW prior
!         to the first call to RDKDAT.
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
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.7.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 3.6.1, 17-JUN-2021 (JDR)
!
!        Edited the header of the RDKER umbrella routine and all its
!        entry entry points to comply with NAIF standard.
!
!-    SPICELIB Version 3.6.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 3.5.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 3.4.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 3.3.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 3.2.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 3.1.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
!
!        Entry points RDKNEW and RDKDAT have been updated so as to be
!        able to parse text kernel lines containing tab characters.
!
!-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 2.3.0, 14-NOV-2005 (BVS)
!
!        Reinstated HP_C environment.
!
!-    SPICELIB Version 2.2.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
!
!        File rdker.f made a master file so as to
!        add the ZZSETNNREAD call. This call will exist
!        only in FORTRAN source intended for conversion
!        to C by the f2c utility.
!
!        The ZZSETNNREAD call activates and deactivates
!        the non-native text line read capability for the
!        CSPICE toolkit.
!
!-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDIF to END IF.
!
!-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!
!         The entry point RDKLIN was added.
!
!-    SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
!
!         Updated for port to NeXT. The "previous kernel" is now closed
!         only if there actually was a previous kernel.
!
!-    SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
!
!         RDKER now initializes the variables BEGDAT and BEGTXT
!         in a portable way. On the first valid entry to this routine,
!         the backslash character in the form CHAR(92) is concatenated
!         individually to 'begindata' and 'begintext'.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.1.0, 07-DEC-1990 (HAN)
!
!         The declarations for BEGDAT and BEGTXT were changed from
!         CHARACTER*10 to CHARACTER*(*).
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     read a kernel file
!
!-&


! $ Revisions
!
!-     SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!
!         The entry point RDKLIN was added.
!
!-     SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
!
!         Updated for port to NeXT. The "previous kernel" is now closed
!         only if there actually was a previous kernel.
!
!         In the last version of this routine, on the first entry into
!         the routine, the variable FILE, which records the name of
!         the last kernel accessed, was passed to CLTEXT.  CLTEXT
!         executed an INQUIRE statement using this name, which was
!         not initialized. On the NeXT, this caused the INQUIRE
!         statement to fail.
!
!
!-     SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
!
!         RDKER now initializes the variables BEGDAT and BEGTXT
!         in a portable way. On the first valid entry to this routine,
!         the backslash character in the form CHAR(92) is concatenated
!         individually to 'begindata' and 'begintext'. As a result of
!         this change, this module is no longer considered environment
!         specific. All references in the header to the previous method
!         of initialization were removed.
!
!         FILE is now initialized to ' '. Before this modification, if
!         a call to RDKDAT was performed prior to RDKNEW, RDTEXT
!         would have printed out garbage (on some machines) in its
!         error message when notifying the user that it couldn't read
!         from FILE.
!
!-     SPICELIB Version 1.1.0, 7-DEC-1990 (HAN)
!
!         The declarations for BEGDAT and BEGTXT were changed from
!         CHARACTER*10 to CHARACTER*(*). The fixed length of 10 was
!         not long enough.
!
!-     Beta Version 1.1.0, 9-MAR-1989 (HAN)
!
!         Moved the declaration of the parameters BEGDAT and
!         BEGTXT from the code to the $Declarations section.
!         Filled out the Brief I/O and $Parameters sections.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           rtrim

      logical               return
      logical               failed

!
!     Local parameters
!
!
!     Because some environments (such as the SUN) treat the backslash
!     character as a special character, some gyrations are needed to
!     put it into a variable in a "portable" way. This is the reason
!     for the following block of declarations. Admittedly this is
!     bizarre, but it works.
!
      character*(*)         dat
      parameter           ( dat = 'begindata' )

      character*(*)         txt
      parameter           ( txt = 'begintext' )

      character*(*)         blank
      parameter           ( blank = ' ' )

!
!     The ASCII decimal code for the tab character is 9.
!
      integer(ip)           tabcde
      parameter           ( tabcde = 9 )

      integer(ip)           mrklen
      parameter           ( mrklen = 10 )

      integer(ip)           bslash
      parameter           ( bslash = 92 )

      integer(ip)           lnsize
      parameter           ( lnsize = 80 )

      integer(ip)           filsiz
      parameter           ( filsiz = 255 )

      integer(ip)           intext
      parameter           ( intext = 1 )

      integer(ip)           indata
      parameter           ( indata = intext + 1 )

      integer(ip)           endoff
      parameter           ( endoff = indata + 1 )

!
!     Local variables
!
      character*(mrklen)    begdat
      character*(mrklen)    begtxt
      character*(filsiz)    file
      character*(lnsize)    first

      integer(ip)           i
      integer(ip)           r
      integer(ip)           status
      integer(ip)           linnum

      logical               end
      logical               frstim

!
!     Save EVERYTHING.
!
      save

!
!     Initial values
!
      data                  frstim / .true. /
      data                  file   / ' '    /
      data                  linnum /  0     /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'RDKER' )
      end if

!
!     Calling RDKER directly is a serious breach of protocol.
!     If RDKER is called, an error is signaled.
!

      call setmsg ( 'RDKER: You have called an entry which '   // &
                    'performs no run-time function. This '     // &
                    'may indicate a bug. Please check the '    // &
                    'documentation for the subroutine RDKER.'     )


      call sigerr ( 'SPICE(BOGUSENTRY)' )


      call chkout ( 'RDKER' )
      return



!$Procedure RDKNEW ( Open and initialize a new kernel file )

      entry rdknew ( kernel )

! $ Abstract
!
!     Open and initialize a SPICE ASCII kernel file.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         KERNEL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KERNEL     I   Kernel file.
!
! $ Detailed_Input
!
!     KERNEL   is the name of the SPICE ASCII kernel file to be opened
!              and initialized.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
!     by RDKDAT.
!
! $ Particulars
!
!     RDKNEW should be called prior to the first call to RDKDAT.
!     RDKNEW opens the kernel file and RDKDAT reads the lines of
!     data in the file.
!
! $ Examples
!
!     In the following example, RDKNEW and RDKDAT are used to read
!     the contents of a kernel file.
!
!     Let the file KERNEL contain the following lines.
!
!        =============================================================
!
!        DELTA_T_A is defined to be 32.184 seconds, and should not
!        be changed except under the most unusual circumstances.
!
!        \begindata
!
!        DELTA_T_A       =   32.184
!
!        \begintext
!
!        The next three items determine the relativistic correction
!        in the difference ET - TAI. To turn the correction off,
!        just set K to zero.
!
!        \begindata
!
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!
!        =============================================================
!
!     Then the code fragment
!
!        CALL RDKNEW ( KERNEL )
!        CALL RDKDAT ( LINE, EOF )
!
!        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
!           WRITE (6,*) LINE
!           CALL RDKDAT ( LINE, EOF )
!        END DO
!
!     prints the following lines.
!
!        =============================================================
!        DELTA_T_A       =   32.184
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!        =============================================================
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
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
!
!        This entry point has been updated so as to be
!        able to parse text kernel lines containing tab
!        characters.
!
!-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
!
!        File rdker.f made a master file so as to
!        add the ZZSETNNREAD call. This call will exist
!        only in FORTRAN source intended for conversion
!        to C by the f2c utility.
!
!        The ZZSETNNREAD call activates and deactivates
!        the non-native text line read capability for the
!        CSPICE toolkit.
!
!-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!
!         The entry point RDKLIN was added.
!
!-    SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
!
!         RDKER now initializes the variables BEGDAT and BEGTXT
!         in a portable way. On the first valid entry to this routine,
!         the backslash character in the form CHAR(92) is concatenated
!         individually to 'begindata' and 'begintext'.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     open and initialize a new kernel file
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'RDKNEW' )
      end if

!
!     Initialize the data delimiters if it hasn't been done already.
!
      if ( frstim ) then

         begdat = char(bslash) // dat
         begtxt = char(bslash) // txt

         frstim = .false.

      else
!
!        Close the previous file, if it hasn't been closed already.
!
         call cltext ( file )

      end if

!
!     Close the new file, too, in case they are the same. No sense
!     burning up logical units.
!
      call cltext ( kernel )

!
!     Read the first line of the file. It can't possibly be a data
!     line, since data must be preceded by a \begindata marker, so
!     we needn't take any pains to save it.
!
!     We also initialize LINNUM to 1 so we know
!     the line number of the last line read and can return this
!     information from RDKLIN.
!


      call rdtext ( kernel, first, end )


!
!     Replace any tab characters with blanks.
!
      r = rtrim ( first )

      do i = 1, r

         if ( ichar(first(i:i)) .eq. tabcde ) then

            first(i:i) = ' '

         end if

      end do


      call ljust ( first, first )
      linnum =  1

!
!     The first line is enough to set the status for subsequent
!     calls to RDKDAT.
!
      if ( end ) then

         status = endoff
         call cltext ( kernel )

      else if ( first .eq. begdat ) then
         status = indata

      else
         status = intext
      end if

!
!     Save the name of the file for future reference.
!
      file = kernel


      call chkout ( 'RDKNEW' )
      return



!$Procedure RDKDAT ( Read the next data line from a kernel file )

      entry rdkdat ( line, eof )

! $ Abstract
!
!     Read the next line of data from a SPICE ASCII kernel file.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     CHARACTER*(*)         LINE
!     LOGICAL               EOF
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       O   Next line of kernel data.
!     EOF        O   End of file indicator.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     LINE     is the next line of data from the kernel file most
!              recently opened by NEWKER. Data lines are non-blank lines
!              which lie between \begindata and \begintext markers.
!              Lines are returned left justified.
!
!     EOF      is .TRUE. when the end of the kernel file has been
!              reached, and is .FALSE. otherwise. The kernel file is
!              closed automatically when the end of the file is reached.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
!     by RDKDAT.
!
! $ Particulars
!
!     RDKDAT is used internally by RDKVAR to retrieve successive lines
!     of data from the current kernel file. It exists primarily to
!     relieve RDKVAR of the responsibility of dealing with comment
!     blocks and blank lines.
!
! $ Examples
!
!     In the following example, RDKNEW and RDKDAT are used to read
!     the contents of a kernel file.
!
!     Let the file KERNEL contain the following lines.
!
!        =============================================================
!
!        DELTA_T_A is defined to be 32.184 seconds, and should not
!        be changed except under the most unusual circumstances.
!
!        \begindata
!
!        DELTA_T_A       =   32.184
!
!        \begintext
!
!        The next three items determine the relativistic correction
!        in the difference ET - TAI. To turn the correction off,
!        just set K to zero.
!
!        \begindata
!
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!
!        =============================================================
!
!     Then the code fragment
!
!        CALL RDKNEW ( KERNEL )
!        CALL RDKDAT ( LINE, EOF )
!
!        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
!           WRITE (6,*) LINE
!           CALL RDKDAT ( LINE, EOF )
!        END DO
!
!     prints the following lines.
!
!        =============================================================
!        DELTA_T_A       =   32.184
!        K               =    1.657D-3
!        ORBIT_ECC       =    1.671D-2
!        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!        =============================================================
!
! $ Restrictions
!
!     1)  The input file must be opened and initialized by NEWKER prior
!         to the first call to RDKDAT.
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
!
!        This entry point has been updated so as to be
!        able to parse text kernel lines containing tab
!        characters.
!
!-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
!
!        File rdker.f made a master file so as to
!        add the ZZSETNNREAD call. This call will exist
!        only in FORTRAN source intended for conversion
!        to C by the f2c utility.
!
!        The ZZSETNNREAD call activates and deactivates
!        the non-native text line read capability for the
!        CSPICE toolkit.
!
!-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDIF to END IF.
!
!-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!
!        The entry point RDKLIN was added.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     read the next data line from a kernel file
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 23-OCT-1989 (HAN)
!
!        A FAILED test was added to the DO-loop which reads
!        lines in the kernel file.
!
!        If the error action was set to 'RETURN' an infinite loop
!        could have resulted if RDTEXT failed and the loop conditions
!        were satisfied.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'RDKDAT' )
      end if

!
!     If the previous call detected the end of the file,
!     this one should do the same.
!
      if ( status .eq. endoff ) then
         eof = .true.
         call chkout ( 'RDKDAT' )
         return
      end if

!
!     Well, at least we can try to read a line. Adjust the status as
!     needed, return if appropriate, read another line if necessary.
!     Basically, we're looking for a non-blank line in a data segment.
!
!     Note that after every read, we increment LINNUM so we know
!     the line number of the last line read and can return this
!     information from RDKLIN.
!
      line = blank

      do while ( ( .not. failed() )          .and. &
                 ( status .eq. intext  .or.  line .eq. blank ) )


         call rdtext ( file, line, eof )


!
!        Replace any tab characters with blanks.
!
         r = rtrim ( line )

         do i = 1, r

            if (  ichar(line(i:i)) .eq. tabcde ) then

               line(i:i) = ' '

            end if

         end do


         call ljust ( line, line )
         linnum = linnum + 1


         if ( eof ) then
            status = endoff
            call cltext ( file )
            call chkout ( 'RDKDAT' )
            return

         else if ( line .eq. begtxt ) then
            status = intext

         else if ( line .eq. begdat ) then
            status = indata
            line   = blank
         end if

      end do


      call chkout ( 'RDKDAT' )
      return


!$Procedure RDKLIN ( Reading kernel at line number )

      entry      rdklin ( kernel, number )

! $ Abstract
!
!     Return the name of file and line number of the last line read by
!     the entry point RDKDAT.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         KERNEL
!     INTEGER               NUMBER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KERNEL     O   The name of the current file that is being read.
!     NUMBER     O   The line number of the last line read in the file.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     KERNEL   is the name of the last file supplied via a call to
!              RDKNEW. If no call to RDKNEW have been made KERNEL is
!              returned as a blank. If KERNEL is not sufficiently long
!              to hold th name of the file, the file name will be
!              truncated on the right.
!
!     NUMBER   is the number of the last line in KERNEL returned by a
!              call to RDKDAT. If no call to RDKNEW or RDKDAT have been
!              made NUMBER is returned with the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If no calls to RDKNEW have been made, KERNEL is returned as
!         a blank and NUMBER is returned with the value 0.
!
!     2)  If no calls to RDKDAT have been made but RDKNEW has been
!         called, NUMBER is returned with the value 1.
!
!     3)  If KERNEL is not sufficiently long to hold the name of the
!         file being read, the name will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility to aid in determining the last
!     line read in a text file that is being read via RDKDAT.
!
!     It is particular useful in pointing out the location of
!     an error in an input file.
!
! $ Examples
!
!     Suppose that you are processing a file and have detected an
!     error in the syntax in the file. The following code fragment
!     illustrates how you can use this routine to inform a user of
!     the location of the error in the file.
!
!        CALL RDKLIN ( FILE, NUMBER )
!        R =  RTRIM  ( FILE )
!
!        WRITE (*,*) 'An error occurred while reading line ', NUMBER
!        WRITE (*,*) 'of the file ''', FILE(1:R), ''''
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Corrected
!        output argument name in $Declarations section (changed FILE to
!        KERNEL)
!
!-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
!
!        File rdker.f made a master file so as to
!        add the ZZSETNNREAD call. This call will exist
!        only in FORTRAN source intended for conversion
!        to C by the f2c utility.
!
!        The ZZSETNNREAD call activates and deactivates
!        the non-native text line read capability for the
!        CSPICE toolkit.
!
!-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Determine the last line read from a kernel file.
!
!-&


!
!     Not much to do here.  Just copy the information and return.
!
      kernel = file
      number = linnum

      return
      end
