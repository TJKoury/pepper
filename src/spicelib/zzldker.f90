!$Procedure ZZLDKER ( Load a kernel )

      subroutine zzldker ( file, nofile, filtyp, handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the architecture and type of a file and load
!     the file into the appropriate SPICE subsystem
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      PRIVATE
!
! $ Declarations

      implicit none
      character*(*)         file
      character*(*)         nofile
      character*(*)         filtyp
      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   The name of a file to be loaded.
!     NOFILE     I   A message to issue if FILE cannot be located
!     FILTYP     O   The type of kernel.
!     HANDLE     O   The handle associated with the loaded kernel.
!
! $ Detailed_Input
!
!     FILE       is the name of a file that is anticipated to
!                be a SPICE kernel.
!
!     NOFILE     is a template for the message that should be created
!                with SETMSG if a problem is identified with FILE. The
!                message should have the form: "[text] '#' [text] #" The
!                first octothorpe ('#') will be replaced by the name of
!                the file. The second by a descriptive message.
!
! $ Detailed_Output
!
!     FILTYP     is the type of the kernel as determined by the
!                SPICE file record of the file or by various
!                heuristics.  Possible return values are:
!
!                  TEXT   ---  if FILE is interpreted as a text kernel
!                              suitable for loading via LDPOOL.  No
!                              attempt is made to distinguish between
!                              different types of text kernels.
!                  SPK   |
!                  CK    |
!                  PCK   |---  if FILE is a binary PCK file.
!                  DSK   |
!                  EK    |
!
!                If a failure occurs during the attempt to load
!                the FILE, FILTYP will be returned as the blank string.
!
!     HANDLE     is the DAF or DAS handle that is associated with the
!                file.  If the FILTYP of the file is 'TEXT', HANDLE
!                will be set to zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the specified file does not exist, the error
!        SPICE(NOSUCHFILE) will be signaled.
!
!     2) If the specified file can be identified as unloadable
!        because it is a transfer format file, the error
!        SPICE(TRANSFERFILE) will be signaled.
!
!     3) If the specified file can be identified as unloadable
!        because it is an obsolete text E-kernel, the error
!        SPICE(TYPE1TEXTEK) will be signaled.
!
!     4) If the specified file can be recognized as a DAF/DAS file
!        but is not one of the currently recognized binary kernel
!        types, the error SPICE(UNKNOWNKERNELTYPE) will be signaled.
!
!     5) FILTYP is not sufficiently long to hold the full text of the
!        type of the kernel, the value returned will be the truncation
!        of the value.  As currently implemented this truncated type is
!        sufficient to distinguish between the various types of
!        kernels.
!
!     6) If the FILE cannot be loaded, HANDLE will be set to zero.
!
!     7) All other problems associated with the loading of FILE
!        are diagnosed by the routines called by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is intended as a supporting routine for the
!     SPICE routine FURNSH.  It handles the task of loading
!     an arbitrary kernel without the caller having to specify
!     the type of the kernel.
!
! $ Examples
!
!     None.  (After all it's a private routine)
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
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-OCT-2021 (BVS)
!
!        Wrapped KERLIN and TERMIN declarations in FORTRAN environment
!        block to silence declared-not-used complaints from f2c.
!
!-    SPICELIB Version 2.0.0, 20-JAN-2016 (NJB)
!
!        Added ability to load DSK files.
!
!-    SPICELIB Version 1.17.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.16.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.8.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.7.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.6.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.3.0, 03-OCT-2005 (EDW)
!
!        Source file zzldker.f converted to master file.
!        Modification occurred to prevent f2c's versions
!        from making the zzascii test. CSPICE now
!        includes coed to allow reading of non native text files.
!
!-    SPICELIB Version 1.2.0, 17-FEB-2004 (EDW) (BVS)
!
!        Added the ZZASCII terminator test for text files. Used a
!        working line length of 132 characters (maximum text kernel
!        line size.)
!
!-    SPICELIB Version 1.1.0, 24-JUN-2002 (EDW)
!
!        Added a call to ZZBODKIK to run the
!        NAIF_BODY_NAME/CODE read/check routine
!        whenever a text kernel loads.
!
!-    SPICELIB Version 1.0.0, 04-JUN-1999 (WLT)
!
!
!-&
!
!     SPICELIB Functions
!
      logical               exists
      logical               failed
      logical               return


!
!     Local Variables.
!
      integer(ip)           wdsize
      parameter           ( wdsize = 32 )

      integer(ip)           eoltsz
      parameter           ( eoltsz = 5 )

      integer(ip)           klinsz
      parameter           ( klinsz = 132 )

      character*(wdsize)    versn
      character*(wdsize)    arch
      character*(wdsize)    mytype
      character*(eoltsz)    termin
      character*(klinsz)    kerlin

      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZLDKER' )

      if ( .not. exists( file ) ) then

         call setmsg ( nofile )
         call errch  ( '#', file )
         call errch  ( '#', 'could not be located.' )
         call sigerr ( 'SPICE(NOSUCHFILE)'  )
         call chkout ( 'ZZLDKER' )
         return

      end if


      call getfat ( file, arch, mytype )

!
!     Possible values for the architecture are:
!
!        DAF -- The file is based on the DAF architecture.
!        DAS -- The file is based on the DAS architecture.
!        XFR -- The file is in a SPICE transfer file format.
!        DEC -- The file is an old SPICE decimal text file.
!        ASC -- An ASCII text file.
!        KPL -- Kernel Pool File (i.e., a text kernel)
!        TXT -- An ASCII text file.
!        TE1 -- Text E-Kernel type 1.
!         ?  -- The architecture could not be determined.
!
!     Some of these are obviously losers.
!
      if (      arch .eq. 'XFR' &
           .or. arch .eq. 'DEC' ) then

         call setmsg ( nofile )
         call errch  ( '#', file )
         call errch  ( '#', 'is a transfer format file. ' &
         //                 'Transfer format files cannot be ' &
         //                 'loaded. ' )
         call sigerr ( 'SPICE(TRANSFERFILE)'  )
         call chkout ( 'ZZLDKER' )
         return

      else if ( arch .eq. 'TE1' ) then

         call setmsg ( nofile )
         call errch  ( '#', file )
         call errch  ( '#', 'is a type 1 text E-kernel.  These ' &
         //                 'files are obsolete and cannot be ' &
         //                 'loaded. ' )
         call sigerr ( 'SPICE(TYPE1TEXTEK)' )
         call chkout ( 'ZZLDKER' )
         return

      end if

!
!     That takes care of the obvious errors.  Try loading the
!     kernel.
!
      handle = 0
      filtyp = ' '

      if ( arch .eq. 'DAF' )      then

         if      ( mytype .eq. 'SPK' ) then

            call spklef ( file, handle )

         else if ( mytype .eq. 'CK' ) then

            call cklpf ( file, handle )

         else if ( mytype .eq. 'PCK' ) then

            call pcklof ( file, handle )

         else

            call tkvrsn ( 'TOOLKIT', versn )
            call setmsg ( nofile )
            call errch  ( '#', file )
            call errch  ( '#', 'is a "#" DAF file. This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            call errch  ( '#', mytype  )
            call errch  ( '#', versn )
            call sigerr ( 'SPICE(UNKNOWNKERNELTYPE)' )
            call chkout ( 'ZZLDKER' )
            return

         end if

         filtyp = mytype


      else if ( arch .eq. 'DAS' ) then

         if ( mytype .eq. 'EK' ) then

            call eklef ( file, handle )

         else if ( mytype .eq. 'DSK' ) then

            call zzdsklsf ( file, handle )

         else

            call tkvrsn ( 'TOOLKIT', versn )
            call setmsg ( nofile )
            call errch  ( '#', file )
            call errch  ( '#', 'is a "#" DAS file.  This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            call errch  ( '#', mytype  )
            call errch  ( '#', versn )
            call sigerr ( 'SPICE(UNKNOWNKERNELTYPE)' )
            call chkout ( 'ZZLDKER' )
            return

         end if

         filtyp = mytype

      else




!
!        Check for line terminator compatibility with this platform.
!        .TRUE. means that ZZASCII will compare terminator
!        detected with the one native to this platform and will
!        stop if they don't match.
!
         call zzascii( file, kerlin, .true., termin )


!
!        Load the file using the text file loader.
!
         call ldpool ( file )

         if ( .not. failed() ) then
            filtyp = 'TEXT'
!
!           Cause the kernel pool mechanism to perform
!           the standard error checks on the pool
!           data.
!
            call zzbodkik()
         end if

      end if

      call chkout ( 'ZZLDKER' )
      return
      end
