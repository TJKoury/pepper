!$Procedure ZZLDKER ( Load a kernel )

      SUBROUTINE ZZLDKER ( FILE, NOFILE, FILTYP, HANDLE )

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

      IMPLICIT NONE
      CHARACTER*(*)         FILE
      CHARACTER*(*)         NOFILE
      CHARACTER*(*)         FILTYP
      INTEGER               HANDLE

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
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN


!
!     Local Variables.
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )

      INTEGER               KLINSZ
      PARAMETER           ( KLINSZ = 132 )

      CHARACTER*(WDSIZE)    VERSN
      CHARACTER*(WDSIZE)    ARCH
      CHARACTER*(WDSIZE)    MYTYPE
      CHARACTER*(EOLTSZ)    TERMIN
      CHARACTER*(KLINSZ)    KERLIN

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZLDKER' )

      IF ( .NOT. EXISTS( FILE ) ) THEN

         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'could not be located.' )
         CALL SIGERR ( 'SPICE(NOSUCHFILE)'  )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN

      END IF


      CALL GETFAT ( FILE, ARCH, MYTYPE )

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
      IF (      ARCH .EQ. 'XFR' &
           .OR. ARCH .EQ. 'DEC' ) THEN

         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'is a transfer format file. ' &
         //                 'Transfer format files cannot be ' &
         //                 'loaded. ' )
         CALL SIGERR ( 'SPICE(TRANSFERFILE)'  )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN

      ELSE IF ( ARCH .EQ. 'TE1' ) THEN

         CALL SETMSG ( NOFILE )
         CALL ERRCH  ( '#', FILE )
         CALL ERRCH  ( '#', 'is a type 1 text E-kernel.  These ' &
         //                 'files are obsolete and cannot be ' &
         //                 'loaded. ' )
         CALL SIGERR ( 'SPICE(TYPE1TEXTEK)' )
         CALL CHKOUT ( 'ZZLDKER' )
         RETURN

      END IF

!
!     That takes care of the obvious errors.  Try loading the
!     kernel.
!
      HANDLE = 0
      FILTYP = ' '

      IF ( ARCH .EQ. 'DAF' )      THEN

         IF      ( MYTYPE .EQ. 'SPK' ) THEN

            CALL SPKLEF ( FILE, HANDLE )

         ELSE IF ( MYTYPE .EQ. 'CK' ) THEN

            CALL CKLPF ( FILE, HANDLE )

         ELSE IF ( MYTYPE .EQ. 'PCK' ) THEN

            CALL PCKLOF ( FILE, HANDLE )

         ELSE

            CALL TKVRSN ( 'TOOLKIT', VERSN )
            CALL SETMSG ( NOFILE )
            CALL ERRCH  ( '#', FILE )
            CALL ERRCH  ( '#', 'is a "#" DAF file. This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            CALL ERRCH  ( '#', MYTYPE  )
            CALL ERRCH  ( '#', VERSN )
            CALL SIGERR ( 'SPICE(UNKNOWNKERNELTYPE)' )
            CALL CHKOUT ( 'ZZLDKER' )
            RETURN

         END IF

         FILTYP = MYTYPE


      ELSE IF ( ARCH .EQ. 'DAS' ) THEN

         IF ( MYTYPE .EQ. 'EK' ) THEN

            CALL EKLEF ( FILE, HANDLE )

         ELSE IF ( MYTYPE .EQ. 'DSK' ) THEN

            CALL ZZDSKLSF ( FILE, HANDLE )

         ELSE

            CALL TKVRSN ( 'TOOLKIT', VERSN )
            CALL SETMSG ( NOFILE )
            CALL ERRCH  ( '#', FILE )
            CALL ERRCH  ( '#', 'is a "#" DAS file.  This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            CALL ERRCH  ( '#', MYTYPE  )
            CALL ERRCH  ( '#', VERSN )
            CALL SIGERR ( 'SPICE(UNKNOWNKERNELTYPE)' )
            CALL CHKOUT ( 'ZZLDKER' )
            RETURN

         END IF

         FILTYP = MYTYPE

      ELSE




!
!        Check for line terminator compatibility with this platform.
!        .TRUE. means that ZZASCII will compare terminator
!        detected with the one native to this platform and will
!        stop if they don't match.
!
         CALL ZZASCII( FILE, KERLIN, .TRUE., TERMIN )


!
!        Load the file using the text file loader.
!
         CALL LDPOOL ( FILE )

         IF ( .NOT. FAILED() ) THEN
            FILTYP = 'TEXT'
!
!           Cause the kernel pool mechanism to perform
!           the standard error checks on the pool
!           data.
!
            CALL ZZBODKIK()
         END IF

      END IF

      CALL CHKOUT ( 'ZZLDKER' )
      RETURN
      END
