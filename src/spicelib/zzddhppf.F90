!$Procedure ZZDDHPPF ( Private --- DDH Prepare Preexisting File )

      subroutine zzddhppf ( unit, arch, bff )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Prepare preexisting binary file for entry into the handle
!     table.
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
      include              'zzftprms.inc'

      integer(ip)           unit
      integer(ip)           arch
      integer(ip)           bff

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit attached to the binary file.
!     ARCH       I   Integer code indicating the file architecture.
!     BFF        O   Integer code indicating the binary file format.
!
! $ Detailed_Input
!
!     UNIT       is a logical unit attached to the binary file to be
!                prepared for inclusion into the handle table.
!
!     ARCH       is an integer that indicates the architecture of
!                the file attached to UNIT.  Acceptable values are
!                the parameters:
!
!                   DAF
!                   DAS
!
!                defined in ZZDDHMAN.INC.
!
! $ Detailed_Output
!
!     BFF        is an integer that indicates the binary file format
!                of the DAF attached to UNIT.  Possible values are
!                the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                defined in ZZDDHMAN.INC.
!
! $ Parameters
!
!     See the include file ZZDDHMAN.INC.
!
! $ Exceptions
!
!     1) SPICE(UNKNOWNFILARC) is signaled when ARCH is not in the
!        range of codes for known file architectures or the binary
!        file's ID word is unknown to IDW2AT.  BFF is set to 0 when
!        this error is signaled.  UNIT is not closed.
!
!     2) SPICE(FILEREADFAILED) is signaled when either of the two
!        READ statements in the module returns non-zero IOSTAT, thus
!        indicating read failure.  BFF is set to 0 in this case.  Unit
!        is not closed.
!
!     3) SPICE(FILARCMISMATCH) is signaled when the file attached to
!        UNIT is determined to utilize an architecture that is
!        different from the one to which the input argument ARCH
!        refers.  Unit is not closed.
!
!     4) SPICE(UNKNOWNBFF) is signaled whenever the binary file
!        format detection algorithm reaches a state of uncertainty
!        for DAFs.  This can be the result of several conditions,
!        an empty pre-N0052 DAF, reading a DAF with an unknown BFF
!        from a future toolkit, etc.  In all cases, BFF is set to 0.
!        Unit is not closed.
!
!     5) If a pre-FTP string binary is loaded, no FTP based
!        diagnostics are performed, and the file is assumed to be
!        in proper, working order.
!
! $ Files
!
!     This routine reads at least one, and potentially, several records
!     from the file attached to UNIT.
!
! $ Particulars
!
!     This routine exists to prepare a binary file for inclusion
!     in the handle table in ZZDDHMAN.  This includes verifying
!     that the file is suitable to load and determining the binary
!     file format where possible.
!
!     For DAF files:
!
!        The binary file format of old (pre-N0050) binaries is
!        detectable if the file is non-empty and undamaged.
!        New files contain the binary file format identification
!        string in the file record along with the FTP error
!        detection string.  They are correctly identified in most
!        cases, including damaged.
!
!     For DAS files:
!
!        The binary file format of old (pre-N0052) binaries is
!        not detectable.  This this module will assume that any
!        old DAS binaries are of the native format.  New binaries
!        include the binary file format identification string as
!        well as the FTP error detection string.  They are
!        correctly identified in most cases as well.
!
!     FTP Error Detection:
!
!        FTP error detection occurs when at least part of the
!        detection string is detected in the file record.  When
!        absent, no errors are signaled and the file is then
!        assumed to be an old binary.  In the event that the FTP
!        detection string is present, and additional unknown
!        sequences are present, diagnostics are only performed on
!        sequences known to this version of the toolkit.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
!
! $ Restrictions
!
!     1) The file attached to UNIT was written on a platform whose
!        characters are of a single byte in length.
!
!     2) Numeric data when read as characters from the UNIT
!        preserves the bit patterns present in the file in
!        memory.
!
!     3) The intrinsic ICHAR preserves the bit pattern of the
!        character byte read from a file.  Namely if one examines
!        the integer created the 8 least significant bits will be
!        precisely those found in the character.
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
!-    SPICELIB Version 2.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
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
!-    SPICELIB Version 2.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
!
!        This routine was updated to load binaries created by
!        N0051 versions of Sun Solaris Native C Toolkits.  See
!        the Revisions section for details.
!
!-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
!
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
!
!        Shortly after releasing N0052, a few of our users
!        discovered that they were unable to load binary
!        DAFs created with the N0051 Sun Solaris Native C
!        (SUN-SOLARIS-NATIVE_C) Toolkits.  The reason for this
!        is the previous version of ZZDDHPPF released with N0052
!        assumed that if a DAF file record possessed a valid
!        FTP error detection string, then it must contain a
!        binary file format ID string as well.  Both were added
!        to the DAF file record in N0050.
!
!        However, a bug in the N0051 version of the ZZPLATFM
!        master file, the source of the binary file format ID
!        string for a given platform, neglected to assign a
!        value to the string.  Since it was a C environment
!        and the implementation of ZZPLATFM resulted in the
!        string being a static variable, it was initialized
!        to nulls and written into the file.
!
!        This version of ZZDDHPPF has been extended to recognize
!        a null binary file format ID, and apply the byte
!        examination algorithm used on pre-N0050 DAFs to determine
!        its format.
!
!-&


!
!     SPICELIB Functions
!
      integer(ip)           isrchc
      integer(ip)           pos

      logical               return

!
!     Local Parameters
!
!     Number of characters to be read in from a record.
!
      integer(ip)           numchr
      parameter           ( numchr = 1000 )

!
!     Bounding indices for the window that brackets the FTP
!     error detection string in the file record.
!
      integer(ip)           ftpstr
      parameter           ( ftpstr = 500 )

      integer(ip)           ftpend
      parameter           ( ftpend = 1000 )

!
!     Index of the start of the binary file format identification
!     string in DAF binaries.
!
      integer(ip)           dafbff
      parameter           ( dafbff = 89 )

!
!     Index of the start of the binary file format identification
!     string in DAS binaries.
!
      integer(ip)           dasbff
      parameter           ( dasbff = 85 )

!
!     Size of the binary format identification string.
!
      integer(ip)           bffsiz
      parameter           ( bffsiz = 8 )

!
!     Index of the first byte of NI in the DAF file record.
!
      integer(ip)           nipos
      parameter           ( nipos = 13 )

!
!     Index of the first byte of NSUM in the DAF descriptor record.
!
      integer(ip)           nsmpos
      parameter           ( nsmpos = 17 )

!
!     Index of the first byte of FDREC in the DAF file record.
!
      integer(ip)           fdrpos
      parameter           ( fdrpos = 77 )

!
!     Integer code such that CHAR(INTNUL) produces the NULL character.
!
      integer(ip)           intnul
      parameter           ( intnul = 0 )

!
!     IDW2AT Output Argument Lengths.
!
      integer(ip)           idwlen
      parameter           ( idwlen = 4 )

!
!     NULLID is the index of the extended STRBFF "NULL" string ID.
!
      integer(ip)           nullid
      parameter           ( nullid = numbff+1 )

!
!     Local Variables
!
      character*(1)         carg
      character*(numchr)    chrrec
      character*(bffsiz)    bffidw
      character*(idwlen)    filarc
      character*(idwlen)    filtyp
      character*(sizdlm)    ftpdlm
      character*(sizend)    ftplft
      character*(sizstr)    ftpmem
      character*(sizend)    ftprgt
      character*(1)         null
      character*(strsiz)    strarc ( numarc )
      character*(strsiz)    strbff ( nullid )

      integer(ip)           fdrec
      integer(ip)           ftppos
      integer(ip)           i
      integer(ip)           iostat
      integer(ip)           tstarc

      logical               first
      logical               found
      logical               ftperr

!
!     Statement Functions
!
      integer(ip)           zzichr

!
!     Saved Variables
!
      save                  first
      save                  ftpdlm
      save                  ftplft
      save                  ftpmem
      save                  ftprgt
      save                  null
      save                  strarc
      save                  strbff

!
!     Data Statements
!
      data                  first  / .true. /

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
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZDDHPPF' )
      end if

!
!     If this is the first time into the routine, populate local
!     copies of reference values.  This includes the names of the
!     BFF parameters, the names of the ARCH parameters, and the
!     local copy of the FTP string.
!
      if ( first ) then

!
!        Construct and store the NULL valued byte.
!
         null = char(intnul)

!
!        Retrieve the BFF and ARCH names.
!
         do i = 1, numbff
            call zzddhgsd ( 'BFF', i, strbff(i) )
         end do

         do i = 1, numarc
            call zzddhgsd ( 'ARCH', i, strarc(i) )
         end do

!
!        Extend STRBFF to include the null BFFID.  This addresses
!        the N0051 Sun Solaris Native C toolkit binary files.
!
         do i = 1, bffsiz
            strbff(nullid) (i:i) = null
         end do

!
!        Fetch the FTP string.
!
         call zzftpstr ( ftpmem, ftplft, ftprgt, ftpdlm )

!
!        Set FIRST to FALSE so we will not reassign any of these values.
!
         first = .false.

      end if

!
!     Get the simple consistency checks out of the way first.  Is
!     the input ARCH value valid?
!
      if ( ( arch .le. 0 ) .or. ( arch .gt. numarc ) ) then

         bff = 0

         call setmsg ( 'The integer code, ''#'' indicating the file ' &
         //            'architecture to examine is out of range.'     )
         call errint ( '#', arch                                      )
         call sigerr ( 'SPICE(UNKNOWNFILARC)'                         )
         call chkout ( 'ZZDDHPPF'                                     )
         return

      end if

!
!     Read the first record from the file as a string of NUMCHR
!     characters.
!
      read ( unit, rec=1, iostat=iostat ) chrrec

!
!     Check for read failure.
!
      if ( iostat .ne. 0 ) then

         bff = 0

         call setmsg ( 'Error reading the file record from the ' &
         //            'binary DAF file ''#''.  IOSTAT = #.'     )
         call errfnm ( '#', unit                                 )
         call errint ( '#', iostat                               )
         call sigerr ( 'SPICE(FILEREADFAILED)'                   )
         call chkout ( 'ZZDDHPPF'                                )
         return

      end if

!
!     First check the ID word from the input file.
!
      call idw2at ( chrrec(1:8), filarc, filtyp )

!
!     Now locate FILARC in the STRARC array.
!
      tstarc = isrchc ( filarc, numarc, strarc )

!
!     If FILARC was not found, signal an appropriate error.
!
      if ( tstarc .eq. 0 ) then

         bff = 0

         call setmsg ( 'The file, #, has a unidentified file ' &
         //            'architecture.  Check that this file ' &
         //            'is a properly created binary SPICE ' &
         //            'kernel.'                               )
         call errfnm ( '#', unit                               )
         call sigerr ( 'SPICE(UNKNOWNFILARC)'                  )
         call chkout ( 'ZZDDHPPF'                              )
         return

!
!     Otherwise we have an architecture mismatch error, if
!     FILARC does not agree with ARCH.
!
      else if ( tstarc .ne. arch ) then

         bff = 0

         call setmsg ( 'A request to load the # file, $, has ' &
         //            'been made by the % system.  This operation ' &
         //            'is not permitted.'                           )
         call errch  ( '#', strarc(tstarc)                           )
         call errfnm ( '$', unit                                     )
         call errch  ( '%', strarc(arch)                             )
         call sigerr ( 'SPICE(FILARCHMISMATCH)'                      )
         call chkout ( 'ZZDDHPPF'                                    )
         return

      end if

!
!     Now check for possible FTP transfer errors.
!
      call zzftpchk ( chrrec(ftpstr:ftpend), ftperr )

      if ( ftperr ) then

         bff = 0

         call setmsg ( 'FTP transfer error detected.  This ' &
         //            'binary $, ''#'', has most likely been ' &
         //            'corrupted by an ASCII mode FTP transfer. ' &
         //            'Obtain the file using IMAGE or BINARY ' &
         //            'transfer mode from the source.'            )
         call errch  ( '$', strarc(tstarc)                         )
         call errfnm ( '#', unit                                   )
         call sigerr ( 'SPICE(FTPXFERERROR)'                       )
         call chkout ( 'ZZDDHPPF'                                  )
         return

      end if

!
!     Now this search is redundant, but the presence of the
!     FTPLFT string in the latter half of the file record
!     is fairly conclusive evidence that this is a "new" binary,
!     and we can expect to locate the binary file format
!     identification string.
!
      ftppos = pos ( chrrec(500:1000), ftplft, 1 )

!
!     Check to see if we found FTPLFT.  If so extract the binary
!     file format ID word from the file record.
!
      if ( ftppos .ne. 0 ) then

!
!        Extract BFFIDW from CHRREC.
!
         if ( arch .eq. daf ) then
            bffidw = chrrec ( dafbff : (dafbff+bffsiz-1) )
         else if ( arch .eq. das ) then
            bffidw = chrrec ( dasbff : (dasbff+bffsiz-1) )
         end if

!
!        See if we can find BFFIDW in the STRBFF list.
!
         bff = isrchc ( bffidw, numbff + 1, strbff )

!
!        Check to see if BFF is 0, if it is, signal an error since
!        this indicates an unrecognized BFF.
!
         if ( bff .eq. 0 ) then

            call setmsg ( 'The file ''#'' utilizes the binary file ' &
            //            'format ''#''.  This format is currently ' &
            //            'unknown to this toolkit.  A toolkit ' &
            //            'update may be in order.'                  )
            call errfnm ( '#', unit                                  )
            call errch  ( '#', bffidw                                )
            call sigerr ( 'SPICE(UNKNOWNBFF)'                        )
            call chkout ( 'ZZDDHPPF'                                 )
            return

         end if

!
!        See if we have a NULLID situation, if not check out and
!        return as swe have identified the BFF.
!
         if ( bff .ne. nullid ) then
            call chkout ( 'ZZDDHPPF' )
            return
         end if

      end if

!
!     There is no FTP string, if the file is a DAS, we have to
!     assume it is of the native architecture.
!
      if ( arch .eq. das ) then

         call zzplatfm ( 'FILE_FORMAT', bffidw )
         call ucase    ( bffidw,        bffidw )

         bff = isrchc ( bffidw, numbff, strbff )

         if ( bff .eq. 0 ) then

            call setmsg ( 'The native architecture for this ' &
            //            'platform is unknown to this version of ' &
            //            'the toolkit. This is a severe problem ' &
            //            'that should never occur, please contact ' &
            //            'NAIF.'                                    )
            call sigerr ( 'SPICE(BUG)'                               )
            call chkout ( 'ZZDDHPPF'                                 )
            return

         end if

         call chkout ( 'ZZDDHPPF' )
         return

      end if

!
!     If we reach this point, then we are either dealing with
!     an old DAF (created by a pre-N0050 toolkit) or one of the
!     DAFs created by the N0051 Sun Solaris Native C version of
!     the toolkit.  This requires an examination of the bits
!     and bytes in the file that works this way:
!
!        Since in a valid DAF, 2 <= NI <= 250, we can easily
!        determine whether the 4 bytes used to store NI in the
!        file record are little or big endian.  If we discover
!        that the integer is encoded as big-endian, then stop
!        as this file must be 'BIG-IEEE'.  If it is little
!        endian, then locate the first descriptor record
!        in the file.
!
!        Read the first descriptor record.  Extract NSUM, the
!        3rd DP from the record.  If it is 0.0D0, signal an error
!        as this is an empty DAF and we can not determine its
!        type.  If it's non-zero, then check to see if the first
!        4 bytes are "0s".  If they are it must be 'LTL-IEEE'.
!        Otherwise pass it off to ZZDDHIVF to discriminate between
!        'VAX-GFLT' and 'VAX-DFLT'.  We know the first 4 bytes must
!        be "0s" in the 'LTL-IEEE" case, since NSUM is subject to
!        the following inequality: 1 <= NSUM <= 125
!
!     Having laid out the scheme, let's get to it.  First take a
!     look at the four character bytes that hold NI.  These bytes
!     be one of the following:
!
!        Little Endian:  VAL, 0, 0, 0
!           Big Endian:    0, 0, 0, VAL
!
!     where VAL is some non-zero value.
!
      if ( ( chrrec(     nipos:nipos     ) .eq. null ) .and. &
           ( chrrec( (nipos+1):(nipos+1) ) .eq. null ) .and. &
           ( chrrec( (nipos+2):(nipos+2) ) .eq. null ) .and. &
           ( chrrec( (nipos+3):(nipos+3) ) .ne. null )       ) then

         bff = bigi3e

      else if ( ( chrrec(     nipos:nipos     ) .ne. null ) .and. &
                ( chrrec( (nipos+1):(nipos+1) ) .eq. null ) .and. &
                ( chrrec( (nipos+2):(nipos+2) ) .eq. null ) .and. &
                ( chrrec( (nipos+3):(nipos+3) ) .eq. null )       ) then

!
!        At this point we know we are dealing with a little endian
!        file.  Locate the first descriptor record.
!
         fdrec = zzichr( chrrec(     fdrpos:fdrpos     ) )
         fdrec = zzichr( chrrec( (fdrpos+1):(fdrpos+1) ) )*16   + fdrec
         fdrec = zzichr( chrrec( (fdrpos+2):(fdrpos+2) ) )*256  + fdrec
         fdrec = zzichr( chrrec( (fdrpos+3):(fdrpos+3) ) )*4096 + fdrec

!
!        Read the record into CHRREC.
!
         read ( unit, rec=fdrec, iostat=iostat ) chrrec

!
!        Check for read failure.
!
         if ( iostat .ne. 0 ) then

            bff = 0

            call setmsg ( 'Error reading a descriptor record from ' &
            //            'the binary DAF file ''#''.  IOSTAT = #.' )
            call errfnm ( '#', unit                                 )
            call errint ( '#', iostat                               )
            call sigerr ( 'SPICE(FILEREADFAILED)'                   )
            call chkout ( 'ZZDDHPPF'                                )
            return

         end if

!
!        Now examine the NSUM DP in this record to determine the
!        architecture.
!
         if ( ( chrrec(     nsmpos:nsmpos )     .eq. null ) .and. &
              ( chrrec( (nsmpos+1):(nsmpos+1) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+2):(nsmpos+2) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+3):(nsmpos+3) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+4):(nsmpos+4) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+5):(nsmpos+5) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+6):(nsmpos+6) ) .eq. null ) .and. &
              ( chrrec( (nsmpos+7):(nsmpos+7) ) .eq. null )       ) then

!
!           In this case we have an empty DAF, and can not distinguish
!           between little endian formats.  Signal an error and return.
!
            bff = 0

            call setmsg ( 'The DAF, ''#'', appears to contain no ' &
            //            'data.  As such, its binary file format ' &
            //            'can not be determined which prevents ' &
            //            'it from being loaded.'                   )
            call errfnm ( '#', unit                                 )
            call sigerr ( 'SPICE(UNKNOWNBFF)'                       )
            call chkout ( 'ZZDDHPPF'                                )
            return

         else if ( ( chrrec(     nsmpos:nsmpos     ) .eq. null ) .and. &
                   ( chrrec( (nsmpos+1):(nsmpos+1) ) .eq. null ) .and. &
                   ( chrrec( (nsmpos+2):(nsmpos+2) ) .eq. null ) .and. &
                   ( chrrec( (nsmpos+3):(nsmpos+3) ) .eq. null )  ) then

!
!           In this case the file is little endian IEEE.  Set BFF.
!
            bff = ltli3e

         else

!
!           We are probably looking at a VAX file.  Find out which
!           format.
!
            call zzddhivf ( chrrec(nsmpos:(nsmpos+7)), bff, found )

            if ( .not. found ) then

               bff = 0

               call setmsg ( 'Unable to determine the binary file ' &
               //            'format of DAF ''#''.'                 )
               call errfnm ( '#', unit                              )
               call sigerr ( 'SPICE(UNKNOWNBFF)'                    )
               call chkout ( 'ZZDDHPPF'                             )
               return

            end if

         end if

      else

         bff = 0

      end if

      call chkout ( 'ZZDDHPPF' )
      return

      end
