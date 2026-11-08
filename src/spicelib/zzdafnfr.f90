!$Procedure ZZDAFNFR ( Private --- DAF write New File Record )

      subroutine zzdafnfr ( lun, &
                            idword, &
                            nd, &
                            ni, &
                            ifname, &
                            fward, &
                            bward, &
                            free, &
                            format  )
      use pepper_kinds, only: ip

! $ Abstract
!
!    Write the file record to a new DAF file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     UTILITY
!
! $ Declarations

      implicit none

      include              'zzftprms.inc'

      integer(ip)           lun
      character(*)          idword
      integer(ip)           nd
      integer(ip)           ni
      character(*)          ifname
      integer(ip)           fward
      integer(ip)           bward
      integer(ip)           free
      character(*)          format


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUN        I   Logical unit number of an open DAF file.
!     IDWORD     I   DAF ID word.
!     ND         I   Number of double precision components in a summary.
!     NI         I   Number of integer components in a summary.
!     IFNAME     I   Internal filename.
!     FWARD      I   First descriptor record.
!     BWARD      I   Last descriptor record.
!     FREE       I   First free address.
!     FORMAT     I   File binary format identifier string.
!
! $ Detailed_Input
!
!     LUN        is a logical unit number of a DAF whose first record is
!                to be created with a DAF file record bearing the
!                attributes specified by the other arguments.
!
!     IDWORD     is the 'ID word' contained in the first eight
!                characters of the file record.
!
!     ND,        are the number of double precision and integer
!     NI         components, respectively, in each array summary
!                in the specified file.
!
!     IFNAME     is the internal filename to be stored in the file
!                record for identification purposes.
!
!     FWARD,     are the record numbers of the first and last
!     BWARD      descriptor records in the DAF file, respectively.
!                FWARD is greater than 2 whenever reserved records
!                are present.
!
!     FREE       is the first free address pointer.  This integer
!                stores the first free DAF address for writing the
!                next array to be appended to the file.
!
!     FORMAT     is a character string that indicates what the numeric
!                binary format the DAF is utilizing.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See include file zzftprms.inc.
!
! $ Exceptions
!
!     1) If any errors occur from the WRITE to the logical unit LUN,
!        the error SPICE(DAFWRITEFAIL) is signaled. Before returning
!        to the caller, the file attached to LUN is closed and deleted.
!
! $ Files
!
!     This routine writes to the first record of the DAF whose
!     logical unit is LUN.
!
! $ Particulars
!
!     This routine assembles the file record and writes it to the
!     first record in a DAF.  Its purpose is to write new file
!     records only.  For updates, use DAFWFR.
!
!     Make certain the caller checks FAILED() after this returns, since
!     on error it closes and deletes the file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) An individual character must occupy 1 byte of space and
!        conform to the ASCII standard.
!
!     2) The word size for the machine should be at least 32 bits,
!        else the computations to null pad the gaps in the file
!        record may overstep record boundaries.
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
!-    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments. Reordered header sections.
!
!-    SPICELIB Version 1.0.1, 11-DEC-2001 (FST)
!
!        Corrected the omission of IDWORD from the Brief_I/O and
!        Detailed_Input sections of the module header.
!
!-    SPICELIB Version 1.0.0, 02-MAR-1999 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           rtrim
      logical               return

!
!     Local Parameters
!
!     Amount of space measured in characters necessary to
!     null pad between the last character of FORMAT and the
!     first character of FTPSTR to keep FTPSTR at character
!     700 in a 1024 byte record.
!
      integer(ip)           preftp
      parameter           ( preftp = 603 )

!
!     Amount of space measured in characters necessary to
!     null pad from the last character of FTPSTR to the
!     end of the file record. Note: This value assumes the
!     length of the file record is 1024 bytes.  The DAF
!     specification only requires the presence of 1000
!     characters, so this may require modification for
!     non-standard platforms.
!
      integer(ip)           pstftp
      parameter           ( pstftp = 297 )

!
!     Lengths of internal file name, ID word, and format word.
!
      integer(ip)           fmtlen
      parameter           ( fmtlen = 8 )

      integer(ip)           idwlen
      parameter           ( idwlen = 8 )

      integer(ip)           ifnlen
      parameter           ( ifnlen = 60 )

!
!     Local Variables
!
      character(1)          nullch
      character(fmtlen)     locfmt
      character(idwlen)     locidw
      character(ifnlen)     locifn
      character(preftp)     prenul
      character(pstftp)     pstnul
      character(sizdlm)     delim
      character(sizend)     lftbkt
      character(sizend)     rgtbkt
      character(sizftp)     ftpstr
      character(sizstr)     tststr

      integer(ip)           i
      integer(ip)           iostat

      logical               first

!
!     Saved Variables
!
      save                first
      save                ftpstr
      save                prenul
      save                pstnul

!
!     Data Statements
!
      data   first        / .true. /

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     On the first pass, format the PRENUL and PSTNUL strings,
!     and build FTPSTR from its components.
!
      if ( first ) then

!
!        Store NULL into NULLCH.
!
         nullch = char(0)

!
!        Set all of the characters of PRENUL to nulls.
!
         do i = 1, preftp

            prenul (i:i) = nullch

         end do

!
!        Set all of the characters of PSTNUL to nulls.
!
         do i = 1, pstftp

            pstnul (i:i) = nullch

         end do

!
!        Build FTPSTR from its components that come back from
!        ZZFTPSTR.  This private SPICE routine returns the
!        following components:
!7
!           TSTSTR - The test component of the FTP string
!           LFTBKT - The left bracketing, printable, component of
!                    the FTP string.
!           RGTBKT - The right bracketing, printable, component of
!                    the FTP string.
!           DELIM  - The printable delimiter that separates the
!                    individual test character blocks in TSTSTR.
!
!        which are assembled into the FTP string as it appears in
!        the DAF file record.
!
         call zzftpstr ( tststr, lftbkt, rgtbkt, delim )

         ftpstr = lftbkt( 1:rtrim(lftbkt) )                          // &
                  tststr( 1:rtrim(tststr) )                          // &
                  rgtbkt( 1:rtrim(rgtbkt) )

!
!        Stop this block from executing except on the first pass.
!
         first = .false.

      end if

!
!     Make local copies of each of the string arguments.  This way we
!     maintain the proper sizes for each of the string objects, in
!     the event larger or smaller strings are passed in.
!
      locidw = idword
      locifn = ifname
      locfmt = format

!
!     Write the file record components out to the first record of the
!     file.
!
      write ( unit = lun, rec=1, iostat=iostat ) locidw, &
                                                 nd, &
                                                 ni, &
                                                 locifn, &
                                                 fward, &
                                                 bward, &
                                                 free, &
                                                 locfmt, &
                                                 prenul, &
                                                 ftpstr, &
                                                 pstnul
!
!     Check IOSTAT for errors.
!
      if ( iostat /= 0 ) then

!
!        Since we are unable to write to the file record, make
!        certain the output file is destroyed.
!
         call setmsg ( 'Attempt to write file ''#'' failed. '   // &
                       'Value of IOSTAT was #. The file has '   // &
                       'been deleted.'                           )
         call errfnm ( '#', lun                                  )
         call errint ( '#', iostat                               )

         close ( unit = lun, status = 'DELETE' )

         call sigerr ( 'SPICE(DAFWRITEFAIL)' )
         return

      end if


      return
      end
