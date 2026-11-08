!$Procedure ZZDDHMAN ( Private --- DAF/DAS Handle Manager )

      subroutine zzddhman ( lock,   arch,   fname,  method, &
                            handle, unit,   intamh, intarc, &
                            intbff, native, found,  kill    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is an umbrella routine for a collection of entry points
!     to the DAF/DAS handle manager.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     DAS
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'zzddhman.inc'

      logical               lock
      character*(*)         arch
      character*(*)         fname
      character*(*)         method
      integer               handle
      integer               unit
      integer               intamh
      integer               intarc
      integer               intbff
      logical               native
      logical               found
      logical               kill

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LOCK       I   HLU
!     ARCH      I/O  OPN, CLS, HLU, UNL
!     FNAME     I/O  OPN, NFO, FNH
!     METHOD    I/O  OPN
!     HANDLE    I/O  OPN, CLS, HLU, UNL, ISN, NFO, FNH, LUH
!     UNIT      I/O  HLU, LUH
!     INTAMH     O   NFO
!     INTARC     O   NFO
!     INTBFF     O   NFO
!     NATIVE     O   ISN
!     FOUND      O   ISN, NFO, FNH, LUH
!     KILL       I   CLS
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     See the include file 'zzddhman.inc' for details of parameter
!     definitions used within this module.
!
! $ Exceptions
!
!     1) If ZZDDHMAN is called directly, the error SPICE(BOGUSENTRY)
!        is signaled.
!
!     2) See entry points ZZDDHOPN, ZZDDHCLS, ZZDDHHLU, ZZDDHUNL,
!        ZZDDHISN, ZZDDHNFO, ZZDDHFNH, and ZZDDHLUH for exceptions
!        specific to those entry points.
!
! $ Files
!
!     This set of routines is intended to provide low-level services
!     for the creation, updating, and reading of Fortran direct access
!     files utilized by the DAF and DAS systems within SPICE.
!
! $ Particulars
!
!     ZZDDHMAN serves as an umbrella, allowing data to be shared by
!     its entry points:
!
!        ZZDDHOPN       Open file.
!        ZZDDHCLS       Close file.
!        ZZDDHHLU       Handle to logical unit.
!        ZZDDHUNL       Unlock handle from unit.
!        ZZDDHISN       Is the file native architecture?
!        ZZDDHNFO       Fetch information about a handle.
!        ZZDDHFNH       Filename to handle.
!        ZZDDFLUH       Logical unit to handle.
!
!     This umbrella serves a variety of functions to the DAS/DAF
!     families of routines.
!
!        (1) DAF/DAS handle consolidation
!        (2) Binary file format detection and tracking
!        (3) FTP error detection services
!        (4) Logical unit sharing
!        (5) Filename and unit to handle mapping services
!
! $ Examples
!
!     See individual entry points for pointers to modules that utilize
!     their capabilities.
!
! $ Restrictions
!
!     1) Changing the current working directory of a program when
!        more than UTSIZE files are loaded into this interface requires
!        that all filenames passed into ZZDDHOPN are specified with
!        absolute pathnames.  Otherwise the OPEN/CLOSE switching
!        logic will fail to OPEN files that are loaded.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 01-OCT-2021 (NJB)
!
!        Corrected typos in a long error message and in comments.
!
!-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
!
!        Added the "magic number" column to the file table.
!
!-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
!
!        The entry point ZZDDHOPN now invokes ZZPLTCHK, to verify
!        that the runtime environment's binary file format matches
!        the one for which the toolkit is configured.
!
!        The entry point ZZDDHCLS has had its argument list augmented
!        to include a "KILL" flag.  Check the entry point header for
!        details.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
!
!        The toolkit source code is far more sensitive to blind
!        porting of source packaged for one environment to another.
!        This sensitivity has already caused a few of our users
!        some difficulty.  In an attempt to address these problems
!        with future toolkits, ZZDDHOPN now invokes ZZPLTCHK on
!        it's first pass.  This will perform any necessary checks
!        on the runtime environment against the values recorded
!        in ZZPLATFM and other environment specific components
!        of the library.
!
!        As of this release, all that is verified is that the
!        BFF ID listed in ZZPLATFM is compatible with the runtime
!        environment.  See ZZPLTCHK's header for details.
!
!-&

!
!     SPICELIB Functions
!
      integer               bsrchi
      integer               isrchc
      integer               isrchi
      integer               rtrim
      integer               zzddhclu

      logical               failed
      logical               return

!
!     Local Variables
!
      character*(filen)     locfnm
      character*(strsiz)    tmpstr

      double precision      mnm

      integer               accmet
      integer               bff
      integer               filarc
      integer               i
      integer               inqhan
      integer               iostat
      integer               lchar
      integer               locked
      integer               loclun
      integer               supidx

      logical               error
      logical               inqext
      logical               inqopn
      logical               locfnd
      logical               platok

!
!     This logical allows initialization code to execute.
!
      logical               first
      logical               opnfst

!
!     These strings store the labels for the parameters defined
!     in the include file and retrieved by ZZDDHINI.
!
      character*(strsiz)    stramh ( numamh )
      character*(strsiz)    strarc ( numarc )
      character*(strsiz)    strbff ( numbff )

!
!     The file table consists of a set of arrays which serve as
!     'columns' of the table.  The sets of elements having the same
!     index in the arrays form the 'rows' of the table.  Each column
!     contains a particular type of information; each row contains
!     all of the information pertaining to a particular file.
!
!     All column names in the file table begin with 'FT'.  The columns
!     are:
!
!        ABS      Absolute value of HAN
!        AMH      File access method
!        ARC      File architecture
!        BFF      Binary file format
!        HAN      Handle
!        NAM      Filename
!        RTM      RTRIM (right trimmed value for NAM)
!        MNM      Unique DP number (the Magic NuMber ;)
!
!     New 'rows' are added to the end of the list; the list is repacked
!     whenever a file is removed from the list.
!
!     NFT is the number of files currently loaded; this may not be
!     greater than FTSIZE.  FINDEX refers to a file of interest within
!     the table.  Since handles are always assigned in an increasing
!     fashion, FTABS is guaranteed to be a sorted list.  We will use
!     this fact to improve handle lookups in the file table.
!
      integer               nft

      integer               ftabs  ( ftsize )
      integer               ftamh  ( ftsize )
      integer               ftarc  ( ftsize )
      integer               ftbff  ( ftsize )
      integer               fthan  ( ftsize )
      character*(filen)     ftnam  ( ftsize )
      integer               ftrtm  ( ftsize )
      double precision      ftmnm  ( ftsize )

      integer               findex

!
!     NEXT stores the next handle to be used for file access.  This
!     could be either for read or write based operations. NEXT is
!     incremented just before entries in the file table are made.
!     It begins as zero valued.
!
      integer               next

!
!     The unit table consists of a set of arrays which serve as
!     'columns' of the table.  The sets of elements having the same
!     index in the arrays form the 'rows' of the table.  Each column
!     contains a particular type of information; each row contains
!     all of the information pertaining to a particular logical unit.
!
!     All column names in the unit table begin with 'UT'.  The columns
!     are:
!
!        CST      Cost to remove the file from the unit table
!        HAN      Handle
!        LCK      Is this logical unit locked to this handle?
!        LUN      Logical unit
!
!     New 'rows' are added to the end of the list; the list is repacked
!     whenever a logical unit is no longer needed.
!
!     NUT is the number of units currently stored in the table; this
!     may not exceed UTSIZE.  UINDEX refers to a unit of interest
!     within the table.
!
      integer               nut

      integer               utcst ( utsize )
      integer               uthan ( utsize )
      logical               utlck ( utsize )
      integer               utlun ( utsize )

      integer               uindex

!
!     The following stores the native binary file format, a list of
!     codes for supported binary formats, and the number of entries
!     in SUPBFF.
!
      integer               natbff
      integer               supbff ( numbff )
      integer               numsup

!
!     Request counter used to determine cost.
!
      integer               reqcnt

!
!     Saved Variables
!
      save                  first
      save                  opnfst

      save                  stramh
      save                  strarc
      save                  strbff

      save                  nft
      save                  ftabs
      save                  ftamh
      save                  ftarc
      save                  ftbff
      save                  fthan
      save                  ftnam
      save                  ftrtm
      save                  ftmnm

      save                  next

      save                  nut
      save                  utcst
      save                  uthan
      save                  utlck
      save                  utlun

      save                  natbff
      save                  supbff
      save                  numsup

      save                  reqcnt


!
!     Data Statements
!
      data                  first  / .true.                  /
      data                  opnfst / .true.                  /
      data                  nft    / 0                       /
      data                  next   / 0                       /
      data                  nut    / 0                       /
      data                  reqcnt / 0                       /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin  ( 'ZZDDHMAN' )
         call sigerr ( 'SPICE(BOGUSENTRY)' )
         call chkout ( 'ZZDDHMAN' )
      end if

      return




!$Procedure ZZDDHOPN ( Private --- Load file )

      entry zzddhopn ( fname, method, arch, handle )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Load a new direct access file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAS
!     DAF
!     PRIVATE
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     CHARACTER*(*)         METHOD
!     CHARACTER*(*)         ARCH
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of file to be loaded.
!     METHOD     I   Access method used to load the file.
!     ARCH       I   Expected architecture of the file to load.
!     HANDLE     O   Handle assigned to file.
!
! $ Detailed_Input
!
!     FNAME      is the file name of the file to be loaded for direct
!                access.
!
!     METHOD     is the method by which to load the file. Acceptable
!                values are:
!
!                   'READ'    - Load existing file for read access.
!                   'WRITE'   - Load existing file for write access.
!                   'SCRATCH' - Load scratch file.
!                   'NEW'     - Load a new file for write access.
!
!                Note: The value of METHOD is case-insensitive.
!
!     ARCH       is the architecture of the file to be loaded.
!                Acceptable values are:
!
!                   'DAF'    - Load a DAF file
!                   'DAS'    - Load a DAS file
!
!                Note: The value of ARCH is case-insensitive.
!
! $ Detailed_Output
!
!     HANDLE     is the file handle associated with the file.  This
!                handle is used to identify the file in subsequent
!                calls to other routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) The error SPICE(UNSUPPORTEDMETHOD) is signaled when the
!        METHOD input argument is improperly specified.  The value of
!        the output argument HANDLE is undefined when this error is
!        signaled.
!
!     2) The error SPICE(UNSUPPORTEDARCH) is signaled when the ARCH
!        input argument is improperly specified.  The value of the
!        output argument HANDLE is undefined when this error is
!        signaled.
!
!     3) The error SPICE(UTFULL) is signaled whenever METHOD is
!        set to 'SCRATCH' and no available units exist in the
!        unit table for locking.  The value of the output argument
!        HANDLE is undefined when this error is signaled.
!
!     4) The error SPICE(BLANKFILENAME) is signaled whenever METHOD
!        is set to 'READ', 'WRITE', or 'NEW' and the FNAME argument
!        is a blank string.  The value of the output argument HANDLE
!        is undefined when this error is signaled.
!
!     5) The error SPICE(FILENOTFOUND) is signaled whenever METHOD
!        is set to 'READ' or 'WRITE' and an INQUIRE performed on FNAME
!        indicates the file does not exist.  The value of the output
!        argument HANDLE is undefined when this error is signaled.
!
!     6) The error SPICE(IMPROPEROPEN) is signaled if the file
!        associated with FNAME is attached to a unit from some
!        source external to ZZDDHMAN's entry points.  The value of the
!        output argument HANDLE is undefined when this error is
!        signaled.
!
!     7) The error SPICE(FILARCMISMATCH) is signaled when a file is
!        loaded for 'READ' or 'WRITE' and the architecture of the
!        existing file disagrees with that of the input argument ARCH.
!        The value of the output argument HANDLE is undefined when
!        this error is signaled.
!
!     8) The error SPICE(FILEOPENCONFLICT) is signaled when an attempt
!        to load an already loaded file for any access other than READ.
!        The value of the output argument HANDLE is undefined when this
!        error is signaled.
!
!     9) The error SPICE(RWCONFLICT) is signaled when an attempt to
!        load a file for READ access that is already loaded into the
!        handle manager with a conflicting access method.  The value of
!        the output argument HANDLE is undefined when this error is
!        signaled.
!
!    10) The error SPICE(FTFULL) is signaled when an attempt to load
!        more than the maximum number of allowable files, FTSIZE,
!        is made.  The value of the output argument HANDLE is undefined
!        when this error is signaled.
!
!    11) The error SPICE(FILEOPENFAIL) is signaled whenever the
!        the file open fails with non-zero IOSTAT.  The value of the
!        output argument HANDLE is undefined when this error is
!        signaled.
!
!    12) The error SPICE(UNSUPPORTEDBFF) is signaled whenever the file
!        to be opened utilizes a binary file format that the platform
!        does not currently support.  The value of the output argument
!        HANDLE is undefined when this error is signaled.
!
!    13) When loading files with METHOD set to 'NEW', any errors
!        generated by this routine will cause the newly created file
!        to be deleted.
!
!    14) If the toolkit source is improperly configured for the
!        runtime environment, routines in the call tree of this
!        routine may signal errors.
!
! $ Files
!
!     1) All direct access files loaded by this routine for
!        access methods other than 'SCRATCH' are specified by name.
!
!     2) Files opened with access method 'SCRATCH' are referenced
!        only by their logical unit.
!
! $ Particulars
!
!     This private routine is designed to provide a common, unified
!     file load interface for DAF and DAS.
!
! $ Examples
!
!     See DAFOPR, DAFONW, DAFOPW, DASOPR, DASOPS, DASONW, DASOPW for
!     sample usage.
!
! $ Restrictions
!
!     1) Files loaded through this interface should not be opened by
!        any other mechanism until the appropriate call to ZZDDHCLS
!        is made.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
!
!        Updated for the new "magic number" column in the file table.
!
!-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
!
!        This entry point was updated to perform checks on the
!        runtime environment, to verify that the source is properly
!        configured for execution on this environment.  See the
!        Revisions section of ZZDDHMAN for details.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZDDHOPN' )
      end if

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         if ( failed() ) then
            call chkout ( 'ZZDDHOPN' )
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     On first pass, perform any runtime environment checks.
!
      if ( opnfst ) then

         call zzpltchk ( platok )

         if ( failed() ) then
            call chkout ( 'ZZDDHOPN' )
            return
         end if

!
!        Clear OPNFST, since we've performed the diagnostics.
!
         opnfst = .false.

      end if

!
!     Initialize the value of HANDLE to 0.  In the event an error
!     is signaled this invalid value will be returned to the caller
!     for safety.
!
      handle = 0

!
!     Left justify FNAME to compress off any leading spaces.
!
      call ljust ( fname, locfnm )

!
!     Translate the value of the requested access method to the
!     corresponding integer code.
!
      tmpstr = method
      call ucase ( tmpstr, tmpstr )
      accmet = isrchc ( tmpstr, numamh, stramh )

!
!     Check if the code was located.
!
      if ( accmet .eq. 0 ) then

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call setmsg ( 'The attempt to load file, ''#'', with access ' &
         //            'method, ''#'', failed because this access ' &
         //            'method is unsupported.'                        )
         call errch  ( '#', locfnm                                     )
         call errch  ( '#', method                                     )
         call sigerr ( 'SPICE(UNSUPPORTEDMETHOD)'                      )
         call chkout ( 'ZZDDHOPN'                                      )
         return

      end if

!
!     Translate the value of the requested file architecture to
!     the appropriate integer code.
!
      tmpstr = arch
      call ucase ( tmpstr, tmpstr )
      filarc = isrchc ( tmpstr, numarc, strarc )

!
!     Check if the code was located.
!
      if ( filarc .eq. 0 ) then

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call setmsg ( 'The attempt to load file, ''#'', with ' &
         //            'architecture, ''#'', failed because this ' &
         //            'file architecture is unsupported.'             )
         call errch  ( '#', locfnm                                     )
         call errch  ( '#', arch                                       )
         call sigerr ( 'SPICE(UNSUPPORTEDARCH)'                        )
         call chkout ( 'ZZDDHOPN'                                      )
         return

      end if

!
!     Perform any preliminary checks that must be done before
!     fetching a logical unit from the unit table.  This requires
!     branching based on ACCMET's value.
!
      if ( accmet .eq. scrtch ) then

!
!        Check to see if there are enough units available for locking
!        in the unit table.  If not, signal an error as all files
!        open with SCRTCH access must be locked to their units.
!
         locked = zzddhclu ( utlck, nut )

         if ( locked .ge. (utsize - rsvunt) ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call setmsg ( 'The maximum number of units are locked ' &
            //            'to handles.  As such, there is no room ' &
            //            'to open the requested scratch file.'     )
            call sigerr ( 'SPICE(UTFULL)'                           )
            call chkout ( 'ZZDDHOPN'                                )
            return

         end if

!
!     The NEW, READ, and WRITE access methods perform the same
!     checks on LOCFNM.
!
      else if ( ( accmet .eq. new   ) .or. &
                ( accmet .eq. read  ) .or. &
                ( accmet .eq. write )      ) then

!
!        Check for a non-blank file name.
!
         if ( locfnm .eq. ' ' ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call setmsg ( 'The attempt to load the file has failed, ' &
            //            'because the filename is blank.'            )
            call sigerr ( 'SPICE(BLANKFILENAME)'                      )
            call chkout ( 'ZZDDHOPN'                                  )
            return

         end if

      end if

      mnm = 0.d0

!
!     In the READ or WRITE cases verify that LOCFNM is not already
!     in the file table.
!
      if ( ( accmet .eq. read  ) .or. &
           ( accmet .eq. write )      ) then

!
!        Check to see if the file associated with LOCFNM is already in
!        the file table.
!
         call zzddhf2h ( locfnm,  ftabs,  ftamh,  ftarc, ftbff, &
                         fthan,   ftnam,  ftrtm,  ftmnm, nft,   utcst, &
                         uthan,   utlck,  utlun,  nut,   inqext, &
                         inqopn,  inqhan, locfnd, mnm                 )

!
!        First, check FAILED(), and return if anything has gone awry.
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         if ( failed() ) then
            call chkout ( 'ZZDDHOPN' )
            return
         end if

!
!        Now perform some simple sanity checks before preparing to
!        load the file.  First check to see if the file exists, it must
!        if we are going to open it with ACCMET set to READ or WRITE.
!
         if ( .not. inqext ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call setmsg ( 'The file ''#'' does not exist.' )
            call errch  ( '#', locfnm                      )
            call sigerr ( 'SPICE(FILENOTFOUND)'            )
            call chkout ( 'ZZDDHOPN'                       )
            return

         end if

!
!        Now if the file was not found in the file table, and it is
!        attached to a unit, this presents a problem.
!
         if ( ( .not. locfnd ) .and. ( inqopn ) ) then

!
!           Get the unit to include in the error message.
!
            inquire ( file   = locfnm, &
                      number = loclun, &
                      iostat = iostat  )

!
!           Since we performed a very similar INQUIRE statement in
!           ZZDDHF2H, a non-zero IOSTAT value indicates a severe error.
!
            if ( iostat .ne. 0 ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call setmsg ( 'INQUIRE failed.' )
               call sigerr ( 'SPICE(BUG)'      )
               call chkout ( 'ZZDDHOPN'        )
               return

            end if

!
!           Signal the error. Recall HANDLE was initialized to 0, and
!           this invalid value is returned to the caller.
!
            call setmsg ( 'The file ''#'' is already connected to ' &
            //            'unit #.'                                 )
            call errch  ( '#', locfnm                               )
            call errint ( '#', loclun                               )
            call sigerr ( 'SPICE(IMPROPEROPEN)'                     )
            call chkout ( 'ZZDDHOPN'                                )
            return

         end if

!
!        Lastly check to see if the file in the file table, and
!        perform the appropriate sanity checks.
!
         if ( locfnd ) then

            findex = bsrchi ( abs(inqhan), nft, ftabs )

!
!           Check to see if the requested architecture does not match
!           that of the entry in the file table.
!
            if ( filarc .ne. ftarc(findex) ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call setmsg ( 'The attempt to load file ''#'' as a # ' &
               //            'has failed because it is already loaded ' &
               //            'as a #.'                                 )
               call errch  ( '#', locfnm                               )
               call errch  ( '#', strarc(filarc)                       )
               call errch  ( '#', strarc(ftarc(findex))                )
               call sigerr ( 'SPICE(FILARCMISMATCH)'                   )
               call chkout ( 'ZZDDHOPN'                                )
               return

            end if

!
!           Check to see if the access method is anything other
!           than READ.  If so, signal the appropriate error.
!           Note: this is only for READ.
!
            if ( accmet .ne. read ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call setmsg ( 'File ''#'' already loaded.' )
               call errch  ( '#', locfnm                  )
               call sigerr ( 'SPICE(FILEOPENCONFLICT)'    )
               call chkout ( 'ZZDDHOPN'                   )
               return

            end if

!
!           If we reach here, then we have a file that exists
!           in the table, and the caller is attempting to load it
!           for READ access.  Check to make certain it is not
!           already loaded with another method.
!
            if ( accmet .ne. ftamh(findex) ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call setmsg ( 'Unable to load file ''#'' for # ' &
               //            'access.  It is already loaded with ' &
               //            'the conflicting access #.'           )
               call errch  ( '#', locfnm                           )
               call errch  ( '#', stramh(accmet)                   )
               call errch  ( '#', stramh(ftamh(findex))            )
               call sigerr ( 'SPICE(RWCONFLICT)'                   )
               call chkout ( 'ZZDDHOPN'                            )
               return

            end if

!
!           If we make it this far, the file is in the file table
!           and all the sanity checks have passed. Return to the
!           caller as this is effectively a no-op.
!
            handle = fthan ( findex )

            call chkout( 'ZZDDHOPN' )
            return

         end if

      end if

!
!     Now check to see if there is room in the file table for this
!     new file.
!
      if ( nft .eq. ftsize ) then

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call setmsg ( 'The file table is full, with # entries. ' &
         //            'As a result, the file ''#'' could not be ' &
         //            'loaded.'                                   )
         call errint ( '#', nft                                    )
         call errch  ( '#', locfnm                                 )
         call sigerr ( 'SPICE(FTFULL)'                             )
         call chkout ( 'ZZDDHOPN'                                  )
         return

      end if

!
!     We are about to attempt a HANDLE to LUN connection, increment
!     the request counter.
!
      call zzddhrcm ( nut, utcst, reqcnt )

!
!     Free up a logical unit in the UNIT table for our usage.
!
      call zzddhgtu ( utcst, uthan, utlck, utlun, nut, uindex )

!
!     Check FAILED() since ZZDDHGTU may have invoked GETLUN.
!     Recall HANDLE was initialized to 0, and this invalid
!     value is returned to the caller.
!
      if ( failed() ) then
         call chkout ( 'ZZDDHOPN' )
         return
      end if

!
!     Trim up the filename.
!
      if ( accmet .ne. scrtch ) then
         lchar = rtrim ( locfnm )
      end if

!
!     If we have made it this far, then we're ready to perform the
!     appropriate open.  First get the handle ready.
!
      next = next + 1

!
!     Determine the sign of the new handle based on the requested
!     METHOD.
!
      if ( accmet .eq. read ) then
         uthan ( uindex ) = next
      else
         uthan ( uindex ) = -next
      end if

!
!     The code that follows is structured a little strangely.  This
!     discussion is an attempt to clarify what the code does and
!     the motivation that led to its peculiar construction.
!
!     First, the file, scratch or otherwise, is opened with the
!     appropriate OPEN statement.  Then, the logical ERROR is set
!     to TRUE or FALSE depending on whether and IOSTAT error has
!     occurred as a result of the OPEN.  At this point, the code
!     enters into a IF block structured in the following manner:
!
!        IF ( ERROR ) THEN
!
!           Signal the IOSTAT related error from the OPEN statement.
!
!        ELSE IF ( ACCMET .EQ SCRTCH ) THEN
!
!           Attempt to INQUIRE on the UNIT assigned to the scratch
!           file to determine its name.  Store a default value,
!           in the event one is not returned.
!
!        ELSE IF ( ACCMET .EQ. READ ) .OR. ( ACCMET .EQ. WRITE ) THEN
!
!           Examine the preexisting file to determine if its FTP
!           detection string, file architecture, and binary
!           file format are acceptable.  If not, then signal the
!           error, set ERROR to TRUE, and do not check out or
!           return.
!
!        END IF
!
!        IF ( ERROR ) THEN
!
!           Remove the UNIT from the unit table. Decrement NEXT,
!           since the current value is not to be assigned as
!           a handle for this file. Check out and return.
!
!        END IF
!
!     The reason the code is structured in this unusual fashion
!     is to allow for a single treatment of the clean up on error
!     code to exist.
!

!
!     Perform the OPEN.  Branch on the appropriate access method.
!
      if ( accmet .eq. scrtch ) then

         open ( unit   = utlun(uindex), &
                access = 'DIRECT', &
                recl   = recl, &
                status = 'SCRATCH', &
                iostat = iostat           )

         bff = natbff

      else if ( accmet .eq. new ) then

         open ( unit   = utlun(uindex), &
                file   = locfnm(1:lchar), &
                access = 'DIRECT', &
                recl   = recl, &
                status = 'NEW', &
                iostat = iostat           )

         bff = natbff

      else if ( accmet .eq. read ) then

         open ( unit   = utlun(uindex), &
                file   = locfnm(1:lchar), &
                access = 'DIRECT', &
                recl   = recl, &
                status = 'OLD', &
                iostat = iostat           )

      else if ( accmet .eq. write ) then

         open ( unit   = utlun(uindex), &
                file   = locfnm(1:lchar), &
                access = 'DIRECT', &
                recl   = recl, &
                status = 'OLD', &
                iostat = iostat           )

      end if

!
!     Verify that IOSTAT is non-zero.
!
      error = ( iostat .ne. 0 )

!
!     Partially process the error.
!
      if ( error ) then

!
!        Now signal the error, but delay cleaning up and checking
!        out until leaving this IF block.
!
         if ( accmet .eq. scrtch ) then
            call setmsg ( 'Attempt to open scratch file failed. ' &
            //            'IOSTAT was #.'                         )
         else if ( accmet .eq. new ) then
            call setmsg ( 'Attempt to create new file, ''$'' ' &
            //            'failed. IOSTAT was #.'                 )
         else
            call setmsg ( 'Attempt to open file, ''$'' for % ' &
            //            'access failed. IOSTAT was #.'          )
         end if

         call errch  ( '$', locfnm                                )
         call errch  ( '%', stramh ( accmet )                     )
         call errint ( '#', iostat                                )
         call sigerr ( 'SPICE(FILEOPENFAIL)'                      )

!
!     If no IOSTAT based error has occurred as a result of the OPEN
!     statement, then perform any remaining checks or I/O operations
!     that are necessary to support loading the file.
!
      else if ( accmet .eq. scrtch ) then

!
!        Inquire on the logical unit to produce the file name for
!        the scratch file.  Set the initial value of LOCFNM, in case
!        the INQUIRE does not replace it.
!
         locfnm = '# SCRATCH FILE'
         call repmc ( locfnm, '#', strarc(filarc), locfnm )

         inquire ( unit   = utlun(uindex), &
                   name   = locfnm, &
                   iostat = iostat         )

!
!        In the event that this INQUIRE failed, replace the value
!        stored in LOCFNM with the initial value.
!
         if ( iostat .ne. 0 ) then

            locfnm = '# SCRATCH FILE'
            call repmc ( locfnm, '#', strarc(filarc), locfnm )

         end if

!
!        Store the RTRIM value of this filename in LCHAR.
!
         lchar = rtrim ( locfnm )

      else if ( ( accmet .eq. read  ) .or. &
                ( accmet .eq. write )      ) then

!
!        Check for FTP errors, verify that FILARC is appropriate,
!        and determine the binary file format of the preexisting
!        file LOCFNM.
!
         call zzddhppf ( utlun(uindex), filarc, bff )

!
!        Set ERROR.
!
         error = failed()

!
!        If no error has occurred, verify that BFF is among the
!        list of supported format ID codes for the requested access
!        method.
!
         if ( .not. error ) then

!
!           This platform supports reading from files whose
!           format codes are listed in SUPBFF.
!
            if ( accmet .eq. read ) then

               supidx = isrchi ( bff, numsup, supbff )

               if ( supidx .eq. 0 ) then

!
!                 Delay clean up and check out.
!
                  error = .true.

                  if ( bff .eq. 0 ) then

                     call setmsg ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  This ' &
                     //            'file utilizes an unknown binary ' &
                     //            'file format.  This error may ' &
                     //            'result from attempting to open a ' &
                     //            'corrupt file or one of an ' &
                     //            'unknown type.'                     )
                     call errch  ( '#', locfnm                         )
                     call sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

                  else

                     call setmsg ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  The ' &
                     //            'non-native binary file format ' &
                     //            '''#'' is not currently supported ' &
                     //            'on this platform.  Obtain a ' &
                     //            'transfer format version, and ' &
                     //            'convert it to the native format. ' &
                     //            'See the Convert User''s Guide ' &
                     //            'for details.'                      )
                     call errch  ( '#', locfnm                         )
                     call errch  ( '#', strbff(bff)                    )
                     call sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

                  end if

               end if

!
!           This platform only supports writing to files whose
!           binary formats are native.
!
            else

!
!              Delay clean up and check out.
!
               if ( bff .eq. 0 ) then

                  error = .true.

                  call setmsg ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes an unknown binary ' &
                  //            'file format.  This error may ' &
                  //            'result from attempting to open a ' &
                  //            'corrupt file or one of an unknown ' &
                  //            'type.'                              )
                  call errch  ( '#', locfnm                          )
                  call sigerr ( 'SPICE(UNSUPPORTEDBFF)'              )

               else if ( bff .ne. natbff ) then

                  error = .true.

                  call setmsg ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes the non-native ' &
                  //            'binary file format ''#''.  At ' &
                  //            'this time only files of the ' &
                  //            'native format, ''#'', are ' &
                  //            'supported for write access.  See ' &
                  //            'the Convert User''s Guide for ' &
                  //            'details.'                          )
                  call errch  ( '#', locfnm                         )
                  call errch  ( '#', strbff(bff)                    )
                  call errch  ( '#', strbff(natbff)                 )
                  call sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

               end if

            end if

         end if

      end if

!
!     If an error has occurred as a result of opening the file or
!     examining its contents, clean up and check out.
!
      if ( error ) then

!
!        Close the unit we were using.  Remember to delete the file
!        if it was a 'new' one.
!
         if ( accmet .eq. new ) then
            close ( unit = utlun(uindex), status = 'DELETE' )
         else
            close ( unit = utlun(uindex) )
         end if

!
!        Remove the unit from the unit table, since this UNIT
!        is no longer in use.
!
         call zzddhrmu ( uindex, nft, utcst, uthan, utlck, utlun, nut )

!
!        Decrement NEXT since this handle was never assigned to
!        a file.
!
         next = next - 1

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call chkout ( 'ZZDDHOPN' )
         return

      end if


!
!     Finish filling out the unit table.
!
      utcst ( uindex ) = reqcnt

!
!     Only scratch files get the units locked to handles, this is
!     because they only exist as long as they have a unit.
!
      utlck ( uindex ) = accmet .eq. scrtch

!
!     Now fill out the file table.
!
      nft = nft + 1

!
!     Use the absolute value of the handle used to index the file
!     table.
!
      ftabs ( nft ) = iabs ( uthan( uindex ) )

!
!     Assign access method, file architecture, and native binary file
!     format to the appropriate columns.
!
      ftamh ( nft ) = accmet
      ftarc ( nft ) = filarc
      ftbff ( nft ) = bff

!
!     Assign the handle, filename, RTRIM ( FTNAM(NFT) ) as FTRTM, and
!     unique DP number as FTMNM.
!
      fthan ( nft ) = uthan ( uindex )
      ftnam ( nft ) = locfnm(1:lchar)
      ftrtm ( nft ) = lchar
      ftmnm ( nft ) = mnm

!
!     Assign HANDLE the value of the new handle.
!
      handle = fthan( nft )

      call chkout ( 'ZZDDHOPN' )
      return




!$Procedure ZZDDHCLS ( Private --- Close file )

      entry zzddhcls ( handle, arch, kill )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Close the file associated with HANDLE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     DAS
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         ARCH
!     LOGICAL               KILL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle associated with the file to close.
!     ARCH       I   Expected architecture of the handle to close.
!     KILL       I   Logical indicating whether to delete the file.
!
! $ Detailed_Input
!
!     HANDLE     is the file handle associated with the file that is
!                to be closed.
!
!     ARCH       is the expected architecture of the file associated
!                with HANDLE.
!
!     KILL       is a logical that indicates whether to kill the file
!                associated with HANDLE.  Essentially it performs:
!
!                   CLOSE ( UNIT, STATUS = 'DELETE')
!
!                on the file.  This only works if HANDLE is currently
!                assigned a UNIT in the UNIT table.
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
!     1) SPICE(FILARCMISMATCH) is signaled if the specified architecture
!        does not match the one listed in the file table.
!
!     2) SPICE(INVALIDACCESS) is signaled if KILL is set to .TRUE., and
!        HANDLE is associated with a file open for READ access.
!
!     3) SPICE(FILENOTCONNECTED) is signaled if KILL is set to .TRUE.,
!        and the file associated with handle is not currently in the
!        unit table.  The file is removed from the file table (closed)
!        as a result, even if this error is signaled.
!
!     4) If HANDLE is not found in the file table, this routine simply
!        returns to the caller.
!
! $ Files
!
!     This routine will close the file associated with HANDLE if it
!     is currently utilizing a logical unit.
!
! $ Particulars
!
!     This routine closes files in the file table and performs
!     any necessary operations to facilitate the proper disconnect
!     from any logical unit.
!
!     This routine may also be used to delete a file that is open
!     for write access if it currently has an entry in the UNIT table.
!
! $ Examples
!
!     See DAFCLS or DASLLC.
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
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
!
!        Updated for the new "magic number" column in the file table.
!
!-    SPICELIB Version 2.0.0, 02-APR-2001 (FST)
!
!        Added a "KILL" argument to the argument list of the routine.
!        This will allow certain raw close statements to be replaced
!        with calls to ZZDDHCLS.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!-&
!
! $ Revisions
!
!-    SPICELIB Version 2.0.0, 02-APR-2002 (FST)
!
!        Added the error SPICE(FILENOTCONNECTED) since the KILL
!        functionality is only required in the entry points:
!
!            DASFM - DASOPN, DASONW
!            DAFAH - DAFOPN, DAFONW
!
!        These routines open new files, so they reference newly
!        created handles that have entries in the UNIT table. Thus
!        the decision was made to signal the error
!        SPICE(FILENOTCONNECTED) rather than connect a file not
!        present in the unit table when KILL is set.
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZDDHCLS' )
      end if

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( failed() ) then
            call chkout ( 'ZZDDHCLS' )
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Find the file in the handle table.
!
      findex = bsrchi ( abs(handle), nft, ftabs )

!
!     Check to see whether we found the handle or not.
!
      if ( findex .eq. 0 ) then
         call chkout ( 'ZZDDHCLS' )
         return
      else if ( fthan(findex) .ne. handle ) then
         call chkout ( 'ZZDDHCLS' )
         return
      end if

!
!     Before actually closing the file, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architecture's code from stepping on another's.
!
      tmpstr = arch
      call ucase ( tmpstr, tmpstr )
      filarc = isrchc ( tmpstr, numarc, strarc )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc .ne. ftarc(findex) ) then

         call setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be closed by routines in ' &
         //            'in the % system.'                         )
         call errch  ( '#', strarc( ftarc(findex) )               )
         call errch  ( '%', tmpstr                                )
         call errch  ( '$', ftnam(findex)                         )
         call sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         call chkout ( 'ZZDDHCLS'                                 )
         return

      end if

!
!     Now check that if KILL is set, the file is accessible for
!     WRITE.
!
      if ( ( kill ) .and. ( ftamh(findex) .eq. read ) ) then

         call setmsg ( '# file $ is open for READ access.  Attempt ' &
         //            'to close and delete file has failed. '       )
         call errch  ( '#', strarc( ftarc(findex) )                  )
         call errch  ( '#', ftnam(findex)                            )
         call sigerr ( 'SPICE(INVALIDACCESS)'                        )
         call chkout ( 'ZZDDHCLS'                                    )
         return

      end if

!
!     Buffer the access method for HANDLE, since we may need it
!     when deciding which close to perform.
!
      accmet = ftamh (findex)

!
!     If we reach here, we need to remove the row FINDEX from
!     the file table.
!
      do i = findex+1, nft

         ftabs(i-1) = ftabs(i)
         ftamh(i-1) = ftamh(i)
         ftarc(i-1) = ftarc(i)
         ftbff(i-1) = ftbff(i)
         fthan(i-1) = fthan(i)
         ftnam(i-1) = ftnam(i)
         ftrtm(i-1) = ftrtm(i)
         ftmnm(i-1) = ftmnm(i)

      end do

      nft = nft - 1

!
!     Locate HANDLE in the unit table.
!
      uindex = isrchi ( handle, nut, uthan )

      if ( uindex .ne. 0 ) then

!
!        Close the unit.
!
         if ( ( kill ) .and. ( accmet .ne. scrtch ) ) then
            close ( unit = utlun(uindex), status = 'DELETE' )
         else
            close ( unit = utlun(uindex) )
         end if

!
!        Remove its entry from the unit table.
!
         call zzddhrmu ( uindex, nft, utcst, uthan, utlck, utlun, nut )

      else

!
!        First, check to see if KILL is set, if it is signal an error
!        since we are unable to delete the file.
!
         if ( ( kill ) .and. ( accmet .ne. scrtch ) ) then

            call setmsg ( 'File successfully closed.  Unable to ' &
            //            'delete file as requested.  File not ' &
            //            'currently present in the UNIT table. ' )
            call sigerr ( 'SPICE(FILENOTCONNECTED)'               )
            call chkout ( 'ZZDDHCLS'                              )
            return

         end if

!
!        If we were unable to find the HANDLE in the unit table,
!        check to see if we have to clean up the UNIT table.
!
         if ( nft .lt. nut ) then

            uindex = isrchi ( 0, nut, uthan )

!
!           Now check to see if we located a zero valued handle.
!           If we did not manage to, then this is an error condition,
!           since we have more LUNs listed in the unit table than
!           files in the file table.
!
            if ( uindex .eq. 0 ) then
               call setmsg ( 'There are less files in the file table ' &
               //            'than units in the unit table, and no ' &
               //            'row with a zero-valued handle can be ' &
               //            'found.  This should never occur.'        )
               call sigerr ( 'SPICE(BUG)'                              )
               call chkout ( 'ZZDDHCLS'                                )
               return
            end if

!
!           Free the unit.
!
            call frelun ( utlun(uindex) )

!
!           Compress the table.
!
            do i = (uindex + 1), nut

               utcst(i-1) = utcst(i)
               uthan(i-1) = uthan(i)
               utlck(i-1) = utlck(i)
               utlun(i-1) = utlun(i)

            end do

!
!           Decrement NUT.
!
            nut = nut - 1

         end if
      end if

      call chkout ( 'ZZDDHCLS' )
      return




!$Procedure ZZDDHHLU ( Private --- Handle to Logical Unit )

      entry zzddhhlu ( handle, arch, lock, unit )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the logical unit associated with a handle, in the event
!     the handle is not connected to a unit, connect it to one.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAS
!     DAF
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         ARCH
!     LOGICAL               LOCK
!     INTEGER               UNIT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle associated with the file of interest.
!     ARCH       I   Expected file architecture.
!     LOCK       I   Logical indicating to lock UNIT to HANDLE.
!     UNIT       O   Corresponding logical unit.
!
! $ Detailed_Input
!
!     HANDLE     is the handle associated with the file to retrieve a
!                logical unit.
!
!     ARCH       is the expected file architecture of the file
!                associated with HANDLE.
!
!     LOCK       is a logical that indicates whether the UNIT should be
!                locked to HANDLE.  Locked units will keep the files
!                open and assigned the same logical unit.  They may
!                only be unlocked by calling ZZDDHUNL.
!
! $ Detailed_Output
!
!     UNIT       is the logical unit that is currently assigned to
!                HANDLE.  This unit may be used to perform I/O
!                operations on the file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) The error SPICE(NOSUCHHANDLE) is signaled when HANDLE is not
!        found in the file table.  The value of UNIT is undefined when
!        this error occurs.
!
!     2) The error SPICE(FILARCMISMATCH) is signaled if the specified
!        architecture does not match the one listed for HANDLE in the
!        file table.  The value of UNIT is undefined when this error
!        occurs.
!
!     3) SPICE(FILEOPENFAIL) is signaled only when an attempt to
!        attach a logical unit to the file associated with HANDLE
!        fails.  The value of UNIT is undefined when this error
!        occurs.
!
!     4) The error SPICE(HLULOCKFAILED) is signaled when the input
!        LOCK logical has a value of TRUE, and there are no free
!        'lockable' units left in the unit table.  The value of UNIT
!        returned when this error is signaled is undefined.
!
! $ Files
!
!     If HANDLE refers to a file not currently connected to a logical
!     unit, this routine will locate an entry in the unit table;
!     disconnect it if necessary; and connect the file associated with
!     HANDLE in its place.
!
! $ Particulars
!
!     This routine is used to retrieve a logical unit for a desired
!     handle.  It also serves as a mechanism for locking the UNIT
!     to HANDLE relationship for a particular handle.  This routine
!     can not be used to unlock this relationship.  See ZZDDHUNL for
!     that functionality.
!
! $ Examples
!
!     See DAFHLU or DASHLU for sample usage.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 01-OCT-2021 (NJB)
!
!        Fixed typo in long error message and in comments.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZDDHHLU' )
      end if

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( failed() ) then
            unit = 0
            call chkout ( 'ZZDDHHLU' )
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Locate HANDLE in the file table.
!
      findex = bsrchi ( abs(handle), nft, ftabs )

      if ( findex .eq. 0 ) then
         error = .true.
      else if ( fthan(findex) .ne. handle ) then
         error = .true.
      else
         error = .false.
      end if

      if ( error ) then

         unit = 0

         call setmsg ( 'There is no file loaded with handle = #' )
         call errint ( '#', handle                               )
         call sigerr ( 'SPICE(NOSUCHHANDLE)'                     )
         call chkout ( 'ZZDDHHLU'                                )
         return
      end if

!
!     Before actually fetching the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      tmpstr = arch
      call ucase ( tmpstr, tmpstr )
      filarc = isrchc ( tmpstr, numarc, strarc )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc .ne. ftarc(findex) ) then

         unit = 0

         call setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         call errch  ( '#', strarc( ftarc(findex) )               )
         call errch  ( '%', tmpstr                                )
         call errch  ( '$', ftnam(findex)                         )
         call sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         call chkout ( 'ZZDDHHLU'                                 )
         return

      end if

!
!     If we make it this far, then we will be processing a handle
!     to logical unit request.  Increment REQCNT.
!
      call zzddhrcm ( nut, utcst, reqcnt )

!
!     Now check to see if the handle is already present in the
!     unit table.
!
      uindex = isrchi ( handle, nut, uthan )

!
!     Check to see if we didn't locate the HANDLE in the table.
!     If we didn't, open the file associated with HANDLE again,
!     and get it into the unit table.
!
      if ( uindex .eq. 0 ) then

!
!        We need a unit from the unit table, get one.
!
         call zzddhgtu ( utcst, uthan, utlck, utlun, nut, uindex )

!
!        Check FAILED, since ZZDDHGTU may have invoked GETLUN.
!
         if ( failed() ) then

            unit = 0

            call chkout ( 'ZZDDHHLU' )
            return
         end if

!
!        Re-attach the file to a logical unit.  Branch based on the
!        access method stored in the file table.
!
         if ( ( ftamh(findex) .eq. new   ) .or. &
              ( ftamh(findex) .eq. write )      ) then

            open ( unit   = utlun(uindex), &
                   file   = ftnam(findex)(1:ftrtm(findex)), &
                   access = 'DIRECT', &
                   recl   = recl, &
                   status = 'OLD', &
                   iostat = iostat           )

         else if ( ftamh(findex) .eq. read ) then

            open ( unit   = utlun(uindex), &
                   file   = ftnam(findex)(1:ftrtm(findex)), &
                   access = 'DIRECT', &
                   recl   = recl, &
                   status = 'OLD', &
                   iostat = iostat           )

         else

            unit = 0

            call setmsg ( 'Invalid access method. This error ' &
            //            'should never be signaled.'          )
            call sigerr ( 'SPICE(BUG)'                         )
            call chkout ( 'ZZDDHHLU'                           )
            return
         end if

!
!        Check IOSTAT for troubles.
!
         if ( iostat .ne. 0 ) then

!
!           The re-open was unsuccessful, leave the entry in the file
!           table and clean up the row in the unit table before
!           returning.  Normally when we call ZZDDHRMU it is to
!           remove a unit from the unit table.  In this case we
!           know the unit will remain since we have not decreased
!           the entries in the file table.
!
            call zzddhrmu ( uindex, nft,   utcst, uthan, &
                            utlck,  utlun, nut           )

!
!           Now signal the error.
!
            unit = 0

            call setmsg ( 'Attempt to reconnect logical unit to ' &
            //            'file ''#'' failed. IOSTAT was #.'      )
            call errch  ( '#', ftnam(findex)                      )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEOPENFAIL)'                   )
            call chkout ( 'ZZDDHHLU'                              )
            return

         end if

!
!        Lastly populate the unit table values.
!
         uthan(uindex) = fthan(findex)
         utlck(uindex) = .false.

      end if

!
!     At this point UINDEX points to the row in the unit table that
!     contains the connection information.  We need to update the cost
!     row with the new value of REQCNT, and then set the lock row to
!     TRUE if a lock request was made.
!
      utcst(uindex) = reqcnt

      if ( lock .and. (.not. utlck(uindex)) ) then

!
!        First check to see if we have enough lockable units
!        left in the unit table.
!
         locked = zzddhclu ( utlck, nut )

         if ( locked .ge. ( utsize - rsvunt - scrunt ) ) then

            unit = 0

            call setmsg ( 'Unable to lock handle for file ''#'' ' &
            //            'to a logical unit.  There are no rows ' &
            //            'available for locking in the unit table.' )
            call errch  ( '#', ftnam(findex)                         )
            call sigerr ( 'SPICE(HLULOCKFAILED)'                     )
            call chkout ( 'ZZDDHHLU'                                 )
            return

         end if

         utlck(uindex) = .true.

      end if

!
!     Set the value of UNIT and return.
!
      unit = utlun(uindex)

      call chkout ( 'ZZDDHHLU' )
      return




!$Procedure ZZDDHUNL ( Private --- Unlock Logical Unit from Handle )

      entry zzddhunl ( handle, arch )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Unlock a logical unit from the specified handle.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAS
!     DAF
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         ARCH
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle assigned to the file to unlock.
!     ARCH       I   Expected architecture of the handle to unlock.
!
! $ Detailed_Input
!
!     HANDLE     is the file handle associated with the file that
!                is to have its logical unit lock released.
!
!     ARCH       is the expected architecture of the file associated
!                with HANDLE.
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
!     1) If HANDLE is 0, not found in the unit table, or found and
!        not currently locked, this routine just returns to the
!        caller.
!
!     2) SPICE(FILARCMISMATCH) is signaled if the specified architecture
!        does not match the one listed in the file table.
!
!     3) If HANDLE is associated with a scratch file, this routine
!        simply returns, as scratch files may not be unlocked from
!        their logical units.
!
! $ Files
!
!     This routine does not explicitly alter the open or closed
!     state of the file associated with HANDLE.
!
! $ Particulars
!
!     This routine allows users to unlock a handle from it's logical
!     unit in the event a handle to logical unit request was made
!     with the LOCK argument set to true.  (DAFHLU and DASHLU both
!     lock units, and require a call to this routine to unlock them).
!
! $ Examples
!
!     See some routine that calls this one (TBD).
!
! $ Restrictions
!
!     This routine utilizes discovery check in and out.  However,
!     routines in the initialization loop may signal the error
!     SPICE(BUG) under the conditions of the existence of a bug.
!     Since this routine utilizes discovery check in and out,
!     no check in or out is performed around the initialization
!     block.  This is by design.
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
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

!
!     Standard SPICE discovery error handling.
!
      if ( return() ) then
         return
      end if

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).  No check out is performed, see the
!        $Restrictions section of the entry point header for details.
!
         if ( failed() ) then
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if
!
!     Prevent the user from locating zero handle rows.  This is not
!     really necessary since zero handle rows in the unit table are
!     empty and awaiting connections.  The state of the UTLCK is
!     not significant.
!
      if ( handle .eq. 0 ) then
         return
      end if

!
!     Look up the handle in the unit table.
!
      uindex = isrchi ( handle, nut, uthan )

!
!     Now check the results of the lookup.  If HANDLE was not found
!     in the unit table or the unit was not locked, just return as
!     there is nothing to do.
!
      if ( uindex .eq. 0 ) then
         return
      else if ( .not. utlck(uindex) ) then
         return
      end if

!
!     Now look up the handle in the table. Remember FTABS is a sorted
!     list in increasing order.
!
      findex = bsrchi ( abs(handle), nft, ftabs )

!
!     Check to see if HANDLE is in the file table.  We know it has
!     to be since it is in the unit table if we make it this far.
!     These checks are just for safety's sake.
!
      if ( findex .eq. 0 ) then

         call chkin  ( 'ZZDDHUNL'                                  )
         call setmsg ( 'HANDLE # was not found in the file table ' &
         //            'but was located in the unit table.  This ' &
         //            'error should never occur.'                 )
         call errint ( '#', handle                                 )
         call sigerr ( 'SPICE(BUG)'                                )
         call chkout ( 'ZZDDHUNL'                                  )
         return

      else if ( fthan(findex) .ne. handle ) then

         call chkin  ( 'ZZDDHUNL'                                  )
         call setmsg ( 'HANDLE # was not found in the file table ' &
         //            'but was located in the unit table.  This ' &
         //            'error should never occur.'                 )
         call errint ( '#', handle                                 )
         call sigerr ( 'SPICE(BUG)'                                )
         call chkout ( 'ZZDDHUNL'                                  )
         return

      end if

!
!     Before actually unlocking the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      tmpstr = arch
      call ucase ( tmpstr, tmpstr )
      filarc = isrchc ( tmpstr, numarc, strarc )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc .ne. ftarc(findex) ) then

         call chkin  ( 'ZZDDHUNL'                                 )
         call setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         call errch  ( '#', strarc( ftarc(findex) )               )
         call errch  ( '%', tmpstr                                )
         call errch  ( '$', ftnam(findex)                         )
         call sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         call chkout ( 'ZZDDHUNL'                                 )
         return

      end if

!
!     Lastly, check to see if the access method for HANDLE indicates
!     scratch access.  If it is, just return, since scratch files
!     can not have their units unlocked.
!
      if ( ftamh(findex) .eq. scrtch ) then
         return
      end if

      utlck(uindex) = .false.

      return




!$Procedure ZZDDHNFO ( Private --- Get information about a Handle )

      entry zzddhnfo ( handle, fname, intarc, intbff, intamh, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get information about the file attached to HANDLE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     DAS
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     CHARACTER*(*)         FNAME
!     INTEGER               INTARC
!     INTEGER               INTBFF
!     INTEGER               INTAMH
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle assigned to file of interest.
!     FNAME      O   Name of the file associated with HANDLE.
!     INTARC     O   Integer code for FNAME's file architecture.
!     INTBFF     O   Integer code for FNAME's binary file format.
!     INTAMH     O   Integer code for FNAME's access method.
!     FOUND      O   Logical that indicates if HANDLE was found.
!
! $ Detailed_Input
!
!     HANDLE     is the file handle associated with the file for which
!                information is requested.
!
! $ Detailed_Output
!
!     FNAME      is the name of the file used associated with HANDLE.
!                This is the name used to load the file originally.
!
!     INTARC     is an integer code for FNAME's file architecture.
!                See the include file 'zzddhman.inc' for particulars.
!                The following are possible outputs:
!
!                   DAS - Direct Access, Segregated File Architecture
!                   DAF - Double Precision Array File Architecture
!
!     INTBFF     is an integer code that represents FNAME's binary
!                file format.  See the include file 'zzddhman.inc' for
!                particulars.  The following are the possible outputs:
!
!                   BIGI3E - Big Endian IEEE Floating Point Format
!                   LTLI3E - Little Endian IEEE Floating Point Format
!                   VAXGFL - VAX G-Float Format
!                   VAXDFL - VAX D-Float Format
!
!     INTAMH     is an integer code that represents FNAME's access
!                method.  See the include file 'zzddhman.inc' for
!                particulars.  The following are possible outputs:
!
!                   READ   - File was loaded for read access
!                   WRITE  - File was loaded for read/write access
!                   NEW    - New file was created for read/write access
!                   SCRTCH - Scratch file created for read/write access
!
!     FOUND      is a logical if set to TRUE indicates that HANDLE
!                was located in the file table.  Otherwise, it was
!                not found in the table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If FOUND is FALSE, then the other output arguments
!        are undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides access to information necessary for
!     translation and other I/O based tasks to modules that are
!     not entry points to this handle manager.
!
! $ Examples
!
!     See ZZDAFGFR, ZZDAFGSR, or ZZDAFGDR for sample usage.
!
! $ Restrictions
!
!     Routines in the call tree of this routine may signal the error
!     SPICE(BUG) under the conditions of the existence of a bug
!     in routines the initialization loop invokes.  Since this
!     routine is error free with the exception of this bug condition,
!     it does not participate in tracing by design.
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
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( failed() ) then
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the handle in the table.  Remember FTABS is sorted
!     listed in increasing order.
!
      findex = bsrchi ( abs(handle), nft, ftabs )

!
!     Check to see if HANDLE is in the handle table.  Remember that
!     we are indexing the table using the absolute value of handle.
!     So include a check to see that HANDLE is FTHAN(FINDEX).
!
      if ( findex .eq. 0 ) then
         fname  = ' '
         intarc = 0
         intbff = 0
         intamh = 0
         found  = .false.
         return
      else if ( fthan(findex) .ne. handle ) then
         fname  = ' '
         intarc = 0
         intbff = 0
         intamh = 0
         found  = .false.
         return
      end if

!
!     If we make it this far, then we have a handle that is in
!     the handle table at row FINDEX.
!
      found  = .true.
      fname  = ftnam(findex) ( 1:ftrtm(findex) )
      intarc = ftarc(findex)
      intbff = ftbff(findex)
      intamh = ftamh(findex)

      return




!$Procedure ZZDDHISN ( Private --- Is Handle Native? )

      entry zzddhisn ( handle, native, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine whether the file attached to HANDLE is uses the
!     binary file format native to the system.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAS
!     DAF
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               HANDLE
!     LOGICAL               NATIVE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle assigned to file to determine format.
!     NATIVE     O   Indicates if the file format is native.
!     FOUND      O   Indicates if HANDLE is currently attached to file.
!
! $ Detailed_Input
!
!     HANDLE     is the file handle associated with the file that is
!                to be determined to be native or not.
!
! $ Detailed_Output
!
!     NATIVE     is a logical that when set to TRUE indicates that
!                the file associated with HANDLE is of the native
!                binary file format for the current platform. If
!                FALSE, then the file is of an alien file format.
!
!     FOUND      is a logical that when set to TRUE indicates that
!                HANDLE was found in the file table and is associated
!                with a file.  If FALSE, then NATIVE remains unchanged,
!                since the file was not found in the table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error Free.
!
!     1) In the event that HANDLE can not be found in the file table,
!        FOUND is set to FALSE and NATIVE is left unchanged.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine simply answers the question:  "Is the file attached
!     to this handle of the native binary file format?"
!
! $ Examples
!
!     See DAFRDA for sample usage.
!
! $ Restrictions
!
!     Routines in the call tree of this routine may signal the error
!     SPICE(BUG) under the conditions of the existence of a bug
!     in routines the initialization loop invokes.  Since this
!     routine is error free with the exception of this bug condition,
!     it does not participate in tracing by design.
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
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( failed() ) then
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the handle in the table. Remember FTABS is sorted
!     listed in increasing order.
!
      findex = bsrchi ( abs(handle), nft, ftabs )

!
!     Check to see if HANDLE is in the handle table.  Remember
!     that we are indexing the table using the absolute value of
!     handle.  So include a check to see that HANDLE is FTHAN(FINDEX).
!
      if ( findex .eq. 0 ) then
         found = .false.
         return
      else if ( fthan(findex) .ne. handle ) then
         found = .false.
         return
      end if

!
!     If we make it this far, then we have found HANDLE in the file
!     table.  Set NATIVE appropriately and FOUND to TRUE.
!
      native = natbff .eq. ftbff(findex)
      found  = .true.

      return





!$Procedure ZZDDHFNH ( Private --- Filename to Handle )

      entry zzddhfnh ( fname, handle, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Retrieve handle associated with filename.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     DAS
!     PRIVATE
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a file previously loaded with ZZDDHOPN.
!     HANDLE     O   Corresponding file handle.
!     FOUND      O   Logical indicating whether HANDLE was located.
!
! $ Detailed_Input
!
!     FNAME      is the name of a file previously loaded with ZZDDHOPN.
!
! $ Detailed_Output
!
!     HANDLE     is the handle associated with the file.
!
!     FOUND      is a logical when TRUE indicates HANDLE was located
!                for FNAME.  If FALSE no handle was found associated
!                with FNAME.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) ZZDDHF2H in the call tree of this routine performs I/O
!        functions and may signal errors that are the result of
!        I/O failures.  See ZZDDHF2H header for details.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is provided for completeness and serves only to
!     support the DAFFNH and DASFNH interfaces.
!
! $ Examples
!
!     See DAFFNH or DASFNH for sample usage.
!
! $ Restrictions
!
!     1) On VAX environments, this routine may only be used when
!        FNAME refers to a DAF or DAS file.  An error may be
!        signaled when used with unopened files that utilize other
!        architectures.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
!
!        Updated for the new "magic number" column in the file table.
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

      if ( return() ) then
         return
      else
         call chkin ( 'ZZDDHFNH' )
      end if

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( failed() ) then
            handle = 0
            call chkout ( 'ZZDDHFNH' )
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Left justify FNAME to trim any leading white space.
!
      call ljust ( fname, locfnm )

!
!     Look up FNAME in the handle table.
!
      call zzddhf2h ( locfnm,  ftabs,  ftamh, ftarc, ftbff, &
                      fthan,   ftnam,  ftrtm, ftmnm, nft,   utcst, &
                      uthan,   utlck,  utlun, nut,   inqext, &
                      inqopn,  inqhan, found, mnm                 )

!
!     Check found and set HANDLE if we have got one.  No need to
!     check FAILED() since ZZDDHF2H returns FOUND set to FALSE on
!     error.
!
      if ( found ) then
         handle = inqhan
      else
         handle = 0
      end if

      call chkout ( 'ZZDDHFNH' )
      return




!$Procedure ZZDDHLUH ( Private --- Logical Unit to Handle )

      entry zzddhluh ( unit, handle, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the handle associated with a logical unit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAS
!     DAF
!     PRIVATE
!
! $ Declarations
!
!     INTEGER               UNIT
!     INTEGER               HANDLE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit connected to a file.
!     HANDLE     O   Corresponding handle.
!     FOUND      O   Logical indicating the handle was located.
!
! $ Detailed_Input
!
!     UNIT       is the logical unit to which a file managed by DDH is
!                currently connected.
!
! $ Detailed_Output
!
!     HANDLE     is the handle associated with the logical unit of
!                interest.
!
!     FOUND      is a logical flag if TRUE indicates that a HANDLE
!                was found associated with UNIT.  If FALSE indicates
!                no handle was found for UNIT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If UNIT is not found in the unit table, HANDLE is undefined
!        and FOUND is set to .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is provided for completeness and serves only to
!     support the DAFLUH and DASLUH interfaces.
!
! $ Examples
!
!     See DAFLUH or DASLUH for usage.
!
! $ Restrictions
!
!     Routines in the call tree of this routine may signal the error
!     SPICE(BUG) under the conditions of the existence of a bug
!     in routines the initialization loop invokes.  Since this
!     routine is error free with the exception of this bug condition,
!     it does not participate in tracing by design.
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
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( failed() ) then
            handle = 0
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the unit in the table.
!
      uindex = isrchi ( unit, nut, utlun )

      if ( uindex .eq. 0 ) then
         handle = 0
         found  = .false.
         return
      else if ( uthan(uindex) .eq. 0 ) then
         handle = 0
         found  = .false.
         return
      end if

!
!     We've got a handle, store the value and return.
!
      handle = uthan(uindex)
      found  = .true.

      return

      end
