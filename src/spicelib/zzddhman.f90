!$Procedure ZZDDHMAN ( Private --- DAF/DAS Handle Manager )

      SUBROUTINE ZZDDHMAN ( LOCK,   ARCH,   FNAME,  METHOD, &
                            HANDLE, UNIT,   INTAMH, INTARC, &
                            INTBFF, NATIVE, FOUND,  KILL    )

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

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      LOGICAL               LOCK
      CHARACTER*(*)         ARCH
      CHARACTER*(*)         FNAME
      CHARACTER*(*)         METHOD
      INTEGER               HANDLE
      INTEGER               UNIT
      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      LOGICAL               NATIVE
      LOGICAL               FOUND
      LOGICAL               KILL

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
      INTEGER               BSRCHI
      INTEGER               ISRCHC
      INTEGER               ISRCHI
      INTEGER               RTRIM
      INTEGER               ZZDDHCLU

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Variables
!
      CHARACTER*(FILEN)     LOCFNM
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      MNM

      INTEGER               ACCMET
      INTEGER               BFF
      INTEGER               FILARC
      INTEGER               I
      INTEGER               INQHAN
      INTEGER               IOSTAT
      INTEGER               LCHAR
      INTEGER               LOCKED
      INTEGER               LOCLUN
      INTEGER               SUPIDX

      LOGICAL               ERROR
      LOGICAL               INQEXT
      LOGICAL               INQOPN
      LOGICAL               LOCFND
      LOGICAL               PLATOK

!
!     This logical allows initialization code to execute.
!
      LOGICAL               FIRST
      LOGICAL               OPNFST

!
!     These strings store the labels for the parameters defined
!     in the include file and retrieved by ZZDDHINI.
!
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

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
      INTEGER               NFT

      INTEGER               FTABS  ( FTSIZE )
      INTEGER               FTAMH  ( FTSIZE )
      INTEGER               FTARC  ( FTSIZE )
      INTEGER               FTBFF  ( FTSIZE )
      INTEGER               FTHAN  ( FTSIZE )
      CHARACTER*(FILEN)     FTNAM  ( FTSIZE )
      INTEGER               FTRTM  ( FTSIZE )
      DOUBLE PRECISION      FTMNM  ( FTSIZE )

      INTEGER               FINDEX

!
!     NEXT stores the next handle to be used for file access.  This
!     could be either for read or write based operations. NEXT is
!     incremented just before entries in the file table are made.
!     It begins as zero valued.
!
      INTEGER               NEXT

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
      INTEGER               NUT

      INTEGER               UTCST ( UTSIZE )
      INTEGER               UTHAN ( UTSIZE )
      LOGICAL               UTLCK ( UTSIZE )
      INTEGER               UTLUN ( UTSIZE )

      INTEGER               UINDEX

!
!     The following stores the native binary file format, a list of
!     codes for supported binary formats, and the number of entries
!     in SUPBFF.
!
      INTEGER               NATBFF
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               NUMSUP

!
!     Request counter used to determine cost.
!
      INTEGER               REQCNT

!
!     Saved Variables
!
      SAVE                  FIRST
      SAVE                  OPNFST

      SAVE                  STRAMH
      SAVE                  STRARC
      SAVE                  STRBFF

      SAVE                  NFT
      SAVE                  FTABS
      SAVE                  FTAMH
      SAVE                  FTARC
      SAVE                  FTBFF
      SAVE                  FTHAN
      SAVE                  FTNAM
      SAVE                  FTRTM
      SAVE                  FTMNM

      SAVE                  NEXT

      SAVE                  NUT
      SAVE                  UTCST
      SAVE                  UTHAN
      SAVE                  UTLCK
      SAVE                  UTLUN

      SAVE                  NATBFF
      SAVE                  SUPBFF
      SAVE                  NUMSUP

      SAVE                  REQCNT


!
!     Data Statements
!
      DATA                  FIRST  / .TRUE.                  /
      DATA                  OPNFST / .TRUE.                  /
      DATA                  NFT    / 0                       /
      DATA                  NEXT   / 0                       /
      DATA                  NUT    / 0                       /
      DATA                  REQCNT / 0                       /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZDDHMAN' )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'ZZDDHMAN' )
      END IF

      RETURN




!$Procedure ZZDDHOPN ( Private --- Load file )

      ENTRY ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )

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
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHOPN' )
      END IF

!
!     Do the initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     On first pass, perform any runtime environment checks.
!
      IF ( OPNFST ) THEN

         CALL ZZPLTCHK ( PLATOK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF

!
!        Clear OPNFST, since we've performed the diagnostics.
!
         OPNFST = .FALSE.

      END IF

!
!     Initialize the value of HANDLE to 0.  In the event an error
!     is signaled this invalid value will be returned to the caller
!     for safety.
!
      HANDLE = 0

!
!     Left justify FNAME to compress off any leading spaces.
!
      CALL LJUST ( FNAME, LOCFNM )

!
!     Translate the value of the requested access method to the
!     corresponding integer code.
!
      TMPSTR = METHOD
      CALL UCASE ( TMPSTR, TMPSTR )
      ACCMET = ISRCHC ( TMPSTR, NUMAMH, STRAMH )

!
!     Check if the code was located.
!
      IF ( ACCMET .EQ. 0 ) THEN

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         CALL SETMSG ( 'The attempt to load file, ''#'', with access ' &
         //            'method, ''#'', failed because this access ' &
         //            'method is unsupported.'                        )
         CALL ERRCH  ( '#', LOCFNM                                     )
         CALL ERRCH  ( '#', METHOD                                     )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDMETHOD)'                      )
         CALL CHKOUT ( 'ZZDDHOPN'                                      )
         RETURN

      END IF

!
!     Translate the value of the requested file architecture to
!     the appropriate integer code.
!
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )

!
!     Check if the code was located.
!
      IF ( FILARC .EQ. 0 ) THEN

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         CALL SETMSG ( 'The attempt to load file, ''#'', with ' &
         //            'architecture, ''#'', failed because this ' &
         //            'file architecture is unsupported.'             )
         CALL ERRCH  ( '#', LOCFNM                                     )
         CALL ERRCH  ( '#', ARCH                                       )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDARCH)'                        )
         CALL CHKOUT ( 'ZZDDHOPN'                                      )
         RETURN

      END IF

!
!     Perform any preliminary checks that must be done before
!     fetching a logical unit from the unit table.  This requires
!     branching based on ACCMET's value.
!
      IF ( ACCMET .EQ. SCRTCH ) THEN

!
!        Check to see if there are enough units available for locking
!        in the unit table.  If not, signal an error as all files
!        open with SCRTCH access must be locked to their units.
!
         LOCKED = ZZDDHCLU ( UTLCK, NUT )

         IF ( LOCKED .GE. (UTSIZE - RSVUNT) ) THEN

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            CALL SETMSG ( 'The maximum number of units are locked ' &
            //            'to handles.  As such, there is no room ' &
            //            'to open the requested scratch file.'     )
            CALL SIGERR ( 'SPICE(UTFULL)'                           )
            CALL CHKOUT ( 'ZZDDHOPN'                                )
            RETURN

         END IF

!
!     The NEW, READ, and WRITE access methods perform the same
!     checks on LOCFNM.
!
      ELSE IF ( ( ACCMET .EQ. NEW   ) .OR. &
                ( ACCMET .EQ. READ  ) .OR. &
                ( ACCMET .EQ. WRITE )      ) THEN

!
!        Check for a non-blank file name.
!
         IF ( LOCFNM .EQ. ' ' ) THEN

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            CALL SETMSG ( 'The attempt to load the file has failed, ' &
            //            'because the filename is blank.'            )
            CALL SIGERR ( 'SPICE(BLANKFILENAME)'                      )
            CALL CHKOUT ( 'ZZDDHOPN'                                  )
            RETURN

         END IF

      END IF

      MNM = 0.D0

!
!     In the READ or WRITE cases verify that LOCFNM is not already
!     in the file table.
!
      IF ( ( ACCMET .EQ. READ  ) .OR. &
           ( ACCMET .EQ. WRITE )      ) THEN

!
!        Check to see if the file associated with LOCFNM is already in
!        the file table.
!
         CALL ZZDDHF2H ( LOCFNM,  FTABS,  FTAMH,  FTARC, FTBFF, &
                         FTHAN,   FTNAM,  FTRTM,  FTMNM, NFT,   UTCST, &
                         UTHAN,   UTLCK,  UTLUN,  NUT,   INQEXT, &
                         INQOPN,  INQHAN, LOCFND, MNM                 )

!
!        First, check FAILED(), and return if anything has gone awry.
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHOPN' )
            RETURN
         END IF

!
!        Now perform some simple sanity checks before preparing to
!        load the file.  First check to see if the file exists, it must
!        if we are going to open it with ACCMET set to READ or WRITE.
!
         IF ( .NOT. INQEXT ) THEN

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            CALL SETMSG ( 'The file ''#'' does not exist.' )
            CALL ERRCH  ( '#', LOCFNM                      )
            CALL SIGERR ( 'SPICE(FILENOTFOUND)'            )
            CALL CHKOUT ( 'ZZDDHOPN'                       )
            RETURN

         END IF

!
!        Now if the file was not found in the file table, and it is
!        attached to a unit, this presents a problem.
!
         IF ( ( .NOT. LOCFND ) .AND. ( INQOPN ) ) THEN

!
!           Get the unit to include in the error message.
!
            INQUIRE ( FILE   = LOCFNM, &
                      NUMBER = LOCLUN, &
                      IOSTAT = IOSTAT  )

!
!           Since we performed a very similar INQUIRE statement in
!           ZZDDHF2H, a non-zero IOSTAT value indicates a severe error.
!
            IF ( IOSTAT .NE. 0 ) THEN

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               CALL SETMSG ( 'INQUIRE failed.' )
               CALL SIGERR ( 'SPICE(BUG)'      )
               CALL CHKOUT ( 'ZZDDHOPN'        )
               RETURN

            END IF

!
!           Signal the error. Recall HANDLE was initialized to 0, and
!           this invalid value is returned to the caller.
!
            CALL SETMSG ( 'The file ''#'' is already connected to ' &
            //            'unit #.'                                 )
            CALL ERRCH  ( '#', LOCFNM                               )
            CALL ERRINT ( '#', LOCLUN                               )
            CALL SIGERR ( 'SPICE(IMPROPEROPEN)'                     )
            CALL CHKOUT ( 'ZZDDHOPN'                                )
            RETURN

         END IF

!
!        Lastly check to see if the file in the file table, and
!        perform the appropriate sanity checks.
!
         IF ( LOCFND ) THEN

            FINDEX = BSRCHI ( ABS(INQHAN), NFT, FTABS )

!
!           Check to see if the requested architecture does not match
!           that of the entry in the file table.
!
            IF ( FILARC .NE. FTARC(FINDEX) ) THEN

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               CALL SETMSG ( 'The attempt to load file ''#'' as a # ' &
               //            'has failed because it is already loaded ' &
               //            'as a #.'                                 )
               CALL ERRCH  ( '#', LOCFNM                               )
               CALL ERRCH  ( '#', STRARC(FILARC)                       )
               CALL ERRCH  ( '#', STRARC(FTARC(FINDEX))                )
               CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                   )
               CALL CHKOUT ( 'ZZDDHOPN'                                )
               RETURN

            END IF

!
!           Check to see if the access method is anything other
!           than READ.  If so, signal the appropriate error.
!           Note: this is only for READ.
!
            IF ( ACCMET .NE. READ ) THEN

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               CALL SETMSG ( 'File ''#'' already loaded.' )
               CALL ERRCH  ( '#', LOCFNM                  )
               CALL SIGERR ( 'SPICE(FILEOPENCONFLICT)'    )
               CALL CHKOUT ( 'ZZDDHOPN'                   )
               RETURN

            END IF

!
!           If we reach here, then we have a file that exists
!           in the table, and the caller is attempting to load it
!           for READ access.  Check to make certain it is not
!           already loaded with another method.
!
            IF ( ACCMET .NE. FTAMH(FINDEX) ) THEN

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               CALL SETMSG ( 'Unable to load file ''#'' for # ' &
               //            'access.  It is already loaded with ' &
               //            'the conflicting access #.'           )
               CALL ERRCH  ( '#', LOCFNM                           )
               CALL ERRCH  ( '#', STRAMH(ACCMET)                   )
               CALL ERRCH  ( '#', STRAMH(FTAMH(FINDEX))            )
               CALL SIGERR ( 'SPICE(RWCONFLICT)'                   )
               CALL CHKOUT ( 'ZZDDHOPN'                            )
               RETURN

            END IF

!
!           If we make it this far, the file is in the file table
!           and all the sanity checks have passed. Return to the
!           caller as this is effectively a no-op.
!
            HANDLE = FTHAN ( FINDEX )

            CALL CHKOUT( 'ZZDDHOPN' )
            RETURN

         END IF

      END IF

!
!     Now check to see if there is room in the file table for this
!     new file.
!
      IF ( NFT .EQ. FTSIZE ) THEN

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         CALL SETMSG ( 'The file table is full, with # entries. ' &
         //            'As a result, the file ''#'' could not be ' &
         //            'loaded.'                                   )
         CALL ERRINT ( '#', NFT                                    )
         CALL ERRCH  ( '#', LOCFNM                                 )
         CALL SIGERR ( 'SPICE(FTFULL)'                             )
         CALL CHKOUT ( 'ZZDDHOPN'                                  )
         RETURN

      END IF

!
!     We are about to attempt a HANDLE to LUN connection, increment
!     the request counter.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Free up a logical unit in the UNIT table for our usage.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!     Check FAILED() since ZZDDHGTU may have invoked GETLUN.
!     Recall HANDLE was initialized to 0, and this invalid
!     value is returned to the caller.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDDHOPN' )
         RETURN
      END IF

!
!     Trim up the filename.
!
      IF ( ACCMET .NE. SCRTCH ) THEN
         LCHAR = RTRIM ( LOCFNM )
      END IF

!
!     If we have made it this far, then we're ready to perform the
!     appropriate open.  First get the handle ready.
!
      NEXT = NEXT + 1

!
!     Determine the sign of the new handle based on the requested
!     METHOD.
!
      IF ( ACCMET .EQ. READ ) THEN
         UTHAN ( UINDEX ) = NEXT
      ELSE
         UTHAN ( UINDEX ) = -NEXT
      END IF

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
      IF ( ACCMET .EQ. SCRTCH ) THEN

         OPEN ( UNIT   = UTLUN(UINDEX), &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'SCRATCH', &
                IOSTAT = IOSTAT           )

         BFF = NATBFF

      ELSE IF ( ACCMET .EQ. NEW ) THEN

         OPEN ( UNIT   = UTLUN(UINDEX), &
                FILE   = LOCFNM(1:LCHAR), &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'NEW', &
                IOSTAT = IOSTAT           )

         BFF = NATBFF

      ELSE IF ( ACCMET .EQ. READ ) THEN

         OPEN ( UNIT   = UTLUN(UINDEX), &
                FILE   = LOCFNM(1:LCHAR), &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT           )

      ELSE IF ( ACCMET .EQ. WRITE ) THEN

         OPEN ( UNIT   = UTLUN(UINDEX), &
                FILE   = LOCFNM(1:LCHAR), &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT           )

      END IF

!
!     Verify that IOSTAT is non-zero.
!
      ERROR = ( IOSTAT .NE. 0 )

!
!     Partially process the error.
!
      IF ( ERROR ) THEN

!
!        Now signal the error, but delay cleaning up and checking
!        out until leaving this IF block.
!
         IF ( ACCMET .EQ. SCRTCH ) THEN
            CALL SETMSG ( 'Attempt to open scratch file failed. ' &
            //            'IOSTAT was #.'                         )
         ELSE IF ( ACCMET .EQ. NEW ) THEN
            CALL SETMSG ( 'Attempt to create new file, ''$'' ' &
            //            'failed. IOSTAT was #.'                 )
         ELSE
            CALL SETMSG ( 'Attempt to open file, ''$'' for % ' &
            //            'access failed. IOSTAT was #.'          )
         END IF

         CALL ERRCH  ( '$', LOCFNM                                )
         CALL ERRCH  ( '%', STRAMH ( ACCMET )                     )
         CALL ERRINT ( '#', IOSTAT                                )
         CALL SIGERR ( 'SPICE(FILEOPENFAIL)'                      )

!
!     If no IOSTAT based error has occurred as a result of the OPEN
!     statement, then perform any remaining checks or I/O operations
!     that are necessary to support loading the file.
!
      ELSE IF ( ACCMET .EQ. SCRTCH ) THEN

!
!        Inquire on the logical unit to produce the file name for
!        the scratch file.  Set the initial value of LOCFNM, in case
!        the INQUIRE does not replace it.
!
         LOCFNM = '# SCRATCH FILE'
         CALL REPMC ( LOCFNM, '#', STRARC(FILARC), LOCFNM )

         INQUIRE ( UNIT   = UTLUN(UINDEX), &
                   NAME   = LOCFNM, &
                   IOSTAT = IOSTAT         )

!
!        In the event that this INQUIRE failed, replace the value
!        stored in LOCFNM with the initial value.
!
         IF ( IOSTAT .NE. 0 ) THEN

            LOCFNM = '# SCRATCH FILE'
            CALL REPMC ( LOCFNM, '#', STRARC(FILARC), LOCFNM )

         END IF

!
!        Store the RTRIM value of this filename in LCHAR.
!
         LCHAR = RTRIM ( LOCFNM )

      ELSE IF ( ( ACCMET .EQ. READ  ) .OR. &
                ( ACCMET .EQ. WRITE )      ) THEN

!
!        Check for FTP errors, verify that FILARC is appropriate,
!        and determine the binary file format of the preexisting
!        file LOCFNM.
!
         CALL ZZDDHPPF ( UTLUN(UINDEX), FILARC, BFF )

!
!        Set ERROR.
!
         ERROR = FAILED()

!
!        If no error has occurred, verify that BFF is among the
!        list of supported format ID codes for the requested access
!        method.
!
         IF ( .NOT. ERROR ) THEN

!
!           This platform supports reading from files whose
!           format codes are listed in SUPBFF.
!
            IF ( ACCMET .EQ. READ ) THEN

               SUPIDX = ISRCHI ( BFF, NUMSUP, SUPBFF )

               IF ( SUPIDX .EQ. 0 ) THEN

!
!                 Delay clean up and check out.
!
                  ERROR = .TRUE.

                  IF ( BFF .EQ. 0 ) THEN

                     CALL SETMSG ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  This ' &
                     //            'file utilizes an unknown binary ' &
                     //            'file format.  This error may ' &
                     //            'result from attempting to open a ' &
                     //            'corrupt file or one of an ' &
                     //            'unknown type.'                     )
                     CALL ERRCH  ( '#', LOCFNM                         )
                     CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )

                  ELSE

                     CALL SETMSG ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  The ' &
                     //            'non-native binary file format ' &
                     //            '''#'' is not currently supported ' &
                     //            'on this platform.  Obtain a ' &
                     //            'transfer format version, and ' &
                     //            'convert it to the native format. ' &
                     //            'See the Convert User''s Guide ' &
                     //            'for details.'                      )
                     CALL ERRCH  ( '#', LOCFNM                         )
                     CALL ERRCH  ( '#', STRBFF(BFF)                    )
                     CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )

                  END IF

               END IF

!
!           This platform only supports writing to files whose
!           binary formats are native.
!
            ELSE

!
!              Delay clean up and check out.
!
               IF ( BFF .EQ. 0 ) THEN

                  ERROR = .TRUE.

                  CALL SETMSG ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes an unknown binary ' &
                  //            'file format.  This error may ' &
                  //            'result from attempting to open a ' &
                  //            'corrupt file or one of an unknown ' &
                  //            'type.'                              )
                  CALL ERRCH  ( '#', LOCFNM                          )
                  CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'              )

               ELSE IF ( BFF .NE. NATBFF ) THEN

                  ERROR = .TRUE.

                  CALL SETMSG ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes the non-native ' &
                  //            'binary file format ''#''.  At ' &
                  //            'this time only files of the ' &
                  //            'native format, ''#'', are ' &
                  //            'supported for write access.  See ' &
                  //            'the Convert User''s Guide for ' &
                  //            'details.'                          )
                  CALL ERRCH  ( '#', LOCFNM                         )
                  CALL ERRCH  ( '#', STRBFF(BFF)                    )
                  CALL ERRCH  ( '#', STRBFF(NATBFF)                 )
                  CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'             )

               END IF

            END IF

         END IF

      END IF

!
!     If an error has occurred as a result of opening the file or
!     examining its contents, clean up and check out.
!
      IF ( ERROR ) THEN

!
!        Close the unit we were using.  Remember to delete the file
!        if it was a 'new' one.
!
         IF ( ACCMET .EQ. NEW ) THEN
            CLOSE ( UNIT = UTLUN(UINDEX), STATUS = 'DELETE' )
         ELSE
            CLOSE ( UNIT = UTLUN(UINDEX) )
         END IF

!
!        Remove the unit from the unit table, since this UNIT
!        is no longer in use.
!
         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )

!
!        Decrement NEXT since this handle was never assigned to
!        a file.
!
         NEXT = NEXT - 1

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         CALL CHKOUT ( 'ZZDDHOPN' )
         RETURN

      END IF


!
!     Finish filling out the unit table.
!
      UTCST ( UINDEX ) = REQCNT

!
!     Only scratch files get the units locked to handles, this is
!     because they only exist as long as they have a unit.
!
      UTLCK ( UINDEX ) = ACCMET .EQ. SCRTCH

!
!     Now fill out the file table.
!
      NFT = NFT + 1

!
!     Use the absolute value of the handle used to index the file
!     table.
!
      FTABS ( NFT ) = IABS ( UTHAN( UINDEX ) )

!
!     Assign access method, file architecture, and native binary file
!     format to the appropriate columns.
!
      FTAMH ( NFT ) = ACCMET
      FTARC ( NFT ) = FILARC
      FTBFF ( NFT ) = BFF

!
!     Assign the handle, filename, RTRIM ( FTNAM(NFT) ) as FTRTM, and
!     unique DP number as FTMNM.
!
      FTHAN ( NFT ) = UTHAN ( UINDEX )
      FTNAM ( NFT ) = LOCFNM(1:LCHAR)
      FTRTM ( NFT ) = LCHAR
      FTMNM ( NFT ) = MNM

!
!     Assign HANDLE the value of the new handle.
!
      HANDLE = FTHAN( NFT )

      CALL CHKOUT ( 'ZZDDHOPN' )
      RETURN




!$Procedure ZZDDHCLS ( Private --- Close file )

      ENTRY ZZDDHCLS ( HANDLE, ARCH, KILL )

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
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHCLS' )
      END IF

!
!     Do the initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHCLS' )
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Find the file in the handle table.
!
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )

!
!     Check to see whether we found the handle or not.
!
      IF ( FINDEX .EQ. 0 ) THEN
         CALL CHKOUT ( 'ZZDDHCLS' )
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         CALL CHKOUT ( 'ZZDDHCLS' )
         RETURN
      END IF

!
!     Before actually closing the file, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architecture's code from stepping on another's.
!
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN

         CALL SETMSG ( 'Logical unit associated with # file $, ' &
         //            'is trying to be closed by routines in ' &
         //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHCLS'                                 )
         RETURN

      END IF

!
!     Now check that if KILL is set, the file is accessible for
!     WRITE.
!
      IF ( ( KILL ) .AND. ( FTAMH(FINDEX) .EQ. READ ) ) THEN

         CALL SETMSG ( '# file $ is open for READ access.  Attempt ' &
         //            'to close and delete file has failed. '       )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )                  )
         CALL ERRCH  ( '#', FTNAM(FINDEX)                            )
         CALL SIGERR ( 'SPICE(INVALIDACCESS)'                        )
         CALL CHKOUT ( 'ZZDDHCLS'                                    )
         RETURN

      END IF

!
!     Buffer the access method for HANDLE, since we may need it
!     when deciding which close to perform.
!
      ACCMET = FTAMH (FINDEX)

!
!     If we reach here, we need to remove the row FINDEX from
!     the file table.
!
      DO I = FINDEX+1, NFT

         FTABS(I-1) = FTABS(I)
         FTAMH(I-1) = FTAMH(I)
         FTARC(I-1) = FTARC(I)
         FTBFF(I-1) = FTBFF(I)
         FTHAN(I-1) = FTHAN(I)
         FTNAM(I-1) = FTNAM(I)
         FTRTM(I-1) = FTRTM(I)
         FTMNM(I-1) = FTMNM(I)

      END DO

      NFT = NFT - 1

!
!     Locate HANDLE in the unit table.
!
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )

      IF ( UINDEX .NE. 0 ) THEN

!
!        Close the unit.
!
         IF ( ( KILL ) .AND. ( ACCMET .NE. SCRTCH ) ) THEN
            CLOSE ( UNIT = UTLUN(UINDEX), STATUS = 'DELETE' )
         ELSE
            CLOSE ( UNIT = UTLUN(UINDEX) )
         END IF

!
!        Remove its entry from the unit table.
!
         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )

      ELSE

!
!        First, check to see if KILL is set, if it is signal an error
!        since we are unable to delete the file.
!
         IF ( ( KILL ) .AND. ( ACCMET .NE. SCRTCH ) ) THEN

            CALL SETMSG ( 'File successfully closed.  Unable to ' &
            //            'delete file as requested.  File not ' &
            //            'currently present in the UNIT table. ' )
            CALL SIGERR ( 'SPICE(FILENOTCONNECTED)'               )
            CALL CHKOUT ( 'ZZDDHCLS'                              )
            RETURN

         END IF

!
!        If we were unable to find the HANDLE in the unit table,
!        check to see if we have to clean up the UNIT table.
!
         IF ( NFT .LT. NUT ) THEN

            UINDEX = ISRCHI ( 0, NUT, UTHAN )

!
!           Now check to see if we located a zero valued handle.
!           If we did not manage to, then this is an error condition,
!           since we have more LUNs listed in the unit table than
!           files in the file table.
!
            IF ( UINDEX .EQ. 0 ) THEN
               CALL SETMSG ( 'There are less files in the file table ' &
               //            'than units in the unit table, and no ' &
               //            'row with a zero-valued handle can be ' &
               //            'found.  This should never occur.'        )
               CALL SIGERR ( 'SPICE(BUG)'                              )
               CALL CHKOUT ( 'ZZDDHCLS'                                )
               RETURN
            END IF

!
!           Free the unit.
!
            CALL FRELUN ( UTLUN(UINDEX) )

!
!           Compress the table.
!
            DO I = (UINDEX + 1), NUT

               UTCST(I-1) = UTCST(I)
               UTHAN(I-1) = UTHAN(I)
               UTLCK(I-1) = UTLCK(I)
               UTLUN(I-1) = UTLUN(I)

            END DO

!
!           Decrement NUT.
!
            NUT = NUT - 1

         END IF
      END IF

      CALL CHKOUT ( 'ZZDDHCLS' )
      RETURN




!$Procedure ZZDDHHLU ( Private --- Handle to Logical Unit )

      ENTRY ZZDDHHLU ( HANDLE, ARCH, LOCK, UNIT )

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
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHHLU' )
      END IF

!
!     Do the initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         IF ( FAILED() ) THEN
            UNIT = 0
            CALL CHKOUT ( 'ZZDDHHLU' )
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Locate HANDLE in the file table.
!
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )

      IF ( FINDEX .EQ. 0 ) THEN
         ERROR = .TRUE.
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         ERROR = .TRUE.
      ELSE
         ERROR = .FALSE.
      END IF

      IF ( ERROR ) THEN

         UNIT = 0

         CALL SETMSG ( 'There is no file loaded with handle = #' )
         CALL ERRINT ( '#', HANDLE                               )
         CALL SIGERR ( 'SPICE(NOSUCHHANDLE)'                     )
         CALL CHKOUT ( 'ZZDDHHLU'                                )
         RETURN
      END IF

!
!     Before actually fetching the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN

         UNIT = 0

         CALL SETMSG ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHHLU'                                 )
         RETURN

      END IF

!
!     If we make it this far, then we will be processing a handle
!     to logical unit request.  Increment REQCNT.
!
      CALL ZZDDHRCM ( NUT, UTCST, REQCNT )

!
!     Now check to see if the handle is already present in the
!     unit table.
!
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )

!
!     Check to see if we didn't locate the HANDLE in the table.
!     If we didn't, open the file associated with HANDLE again,
!     and get it into the unit table.
!
      IF ( UINDEX .EQ. 0 ) THEN

!
!        We need a unit from the unit table, get one.
!
         CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

!
!        Check FAILED, since ZZDDHGTU may have invoked GETLUN.
!
         IF ( FAILED() ) THEN

            UNIT = 0

            CALL CHKOUT ( 'ZZDDHHLU' )
            RETURN
         END IF

!
!        Re-attach the file to a logical unit.  Branch based on the
!        access method stored in the file table.
!
         IF ( ( FTAMH(FINDEX) .EQ. NEW   ) .OR. &
              ( FTAMH(FINDEX) .EQ. WRITE )      ) THEN

            OPEN ( UNIT   = UTLUN(UINDEX), &
                   FILE   = FTNAM(FINDEX)(1:FTRTM(FINDEX)), &
                   ACCESS = 'DIRECT', &
                   RECL   = RECL, &
                   STATUS = 'OLD', &
                   IOSTAT = IOSTAT           )

         ELSE IF ( FTAMH(FINDEX) .EQ. READ ) THEN

            OPEN ( UNIT   = UTLUN(UINDEX), &
                   FILE   = FTNAM(FINDEX)(1:FTRTM(FINDEX)), &
                   ACCESS = 'DIRECT', &
                   RECL   = RECL, &
                   STATUS = 'OLD', &
                   IOSTAT = IOSTAT           )

         ELSE

            UNIT = 0

            CALL SETMSG ( 'Invalid access method. This error ' &
            //            'should never be signaled.'          )
            CALL SIGERR ( 'SPICE(BUG)'                         )
            CALL CHKOUT ( 'ZZDDHHLU'                           )
            RETURN
         END IF

!
!        Check IOSTAT for troubles.
!
         IF ( IOSTAT .NE. 0 ) THEN

!
!           The re-open was unsuccessful, leave the entry in the file
!           table and clean up the row in the unit table before
!           returning.  Normally when we call ZZDDHRMU it is to
!           remove a unit from the unit table.  In this case we
!           know the unit will remain since we have not decreased
!           the entries in the file table.
!
            CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                            UTLCK,  UTLUN, NUT           )

!
!           Now signal the error.
!
            UNIT = 0

            CALL SETMSG ( 'Attempt to reconnect logical unit to ' &
            //            'file ''#'' failed. IOSTAT was #.'      )
            CALL ERRCH  ( '#', FTNAM(FINDEX)                      )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEOPENFAIL)'                   )
            CALL CHKOUT ( 'ZZDDHHLU'                              )
            RETURN

         END IF

!
!        Lastly populate the unit table values.
!
         UTHAN(UINDEX) = FTHAN(FINDEX)
         UTLCK(UINDEX) = .FALSE.

      END IF

!
!     At this point UINDEX points to the row in the unit table that
!     contains the connection information.  We need to update the cost
!     row with the new value of REQCNT, and then set the lock row to
!     TRUE if a lock request was made.
!
      UTCST(UINDEX) = REQCNT

      IF ( LOCK .AND. (.NOT. UTLCK(UINDEX)) ) THEN

!
!        First check to see if we have enough lockable units
!        left in the unit table.
!
         LOCKED = ZZDDHCLU ( UTLCK, NUT )

         IF ( LOCKED .GE. ( UTSIZE - RSVUNT - SCRUNT ) ) THEN

            UNIT = 0

            CALL SETMSG ( 'Unable to lock handle for file ''#'' ' &
            //            'to a logical unit.  There are no rows ' &
            //            'available for locking in the unit table.' )
            CALL ERRCH  ( '#', FTNAM(FINDEX)                         )
            CALL SIGERR ( 'SPICE(HLULOCKFAILED)'                     )
            CALL CHKOUT ( 'ZZDDHHLU'                                 )
            RETURN

         END IF

         UTLCK(UINDEX) = .TRUE.

      END IF

!
!     Set the value of UNIT and return.
!
      UNIT = UTLUN(UINDEX)

      CALL CHKOUT ( 'ZZDDHHLU' )
      RETURN




!$Procedure ZZDDHUNL ( Private --- Unlock Logical Unit from Handle )

      ENTRY ZZDDHUNL ( HANDLE, ARCH )

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
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Do the initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).  No check out is performed, see the
!        $Restrictions section of the entry point header for details.
!
         IF ( FAILED() ) THEN
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF
!
!     Prevent the user from locating zero handle rows.  This is not
!     really necessary since zero handle rows in the unit table are
!     empty and awaiting connections.  The state of the UTLCK is
!     not significant.
!
      IF ( HANDLE .EQ. 0 ) THEN
         RETURN
      END IF

!
!     Look up the handle in the unit table.
!
      UINDEX = ISRCHI ( HANDLE, NUT, UTHAN )

!
!     Now check the results of the lookup.  If HANDLE was not found
!     in the unit table or the unit was not locked, just return as
!     there is nothing to do.
!
      IF ( UINDEX .EQ. 0 ) THEN
         RETURN
      ELSE IF ( .NOT. UTLCK(UINDEX) ) THEN
         RETURN
      END IF

!
!     Now look up the handle in the table. Remember FTABS is a sorted
!     list in increasing order.
!
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )

!
!     Check to see if HANDLE is in the file table.  We know it has
!     to be since it is in the unit table if we make it this far.
!     These checks are just for safety's sake.
!
      IF ( FINDEX .EQ. 0 ) THEN

         CALL CHKIN  ( 'ZZDDHUNL'                                  )
         CALL SETMSG ( 'HANDLE # was not found in the file table ' &
         //            'but was located in the unit table.  This ' &
         //            'error should never occur.'                 )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZDDHUNL'                                  )
         RETURN

      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN

         CALL CHKIN  ( 'ZZDDHUNL'                                  )
         CALL SETMSG ( 'HANDLE # was not found in the file table ' &
         //            'but was located in the unit table.  This ' &
         //            'error should never occur.'                 )
         CALL ERRINT ( '#', HANDLE                                 )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZDDHUNL'                                  )
         RETURN

      END IF

!
!     Before actually unlocking the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      TMPSTR = ARCH
      CALL UCASE ( TMPSTR, TMPSTR )
      FILARC = ISRCHC ( TMPSTR, NUMARC, STRARC )

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      IF ( FILARC .NE. FTARC(FINDEX) ) THEN

         CALL CHKIN  ( 'ZZDDHUNL'                                 )
         CALL SETMSG ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         CALL ERRCH  ( '#', STRARC( FTARC(FINDEX) )               )
         CALL ERRCH  ( '%', TMPSTR                                )
         CALL ERRCH  ( '$', FTNAM(FINDEX)                         )
         CALL SIGERR ( 'SPICE(FILARCMISMATCH)'                    )
         CALL CHKOUT ( 'ZZDDHUNL'                                 )
         RETURN

      END IF

!
!     Lastly, check to see if the access method for HANDLE indicates
!     scratch access.  If it is, just return, since scratch files
!     can not have their units unlocked.
!
      IF ( FTAMH(FINDEX) .EQ. SCRTCH ) THEN
         RETURN
      END IF

      UTLCK(UINDEX) = .FALSE.

      RETURN




!$Procedure ZZDDHNFO ( Private --- Get information about a Handle )

      ENTRY ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMH, FOUND )

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
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         IF ( FAILED() ) THEN
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Look up the handle in the table.  Remember FTABS is sorted
!     listed in increasing order.
!
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )

!
!     Check to see if HANDLE is in the handle table.  Remember that
!     we are indexing the table using the absolute value of handle.
!     So include a check to see that HANDLE is FTHAN(FINDEX).
!
      IF ( FINDEX .EQ. 0 ) THEN
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.
         RETURN
      END IF

!
!     If we make it this far, then we have a handle that is in
!     the handle table at row FINDEX.
!
      FOUND  = .TRUE.
      FNAME  = FTNAM(FINDEX) ( 1:FTRTM(FINDEX) )
      INTARC = FTARC(FINDEX)
      INTBFF = FTBFF(FINDEX)
      INTAMH = FTAMH(FINDEX)

      RETURN




!$Procedure ZZDDHISN ( Private --- Is Handle Native? )

      ENTRY ZZDDHISN ( HANDLE, NATIVE, FOUND )

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
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         IF ( FAILED() ) THEN
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Look up the handle in the table. Remember FTABS is sorted
!     listed in increasing order.
!
      FINDEX = BSRCHI ( ABS(HANDLE), NFT, FTABS )

!
!     Check to see if HANDLE is in the handle table.  Remember
!     that we are indexing the table using the absolute value of
!     handle.  So include a check to see that HANDLE is FTHAN(FINDEX).
!
      IF ( FINDEX .EQ. 0 ) THEN
         FOUND = .FALSE.
         RETURN
      ELSE IF ( FTHAN(FINDEX) .NE. HANDLE ) THEN
         FOUND = .FALSE.
         RETURN
      END IF

!
!     If we make it this far, then we have found HANDLE in the file
!     table.  Set NATIVE appropriately and FOUND to TRUE.
!
      NATIVE = NATBFF .EQ. FTBFF(FINDEX)
      FOUND  = .TRUE.

      RETURN





!$Procedure ZZDDHFNH ( Private --- Filename to Handle )

      ENTRY ZZDDHFNH ( FNAME, HANDLE, FOUND )

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

      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHFNH' )
      END IF

!
!     Do the initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         IF ( FAILED() ) THEN
            HANDLE = 0
            CALL CHKOUT ( 'ZZDDHFNH' )
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Left justify FNAME to trim any leading white space.
!
      CALL LJUST ( FNAME, LOCFNM )

!
!     Look up FNAME in the handle table.
!
      CALL ZZDDHF2H ( LOCFNM,  FTABS,  FTAMH, FTARC, FTBFF, &
                      FTHAN,   FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                      UTHAN,   UTLCK,  UTLUN, NUT,   INQEXT, &
                      INQOPN,  INQHAN, FOUND, MNM                 )

!
!     Check found and set HANDLE if we have got one.  No need to
!     check FAILED() since ZZDDHF2H returns FOUND set to FALSE on
!     error.
!
      IF ( FOUND ) THEN
         HANDLE = INQHAN
      ELSE
         HANDLE = 0
      END IF

      CALL CHKOUT ( 'ZZDDHFNH' )
      RETURN




!$Procedure ZZDDHLUH ( Private --- Logical Unit to Handle )

      ENTRY ZZDDHLUH ( UNIT, HANDLE, FOUND )

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
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         IF ( FAILED() ) THEN
            HANDLE = 0
            RETURN
         END IF

!
!        Clear FIRST since we've done the initialization.
!
         FIRST = .FALSE.

      END IF

!
!     Look up the unit in the table.
!
      UINDEX = ISRCHI ( UNIT, NUT, UTLUN )

      IF ( UINDEX .EQ. 0 ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         RETURN
      ELSE IF ( UTHAN(UINDEX) .EQ. 0 ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         RETURN
      END IF

!
!     We've got a handle, store the value and return.
!
      HANDLE = UTHAN(UINDEX)
      FOUND  = .TRUE.

      RETURN

      END
