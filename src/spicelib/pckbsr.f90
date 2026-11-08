!$Procedure PCKBSR ( PCK, buffer segments for readers )

      subroutine pckbsr ( fname, &
                          handle, &
                          body, &
                          et, &
                          descr, &
                          ident, &
                          found   )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Load and unload PCK binary files for use by the readers.
!     Buffer segments for readers.
!
! $ Required_Reading
!
!     DAF
!     PCK
!
! $ Keywords
!
!     FILES
!     PCK
!
! $ Declarations

      implicit none

      character*(*)         fname
      integer(ip)           handle
      integer(ip)           body
      real(wp)              et
      real(wp)              descr    ( * )
      character*(*)         ident
      logical               found

      integer(ip)           ftsize
      parameter           ( ftsize =   5000 )

      integer(ip)           btsize
      parameter           ( btsize =     20 )

      integer(ip)           lbpool
      parameter           ( lbpool =     -5 )

      integer(ip)           stsize
      parameter           ( stsize =   5000 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     FNAME      I   PCKLOF
!     HANDLE    I-O  PCKLOF, PCKUOF, PCKSFS
!     BODY       I   PCKSFS
!     ET         I   PCKSFS
!     DESCR      O   PCKSFS
!     IDENT      O   PCKSFS
!
! $ Detailed_Input
!
!     FNAME    is the name of an PCK file to be loaded.
!
!     HANDLE   on input, is the handle of an PCK file to be
!              unloaded.
!
!     BODY     is the NAIF integer code of an ephemeris object,
!              typically a solar system body.
!
!     ET       is a time, in seconds past the epoch J2000 TDB.
!
! $ Detailed_Output
!
!     HANDLE   on output, is the handle of the binary PCK file
!              containing a located segment.
!
!     DESCR    is the descriptor of a located segment.
!
!     IDENT    is the identifier of a located segment.
!
!     FOUND    is a logical flag indicating whether a segment meeting
!              the search criteria was found. FOUND will have the
!              value .TRUE. if an appropriate segment was found during
!              the search; it will have the value of .FALSE.
!              otherwise. If FOUND has the value .FALSE., then either
!              an appropriate segment could not be found in any of the
!              loaded files or there were no PCK kernel files loaded
!              when the request for a segment was made.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of files that may be loaded
!              by PCKLOF at any given time for use by the PCK readers.
!
!     BTSIZE   is the maximum number of bodies whose segments can be
!              buffered by PCKSFS.
!
!     STSIZE   is the maximum number of segments that can be buffered at
!              any given time by PCKSFS.
!
! $ Exceptions
!
!     1)  If PCKBSR is called directly, the error SPICE(BOGUSENTRY)
!         is signaled.
!
!     2)  See entry points PCKLOF, PCKUOF, and PCKSFS for exceptions
!         specific to them.
!
! $ Files
!
!     PCK kernel files are indicated by filename before loading
!     (see PCKLOF) and handle after loading (all other places).
!
! $ Particulars
!
!     PCKBSR serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        PCKLOF       Load PCK binary file.
!        PCKUOF       Unload PCK binary file.
!        PCKSFS       Select file and segment.
!
!     Before a file can be read by the PCK kernel readers, it must be
!     loaded by PCKLOF, which among other things, calls routines to
!     open the specified file.
!
!     Multiple files may be loaded for use simultaneously, and a file
!     need only be loaded once to become a potential search target
!     for any number of subsequent reads.
!
!     Once a PCK kernel file is loaded and opened, it is assigned a file
!     handle, which is used by the calling program to refer to the file
!     in all subsequent calls to PCK routines.
!
!     A file may be removed from the list of files searched by using
!     PCKUOF to unload it.
!
!     PCKSFS performs the search for segments within a file for the
!     PCK kernel readers. It searches through the most recently loaded
!     files first. Within a single file, PCKSFS searches through
!     the segments in reverse order, beginning with the last segment in
!     the file. The search stops when the first appropriate segment is
!     found or all files and segments have been searched without a
!     match.
!
!     PCKSFS buffers information from loaded PCK files to improve access
!     time by preventing unnecessary file reads during segment searches.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose that the data of interest are contained in a PCK file,
!        and that we want to generate a table containing the
!        descriptors of the PCK segments, or a message indicating that
!        no segment was found, for various request times. We are
!        interested in the data coverage of the segments in the file.
!
!        The code example below loads PCK files and performs searches
!        for various epochs, generating a table containing the segment
!        descriptors, if found, or a message indicating that a segment
!        descriptor was not found.
!
!        Use the PCK kernel below as input file for the code example.
!
!           earth_latest_high_prec.bpc
!
!
!        Example code begins here.
!
!
!              PROGRAM PCKBSR_EX1
!              IMPLICIT NONE
!
!
!        C
!        C     Local parameters
!        C
!              DOUBLE PRECISION      DELTA
!              PARAMETER           ( DELTA  = 50000000.D0 )
!
!              INTEGER               BODY
!              PARAMETER           ( BODY   = 3000        )
!
!              INTEGER               DESCSZ
!              PARAMETER           ( DESCSZ = 5           )
!
!              INTEGER               IDSIZE
!              PARAMETER           ( IDSIZE = 40          )
!
!              INTEGER               NEPOCH
!              PARAMETER           ( NEPOCH = 10          )
!
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(IDSIZE)    SEGID
!
!              DOUBLE PRECISION      BEGET
!              DOUBLE PRECISION      DESCR ( DESCSZ )
!              DOUBLE PRECISION      ENDET
!              DOUBLE PRECISION      ET
!
!              INTEGER               BADDR
!              INTEGER               BODYID
!              INTEGER               EADDR
!              INTEGER               FRAMID
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               PCKHDL
!              INTEGER               PCKTYP
!
!              LOGICAL               FOUND
!
!        C
!        C     Load the PCK file.
!        C
!              CALL PCKLOF ( 'earth_latest_high_prec.bpc', PCKHDL )
!
!        C
!        C     Search for segments using evenly spaced epochs.
!        C
!              ET = -86400.D0
!
!              DO I = 1, NEPOCH
!
!                 WRITE(*,*) 'Epoch = ', ET
!
!        C
!        C        Locate the applicable segment (handle and
!        C        descriptor).
!        C
!                 CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
!
!                 IF ( FOUND ) THEN
!
!        C
!        C           Unpack the segment.
!        C
!                    CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
!             .                    BEGET, ENDET,  BADDR,  EADDR  )
!
!                    WRITE(*,*) '   Segment ID: ', SEGID
!                    WRITE(*,*) '   Body ID   : ', BODYID
!                    WRITE(*,*) '   Frame ID  : ', FRAMID
!                    WRITE(*,*) '   PCK Type  : ', PCKTYP
!                    WRITE(*,*) '   Start ET  : ', BEGET
!                    WRITE(*,*) '   End ET    : ', ENDET
!
!                 ELSE
!
!                    WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
!
!                 END IF
!
!                 WRITE(*,*) ' '
!
!        C
!        C        Increment the epoch.
!        C
!                 ET = ET + DELTA
!
!              END DO
!
!        C
!        C     Unload the PCK file.
!        C
!              CALL PCKUOF ( PCKHDL )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Epoch =   -86400.000000000000
!            ***** SEGMENT NOT FOUND *****
!
!         Epoch =    49913600.000000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :   -43135.816087188054
!            End ET    :    86343752.179112613
!
!         Epoch =    99913600.000000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    86343752.179112613
!            End ET    :    172730640.17431438
!
!         Epoch =    149913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    86343752.179112613
!            End ET    :    172730640.17431438
!
!         Epoch =    199913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    172730640.17431438
!            End ET    :    259117528.16951615
!
!         Epoch =    249913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    172730640.17431438
!            End ET    :    259117528.16951615
!
!         Epoch =    299913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    259117528.16951615
!            End ET    :    345504416.16470283
!
!         Epoch =    349913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    345504416.16470283
!            End ET    :    431891304.15988630
!
!         Epoch =    399913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    345504416.16470283
!            End ET    :    431891304.15988630
!
!         Epoch =    449913600.00000000
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    431891304.15988630
!            End ET    :    518278192.15506977
!
!
!     2) In this example multiple PCK files are loaded and searched for
!        segments.
!
!        Use the PCK kernel below as the first input file for the code
!        example.
!
!           earth_latest_high_prec.bpc
!
!
!        Use the PCK kernel below as the second input file for the code
!        example.
!
!           earth_720101_070426.bpc
!
!        Example code begins here.
!
!
!              PROGRAM PCKBSR_EX2
!              IMPLICIT NONE
!
!
!        C
!        C     Local parameters
!        C
!              DOUBLE PRECISION      DELTA
!              PARAMETER           ( DELTA  = 50000000.D0 )
!
!              INTEGER               BODY
!              PARAMETER           ( BODY   = 3000        )
!
!              INTEGER               DESCSZ
!              PARAMETER           ( DESCSZ = 5           )
!
!              INTEGER               IDSIZE
!              PARAMETER           ( IDSIZE = 40          )
!
!              INTEGER               NEPOCH
!              PARAMETER           ( NEPOCH = 10          )
!
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(IDSIZE)    SEGID
!
!              DOUBLE PRECISION      BEGET
!              DOUBLE PRECISION      DESCR ( DESCSZ )
!              DOUBLE PRECISION      ENDET
!              DOUBLE PRECISION      ET
!
!              INTEGER               BADDR
!              INTEGER               BODYID
!              INTEGER               EADDR
!              INTEGER               FRAMID
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               PCKHD1
!              INTEGER               PCKHD2
!              INTEGER               PCKTYP
!
!              LOGICAL               FOUND
!
!        C
!        C     Load the PCK files.
!        C
!              CALL PCKLOF ( 'earth_latest_high_prec.bpc', PCKHD1 )
!              CALL PCKLOF ( 'earth_720101_070426.bpc',    PCKHD2 )
!
!        C
!        C     Search for segments using evenly spaced epochs.
!        C
!              ET = -86400.D0
!
!              DO I = 1, NEPOCH
!
!                 WRITE(*,*) 'Epoch = ', ET
!
!        C
!        C        Locate the applicable segment (handle and
!        C        descriptor).
!        C
!                 CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
!
!                 IF ( FOUND ) THEN
!
!        C
!        C           Unpack the segment.
!        C
!                    CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
!             .                    BEGET, ENDET,  BADDR,  EADDR  )
!
!                    WRITE(*,*) '   Handle    : ', HANDLE
!                    WRITE(*,*) '   Segment ID: ', SEGID
!                    WRITE(*,*) '   Body ID   : ', BODYID
!                    WRITE(*,*) '   Frame ID  : ', FRAMID
!                    WRITE(*,*) '   PCK Type  : ', PCKTYP
!                    WRITE(*,*) '   Start ET  : ', BEGET
!                    WRITE(*,*) '   End ET    : ', ENDET
!
!                 ELSE
!
!                    WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
!
!                 END IF
!
!                 WRITE(*,*) ' '
!
!        C
!        C        Increment the epoch.
!        C
!                 ET = ET + DELTA
!
!              END DO
!
!        C
!        C     Unload the PCK files.
!        C
!              CALL PCKUOF ( PCKHD1 )
!              CALL PCKUOF ( PCKHD2 )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Epoch =   -86400.000000000000
!            Handle    :            2
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :   -74415076.797098771
!            End ET    :    11979698.742793124
!
!         Epoch =    49913600.000000000
!            Handle    :            2
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    11979698.742793124
!            End ET    :    98374474.282683983
!
!         Epoch =    99913600.000000000
!            Handle    :            2
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    98374474.282683983
!            End ET    :    184769249.82257757
!
!         Epoch =    149913600.00000000
!            Handle    :            2
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    98374474.282683983
!            End ET    :    184769249.82257757
!
!         Epoch =    199913600.00000000
!            Handle    :            2
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    184769249.82257757
!            End ET    :    230817665.18534085
!
!         Epoch =    249913600.00000000
!            Handle    :            1
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    172730640.17431438
!            End ET    :    259117528.16951615
!
!         Epoch =    299913600.00000000
!            Handle    :            1
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    259117528.16951615
!            End ET    :    345504416.16470283
!
!         Epoch =    349913600.00000000
!            Handle    :            1
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    345504416.16470283
!            End ET    :    431891304.15988630
!
!         Epoch =    399913600.00000000
!            Handle    :            1
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    345504416.16470283
!            End ET    :    431891304.15988630
!
!         Epoch =    449913600.00000000
!            Handle    :            1
!            Segment ID: Earth PCK, ITRF93 Frame
!            Body ID   :         3000
!            Frame ID  :           17
!            PCK Type  :            2
!            Start ET  :    431891304.15988630
!            End ET    :    518278192.15506977
!
!
! $ Restrictions
!
!     1)  If Fortran I/O errors occur while searching a loaded PCK
!         file, the internal state of this suite of routines may
!         be corrupted. It may be possible to correct the state
!         by unloading the pertinent PCK files and then re-loading
!         them.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR) (BVS) (NJB)
!
!        Updated entry point PCKSFS to always initialize FOUND.
!
!        Edited the header of the PCKBSR umbrella and all its entry
!        points to comply with NAIF standard. Added complete code
!        examples from existing fragments.
!
!        Changed SAVE statements to save each variable individually.
!
!-    SPICELIB Version 2.0.1, 30-JAN-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
!
!        Updated segment pool initialization condition in entry
!        point PCKLOF so that the pool is initialized only if the file
!        table is empty.
!
!-    SPICELIB Version 1.4.0, 03-JAN-2014 (BVS) (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!
!        Increased FTSIZE (from 1000 to 5000).
!
!        Increased STSIZE (from 100 to 5000).
!
!-    SPICELIB Version 1.3.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!           In the PCKSFS 'MAKE ROOM' state, when the suspended
!           activity is 'ADD TO FRONT' and no segment table room is
!           available, the body table's pointer to the current segment
!           list is now set to null. Previously the pointer was allowed
!           to go stale.
!
!-    SPICELIB Version 1.2.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED calls in entry points PCKUOF and PCKSFS.
!
!-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single PCK file, and the list is
!              too large to be buffered, the corresponding body table
!              pointer is now set to null.
!
!           2) An algorithm change has eliminated a bug caused by not
!              updating the current body index when body table entries
!              having empty segment lists were compressed out of the
!              body table. Previously the body table pointer BINDEX
!              could go stale after the compression.
!
!           3) When a already loaded kernel is re-opened with DAFOPR,
!              it now has its link count reset to 1 via a call to
!              DAFCLS.
!
!           4) The load routine PCKLOF now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby
!              avoiding arithmetic overflow.
!
!           5) The unload routine PCKUOF now calls RETURN() on entry and
!              returns if so directed.
!
!           6) In PCKSFS, DAF calls are followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           7) In PCKSFS, a subscript bound violation in a loop
!              termination test was corrected.
!
!        The "re-use interval" feature was introduced to improve speed
!        in the case where repeated, consecutive requests are satisfied
!        by the same segment. For each body, the associated re-use
!        interval marks the time interval containing the previous
!        request time for which the previously returned segment provides
!        the  highest-priority data available.
!
!        The segment list cost algorithm was modified slightly:
!        the contribution of a file search to the cost of a list
!        is included only when the file search is completed. The
!        cost of finding the re-use interval is accounted for when
!        unbuffered searches are required.
!
!        The file table size has been increased to 1000, in order
!        to take advantage of the DAF system's new ability to load
!        1000 files.
!
!        Various small updates and corrections were made to the
!        comments throughout the file.
!
!        In order to simplify the source code, the in-line singly
!        linked list implementation of the segment table has been
!        replaced by an implementation relying on the SPICELIB
!        doubly linked list routines.
!
!-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
!
!        This differs only slightly from the SPKXXX code.
!        The main difference is that the SFS subroutine returns
!        FOUND = .FALSE. if no files are found, rather than returning
!        an error.
!
!-&


! $ Index_Entries
!
!     buffer PCK segments for readers
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dpmax
      real(wp)              dpmin

      integer(ip)           intmax
      integer(ip)           isrchi
      integer(ip)           lnknfn
      integer(ip)           lnknxt
      integer(ip)           lnkprv
      integer(ip)           lnktl

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           dscsiz
      parameter           ( dscsiz = 5 )

      integer(ip)           sidlen
      parameter           ( sidlen = 40 )

      integer(ip)           slen
      parameter           ( slen   = 15 )

      integer(ip)           nd
      parameter           ( nd     = 2 )

      integer(ip)           ni
      parameter           ( ni     = 5 )


!
!     Constants used in the doubly linked list structure:
!
      integer(ip)           forwrd
      parameter           ( forwrd  = 1 )

      integer(ip)           bckwrd
      parameter           ( bckwrd  = 2 )

!
!     Local variables
!

!
!
!     The file table contains the handle and file number of each file
!     that has been loaded for use with the PCK readers. File
!     numbers begin at one, and are incremented until they reach a
!     value of INTMAX() - 1, at which point they are mapped to the
!     range 1:NFT, where NFT is the number of loaded PCK files.
!
!     (A file number is similar to a file handle, but it is assigned
!     and used exclusively by this module. The purpose of file numbers
!     is to keep track of the order in which files are loaded and the
!     order in which they are searched.)
!
!     All names begin with FT.
!
!        HAN      Handle
!        NUM      File number
!
!     NFT is the number of files that have been loaded. NEXT is
!     incremented whenever a new file is loaded to give the file
!     number of the file. FINDEX is the index of whatever file is
!     of current interest at any given time.
!
!     New files are added at the end of the table. As files are
!     removed, succeeding files are moved forward to take up the
!     slack. This keeps the table ordered by file number.
!
      integer(ip)           nft
      integer(ip)           fthan    ( ftsize )
      integer(ip)           ftnum    ( ftsize )
      integer(ip)           next
      integer(ip)           findex

!
!     The body table contains the beginning of the list of the stored
!     segments for each body, and the expense at which that list
!     was constructed. (The expense of a body list is the number of
!     segment descriptors examined during the construction of the list.)
!     It also contains the highest and lowest file numbers searched
!     during the construction of the list.
!
!     For each body, the time bounds of the "re-use interval" of the
!     last segment found are stored.  This interval is the maximal
!     interval containing the epoch of the last request for data for
!     this body, such that the interval is not masked by higher-priority
!     segments.  The handle, segment descriptor, and segment identifier
!     returned on the last request are also stored.
!
!     All names begin with BT.
!
!        BOD      Body
!        EXP      Expense
!        HFS      Highest file (number) searched
!        LFS      Lowest  file (number) searched
!        BEG      Beginning of segment list
!        LB       Lower bound of the re-use interval of
!                 previous segment returned.
!        UB       Upper bound of the re-use interval of
!                 previous segment returned.
!        PRVD     Previous descriptor returned.
!        PRVI     Previous segment identifier returned.
!        PRVH     Previous handle returned.
!        CHKP     Logical indicating that previous segment should
!                 be checked to see whether it satisfies a request.
!        RUEX     Expense of the re-use interval.
!
!     NBT is the number of bodies for which segments are currently
!     being stored in the table. BINDEX is the index of whatever
!     body is of current interest at any given time.
!
!     New bodies are added at the end of the table. As bodies are
!     removed, the last body is moved forward to take up the slack.
!     This keeps the entries in the table contiguous.
!
      character*(sidlen)    btprvi   ( btsize )

      real(wp)              btprvd   ( dscsiz, btsize )
      real(wp)              btlb     ( btsize )
      real(wp)              btub     ( btsize )

      integer(ip)           bindex
      integer(ip)           btbeg    ( btsize )
      integer(ip)           btbod    ( btsize )
      integer(ip)           btexp    ( btsize )
      integer(ip)           bthfs    ( btsize )
      integer(ip)           btlfs    ( btsize )
      integer(ip)           btprvh   ( btsize )
      integer(ip)           btruex   ( btsize )
      integer(ip)           nbt

      logical               btchkp   ( btsize )

!
!     The segment table contains the handle, descriptor, and identifier
!     for each segment that has been found so far.
!
!     The segment table is implemented as a set of arrays indexed by
!     a SPICE doubly linked list structure.  For each body in the
!     body table, there is a segment table list; each node of a list
!     points to data associated with a segment.  In each list, the head
!     node corresponds to the highest-priority segment in that list,
!     and segment priority decreases in the forward direction.
!
!     All names begin with ST.
!
!        POOL     Doubly linked list pool.
!        HAN      Handle
!        DES      Descriptor
!        IDNT     Identifier
!
!     New segments are added to the front or end of a body list
!     as appropriate, according to the rules spelled out under
!     entry point PCKSFS.
!
      integer(ip)           stpool ( 2, lbpool : stsize )
      integer(ip)           sthan  (             stsize )
      real(wp)              stdes  ( dscsiz,     stsize )
      character*(sidlen)    stidnt (             stsize )

!
!     Other stuff
!
      character*(slen)      doing
      character*(slen)      stack    ( 2 )
      character*(slen)      status
      character*(slen)      urgent

      real(wp)              dcd      ( nd )

      integer(ip)           cheap
      integer(ip)           cost
      integer(ip)           crflbg
      integer(ip)           head
      integer(ip)           i
      integer(ip)           icd      ( ni )
      integer(ip)           j
      integer(ip)           minexp
      integer(ip)           new
      integer(ip)           nxtseg
      integer(ip)           p
      integer(ip)           tail
      integer(ip)           top

      logical               fnd
      logical               fndhan

!
!     Saved variables
!
      save                  btbeg
      save                  btbod
      save                  btchkp
      save                  btexp
      save                  bthfs
      save                  btlb
      save                  btlfs
      save                  btprvd
      save                  btprvh
      save                  btprvi

      save                  btruex
      save                  btub
      save                  fthan
      save                  ftnum
      save                  nbt
      save                  next
      save                  nft
      save                  stdes
      save                  sthan
      save                  stidnt

      save                  stpool


!
!     Initial values
!
      data                  nft       / 0 /
      data                  nbt       / 0 /
      data                  next      / 0 /

!
!     Nobody has any business calling PCKBSR directly.
!
      if ( return() ) then
         return
      end if

      call chkin  ( 'PCKBSR' )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'PCKBSR' )

      return



!$Procedure PCKLOF ( PCK, load binary file )

      entry pcklof ( fname, handle )

! $ Abstract
!
!     Load a binary PCK file for use by the readers. Return the
!     handle of the loaded file which is used by other PCK routines to
!     refer to the file.
!
! $ Required_Reading
!
!     DAF
!     PCK
!
! $ Keywords
!
!     FILES
!     PCK
!
! $ Declarations
!
!     CHARACTER*(*)         FNAME
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the file to be loaded.
!     HANDLE     O   Loaded file's handle.
!     FTSIZE     P   Maximum number of loaded PCK files.
!
! $ Detailed_Input
!
!     FNAME    is the character name of the file to be loaded.
!
! $ Detailed_Output
!
!     HANDLE   is the integer handle assigned to the file upon loading.
!              Other PCK routines will subsequently use this number to
!              refer to the file.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of PCK files that may be loaded
!              simultaneously under any circumstances. FTSIZE is
!              currently set to match the maximum number of DAF files
!              that may be loaded simultaneously.
!
! $ Exceptions
!
!     1)  If an attempt is made to open more DAF files than is
!         specified by the parameter FTSIZE in DAF system, an error
!         is signaled by a routine in the call tree of this routine.
!
!     2)  If an attempt is made to load more files than is specified
!         by the parameter FTSIZE in the PCK subsystem, and if the DAF
!         system has room to load another file, the error
!         SPICE(PCKFILETABLEFULL) is signaled. The current setting of
!         FTSIZE does not allow this situation to arise: the DAF system
!         will trap the error before this routine has the chance.
!
!     3)  This routine makes use of DAF file system routines and is
!         subject to all of the constraints imposed by the DAF file
!         system. See the DAF Required Reading daf.req or individual DAF
!         routines for details.
!
! $ Files
!
!     A file specified by FNAME, to be loaded. The file is assigned a
!     handle by PCKLOF, which will be used by other routines to refer
!     to it.
!
! $ Particulars
!
!     If there is room for a new file in the file table, PCKLOF creates
!     an entry for it, and opens the file for reading.
!
!     Also, if the file table is empty, PCKLOF initializes it.
!
! $ Examples
!
!     See $Examples in PCKBSR.
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
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.2, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 30-JAN-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
!
!        Updated segment pool initialization condition in entry
!        point PCKLOF so that the pool is initialized only if the file
!        table is empty.
!
!-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
!
!        Bug fixes:
!
!           1) When an already loaded kernel is opened with DAFOPR,
!              it now has its link count reset to 1 via a call to
!              DAFCLS.
!
!           2) This routine now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby avoiding
!              arithmetic overflow. The numbers in the file table
!              are replaced with consecutive integers in the range
!              1 : NFT, such that the ordering of the numbers is not
!              changed. The HFS and LFS arrays are updated accordingly.
!
!        Also, the flags indicating validity of the re-use intervals
!        are set to .FALSE. here.
!
!-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
!
!-&


! $ Index_Entries
!
!     load PCK file
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'PCKLOF' )
      end if

!
!     Any time we load a file, there is a possibility that the
!     re-use intervals are invalid because they're been superseded
!     by higher-priority data.  Since we're not going to examine
!     the loaded file, simply indicate that all of the re-use
!     intervals are invalid.
!
      do i = 1, nbt
         btchkp(i) = .false.
      end do

!
!     Nothing works unless at least one file has been loaded, so this
!     is as good a place as any to initialize the segment table pool.
!     We want to avoid unnecessary initializations, so we only
!     initialize the list when no files are loaded. It's quite possible
!     to have files loaded and an empty body table, so we don't
!     want to re-initialize just because there are no body table
!     entries.
!
      if ( nft .eq. 0 ) then
         call lnkini ( stsize, stpool )
      end if

!
!     To load a new file, first try to open it for reading.
!
      call dafopr ( fname, handle )

      if ( failed() ) then
         call chkout ( 'PCKLOF' )
         return
      end if

!
!     Determine if the file is already in the table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex .gt. 0 ) then
!
!        The last call we made to DAFOPR added another DAF link to
!        the PCK file.  Remove this link.
!
         call dafcls ( handle )

!
!        Remove the file from the file table and remove its segments
!        from the segment table.  If the segment list for a body
!        becomes empty, remove that body from the body table.
!
         nft = nft - 1

         do i = findex, nft
            fthan( i ) = fthan( i + 1 )
            ftnum( i ) = ftnum( i + 1 )
         end do

         i = 1

         do while ( i .le. nbt )

            p = btbeg( i )

            do while ( p .gt. 0 )
!
!              Find the successor of P, if any.
!
               nxtseg = lnknxt ( p, stpool )

               if ( sthan( p ) .eq. handle ) then
!
!                 The segment corresponding to node P came from
!                 the file we're unloading.  Delete the node for
!                 P from the segment list for body I; if P happens
!                 to be the head node for body I's segment list,
!                 make the successor of P the head of the list.
!
                  call lnkfsl ( p, p, stpool )

                  if ( p .eq. btbeg(i) ) then
                     btbeg( i ) = nxtseg
                  end if

               end if
!
!              Update P.
!
               p = nxtseg

            end do

!
!           If the list for this body is now empty, shorten the current
!           table by one: put all the entries for the last body in the
!           table into the space occupied by the one we've deleted.
!
            if ( btbeg( i ) .le. 0 ) then
!
!              Because all of the re-use intervals are invalid, we need
!              not copy the saved items associated with them.  The
!              items not copied are
!
!                 BTCHKP
!                 BTLB
!                 BTPRVD
!                 BTPRVH
!                 BTPRVI
!                 BTRUEX
!                 BTUB
!
               btbod( i ) = btbod( nbt )
               btexp( i ) = btexp( nbt )
               bthfs( i ) = bthfs( nbt )
               btlfs( i ) = btlfs( nbt )
               btbeg( i ) = btbeg( nbt )

               nbt = nbt - 1

            else

               i = i + 1

            end if

         end do

      else
!
!        This is a new file.  Make sure that there are unused slots
!        in the file table.
!
         if ( nft .eq. ftsize ) then
!
!           This error case can occur only if FTSIZE is larger than
!           the maximum number of open DAF files.  Currently FTSIZE
!           is equal to this limit.
!
            call dafcls ( handle )

            call setmsg ( 'The internal file table is already full, ' // &
                          'with # entries.'         )
            call errint ( '#', ftsize               )
            call sigerr ( 'SPICE(PCKFILETABLEFULL)' )
            call chkout ( 'PCKLOF'                  )
            return

         end if

      end if

!
!     Determine the next file number.  Note that later code assumes
!     that the file number can be incremented by 1, so we can't allow
!     the file number to reach INTMAX().
!
      if ( next .lt. intmax()-1 ) then

         next = next + 1

      else
!
!        The user is to be congratulated:  we've run out of file
!        numbers.
!
!        Re-set the valid file numbers so they lie in the range 1:NFT,
!        with the Ith file in the file table having file number I.
!        First update the LFS and HFS components of the body table
!        according to this mapping.
!
!        Set any body table entries that are lower than FTNUM(1) to
!        zero.
!
         do i = 1, nbt
!
!           Re-map the HFS table for the Ith body.
!
            j = isrchi ( bthfs(i), nft, ftnum )

            if ( j .gt. 0 ) then
!
!              The highest file searched for body I is the Jth file
!              in the file table.
!
               bthfs(i) = j

            else
!
!              The highest file searched for body I is not in the file
!              table.  This occurs when the highest file searched has
!              been unloaded.  Note that this assignment makes all files
!              appear to be "new" when a lookup for body I is performed.
!
               bthfs(i) = 0

            end if

!
!           Re-map the LFS table for the Ith body.
!
            j = isrchi ( btlfs(i), nft, ftnum )

            if ( j .gt. 0 ) then
!
!              The lowest file searched for body I is the Jth file
!              in the file table.
!
               btlfs(i) = j

            else
!
!              The lowest file searched for body I is not in the file
!              table.  This occurs when the lowest file searched has
!              been unloaded.  Force reconstruction of the list by
!              making all files "new."
!
               btlfs(i) = 0
               bthfs(i) = 0

            end if

         end do

!
!        Re-map the file number table itself.
!
         do i = 1, nft

            ftnum(i) = i

         end do

!
!        Assign a new file number.
!
         next = nft + 1

      end if


      nft        = nft  + 1
      fthan(nft) = handle
      ftnum(nft) = next

      call chkout ( 'PCKLOF' )
      return



!$Procedure PCKUOF ( PCK, unload binary file )

      entry pckuof ( handle )

! $ Abstract
!
!     Unload a binary PCK file so that it will no longer be searched by
!     the readers.
!
! $ Required_Reading
!
!     DAF
!     PCK
!
! $ Keywords
!
!     FILES
!     PCK
!
! $ Declarations
!
!     INTEGER               HANDLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of file to be unloaded
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle assigned to the PCK file upon
!              loading.
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
!     1)  Unloading a file that has not been loaded is a no-op.
!         No error is signaled.
!
! $ Files
!
!     The file referred to by HANDLE is unloaded.
!
! $ Particulars
!
!     A PCK file is removed from consideration during a search by the
!     readers by a call to PCKUOF.
!
!     The file table entry corresponding to the file referenced by
!     HANDLE is removed and the file is closed. Any segment table
!     entry which came from the specified file is also deleted.
!
!     If the file specified by HANDLE is not currently loaded in the
!     PCK system, no action is taken.
!
! $ Examples
!
!     See $Examples in PCKBSR.
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
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.1.3, 09-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Improved documentation of $Detailed_Input and $Particulars
!        sections.
!
!-    SPICELIB Version 4.1.2, 30-JAN-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 4.1.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
!
!        Bug fixes:
!
!        1) This routine now calls RETURN() on entry and
!           returns if so directed.
!
!        Also, the flags indicating validity of those re-use intervals
!        whose data comes from the unloaded file are set to .FALSE.
!
!-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
!
!-&


! $ Index_Entries
!
!     unload PCK file
!
!-&


      if ( return() ) then
         return
      end if

!
!     All of the stored segments from the file must be removed
!     from the segment table (by returning the corresponding nodes
!     to the segment table pool.)
!
!     Don't do anything if the given handle is not in the file table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex .eq. 0 ) then
         return
      end if

!
!     First get rid of the entry in the file table. Close the file
!     before wiping out the handle.
!
      call dafcls ( fthan(findex) )

      nft = nft - 1

      do i = findex, nft
         fthan( i ) = fthan( i + 1 )
         ftnum( i ) = ftnum( i + 1 )
      end do

!
!     Check each body list individually. Note that the first node
!     on each list, having no predecessor, must be handled specially.
!
      i = 1

      do while ( i .le. nbt )

         p = btbeg( i )

         do while ( p .gt. 0 )

            nxtseg = lnknxt ( p, stpool )

            if ( sthan(p) .eq. handle ) then

               if ( p .eq. btbeg(i) ) then
                  btbeg( i ) = nxtseg
               end if

               call lnkfsl ( p, p, stpool )

            end if

            p = nxtseg

         end do

!
!        If we happened to get rid of all of the segments for this
!        body, then the body should be deleted from the table: shift
!        all entries for the body at the end of the table into the
!        space occupied by the deleted body.
!
         if ( btbeg(i) .le. 0 ) then

            if ( i .ne. nbt ) then

               btbod (i) = btbod (nbt)
               btexp (i) = btexp (nbt)
               bthfs (i) = bthfs (nbt)
               btlfs (i) = btlfs (nbt)
               btbeg (i) = btbeg (nbt)
               btlb  (i) = btlb  (nbt)
               btub  (i) = btub  (nbt)
               btprvh(i) = btprvh(nbt)
               btprvi(i) = btprvi(nbt)
               btchkp(i) = btchkp(nbt)
               btruex(i) = btruex(nbt)

               call moved ( btprvd(1,nbt), dscsiz, btprvd(1,i) )

            end if

            nbt = nbt - 1

         else

            i = i + 1

         end if

      end do

!
!     Any time we unload a file, we may be removing the file
!     providing data for the re-use interval for one or more bodies.
!     For each body, if the handle associated with the re-use interval
!     happens to be that of the file we're unloading, indicate
!     that the re-use interval is invalid.
!
      do i = 1, nbt

         if ( btchkp(i) ) then

            if ( btprvh(i) .eq. handle ) then
               btchkp(i) = .false.
            end if

         end if

      end do

      return



!$Procedure PCKSFS ( PCK, select file and segment )

      entry pcksfs ( body, et, handle, descr, ident, found )

! $ Abstract
!
!     Search through loaded files to find the first segment applicable
!     to the body and time specified. Buffer searched segments in the
!     process, to attempt to avoid re-reading files.
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     FILES
!     PCK
!
! $ Declarations
!
!     INTEGER               BODY
!     DOUBLE PRECISION      ET
!     INTEGER               HANDLE
!     DOUBLE PRECISION      DESCR  ( * )
!     CHARACTER*(*)         IDENT
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   Body ID.
!     ET         I   Ephemeris time.
!     HANDLE     O   Handle of file containing the applicable segment.
!     DESCR      O   Descriptor of the applicable segment.
!     IDENT      O   Identifier of the applicable segment.
!     FOUND      O   Indicates whether or not a segment was found.
!
! $ Detailed_Input
!
!     BODY     is the NAIF integer code of an ephemeris object,
!              typically a solar system body.
!
!     ET       is a time, in seconds past the epoch J2000 TDB.
!
! $ Detailed_Output
!
!     HANDLE   is the handle of the binary PCK file containing a
!              located segment.
!
!     DESCR    is the descriptor of a located segment.
!
!     IDENT    is the identifier of a located segment.
!
!     FOUND    is a logical flag indicating whether a requested segment
!              was found or not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an attempt is made to call PCKSFS when there aren't any
!         files loaded, the error SPICE(NOLOADEDFILES) is signaled.
!
! $ Files
!
!     All files loaded by PCKLOF are potential search targets for
!     PCKSFS.
!
! $ Particulars
!
!     This routine finds the highest-priority segment, in any loaded
!     PCK file, such that the segment provides data for the specified
!     body and epoch.
!
! $ Examples
!
!     See $Examples in PCKBSR.
!
! $ Restrictions
!
!     1)  If Fortran I/O errors occur while searching a loaded PCK
!         file, the internal state of this suite of routines may
!         be corrupted. It may be possible to correct the state
!         by unloading the pertinent PCK files and then re-loading
!         them.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.3.0, 13-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Relocated initialization of FOUND so it is always
!        executed, even if an error state is indicated by RETURN().
!
!-    SPICELIB Version 4.2.2, 30-JAN-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 4.2.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!           In the PCKSFS 'MAKE ROOM' state, when the suspended
!           activity is 'ADD TO FRONT' and no segment table room is
!           available, the body table's pointer to the current segment
!           list is now set to null. Previously the pointer was allowed
!           to go stale.
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single PCK file, and the list is
!              too large to be buffered, the corresponding body table
!              pointer is now set to null.
!
!           2) An algorithm change has eliminated a bug caused by not
!              updating the current body index when body table entries
!              having empty segment lists were compressed out of the
!              body table. Previously the body table pointer BINDEX
!              could go stale after the compression.
!
!           3) DAF calls are now followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           4) A subscript bound violation in a loop termination test
!              was corrected. The loop is located in the
!              'SEARCH W/O BUFFERING' block; it finds the start of a
!              partial list that is to be freed.
!
!        The "re-use interval" feature was introduced to improve speed
!        in the case where repeated, consecutive requests are satisfied
!        by the same segment.
!
!        The segment list cost algorithm was modified slightly:
!        the contribution of a file search to the cost of a list
!        is included only when the file search is completed. The
!        cost of finding the re-use interval is accounted for when
!        unbuffered searches are required.
!
!        The file table size has been increased to 1000, in order
!        to take advantage of the DAF system's new ability to load
!        1000 files.
!
!        The body table size has been increased to 200 in order to
!        decrease the chance of thrashing due to swapping segment
!        lists for different bodies.
!
!        Various small updates and corrections were made to the
!        comments throughout the file.
!
!        In order to simplify the source code, the in-line singly
!        linked list implementation of the segment table has been
!        replaced by an implementation relying on the SPICELIB
!        doubly linked list routines.
!
!-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
!
!        This differs only slightly from the SPKXXX code.
!        The main difference is that the SFS subroutine returns
!        FOUND = .FALSE. if no files are found, rather than returning
!        an error.
!
!-&


! $ Index_Entries
!
!     select PCK file and segment
!
!-&

!
!     Assume the segment is not found, until it actually is.
!
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'PCKSFS' )

!
!     Buffering segments involves maintaining three tables:  the
!     file table, the body table, and the segment table.  The routine
!     is broken down into various tasks, described below, which
!     perform these manipulations.  A description of the components
!     of each table is provided in the declarations section of PCKBSR.

!
!     Return FOUND as .FALSE. if no files are loaded.  Unlike the SPK
!     case, it's not an error to call this routine if no files are
!     loaded.
!
      if ( nft .eq. 0 ) then
         call chkout ( 'PCKSFS' )
         return
      end if

!
!     The stack of suspended tasks is empty.
!
      top = 0

!
!     In the following loop, we will try to simplify things by
!     doing exactly one thing on each pass through the loop.
!     After each pass, the status of the loop (STATUS) will be
!     adjusted to reflect the next thing that needs to be done.
!     Occasionally, the current task will have to be interrupted
!     until another task can be carried out. (For example, when
!     collecting new segments, an interrupt might place a segment
!     at the front or end of the current body list; when placing
!     the segment on the list, a second interrupt might free up
!     room in the segment table in order to allow the addition
!     to proceed.) In this case, the current task will be saved and
!     restored after the more urgent task has been completed.
!
!     The loop can terminate in only one of two ways (unless an
!     error occurs). First, if an applicable segment is found in
!     the segment table, the  handle, descriptor, and identifier for
!     the segment are returned immediately.  Second, if the table
!     does not contain an applicable segment, and if no files remain
!     to be searched, the loop terminates normally, and no data are
!     returned.
!
!     The individual tasks are described below.
!
!     'NEW BODY'
!
!
!        This indicates that the specified body has no segments stored
!        for it at all. It must be added to the body table.  (This is
!        followed immediately by an OLD FILES search, in which every
!        file loaded is considered an old file.)
!
!     'NEW FILES'
!
!        This indicates that at least one new file has been added
!        since the last time the segment list for the specified
!        body was searched. Find the oldest of these new files,
!        and begin a NEW SEGMENTS search in forward order for
!        segments to add to the front of the list.
!
!     'NEW SEGMENTS'
!
!        Continue a NEW FILES search, adding segments for the specified
!        body to the front of the list.
!
!     'OLD FILES'
!
!        This indicates that although the list has been searched
!        and found to contain no applicable segment, some of the
!        older files remain to be searched. Find the newest of these
!        old files, and begin an OLD SEGMENTS search in backward order.
!
!     'OLD SEGMENTS'
!
!        Continue an OLD FILES search, adding segments for the specified
!        body to the end of the list.
!
!     'CHECK LIST'
!
!        This indicates that the list is ready to be searched,
!        either because no new files have been added, or because
!        segments from a new file or an old file have recently
!        been added.
!
!        The list is never checked until all new files have been
!        searched.
!
!        If an applicable segment is found, it is returned.
!
!     'MAKE ROOM' (Interrupt)
!
!        This indicates that one of the bodies must be removed,
!        along with its stored segments, to make room for another
!        body or segment.  The body (other than the one being searched
!        for) with the smallest expense is selected for this honor.
!
!     'ADD TO FRONT' (Interrupt)
!
!        This indicates that a segment has been found (during the
!        course of a NEW FILES search) and must be added to the front
!        of the list.
!
!     'ADD TO END' (Interrupt)
!
!        This indicates that a segment has been found (during the
!        course of an OLD FILES search) and must be added to the end
!        of the list.
!
!     'SUSPEND'
!
!        This indicates that the current task (DOING) should be
!        interrupted until a more urgent task (URGENT) can be
!        carried out. The current task is placed on a stack for
!        safekeeping.
!
!     'RESUME'
!
!        This indicates that the most recently interrupted task
!        should be resumed immediately.
!
!     '?'
!
!        This indicates that the next task is not immediately
!        apparent: if new files exist, they should be searched;
!        otherwise the list should be checked.
!


!
!     Is the body already in the body table?  This determines what the
!     first task should be.
!
      bindex = isrchi ( body, nbt, btbod )

      if ( bindex .eq. 0 ) then

         status = 'NEW BODY'

      else
!
!        Much of the time, the segment used to satisfy the previous
!        request for a given body will also satisfy the current request
!        for data for that body.  Check whether this is the case.
!
         if ( btchkp(bindex) ) then
!
!           The previous segment found for the current body is a
!           viable candidate for the current request.  See whether
!           the input ET value falls into the re-use interval for this
!           body:  the time interval for which the previously returned
!           segment for this body provides the highest-priority
!           coverage.
!
!           We treat the re-use interval as topologically open because
!           one or both endpoints may belong to higher-priority
!           segments.
!
            if (       ( et .gt. btlb(bindex) ) &
                 .and. ( et .lt. btub(bindex) )  ) then
!
!              The request time is covered by the segment found on
!              the previous request for data for the current body,
!              and this interval is not masked by any higher-priority
!              segments.  The previous segment for this body satisfies
!              the request.
!
               handle = btprvh(bindex)
               ident  = btprvi(bindex)

               call moved ( btprvd(1,bindex), dscsiz, descr )

               found  = .true.

               call chkout ( 'PCKSFS' )
               return

            end if

!
!           Adjust the expense here. If the expense of the list
!           contains a component due to the cost of finding the
!           unbuffered segment providing data for re-use, subtract
!           that component from the expense.
!
            btexp(bindex)  = btexp(bindex) - btruex(bindex)
            btruex(bindex) = 0

!
!           The re-use interval becomes invalid if it didn't satisfy
!           the request.  The validity flag gets re-set below.
!
!           At this point, the previous segment is not a candidate
!           to satisfy the request---at least not until we've verified
!           that
!
!              - The previous segment is still available.
!
!              - The previous segment hasn't been superseded by a more
!                recently loaded segment.
!
            btchkp(bindex) = .false.

         end if


!
!        If the segment list for this body is empty, make sure the
!        expense is reset to 0.
!
         if ( btbeg(bindex) .eq. 0 ) then

            btexp(bindex) = 0

         end if


         status = '?'

      end if


      do while ( status .ne. 'HOPELESS' )
!
!        If new files have been added, they have to be searched.
!        Otherwise, we can go right to the list of stored segments.
!
         if ( status .eq. '?' ) then
!
!           There are two ways to get to this point.
!
!           1)  Status may have been set to '?' prior to the
!               loop DO WHILE ( STATUS .NE. HOPELESS ).
!
!           2)  Status was set to '?' by the NEW SEGMENTS block
!               of code as the result of finishing the read of
!               a new file.
!

            if ( bthfs( bindex ) .lt. ftnum( nft ) ) then
               status = 'NEW FILES'
            else
               status = 'CHECK LIST'
            end if



         else if ( status .eq. 'NEW BODY' ) then
!
!           New bodies are added to the end of the body table. If the
!           table is full, one of the current occupants must be
!           removed to make room for the new one.
!
!           Setting LFS to one more than the highest current
!           file number means the OLD FILES SEARCH that follows will
!           begin with the last-loaded file.
!
!           There is one way to get here:
!
!           1)  The variable STATUS was set to NEW BODY prior to the
!               loop DO WHILE ( STATUS .NE. HOPELESS ).
!
!           Find the cheapest slot in the body table to store
!           the initial information about this body.
!
!           NOTE:  This used to be handled by the MAKE ROOM section.
!           However, trying to handle this special case there was
!           just more trouble than it was worth.
!

            if ( nbt .lt. btsize ) then
!
!              If the body table isn't full, the cheapest place is
!              just the next unused row of the table.
!
               nbt   = nbt + 1
               cheap = nbt

            else
!
!              The body table is full.  Find the least
!              expensive body in the table and remove it.
!
               cheap  = 1
               minexp = btexp(1)

               do i = 2, nbt

                  if ( btexp(i) .lt. minexp ) then
                     cheap  = i
                     minexp = btexp(i)
                  end if

               end do

!
!              If there are any segments associated with the
!              least expensive body, we put them back on the free
!              list.
!
               head = btbeg(cheap)

               if ( head .gt. 0 ) then

                  tail =  - lnkprv ( head, stpool )
                  call lnkfsl ( head, tail, stpool )

               end if

            end if

!
!           Set up a body table entry for the new body.
!
            btbod (cheap) = body
            btexp (cheap) = 0
            bthfs (cheap) = ftnum(nft)
            btlfs (cheap) = ftnum(nft) + 1
            btbeg (cheap) = 0
            btchkp(cheap) = .false.

!
!           The following items associated with the re-use interval
!           need not be initialized at this point:
!
!              BTRUEX
!              BTLB
!              BTUB
!              BTPRVH
!              BTPRVI
!              BTPRVD
!
!           However, we'll give these items initial values to
!           help prevent compilation warnings from zealous
!           compilers.
!
            btruex(cheap) = 0
            btlb  (cheap) = dpmin()
            btub  (cheap) = dpmax()
            btprvh(cheap) = 0
            btprvi(cheap) = ' '
            call cleard ( dscsiz, btprvd(1,cheap) )

!
!           BINDEX is the body table index of the new entry.
!
            bindex = cheap

!
!           Now search the loaded PCK files for segments relating to
!           this body.  We start with the last-loaded files and
!           work backwards.
!
            status = 'OLD FILES'



         else if ( status .eq. 'NEW FILES' ) then
!
!           When new files exist, they should be searched in forward
!           order, beginning with the oldest new file not yet searched.
!           All new files must be searched before the list can be
!           checked, to ensure that the best (newest) segments are
!           being used.
!
!           Begin a forward search, and prepare to look for individual
!           segments from the file.
!
!           The only way to get here is to have STATUS set to
!           the value NEW FILES in the STATUS .EQ. '?' block
!           of the IF structure.
!
!           Find the next file to search; set FINDEX to the
!           corresponding file table entry.
!
            findex = 1

            do while ( bthfs( bindex ) .ge. ftnum( findex ) )
               findex = findex + 1
            end do

            bthfs( bindex ) = ftnum( findex )

            call dafbfs ( fthan( findex ) )

            if ( failed() ) then
               call chkout ( 'PCKSFS' )
               return
            end if

            status = 'NEW SEGMENTS'

!
!           The cost of the list contributed by the new file is
!           zero so far.
!
            cost = 0


         else if ( status .eq. 'NEW SEGMENTS' ) then
!
!           New files are searched in forward order. Segments, when
!           found, are inserted at the front of the list. Invisible
!           segments (alpha > omega) are ignored.
!
!           Each segment examined, whether applicable or not, adds to
!           the expense of the list.
!
!           The only way to get here is from the NEW FILES block
!           of the IF structure.

            call daffna ( fnd )

            if ( failed() ) then
               call chkout ( 'PCKSFS' )
               return
            end if

            if ( .not. fnd ) then
!
!              We're out of segments in the current file.  Decide
!              whether we need to examine another new file, or
!              whether we're ready to check the list.
!
               status = '?'
               btexp( bindex ) = btexp( bindex ) + cost

            else

               call dafgs ( descr )
               call dafus ( descr, nd, ni, dcd, icd )

               if ( failed() ) then
                  call chkout ( 'PCKSFS' )
                  return
               end if

               if (       ( icd(1) .eq. body   ) &
                    .and. ( dcd(1) .le. dcd(2) )  ) then

                  doing  = 'NEW SEGMENTS'
                  urgent = 'ADD TO FRONT'
                  status = 'SUSPEND'

               end if

               cost = cost + 1

            end if
!
!           If we haven't reset the status, we'll return for another
!           'NEW SEGMENTS' pass.
!

         else if ( status .eq. 'OLD FILES' ) then
!
!           When old files must be searched (because the segments
!           in the list are inadequate), they should be searched
!           in backward order, beginning with the newest old file
!           not yet searched. The segment list will be re-checked
!           after each file is searched.  If a match is found,
!           the search terminates, so some old files may not be
!           searched.
!
!           Search from the end, and prepare to look for individual
!           segments from the file.
!
!           You can get to this block in two ways.
!
!           1) We can have a NEW BODY
!
!           2) We have checked the current list (CHECK LIST) for
!              this body, didn't find an applicable segment and
!              have some files left that have not been searched.

            findex = nft

            do while ( btlfs( bindex ) .le. ftnum( findex ) )
               findex = findex - 1
            end do

            call dafbbs ( fthan( findex ) )

            if ( failed() ) then
               call chkout ( 'PCKSFS' )
               return
            end if


            status = 'OLD SEGMENTS'
!
!           The next thing we'll do is search through all the segments
!           of this file for those that applicable to this body.
!           The cost of the list contributed by the current file is
!           zero so far.
!
            cost = 0


         else if ( status .eq. 'OLD SEGMENTS' ) then
!
!           Old files are searched in backward order. Segments, when
!           found, are inserted at the end of the list. Invisible
!           segments (alpha > omega) are ignored.
!
!           Each segment examined, whether applicable or not, adds to
!           the expense of the list.
!
!           There is only one way to get here---from the
!           block 'OLD FILES'.  Note we do not add to the
!           expense of the list for this body until we've
!           completely searched this file.
!
            call daffpa ( fnd )

            if ( failed() ) then
               call chkout ( 'PCKSFS' )
               return
            end if

            if ( .not. fnd ) then
!
!              We've been through all of the segments in this file.
!              Change the lowest file searched indicator for this body
!              to be the current file, and go check the current list.
!
               btlfs( bindex ) =  ftnum( findex )
               btexp( bindex ) =  btexp( bindex ) + cost
               status          = 'CHECK LIST'

            else

               call dafgs ( descr )
               call dafus ( descr, nd, ni, dcd, icd )

               if ( failed() ) then
                  call chkout ( 'PCKSFS' )
                  return
               end if

               if (       ( icd(1) .eq. body   ) &
                    .and. ( dcd(1) .le. dcd(2) )  )  then

                  doing  = 'OLD SEGMENTS'
                  urgent = 'ADD TO END'
                  status = 'SUSPEND'

               end if

               cost = cost + 1

            end if
!
!           If we haven't reset the status, we'll return for another
!           'OLD SEGMENTS' pass.
!

         else if ( status .eq. 'CHECK LIST'   ) then
!
!           Okay, all the new files (and maybe an old file or two) have
!           been searched. Time to look at the list of segments stored
!           for the body to see if one applicable to the specified
!           epoch is hiding in there. If so, return it.  If not,
!           try another old file.  If there are no more old files,
!           give up the ghost.
!
!           There are two ways to get to this point.
!
!           1) From the '?' block.
!           2) From the 'OLD SEGMENTS' block.
!
!           For every segment examined, initialize the re-use interval
!           associated with the current body.
!
            btlb(bindex) = dpmin()
            btub(bindex) = dpmax()
            p            = btbeg( bindex )

            do while ( p .gt. 0 )

               if ( et .gt. stdes(2,p) ) then
!
!                 ET is to the right of the coverage interval of this
!                 segment.
!
                  btlb(bindex) = max ( btlb(bindex), stdes(2,p) )


               else if ( et .lt. stdes(1,p) ) then
!
!                 ET is to the left of the coverage interval of this
!                 segment.
!
                  btub(bindex) = min ( btub(bindex), stdes(1,p) )

               else
!
!                 The segment coverage interval includes ET.
!
                  call moved ( stdes( 1,p ), dscsiz, descr )
                  ident  = stidnt( p )
                  handle = sthan ( p )
                  found  = .true.

!
!                 Set the re-use interval for the current body.
!
                  btlb(bindex) = max ( btlb(bindex), stdes(1,p) )
                  btub(bindex) = min ( btub(bindex), stdes(2,p) )

!
!                 Save the returned output items, in case this segment
!                 may satisfy the next request.
!
                  btprvh(bindex) =  handle
                  btprvi(bindex) =  ident
                  call moved ( descr, dscsiz, btprvd(1,bindex) )
                  btchkp(bindex) =  .true.

                  call chkout ( 'PCKSFS' )
                  return

               end if

!
!              Get the next node.  We avoid LNKNXT here in order
!              to speed up the operation.
!
               p = stpool ( forwrd, p )

            end do

!
!           If we're still here we didn't have information for this
!           body in the segment list.
!
!           If there are more files, search them.
!           Otherwise, things are hopeless, set the status that way.
!
            if ( btlfs( bindex ) .gt. ftnum( 1 ) ) then
               status = 'OLD FILES'
            else
               status = 'HOPELESS'
            end if



         else if ( status .eq. 'MAKE ROOM' ) then
!
!           When adding a segment to a full segment table, one of
!           the current bodies must be dropped. The ideal candidate
!           is the one whose list was constructed at the lowest expense.
!           The candidate should be removed from the body table, and
!           its list transferred to the segment table pool.
!
!           There is ``room'' if the segment table pool contains at
!           least one free node.
!
!           It is possible that a single body requires more than the
!           entire segment table for its own segments. Two things might
!           happen in such a case:
!
!              1) If the list under consideration was being added to at
!                 the end, then a search is continued without buffering
!                 any segments.
!
!              2) If the list was being added to at the beginning, then
!                 that means there was a NEW FILES search going on, and
!                 so a brand new list is constructed for the body, much
!                 as in a 'NEW BODY' task.
!
!           There are two different ways to get to this point.
!
!              1) From 'ADD TO FRONT' if the segment table pool is full.
!              2) From 'ADD TO END' if the segment table pool is full.
!
!           Try to make room by deleting a segment list.  CHEAP will
!           be the index of the "cheapest" segment list in the body
!           table.
!
            minexp = intmax()
            cheap  = 0

            do i = 1, nbt

               if ( i .ne. bindex ) then
!
!                 This list is for a body other than the current
!                 one.
!
                  if (      ( btexp(i) .lt. minexp   ) &
                       .or. ( cheap    .eq. 0        )  )then
!
!                    This list is the cheapest seen so far,
!                    possibly because it's the first one
!                    considered.  At the moment, it's as good
!                    a candidate for removal as any.
!
                     cheap  = i
                     minexp = btexp(i)

                  end if

               end if

            end do


           if ( cheap .eq. 0 ) then
!
!              What we do if there are no delete-able segments
!              depends on the task that was suspended before entering
!              'MAKE ROOM'.
!
               if ( stack( top ) .eq. 'ADD TO END' ) then
!
!                 There's nothing left to do but search the remaining
!                 files and segments without buffering them.
!
                  status = 'SEARCH W/O BUFF'


               else
!
!                 STACK(TOP) is set to 'ADD TO FRONT'.
!
!                 If there is no room left in the table in the middle
!                 of an attempt to add to the front of the list, just
!                 start from scratch by treating all files as
!                 unsearched and doing an OLD FILES search, as would
!                 be done for a new body.
!
!                 Return the current list to the segment table pool.
!
!                 Note that, according to the specification of the
!                 SPICELIB doubly linked list routines, the backward
!                 pointer of a list head is the negative of the tail
!                 node.
!
                  p    =   btbeg ( bindex )
                  tail = - lnkprv( p, stpool )

                  call lnkfsl ( p, tail, stpool )

!
!                 Re-initialize the table for this body, and initiate
!                 an 'OLD FILES' search, just as in 'NEW BODY'.
!                 Also, reset the suspended task stack to be empty.
!
                  btbeg( bindex ) = 0
                  btexp( bindex ) = 0
                  bthfs( bindex ) = ftnum( nft )
                  btlfs( bindex ) = ftnum( nft ) + 1
                  status          = 'OLD FILES'
                  top             = 0

               end if

            else
!
!              Return this cheapest list to the segment pool.
!
               p = btbeg  ( cheap )

               if ( p .gt. 0 ) then

                  tail = - lnkprv ( p, stpool )
                  call lnkfsl ( p, tail, stpool )

               end if

!
!              Fill the deleted body's space in the table with
!              the final entry in the table.
!
               if ( cheap .ne. nbt ) then

                  btbod ( cheap ) = btbod ( nbt )
                  btexp ( cheap ) = btexp ( nbt )
                  bthfs ( cheap ) = bthfs ( nbt )
                  btlfs ( cheap ) = btlfs ( nbt )
                  btbeg ( cheap ) = btbeg ( nbt )
                  btlb  ( cheap ) = btlb  ( nbt )
                  btub  ( cheap ) = btub  ( nbt )
                  btprvh( cheap ) = btprvh( nbt )
                  btprvi( cheap ) = btprvi( nbt )
                  btruex( cheap ) = btruex( nbt )
                  btchkp( cheap ) = btchkp( nbt )


                  call moved ( btprvd(1,nbt), dscsiz, btprvd(1,cheap) )

               end if

!
!              If the final entry in the table happened to be the
!              current body of interest, then we also have to change
!              the current body index.
!
               if ( bindex .eq. nbt ) then
                  bindex = cheap
               end if

!
!              One less body now.
!
               nbt    = nbt - 1
               status = 'RESUME'

            end if
!
!           Either we made room by freeing a non-empty segment list,
!           or we're going to work without additional space.  In the
!           latter case, the state is now 'OLD FILES' or
!           'SEARCH W/O BUFF'.
!


         else if ( status .eq. 'ADD TO FRONT' ) then
!
!           The current segment information should be linked in at
!           the head of the segment list for the current body, and
!           the pertinent body table entry should point to the new
!           head of the list.
!
!           The only way to get here is from the block NEW SEGMENTS
!           after suspending that task.
!
            if ( lnknfn(stpool) .eq. 0 ) then
!
!              There's no room left in the segment pool.  We must make
!              room before continuing.
!
               doing  = 'ADD TO FRONT'
               urgent = 'MAKE ROOM'
               status = 'SUSPEND'

            else
!
!              Allocate a node and link it to the front of the list
!              for the current body.
!
               call lnkan ( stpool, new )

               sthan( new ) = fthan( findex )
               call moved ( descr, dscsiz, stdes(1,new) )
               call dafgn ( stidnt(new) )

               if ( failed() ) then
                  call chkout ( 'PCKSFS' )
                  return
               end if

!
!              If the current list is empty, this append operation
!              is a no-op.
!
               call lnkilb ( new, btbeg(bindex), stpool )
               btbeg( bindex ) = new

               status  = 'RESUME'

            end if



         else if ( status .eq. 'ADD TO END' ) then
!
!           The current segment information should be linked in at
!           the tail of the segment list for the current body.
!
!           The only way to get to this task is from the OLD SEGMENTS
!           block after suspending that task.
!
            if ( lnknfn(stpool) .eq. 0 ) then
!
!              There's no room left in the segment pool.  We must make
!              room before continuing.
!
               doing  = 'ADD TO END'
               urgent = 'MAKE ROOM'
               status = 'SUSPEND'

            else
!
!              Allocate a new node in the segment table pool.
!
               call lnkan ( stpool, new )

               sthan( new ) = fthan( findex )
               call moved ( descr, dscsiz, stdes( 1,new ) )
               call dafgn ( stidnt( new ) )

               if ( failed() ) then
                  call chkout ( 'PCKSFS' )
                  return
               end if

               if ( btbeg(bindex) .le. 0 ) then
!
!                 This is the first node in the list for this body.
!
                  btbeg( bindex ) = new

               else
!
!                 Link the new node to the tail of the list.
!
                  tail = - lnkprv ( btbeg(bindex), stpool )
                  call lnkila ( tail, new, stpool )

               end if

               status = 'RESUME'

            end if



         else if ( status .eq. 'SEARCH W/O BUFF' ) then
!
!           When the segment table is completely full, continue
!           the search by looking through the unchecked portion
!           of the segment list for the current body, and
!           then searching old, unchecked files without buffering
!           their segments.
!
!           The only way to get here is from the MAKE ROOM state
!           via the block ADD TO END.  If you get here there is no
!           free space in the segment table pool.
!
!           At this point, we need to initialize the cost of
!           the re-use interval.
!
            btruex(bindex) = 0

!
!           Need to find the portion of the current body's segment
!           list which comes from the current file of interest.  It
!           will be returned to the segment table pool, since the
!           remainder of the file's segments can't be added to the list.
!
            crflbg = btbeg( bindex )
            fndhan = .false.

            do while (  ( .not. fndhan ) .and. ( crflbg .gt. 0 )  )

               fndhan = sthan( crflbg ) .eq. fthan( findex )

               if ( .not. fndhan ) then
!
!                 Get the next node.  We avoid LNKNXT here in order
!                 to speed up the operation.
!
                  crflbg = stpool ( forwrd, crflbg )

               end if

            end do

            if ( crflbg .gt. 0 ) then
!
!              The sub-list from the current node onwards is to be
!              returned to the segment table pool.  Save this node,
!              since we'll finish searching the list before freeing
!              the sub-list.
!
               p = crflbg

!
!              It may be that the sub-list we're deleting is the
!              entire segment list for this body.  If so, the
!              corresponding body table entry should be set to
!              a non-positive value to indicate an empty segment list.
!
               if ( p .eq. btbeg(bindex) ) then

                  btbeg(bindex) = 0
!
!                 Also in this case, we must initialize the re-use
!                 interval for this body.
!
                  btlb(bindex) = dpmin()
                  btub(bindex) = dpmax()

               end if

!
!              Finish searching through the incomplete list for the
!              desired segment.
!
               do while ( crflbg .gt. 0 )
!
!                 Every segment seen from the current file contributes
!                 to the expense of the re-use interval.
!
                  btruex(bindex) = btruex(bindex) + 1


                  if ( et .gt. stdes(2,crflbg) ) then
!
!                    ET is to the right of the coverage interval of this
!                    segment.
!
                     btlb(bindex) = max( btlb(bindex), stdes(2,crflbg) )


                  else if ( et .lt. stdes(1,crflbg) ) then
!
!                    ET is to the left of the coverage interval of this
!                    segment.
!
                     btub(bindex) = min( btub(bindex), stdes(1,crflbg) )

                  else
!
!                    The segment coverage interval includes ET.
!
                     call moved ( stdes( 1,crflbg ), dscsiz, descr )

                     ident  = stidnt( crflbg )
                     handle = sthan ( crflbg )
                     found  = .true.

!
!                    Set the re-use interval for the current body.
!
                     btlb(bindex) = max( btlb(bindex), stdes(1,crflbg) )
                     btub(bindex) = min( btub(bindex), stdes(2,crflbg) )

!
!                    Save the output items, in case this
!                    segment may be satisfy the next request.
!
                     btprvh(bindex) =  handle
                     btprvi(bindex) =  ident
                     call moved ( descr, dscsiz, btprvd(1,bindex) )
                     btchkp(bindex) =  .true.

!
!                    Update the expense of the list to reflect
!                    the cost of locating this segment.
!
                     btexp(bindex) = btexp(bindex) + btruex(bindex)

!
!                    Free the sub-list we were searching.
!
                     tail = lnktl ( crflbg, stpool )
                     call lnkfsl ( p, tail, stpool )

                     call chkout ( 'PCKSFS' )
                     return

                  end if

!                 Get the next node.  We avoid LNKNXT here in order
!                 to speed up the operation.
!
                  crflbg = stpool ( forwrd, crflbg )

               end do

!
!              Return the sub-list to the segment table pool.
!              CRFLBG at this point is the negative of the list head.
!              The list tail is (by the spec of the SPICELIB doubly
!              linked list routines) the negative of the predecessor
!              of the head.
!
!              Note the list is always non-empty.
!
               tail = -lnkprv( -crflbg, stpool )

               call lnkfsl ( p, tail, stpool )

            end if

!
!           Search through the remaining files without buffering.
!           Recall that a search is already in progress and that a
!           segment is currently under consideration (FND = .TRUE.).
!
            do while ( findex .gt. 0 )

               do while ( fnd )
!
!                 Each segment found contributes to the expense of the
!                 re-use interval.
!
                  btruex(bindex) = btruex(bindex) + 1

                  call dafgs ( descr )
                  call dafus ( descr, nd, ni, dcd, icd )

                  if ( failed() ) then
                     call chkout ( 'PCKSFS' )
                     return
                  end if

                  if ( body .eq. icd(1) ) then
!
!                    This is a segment for the body of interest.
!                    Update the re-use interval for this body.
!
                     if ( et .gt. dcd(2) ) then
!
!                       ET is to the right of the coverage interval
!                       of this segment.
!
                        btlb(bindex) = max( btlb(bindex), dcd(2) )


                     else if ( et .lt. dcd(1) ) then
!
!                       ET is to the left of the coverage interval
!                       of this segment.
!
                        btub(bindex) = min( btub(bindex), dcd(1) )

                     else
!
!                       The segment coverage interval includes ET.
!
                        call dafgn ( ident )

                        if ( failed() ) then
                           call chkout ( 'PCKSFS' )
                           return
                        end if

                        handle = fthan( findex )
                        found  = .true.

!
!                       Set the re-use interval for the current body.
!
                        btlb(bindex) = max( btlb(bindex), dcd(1) )
                        btub(bindex) = min( btub(bindex), dcd(2) )

!
!                       Save the output items, in case this
!                       segment may satisfy the next request.
!
                        btprvh(bindex) =  handle
                        btprvi(bindex) =  ident
                        call moved ( descr, dscsiz, btprvd(1,bindex) )
                        btchkp(bindex) =  .true.

!
!                       Update the expense of the list to reflect
!                       the cost of locating this segment.
!
                        btexp(bindex) = btexp(bindex) + btruex(bindex)

                        call chkout ( 'PCKSFS' )
                        return

                     end if

                  end if

                  call daffpa ( fnd )

                  if ( failed() ) then
                     call chkout ( 'PCKSFS' )
                     return
                  end if

               end do

!
!              Try the next oldest file.
!
               findex = findex -1

               if ( findex .gt. 0 ) then

                  call dafbbs ( fthan( findex ) )
                  call daffpa ( fnd             )

                  if ( failed() ) then
                     call chkout ( 'PCKSFS' )
                     return
                  end if

               end if

            end do

!
!           If you get to here, sorry.
!
            btruex(bindex) = 0
            status         = 'HOPELESS'

!
!        When a task is suspended, the current activity is placed on
!        a stack, to be restored later. Two levels are provided, since
!        some interrupts can be interrupted by others.
!
         else if ( status .eq. 'SUSPEND' ) then

            top          = top + 1
            stack( top ) = doing
            status       = urgent

         else if ( status .eq. 'RESUME' ) then
!
!           Pop the status stack.
!
            status = stack( top )
            top    = top - 1

         end if

      end do

!
!     If we didn't find a segment, don't attempt to use saved
!     outputs from a previous call.  BINDEX will always be set
!     at this point.  Also, zero out the expense of the re-use
!     interval.
!
      if ( bindex .gt. 0 ) then

         btchkp(bindex) = .false.
         btruex(bindex) =  0

      end if


      call chkout ( 'PCKSFS' )
      return
      end
