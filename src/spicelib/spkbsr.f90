!$Procedure SPKBSR ( S/P Kernel, Buffer segments for readers )

      subroutine spkbsr ( fname, &
                          handle, &
                          body, &
                          et, &
                          descr, &
                          ident, &
                          found   )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Load and unload files for use by the readers. Buffer segments
!     for readers.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
!
! $ Declarations

      implicit none

      character(*)          fname
      integer(ip)           handle
      integer(ip)           body
      real(wp)              et
      real(wp)              descr    ( * )
      character(*)          ident
      logical               found

      integer(ip)           ftsize
      parameter           ( ftsize =  5000 )

      integer(ip)           btsize
      parameter           ( btsize =  10000 )

      integer(ip)           lbpool
      parameter           ( lbpool =    -5 )

      integer(ip)           stsize
      parameter           ( stsize =  100000 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     FNAME      I   SPKLEF
!     HANDLE    I-O  SPKLEF, SPKUEF, SPKSFS
!     BODY       I   SPKSFS
!     ET         I   SPKSFS
!     DESCR      O   SPKSFS
!     IDENT      O   SPKSFS
!
! $ Detailed_Input
!
!     FNAME    is the name of an SPK file to be loaded.
!
!     HANDLE   on input is the handle of an SPK file to be
!              unloaded.
!
!     BODY     is the NAIF integer code of an ephemeris object,
!              typically a solar system body.
!
!     ET       is a time, in seconds past the epoch J2000 TDB.
!
! $ Detailed_Output
!
!     HANDLE   on output is the handle of the S/P-kernel file
!              containing a located segment.
!
!     DESCR    is the descriptor of a located segment.
!
!     IDENT    is the identifier of a located segment.
!
!     FOUND    indicates whether a requested segment was found or not.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of ephemeris files that can be
!              loaded by SPKLEF at any given time for use by the
!              readers.
!
!     BTSIZE   is the maximum number of bodies whose segments can be
!              buffered by SPKSFS.
!
!     STSIZE   is the maximum number of segments that can be buffered at
!              any given time by SPKSFS.
!
! $ Exceptions
!
!     1)  If SPKBSR is called directly, the error SPICE(BOGUSENTRY)
!         is signaled.
!
!     2)  See entry points SPKLEF, SPKUEF, and SPKSFS for exceptions
!         specific to them.
!
! $ Files
!
!     S/P-kernel ephemeris files are indicated by filename before
!     loading (see SPKLEF) and handle after loading (all other places).
!
! $ Particulars
!
!     SPKBSR serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        SPKLEF       Load ephemeris file.
!        SPKUEF       Unload ephemeris file.
!        SPKSFS       Select file and segment.
!
!     Before a file can be read by the S/P-kernel readers, it must be
!     loaded by SPKLEF, which among other things, loads the file into
!     the DAF system.
!
!     Up to FTSIZE files may be loaded for use simultaneously, and a
!     file only has to be loaded once to become a potential search
!     target for any number of subsequent reads.
!
!     Once an SPK file has been loaded, it is assigned a file
!     handle, which is used to keep track of the file internally,
!     and which is used by the calling program to refer to the file
!     in all subsequent calls to SPK routines.
!
!     A file may be removed from the list of files for potential
!     searching by unloading it via a call to SPKUEF.
!
!     SPKSFS performs the search for segments within a file for the
!     S/P-kernel readers. It searches through last-loaded files first.
!     Within a single file, it searches through last-inserted segments
!     first, thus assuming that "newest data is best".
!
!     Information on loaded files is used by SPKSFS to manage a buffer
!     of saved segment descriptors and identifiers to speed up access
!     time without having to necessarily perform file reads.
!
! $ Examples
!
!     Suppose that ephemeris data for the Mars Global Surveyor
!     spacecraft relative to Mars are contained in three separate files:
!     PREDICT.SPK contains complete predict ephemeris data for several
!     successive orbits, and UPDATE_1.SPK and UPDATE_2.SPK contain two
!     separate updates to selected intervals within those orbits, based
!     on altimeter fits.
!
!     In the following example, states of the spacecraft are computed
!     in two different ways:
!
!     First, the predict file and one of the update files are both
!     loaded and states are requested for regular intervals within
!     the orbits. The update file is searched through first, and if no
!     data for the requested time is available, the predict file is
!     used.
!
!     Then, the first update file is unloaded, the second update file
!     is loaded, and the same requests are made as above.
!
!     Throughout the two searches, a table is written which contains
!     the state (position and velocity) of the spacecraft, and the
!     file from which the data came, if such data was found, and an
!     error message otherwise.
!
!     It is assumed that the beginning and ending ephemeris times
!     (BEG_ET, END_ET) for the entire span have already been
!     initialized, along with the step-size for each measurement
!     (DELTA). The two routines WRITE_TABLE and WRITE_ERROR do not
!     exist in SPICELIB.
!
!
!           INTEGER               PRED_HNDL
!           INTEGER               UPD1_HNDL
!           INTEGER               UPD2_HNDL
!           INTEGER               HANDLE
!           INTEGER               BODY
!           INTEGER               CENTER
!
!           DOUBLE PRECISION      BEG_ET
!           DOUBLE PRECISION      END_ET
!           DOUBLE PRECISION      DELTA
!           DOUBLE PRECISION      ET
!           DOUBLE PRECISION      DESCR ( 5 )
!           DOUBLE PRECISION      STATE ( 6 )
!
!           CHARACTER*40          IDENT
!           CHARACTER*25          FNAME
!
!           LOGICAL               FOUND
!
!     C
!     C     Load the predict file and the first update file. Since
!     C     last-loaded files get searched first, we want to load the
!     C     update file second.
!     C
!           CALL SPKLEF ( 'PREDICT.SPK',  PRED_HNDL )
!           CALL SPKLEF ( 'UPDATE_1.SPK', UPD1_HNDL )
!
!     C
!     C     NAIF code for the Mars Global Surveyor spacecraft is -94.
!     C
!           BODY = -94
!
!     C
!     C     Compute states for regular intervals between BEG_ET and
!     C     END_ET.
!     C
!           ET = BEG_ET
!
!           DO WHILE ( ET .LE. END_ET )
!
!     C
!     C        Locate the applicable segment (handle and descriptor).
!     C
!              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!              IF ( FOUND ) THEN
!     C
!     C           Evaluate the state, get the name of the file from
!     C           whence the data came, and write the results to the
!     C           table.
!     C
!                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE,
!             .                CENTER )
!
!                 CALL DAFHFN ( HANDLE, FNAME )
!
!                 CALL WRITE_TABLE ( ET, STATE, FNAME )
!
!              ELSE
!
!                 CALL WRITE_ERROR ( ET )
!
!              END IF
!
!     C
!     C        The next time.
!     C
!              ET = ET + DELTA
!
!           END DO
!
!     C
!     C     Unload the first update file, load the second, and do
!     C     everything over again. Since the original file stays
!     C     loaded, the update file once again gets searched first.
!     C
!           CALL SPKUEF (  UPD1_HNDL )
!           CALL SPKLEF ( 'UPDATE_2.SPK', UPD2_HNDL )
!
!           ET = BEG_ET
!
!           DO WHILE ( ET .LE. END_ET )
!
!     C
!     C        Locate the applicable segment.
!     C
!              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!              IF ( FOUND ) THEN
!     C
!     C           Evaluate the state, get the name of the file from
!     C           whence the data came, and write the results to the
!     C           table.
!     C
!                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE,
!             .                CENTER )
!
!                 CALL DAFHFN ( HANDLE, FNAME )
!
!                 CALL WRITE_TABLE ( ET, STATE, FNAME )
!
!              ELSE
!
!                 CALL WRITE_ERROR ( ET )
!
!              END IF
!
!     C
!     C        The next time.
!     C
!              ET = ET + DELTA
!
!           END DO
!
! $ Restrictions
!
!     1)  If Fortran I/O errors occur while searching a loaded SPK
!         file, the internal state of this suite of routines may
!         be corrupted. It may be possible to correct the state
!         by unloading the pertinent SPK files and then re-loading
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
!     J.M. Lynch         (JPL)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.1.0, 13-OCT-2021 (JDR) (BVS) (NJB)
!
!        Increased BTSIZE (from 200 to 10000).
!
!        Updated entry point SPKSFS to always initialize FOUND.
!
!        Edited the header of SPKBSR umbrella routine and its entry
!        points SPKLEF, SPKUEF and SPKSFS.
!
!        Changed SAVE statements to save each variable individually.
!
!-    SPICELIB Version 6.0.1, 15-MAR-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 6.0.0, 17-MAR-2014 (NJB)
!
!        Updated segment pool initialization condition in entry
!        point SPKLEF so that the pool is initialized only if the file
!        table is empty.
!
!-    SPICELIB Version 5.4.0, 13-JUN-2013 (BVS)
!
!        Increased FTSIZE (from 1000 to 5000).
!
!        Increased STSIZE (from 50000 to 100000).
!
!-    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!          is 'ADD TO FRONT' and no segment table room is available,
!          the body table's pointer to the current segment list
!          is now set to null. Previously the pointer was allowed to go
!          stale.
!
!-    SPICELIB Version 5.2.0, 07-APR-2010 (NJB)
!
!        Increased segment table buffer size to 50000 entries.
!
!-    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED calls in entry points SPKUEF and SPKSFS.
!
!        Increased segment table buffer size to 30000 entries.
!
!-    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB)
!
!        Increased segment table buffer size to 10000 entries.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single SPK file, and the list is
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
!           4) The load routine SPKLEF now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby
!              avoiding arithmetic overflow.
!
!           5) The unload routine SPKUEF now calls RETURN() on entry and
!              returns if so directed.
!
!           6) In SPKSFS, DAF calls are followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           7) In SPKSFS, a subscript bound violation in a loop
!              termination test was corrected.
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
!-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
!
!        An interim fix to a bug in SPKBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        1) When loading a file, SPKLEF now checks if the file table is
!           full only after determining that the file is not currently
!           loaded. Previously, if the file table was full and an
!           attempt was made to reload a file, an error was signaled. A
!           new exception was added as a result of this change.
!
!        2) A bug in the way that SPKLEF and SPKUEF clean up the body
!           tables after a file is unloaded was fixed.
!
!        3) Variable declarations were added to the example program
!           so that it can now be compiled.
!
!        4) A cut and paste error in the description of the segment
!           table was corrected.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 09-SEP-1991 (HAN)
!
!        The declaration of the variable STATE in the $Examples section
!        was changed from a 3 dimensional vector to a 6 dimensional
!        vector, and the term state was specified to be the position
!        and velocity of a body relative to another body.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     buffer SPK segments for readers
!
!-&


! $ Revisions
!
!-    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!          is 'ADD TO FRONT' and no segment table room is available,
!          the body table's pointer to the current segment list
!          is now set to null. Previously the pointer was allowed to go
!          stale.
!
!-    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED calls in entry points SPKUEF and SPKSFS.
!
!        Increased segment table buffer size to 30000 entries.
!
!-    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB)
!
!        Increased segment table buffer size to 10000 entries.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single SPK file, and the list is
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
!           4) The load routine SPKLEF now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby
!              avoiding arithmetic overflow.
!
!           5) The unload routine SPKUEF now calls RETURN() on entry and
!              returns if so directed.
!
!           6) In SPKSFS, DAF calls are followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           7) In SPKSFS, a subscript bound violation in a loop
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
      parameter           ( ni     = 6 )


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
!     The file table contains the handle and file number of each file
!     that has been loaded for use with the SPK readers. File
!     numbers begin at one, and are incremented until they reach a
!     value of INTMAX() - 1, at which point they are mapped to the
!     range 1:NFT, where NFT is the number of loaded SPK files.
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
      character(sidlen)     btprvi   ( btsize )

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
!     entry point SPKSFS.
!
      integer(ip)           stpool ( 2, lbpool : stsize )
      integer(ip)           sthan  (             stsize )
      real(wp)              stdes  ( dscsiz,     stsize )
      character(sidlen)     stidnt (             stsize )

!
!     Other stuff
!
      character(slen)       doing
      character(slen)       stack    ( 2 )
      character(slen)       status
      character(slen)       urgent

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
!     Nobody has any business calling SPKBSR directly.
!
      if ( return() ) return

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      return



!$Procedure SPKLEF ( S/P Kernel, Load ephemeris file )

      entry spklef ( fname, handle )

! $ Abstract
!
!     Load an ephemeris file for use by the readers. Return that file's
!     handle, to be used by other SPK routines to refer to the file.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
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
!     FTSIZE     P   Maximum number of loaded SPK files.
!
! $ Detailed_Input
!
!     FNAME    is a string containing the name of the file to be loaded.
!
! $ Detailed_Output
!
!     HANDLE   is an integer handle assigned to the file upon loading.
!              Almost every other SPK routine will subsequently use this
!              number to refer to the file.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of SPK files that may
!              be loaded simultaneously under any circumstances.
!              FTSIZE is currently set to match the maximum number
!              of DAF files that may be loaded simultaneously.
!
! $ Exceptions
!
!     1)  If an attempt is made to open more DAF files than is specified
!         by the parameter FTSIZE in DAFAH, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If an attempt is made to load more files than is specified by
!         the local parameter FTSIZE, and if the DAF system has room to
!         load another file, the error SPICE(SPKFILETABLEFULL) is
!         signaled. The current setting of FTSIZE does not allow this
!         situation to arise: the DAF system will trap the error before
!         this routine has the chance.
!
! $ Files
!
!     A file specified by FNAME, to be loaded. The file is assigned a
!     handle by SPKLEF, which will be used by most other routines to
!     refer to it.
!
! $ Particulars
!
!     If there is room for a new file in the file table, SPKLEF creates
!     an entry for it and loads the file for reading using DAFOPR.
!
! $ Examples
!
!     See $Examples in SPKBSR.
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
!     J.M. Lynch         (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.2, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 5.0.1, 15-MAR-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
!
!        Updated segment pool initialization condition in entry
!        point SPKLEF so that the pool is initialized only if the file
!        table is empty.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!        1) When an already loaded kernel is opened with DAFOPR,
!           it now has its link count reset to 1 via a call to
!           DAFCLS.
!
!        2) This routine now resets all file numbers when
!           the next file number reaches INTMAX()-1, thereby avoiding
!           arithmetic overflow. The numbers in the file table
!           are replaced with consecutive integers in the range
!           1 : NFT, such that the ordering of the numbers is not
!           changed. The HFS and LFS arrays are updated accordingly.
!
!        Also, the flags indicating validity of the re-use intervals
!        are set to .FALSE. here.
!
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        When loading a file, SPKLEF now checks if the file table is
!        full only after determining that the file is not currently
!        loaded. Previously, if the file table was full and an attempt
!        was made to reload a file, an error was signaled. A new
!        exception was added as a result of this change.
!
!        A bug in the way that SPKLEF and SPKUEF clean up the body
!        tables after a file is unloaded was fixed.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     load SPK ephemeris file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!        1) When a loaded kernel is opened with DAFOPR,
!           it now has its link count reset to 1 via a call to
!           DAFCLS.
!
!        2) This routine now resets all file numbers when
!           the next file number reaches INTMAX()-1, thereby avoiding
!           arithmetic overflow. The numbers in the file table
!           are replaced with consecutive integers in the range
!           1 : NFT, such that the ordering of the numbers is not
!           changed. The HFS and LFS arrays are updated accordingly.
!           HFS and LFS entries that have gone stale are set to zero.
!
!        Also, the flags indicating validity of the re-use intervals
!        are set to .FALSE. here.
!
!
!-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
!
!        An interim fix to a bug in SPKBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        When loading a file, SPKLEF now checks if the file table is
!        full only after determining that the file is not currently
!        loaded. Previously, if the file table was full and an attempt
!        was made to reload a file, an error was signaled. A new
!        exception was added as a result of this change.
!
!        A bug in the way that SPKLEF and SPKUEF clean up the body
!        tables after a file is unloaded was fixed.
!
!        If as the result of loading a file that was previously loaded,
!        there are no more segments buffered for a particular body,
!        the counter variable for the bodies is no longer incremented.
!
!        The following code fragment changed:
!
!           IF ( BTBEG( I ) .EQ. 0 ) THEN
!
!              .
!              .
!              .
!              NBT = NBT - 1
!
!           END IF
!
!           I = I + 1
!
!        This is the fix:
!
!           IF ( BTBEG( I ) .EQ. 0 ) THEN
!
!              .
!              .
!              .
!              NBT = NBT - 1
!
!           ELSE
!
!              I = I + 1
!
!           END IF
!
!-    Beta Version 1.1.0, 25-JAN-1990 (IMU)
!
!        If a file that has already been loaded is loaded a second
!        (or third or fourth) time, it should be removed from the
!        file table, and any segments from the file must be removed
!        from the segment lists, just as if the user had unloaded
!        the file before loading it again. This means that a single
!        file cannot occur more than once in the file table.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

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
      if ( nft == 0 ) then
         call lnkini ( stsize, stpool )
      end if

!
!     To load a new file, first try to open it for reading.
!
      call dafopr ( fname, handle )

      if ( failed() ) return

!
!     Determine if the file is already in the table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex > 0 ) then
!
!        The last call we made to DAFOPR added another DAF link to
!        the SPK file.  Remove this link.
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

         do while ( i <= nbt )

            p = btbeg( i )

            do while ( p > 0 )
!
!              Find the successor of P, if any.
!
               nxtseg = lnknxt ( p, stpool )

               if ( sthan( p ) == handle ) then
!
!                 The segment corresponding to node P came from
!                 the file we're unloading.  Delete the node for
!                 P from the segment list for body I; if P happens
!                 to be the head node for body I's segment list,
!                 make the successor of P the head of the list.
!
                  call lnkfsl ( p, p, stpool )

                  if ( p == btbeg(i) ) then
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
            if ( btbeg( i ) <= 0 ) then
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
         if ( nft == ftsize ) then
!
!           This error case can occur only if FTSIZE is larger than
!           the maximum number of open DAF files.  Currently FTSIZE
!           is equal to this limit.
!
            call dafcls ( handle )

            call setmsg ( 'The internal file table is already full, ' // &
                          'with # entries.'         )
            call errint ( '#', ftsize               )
            call sigerr ( 'SPICE(SPKFILETABLEFULL)' )
            return

         end if

      end if

!
!     Determine the next file number.  Note that later code assumes
!     that the file number can be incremented by 1, so we can't allow
!     the file number to reach INTMAX().
!
      if ( next < intmax()-1 ) then

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

            if ( j > 0 ) then
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

            if ( j > 0 ) then
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

      return



!$Procedure SPKUEF ( SPK Kernel, Unload ephemeris file )

      entry spkuef ( handle )

! $ Abstract
!
!     Unload an ephemeris file so that it will no longer be searched by
!     the readers.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
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
!     HANDLE   is the integer handle assigned to the file upon loading.
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
!     A file is removed from consideration by the readers by a call to
!     SPKUEF.
!
!     The file table entry corresponding to the file referenced by
!     HANDLE, is removed. Any segment table entry which came from the
!     specified file is also deleted.
!
!     If the file specified by HANDLE is not currently loaded in the
!     SPK system, no action is taken.
!
! $ Examples
!
!     See $Examples in SPKBSR.
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
!     J.M. Lynch         (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.1.2, 17-JUN-2021 (JDR)
!
!        Updated the header to comply with NAIF standard.
!
!        Removed the reference to SPK required reading from
!        $Literature_References section. Extended $Particulars
!        section.
!
!-    SPICELIB Version 4.1.1, 15-MAR-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!        1) This routine now calls RETURN() on entry and
!           returns if so directed.
!
!        Also, the flags indicating validity of those re-use intervals
!        whose data comes from the unloaded file are set to .FALSE.
!
!
!-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
!
!        An interim fix to a bug in SPKBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        A bug in the way that SPKLEF and SPKUEF clean up the body
!        tables after a file is unloaded was fixed.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 02-MAY-1990 (RET)
!
!        If unloading a file causes all segments in the list for a
!        body to go away, delete that body from the body list.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     unload SPK ephemeris file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!        1) This routine now calls RETURN() on entry and
!           returns if so directed.
!
!        Also, the flags indicating validity of those re-use intervals
!        whose data comes from the unloaded file are set to .FALSE.
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        A bug in the way that SPKLEF and SPKUEF clean up the body
!        tables after a file is unloaded was fixed.
!
!        If as the result of unloading a file there are no more
!        segments buffered for a particular body, the counter variable
!        for the bodies is no longer incremented.
!
!        The following code fragment changed:
!
!           IF ( BTBEG( I ) .EQ. 0 ) THEN
!
!              .
!              .
!              .
!              NBT = NBT - 1
!
!           END IF
!
!           I = I + 1
!
!        This is the fix:
!
!           IF ( BTBEG( I ) .EQ. 0 ) THEN
!
!              .
!              .
!              .
!              NBT = NBT - 1
!
!           ELSE
!
!              I = I + 1
!
!           END IF
!
!-    SPICELIB Version 1.1.0, 2-MAY-1990 (RET)
!
!        If unloading a file causes all segments in the list for a
!        body to go away, delete that body from the body list.
!
!-    Beta Version 1.1.0, 25-JAN-1990 (IMU)
!
!        When unloading a file, close it.
!
!-&


      if ( return() ) return


!
!     All of the stored segments from the file must be removed
!     from the segment table (by returning the corresponding nodes
!     to the segment table pool.)
!
!     Don't do anything if the given handle is not in the file table.
!
      findex = isrchi ( handle, nft, fthan )

      if ( findex == 0 ) return

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

      do while ( i <= nbt )

         p = btbeg( i )

         do while ( p > 0 )

            nxtseg = lnknxt ( p, stpool )

            if ( sthan(p) == handle ) then

               if ( p == btbeg(i) ) then
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
         if ( btbeg(i) <= 0 ) then

            if ( i /= nbt ) then

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

            if ( btprvh(i) == handle ) then
               btchkp(i) = .false.
            end if

         end if

      end do

      return



!$Procedure SPKSFS ( S/P Kernel, Select file and segment )

      entry spksfs ( body, et, handle, descr, ident, found )

! $ Abstract
!
!     Search through loaded SPK files to find the highest-priority
!     segment applicable to the body and time specified and buffer
!     searched segments in the process, to attempt to avoid re-reading
!     files.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
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
!     HANDLE   is the handle of the SPK file containing a located
!              segment.
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
!     1)  If an attempt is made to call SPKSFS when there aren't any
!         files loaded, the error SPICE(NOLOADEDFILES) is signaled.
!
!     2)  If an error occurs while this routine attempts to extract
!         segment descriptors from loaded SPK files, the error is
!         signaled by a routine in the call tree of this routine.
!
!         Note however that I/O errors occurring during reads of DAF
!         double precision records are NOT treated as SPICE errors
!         and are not signaled.
!
! $ Files
!
!     All SPK files loaded by FURNSH or SPKLEF are potential search
!     targets for SPKSFS.
!
! $ Particulars
!
!     This routine finds the highest-priority segment, in any loaded
!     SPK file, such that the segment provides data for the specified
!     body and epoch.
!
! $ Examples
!
!     See $Examples in SPKBSR.
!
! $ Restrictions
!
!     1)  If Fortran I/O errors occur while searching a loaded SPK
!         file, the internal state of this suite of routines may
!         be corrupted. It may be possible to correct the state
!         by unloading the pertinent SPK files and then re-loading
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.3.0, 13-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard. Improved
!        $Abstract description.
!
!        Relocated initialization of FOUND so it is always
!        executed, even if an error state is indicated by RETURN().
!
!        Added entry #2 to $Exceptions section. Added reference to
!        FURNSH routine in $Files section.
!
!-    SPICELIB Version 4.2.1, 15-MAR-2017 (NJB)
!
!        Corrected various spelling errors within comments.
!
!-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!          is 'ADD TO FRONT' and no segment table room is available,
!          the body table's pointer to the current segment list
!          is now set to null. Previously the pointer was allowed to go
!          stale.
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single SPK file, and the list is
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
!              was corrected.
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
!
!-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
!
!        An interim fix to a bug in SPKBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 02-MAY-1990 (RET)
!
!        New error detected.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     select SPK file and segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
!
!        Bug fix:
!
!          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!          is 'ADD TO FRONT' and no segment table room is available,
!          the body table's pointer to the current segment list
!          is now set to null. Previously the pointer was allowed to go
!          stale.
!
!-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED call.
!
!-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!
!        Bug fixes:
!
!           1) When a segment list is freed because the entire list
!              is contributed by a single SPK file, and the list is
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
!-    SPICELIB Version 1.1.0, 2-MAY-1990 (RET)
!
!        If an attempt is made to call SPKSFS when there are no files
!        loaded, an error is now signaled.
!
!-&


!
!     Assume the segment is not found, until it actually is.
!
      found = .false.

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Buffering segments involves maintaining three tables:  the
!     file table, the body table, and the segment table.  The routine
!     is broken down into various tasks, described below, which
!     perform these manipulations.  A description of the components
!     of each table is provided in the declarations section of SPKBSR.

!
!     There must be at least ONE file loaded.
!
      if ( nft == 0 ) then
         call setmsg ( 'At least one SPK file needs to be loaded '   // &
                       'by SPKLEF before beginning a search.'       )
         call sigerr ( 'SPICE(NOLOADEDFILES)'                       )
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

      if ( bindex == 0 ) then

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
            if (       ( et > btlb(bindex) ) &
                 .and. ( et < btub(bindex) )  ) then
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
         if ( btbeg(bindex) == 0 ) then

            btexp(bindex) = 0

         end if


         status = '?'

      end if


      do while ( status /= 'HOPELESS' )
!
!        If new files have been added, they have to be searched.
!        Otherwise, we can go right to the list of stored segments.
!
         if ( status == '?' ) then
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

            if ( bthfs( bindex ) < ftnum( nft ) ) then
               status = 'NEW FILES'
            else
               status = 'CHECK LIST'
            end if



         else if ( status == 'NEW BODY' ) then
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

            if ( nbt < btsize ) then
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

                  if ( btexp(i) < minexp ) then
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

               if ( head > 0 ) then

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
!           Now search the loaded SPK files for segments relating to
!           this body.  We start with the last-loaded files and
!           work backwards.
!
            status = 'OLD FILES'



         else if ( status == 'NEW FILES' ) then
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

            do while ( bthfs( bindex ) >= ftnum( findex ) )
               findex = findex + 1
            end do

            bthfs( bindex ) = ftnum( findex )

            call dafbfs ( fthan( findex ) )

            if ( failed() ) return

            status = 'NEW SEGMENTS'

!
!           The cost of the list contributed by the new file is
!           zero so far.
!
            cost = 0


         else if ( status == 'NEW SEGMENTS' ) then
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

            if ( failed() ) return

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

               if ( failed() ) return

               if (       ( icd(1) == body   ) &
                    .and. ( dcd(1) <= dcd(2) )  ) then

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

         else if ( status == 'OLD FILES' ) then
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

            do while ( btlfs( bindex ) <= ftnum( findex ) )
               findex = findex - 1
            end do

            call dafbbs ( fthan( findex ) )

            if ( failed() ) return


            status = 'OLD SEGMENTS'
!
!           The next thing we'll do is search through all the segments
!           of this file for those that applicable to this body.
!           The cost of the list contributed by the current file is
!           zero so far.
!
            cost = 0


         else if ( status == 'OLD SEGMENTS' ) then
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

            if ( failed() ) return

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

               if ( failed() ) return

               if (       ( icd(1) == body   ) &
                    .and. ( dcd(1) <= dcd(2) )  )  then

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

         else if ( status == 'CHECK LIST'   ) then
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

            do while ( p > 0 )

               if ( et > stdes(2,p) ) then
!
!                 ET is to the right of the coverage interval of this
!                 segment.
!
                  btlb(bindex) = max ( btlb(bindex), stdes(2,p) )


               else if ( et < stdes(1,p) ) then
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
            if ( btlfs( bindex ) > ftnum( 1 ) ) then
               status = 'OLD FILES'
            else
               status = 'HOPELESS'
            end if



         else if ( status == 'MAKE ROOM' ) then
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

               if ( i /= bindex ) then
!
!                 This list is for a body other than the current
!                 one.
!
                  if (      ( btexp(i) < minexp   ) &
                       .or. ( cheap    == 0        )  )then
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


           if ( cheap == 0 ) then
!
!              What we do if there are no delete-able segments
!              depends on the task that was suspended before entering
!              'MAKE ROOM'.
!
               if ( stack( top ) == 'ADD TO END' ) then
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

               if ( p > 0 ) then

                  tail = - lnkprv ( p, stpool )
                  call lnkfsl ( p, tail, stpool )

               end if

!
!              Fill the deleted body's space in the table with
!              the final entry in the table.
!
               if ( cheap /= nbt ) then

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
               if ( bindex == nbt ) then
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


         else if ( status == 'ADD TO FRONT' ) then
!
!           The current segment information should be linked in at
!           the head of the segment list for the current body, and
!           the pertinent body table entry should point to the new
!           head of the list.
!
!           The only way to get here is from the block NEW SEGMENTS
!           after suspending that task.
!
            if ( lnknfn(stpool) == 0 ) then
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

               if ( failed() ) return

!
!              If the current list is empty, this append operation
!              is a no-op.
!
               call lnkilb ( new, btbeg(bindex), stpool )
               btbeg( bindex ) = new

               status  = 'RESUME'

            end if



         else if ( status == 'ADD TO END' ) then
!
!           The current segment information should be linked in at
!           the tail of the segment list for the current body.
!
!           The only way to get to this task is from the OLD SEGMENTS
!           block after suspending that task.
!
            if ( lnknfn(stpool) == 0 ) then
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

               if ( failed() ) return

               if ( btbeg(bindex) <= 0 ) then
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



         else if ( status == 'SEARCH W/O BUFF' ) then
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

            do while (  ( .not. fndhan ) .and. ( crflbg > 0 )  )

               fndhan = sthan( crflbg ) == fthan( findex )

               if ( .not. fndhan ) then
!
!                 Get the next node.  We avoid LNKNXT here in order
!                 to speed up the operation.
!
                  crflbg = stpool ( forwrd, crflbg )

               end if

            end do

            if ( crflbg > 0 ) then
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
               if ( p == btbeg(bindex) ) then

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
               do while ( crflbg > 0 )
!
!                 Every segment seen from the current file contributes
!                 to the expense of the re-use interval.
!
                  btruex(bindex) = btruex(bindex) + 1


                  if ( et > stdes(2,crflbg) ) then
!
!                    ET is to the right of the coverage interval of this
!                    segment.
!
                     btlb(bindex) = max( btlb(bindex), stdes(2,crflbg) )


                  else if ( et < stdes(1,crflbg) ) then
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
            do while ( findex > 0 )

               do while ( fnd )
!
!                 Each segment found contributes to the expense of the
!                 re-use interval.
!
                  btruex(bindex) = btruex(bindex) + 1

                  call dafgs ( descr )
                  call dafus ( descr, nd, ni, dcd, icd )

                  if ( failed() ) return

                  if ( body == icd(1) ) then
!
!                    This is a segment for the body of interest.
!                    Update the re-use interval for this body.
!
                     if ( et > dcd(2) ) then
!
!                       ET is to the right of the coverage interval
!                       of this segment.
!
                        btlb(bindex) = max( btlb(bindex), dcd(2) )


                     else if ( et < dcd(1) ) then
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

                        if ( failed() ) return

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

                        return

                     end if

                  end if

                  call daffpa ( fnd )

                  if ( failed() ) return

               end do

!
!              Try the next oldest file.
!
               findex = findex -1

               if ( findex > 0 ) then

                  call dafbbs ( fthan( findex ) )
                  call daffpa ( fnd             )

                  if ( failed() ) return

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
         else if ( status == 'SUSPEND' ) then

            top          = top + 1
            stack( top ) = doing
            status       = urgent

         else if ( status == 'RESUME' ) then
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
!     at this point.  Also clear the re-use interval's expense.
!
      if ( bindex > 0 ) then

         btchkp(bindex) = .false.
         btruex(bindex) =  0

      end if


      return
      end
