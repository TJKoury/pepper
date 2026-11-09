!$Procedure T_SBSR ( S/P Kernel, Buffer segments for readers )

      SUBROUTINE T_SBSR ( FNAME, &
                          HANDLE, &
                          BODY, &
                          ET, &
                          DESCR, &
                          IDENT, &
                          FOUND   )

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

      IMPLICIT NONE

      CHARACTER*(*)         FNAME
      INTEGER               HANDLE
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      DESCR    ( * )
      CHARACTER*(*)         IDENT
      LOGICAL               FOUND

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE =  500 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE =  100 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =    -5 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE =  1000 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     FNAME      I   T_SLEF
!     HANDLE    I-O  T_SLEF, T_SUEF, T_SSFS
!     BODY       I   T_SSFS
!     ET         I   T_SSFS
!     DESCR      O   T_SSFS
!     IDENT      O   T_SSFS
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
!              loaded by T_SLEF at any given time for use by the
!              readers.
!
!     BTSIZE   is the maximum number of bodies whose segments can be
!              buffered by T_SSFS.
!
!     STSIZE   is the maximum number of segments that can be buffered at
!              any given time by T_SSFS.
!
! $ Exceptions
!
!     1)  If T_SBSR is called directly, the error SPICE(BOGUSENTRY)
!         is signaled.
!
!     2)  See entry points T_SLEF, T_SUEF, and T_SSFS for exceptions
!         specific to them.
!
! $ Files
!
!     S/P-kernel ephemeris files are indicated by filename before
!     loading (see T_SLEF) and handle after loading (all other places).
!
! $ Particulars
!
!     T_SBSR serves as an umbrella, allowing data to be shared by its
!     entry points:
!
!        T_SLEF       Load ephemeris file.
!        T_SUEF       Unload ephemeris file.
!        T_SSFS       Select file and segment.
!
!     Before a file can be read by the S/P-kernel readers, it must be
!     loaded by T_SLEF, which among other things, loads the file into
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
!     searching by unloading it via a call to T_SUEF.
!
!     T_SSFS performs the search for segments within a file for the
!     S/P-kernel readers. It searches through last-loaded files first.
!     Within a single file, it searches through last-inserted segments
!     first, thus assuming that "newest data is best".
!
!     Information on loaded files is used by T_SSFS to manage a buffer
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
!           CALL T_SLEF ( 'PREDICT.SPK',  PRED_HNDL )
!           CALL T_SLEF ( 'UPDATE_1.SPK', UPD1_HNDL )
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
!              CALL T_SSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
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
!           CALL T_SUEF (  UPD1_HNDL )
!           CALL T_SLEF ( 'UPDATE_2.SPK', UPD2_HNDL )
!
!           ET = BEG_ET
!
!           DO WHILE ( ET .LE. END_ET )
!
!     C
!     C        Locate the applicable segment.
!     C
!              CALL T_SSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
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
!        Moved FOUND initialization in entry point T_SSFS.
!
!        Edited the header of T_SBSR umbrella routine and its entry
!        points T_SLEF, T_SUEF and T_SSFS.
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
!        point T_SLEF so that the pool is initialized only if the file
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
!          In the T_SSFS 'MAKE ROOM' state, when the suspended activity
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
!        in MOVED calls in entry points T_SUEF and T_SSFS.
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
!           4) The load routine T_SLEF now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby
!              avoiding arithmetic overflow.
!
!           5) The unload routine T_SUEF now calls RETURN() on entry and
!              returns if so directed.
!
!           6) In T_SSFS, DAF calls are followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           7) In T_SSFS, a subscript bound violation in a loop
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
!        An interim fix to a bug in T_SBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        1) When loading a file, T_SLEF now checks if the file table is
!           full only after determining that the file is not currently
!           loaded. Previously, if the file table was full and an
!           attempt was made to reload a file, an error was signaled. A
!           new exception was added as a result of this change.
!
!        2) A bug in the way that T_SLEF and T_SUEF clean up the body
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
!          In the T_SSFS 'MAKE ROOM' state, when the suspended activity
!          is 'ADD TO FRONT' and no segment table room is available,
!          the body table's pointer to the current segment list
!          is now set to null. Previously the pointer was allowed to go
!          stale.
!
!-    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in MOVED calls in entry points T_SUEF and T_SSFS.
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
!           4) The load routine T_SLEF now resets all file numbers when
!              the next file number reaches INTMAX()-1, thereby
!              avoiding arithmetic overflow.
!
!           5) The unload routine T_SUEF now calls RETURN() on entry and
!              returns if so directed.
!
!           6) In T_SSFS, DAF calls are followed by tests of FAILED()
!              in order to ensure that the main state loop terminates.
!
!           7) In T_SSFS, a subscript bound violation in a loop
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
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               INTMAX
      INTEGER               ISRCHI
      INTEGER               LNKNFN
      INTEGER               LNKNXT
      INTEGER               LNKPRV
      INTEGER               LNKTL

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               SLEN
      PARAMETER           ( SLEN   = 15 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )


!
!     Constants used in the doubly linked list structure:
!
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  = 1 )

      INTEGER               BCKWRD
      PARAMETER           ( BCKWRD  = 2 )

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
      INTEGER               NFT
      INTEGER               FTHAN    ( FTSIZE )
      INTEGER               FTNUM    ( FTSIZE )
      INTEGER               NEXT
      INTEGER               FINDEX

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
      CHARACTER*(SIDLEN)    BTPRVI   ( BTSIZE )

      DOUBLE PRECISION      BTPRVD   ( DSCSIZ, BTSIZE )
      DOUBLE PRECISION      BTLB     ( BTSIZE )
      DOUBLE PRECISION      BTUB     ( BTSIZE )

      INTEGER               BINDEX
      INTEGER               BTBEG    ( BTSIZE )
      INTEGER               BTBOD    ( BTSIZE )
      INTEGER               BTEXP    ( BTSIZE )
      INTEGER               BTHFS    ( BTSIZE )
      INTEGER               BTLFS    ( BTSIZE )
      INTEGER               BTPRVH   ( BTSIZE )
      INTEGER               BTRUEX   ( BTSIZE )
      INTEGER               NBT

      LOGICAL               BTCHKP   ( BTSIZE )

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
!     entry point T_SSFS.
!
      INTEGER               STPOOL ( 2, LBPOOL : STSIZE )
      INTEGER               STHAN  (             STSIZE )
      DOUBLE PRECISION      STDES  ( DSCSIZ,     STSIZE )
      CHARACTER*(SIDLEN)    STIDNT (             STSIZE )

!
!     Other stuff
!
      CHARACTER*(SLEN)      DOING
      CHARACTER*(SLEN)      STACK    ( 2 )
      CHARACTER*(SLEN)      STATUS
      CHARACTER*(SLEN)      URGENT

      DOUBLE PRECISION      DCD      ( ND )

      INTEGER               CHEAP
      INTEGER               COST
      INTEGER               CRFLBG
      INTEGER               HEAD
      INTEGER               I
      INTEGER               ICD      ( NI )
      INTEGER               J
      INTEGER               MINEXP
      INTEGER               NEW
      INTEGER               NXTSEG
      INTEGER               P
      INTEGER               TAIL
      INTEGER               TOP

      LOGICAL               FND
      LOGICAL               FNDHAN

!
!     Saved variables
!
      SAVE                  BTBEG
      SAVE                  BTBOD
      SAVE                  BTCHKP
      SAVE                  BTEXP
      SAVE                  BTHFS
      SAVE                  BTLB
      SAVE                  BTLFS
      SAVE                  BTPRVD
      SAVE                  BTPRVH
      SAVE                  BTPRVI

      SAVE                  BTRUEX
      SAVE                  BTUB
      SAVE                  FTHAN
      SAVE                  FTNUM
      SAVE                  NBT
      SAVE                  NEXT
      SAVE                  NFT
      SAVE                  STDES
      SAVE                  STHAN
      SAVE                  STIDNT

      SAVE                  STPOOL

!
!     Initial values
!
      DATA                  NFT       / 0 /
      DATA                  NBT       / 0 /
      DATA                  NEXT      / 0 /

!
!     Nobody has any business calling T_SBSR directly.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_SBSR' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_SBSR' )

      RETURN



!$Procedure T_SLEF ( S/P Kernel, Load ephemeris file )

      ENTRY T_SLEF ( FNAME, HANDLE )

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
!     handle by T_SLEF, which will be used by most other routines to
!     refer to it.
!
! $ Particulars
!
!     If there is room for a new file in the file table, T_SLEF creates
!     an entry for it and loads the file for reading using DAFOPR.
!
! $ Examples
!
!     See $Examples in T_SBSR.
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
!        point T_SLEF so that the pool is initialized only if the file
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
!        When loading a file, T_SLEF now checks if the file table is
!        full only after determining that the file is not currently
!        loaded. Previously, if the file table was full and an attempt
!        was made to reload a file, an error was signaled. A new
!        exception was added as a result of this change.
!
!        A bug in the way that T_SLEF and T_SUEF clean up the body
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
!        An interim fix to a bug in T_SBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        When loading a file, T_SLEF now checks if the file table is
!        full only after determining that the file is not currently
!        loaded. Previously, if the file table was full and an attempt
!        was made to reload a file, an error was signaled. A new
!        exception was added as a result of this change.
!
!        A bug in the way that T_SLEF and T_SUEF clean up the body
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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_SLEF' )
      END IF

!
!     Any time we load a file, there is a possibility that the
!     re-use intervals are invalid because they're been superseded
!     by higher-priority data.  Since we're not going to examine
!     the loaded file, simply indicate that all of the re-use
!     intervals are invalid.
!
      DO I = 1, NBT
         BTCHKP(I) = .FALSE.
      END DO

!
!     Nothing works unless at least one file has been loaded, so this
!     is as good a place as any to initialize the segment table pool.
!     We want to avoid unnecessary initializations, so we only
!     initialize the list when no files are loaded. It's quite possible
!     to have files loaded and an empty body table, so we don't
!     want to re-initialize just because there are no body table
!     entries.
!
      IF ( NFT .EQ. 0 ) THEN
         CALL LNKINI ( STSIZE, STPOOL )
      END IF

!
!     To load a new file, first try to open it for reading.
!
      CALL DAFOPR ( FNAME, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_SLEF' )
         RETURN
      END IF

!
!     Determine if the file is already in the table.
!
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .GT. 0 ) THEN
!
!        The last call we made to DAFOPR added another DAF link to
!        the SPK file.  Remove this link.
!
         CALL DAFCLS ( HANDLE )

!
!        Remove the file from the file table and remove its segments
!        from the segment table.  If the segment list for a body
!        becomes empty, remove that body from the body table.
!
         NFT = NFT - 1

         DO I = FINDEX, NFT
            FTHAN( I ) = FTHAN( I + 1 )
            FTNUM( I ) = FTNUM( I + 1 )
         END DO

         I = 1

         DO WHILE ( I .LE. NBT )

            P = BTBEG( I )

            DO WHILE ( P .GT. 0 )
!
!              Find the successor of P, if any.
!
               NXTSEG = LNKNXT ( P, STPOOL )

               IF ( STHAN( P ) .EQ. HANDLE ) THEN
!
!                 The segment corresponding to node P came from
!                 the file we're unloading.  Delete the node for
!                 P from the segment list for body I; if P happens
!                 to be the head node for body I's segment list,
!                 make the successor of P the head of the list.
!
                  CALL LNKFSL ( P, P, STPOOL )

                  IF ( P .EQ. BTBEG(I) ) THEN
                     BTBEG( I ) = NXTSEG
                  END IF

               END IF
!
!              Update P.
!
               P = NXTSEG

            END DO

!
!           If the list for this body is now empty, shorten the current
!           table by one: put all the entries for the last body in the
!           table into the space occupied by the one we've deleted.
!
            IF ( BTBEG( I ) .LE. 0 ) THEN
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
               BTBOD( I ) = BTBOD( NBT )
               BTEXP( I ) = BTEXP( NBT )
               BTHFS( I ) = BTHFS( NBT )
               BTLFS( I ) = BTLFS( NBT )
               BTBEG( I ) = BTBEG( NBT )

               NBT = NBT - 1

            ELSE

               I = I + 1

            END IF

         END DO

      ELSE
!
!        This is a new file.  Make sure that there are unused slots
!        in the file table.
!
         IF ( NFT .EQ. FTSIZE ) THEN
!
!           This error case can occur only if FTSIZE is larger than
!           the maximum number of open DAF files.  Currently FTSIZE
!           is equal to this limit.
!
            CALL DAFCLS ( HANDLE )

            CALL SETMSG ( 'The internal file table is already full, ' // &
                          'with # entries.'         )
            CALL ERRINT ( '#', FTSIZE               )
            CALL SIGERR ( 'SPICE(SPKFILETABLEFULL)' )
            CALL CHKOUT ( 'T_SLEF'                  )
            RETURN

         END IF

      END IF

!
!     Determine the next file number.  Note that later code assumes
!     that the file number can be incremented by 1, so we can't allow
!     the file number to reach INTMAX().
!
      IF ( NEXT .LT. INTMAX()-1 ) THEN

         NEXT = NEXT + 1

      ELSE
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
         DO I = 1, NBT
!
!           Re-map the HFS table for the Ith body.
!
            J = ISRCHI ( BTHFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
!
!              The highest file searched for body I is the Jth file
!              in the file table.
!
               BTHFS(I) = J

            ELSE
!
!              The highest file searched for body I is not in the file
!              table.  This occurs when the highest file searched has
!              been unloaded.  Note that this assignment makes all files
!              appear to be "new" when a lookup for body I is performed.
!
               BTHFS(I) = 0

            END IF

!
!           Re-map the LFS table for the Ith body.
!
            J = ISRCHI ( BTLFS(I), NFT, FTNUM )

            IF ( J .GT. 0 ) THEN
!
!              The lowest file searched for body I is the Jth file
!              in the file table.
!
               BTLFS(I) = J

            ELSE
!
!              The lowest file searched for body I is not in the file
!              table.  This occurs when the lowest file searched has
!              been unloaded.  Force reconstruction of the list by
!              making all files "new."
!
               BTLFS(I) = 0
               BTHFS(I) = 0

            END IF

         END DO

!
!        Re-map the file number table itself.
!
         DO I = 1, NFT

            FTNUM(I) = I

         END DO

!
!        Assign a new file number.
!
         NEXT = NFT + 1

      END IF


      NFT        = NFT  + 1
      FTHAN(NFT) = HANDLE
      FTNUM(NFT) = NEXT

      CALL CHKOUT ( 'T_SLEF' )
      RETURN



!$Procedure T_SUEF ( SPK Kernel, Unload ephemeris file )

      ENTRY T_SUEF ( HANDLE )

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
!     T_SUEF.
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
!     See $Examples in T_SBSR.
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
!        An interim fix to a bug in T_SBSR was made. The parameters
!        STSIZE and BTSIZE were increased to be much larger than before
!        (from 100 and 20 to 2000 and 40 respectively). This should
!        keep the boundary errors experienced by Cassini users from
!        occurring again. Version 4.0.0 with a real fix to the
!        boundary problem should be installed in SPICELIB by
!        October 1995
!
!-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!
!        A bug in the way that T_SLEF and T_SUEF clean up the body
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
!        A bug in the way that T_SLEF and T_SUEF clean up the body
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


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SUEF' )

!
!     All of the stored segments from the file must be removed
!     from the segment table (by returning the corresponding nodes
!     to the segment table pool.)
!
!     Don't do anything if the given handle is not in the file table.
!
      FINDEX = ISRCHI ( HANDLE, NFT, FTHAN )

      IF ( FINDEX .EQ. 0 ) THEN
         CALL CHKOUT ( 'T_SUEF' )
         RETURN
      END IF

!
!     First get rid of the entry in the file table. Close the file
!     before wiping out the handle.
!
      CALL DAFCLS ( FTHAN(FINDEX) )

      NFT = NFT - 1

      DO I = FINDEX, NFT
         FTHAN( I ) = FTHAN( I + 1 )
         FTNUM( I ) = FTNUM( I + 1 )
      END DO

!
!     Check each body list individually. Note that the first node
!     on each list, having no predecessor, must be handled specially.
!
      I = 1

      DO WHILE ( I .LE. NBT )

         P = BTBEG( I )

         DO WHILE ( P .GT. 0 )

            NXTSEG = LNKNXT ( P, STPOOL )

            IF ( STHAN(P) .EQ. HANDLE ) THEN

               IF ( P .EQ. BTBEG(I) ) THEN
                  BTBEG( I ) = NXTSEG
               END IF

               CALL LNKFSL ( P, P, STPOOL )

            END IF

            P = NXTSEG

         END DO

!
!        If we happened to get rid of all of the segments for this
!        body, then the body should be deleted from the table: shift
!        all entries for the body at the end of the table into the
!        space occupied by the deleted body.
!
         IF ( BTBEG(I) .LE. 0 ) THEN

            IF ( I .NE. NBT ) THEN

               BTBOD (I) = BTBOD (NBT)
               BTEXP (I) = BTEXP (NBT)
               BTHFS (I) = BTHFS (NBT)
               BTLFS (I) = BTLFS (NBT)
               BTBEG (I) = BTBEG (NBT)
               BTLB  (I) = BTLB  (NBT)
               BTUB  (I) = BTUB  (NBT)
               BTPRVH(I) = BTPRVH(NBT)
               BTPRVI(I) = BTPRVI(NBT)
               BTCHKP(I) = BTCHKP(NBT)
               BTRUEX(I) = BTRUEX(NBT)

               CALL MOVED ( BTPRVD(1,NBT), DSCSIZ, BTPRVD(1,I) )

            END IF

            NBT = NBT - 1

         ELSE

            I = I + 1

         END IF

      END DO

!
!     Any time we unload a file, we may be removing the file
!     providing data for the re-use interval for one or more bodies.
!     For each body, if the handle associated with the re-use interval
!     happens to be that of the file we're unloading, indicate
!     that the re-use interval is invalid.
!
      DO I = 1, NBT

         IF ( BTCHKP(I) ) THEN

            IF ( BTPRVH(I) .EQ. HANDLE ) THEN
               BTCHKP(I) = .FALSE.
            END IF

         END IF

      END DO

      CALL CHKOUT ( 'T_SUEF' )
      RETURN



!$Procedure T_SSFS ( S/P Kernel, Select file and segment )

      ENTRY T_SSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )

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
!     1)  If an attempt is made to call T_SSFS when there aren't any
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
!     All SPK files loaded by FURNSH or T_SLEF are potential search
!     targets for T_SSFS.
!
! $ Particulars
!
!     This routine finds the highest-priority segment, in any loaded
!     SPK file, such that the segment provides data for the specified
!     body and epoch.
!
! $ Examples
!
!     See $Examples in T_SBSR.
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
!        Moved FOUND initialization so it is always executed.
!
!        Edited the header to comply with NAIF standard. Improved
!        $Abstract description.
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
!          In the T_SSFS 'MAKE ROOM' state, when the suspended activity
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
!        An interim fix to a bug in T_SBSR was made. The parameters
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
!          In the T_SSFS 'MAKE ROOM' state, when the suspended activity
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
!        If an attempt is made to call T_SSFS when there are no files
!        loaded, an error is now signaled.
!
!-&

!
!     Assume the segment is not found, until it actually is.
!
      FOUND = .FALSE.

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_SSFS' )
      END IF

!
!     Buffering segments involves maintaining three tables:  the
!     file table, the body table, and the segment table.  The routine
!     is broken down into various tasks, described below, which
!     perform these manipulations.  A description of the components
!     of each table is provided in the declarations section of T_SBSR.

!
!     There must be at least ONE file loaded.
!
      IF ( NFT .EQ. 0 ) THEN
         CALL SETMSG ( 'At least one SPK file needs to be loaded '   // &
                       'by T_SLEF before beginning a search.'       )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'                       )
         CALL CHKOUT ( 'T_SSFS'                                     )
         RETURN
      END IF

!
!     The stack of suspended tasks is empty.
!
      TOP = 0

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
      BINDEX = ISRCHI ( BODY, NBT, BTBOD )

      IF ( BINDEX .EQ. 0 ) THEN

         STATUS = 'NEW BODY'

      ELSE
!
!        Much of the time, the segment used to satisfy the previous
!        request for a given body will also satisfy the current request
!        for data for that body.  Check whether this is the case.
!
         IF ( BTCHKP(BINDEX) ) THEN
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
            IF (       ( ET .GT. BTLB(BINDEX) ) &
                 .AND. ( ET .LT. BTUB(BINDEX) )  ) THEN
!
!              The request time is covered by the segment found on
!              the previous request for data for the current body,
!              and this interval is not masked by any higher-priority
!              segments.  The previous segment for this body satisfies
!              the request.
!
               HANDLE = BTPRVH(BINDEX)
               IDENT  = BTPRVI(BINDEX)

               CALL MOVED ( BTPRVD(1,BINDEX), DSCSIZ, DESCR )

               FOUND  = .TRUE.

               CALL CHKOUT ( 'T_SSFS' )
               RETURN

            END IF

!
!           Adjust the expense here. If the expense of the list
!           contains a component due to the cost of finding the
!           unbuffered segment providing data for re-use, subtract
!           that component from the expense.
!
            BTEXP(BINDEX)  = BTEXP(BINDEX) - BTRUEX(BINDEX)
            BTRUEX(BINDEX) = 0

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
            BTCHKP(BINDEX) = .FALSE.

         END IF


!
!        If the segment list for this body is empty, make sure the
!        expense is reset to 0.
!
         IF ( BTBEG(BINDEX) .EQ. 0 ) THEN

            BTEXP(BINDEX) = 0

         END IF


         STATUS = '?'

      END IF


      DO WHILE ( STATUS .NE. 'HOPELESS' )
!
!        If new files have been added, they have to be searched.
!        Otherwise, we can go right to the list of stored segments.
!
         IF ( STATUS .EQ. '?' ) THEN
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

            IF ( BTHFS( BINDEX ) .LT. FTNUM( NFT ) ) THEN
               STATUS = 'NEW FILES'
            ELSE
               STATUS = 'CHECK LIST'
            END IF



         ELSE IF ( STATUS .EQ. 'NEW BODY' ) THEN
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

            IF ( NBT .LT. BTSIZE ) THEN
!
!              If the body table isn't full, the cheapest place is
!              just the next unused row of the table.
!
               NBT   = NBT + 1
               CHEAP = NBT

            ELSE
!
!              The body table is full.  Find the least
!              expensive body in the table and remove it.
!
               CHEAP  = 1
               MINEXP = BTEXP(1)

               DO I = 2, NBT

                  IF ( BTEXP(I) .LT. MINEXP ) THEN
                     CHEAP  = I
                     MINEXP = BTEXP(I)
                  END IF

               END DO

!
!              If there are any segments associated with the
!              least expensive body, we put them back on the free
!              list.
!
               HEAD = BTBEG(CHEAP)

               IF ( HEAD .GT. 0 ) THEN

                  TAIL =  - LNKPRV ( HEAD, STPOOL )
                  CALL LNKFSL ( HEAD, TAIL, STPOOL )

               END IF

            END IF

!
!           Set up a body table entry for the new body.
!
            BTBOD (CHEAP) = BODY
            BTEXP (CHEAP) = 0
            BTHFS (CHEAP) = FTNUM(NFT)
            BTLFS (CHEAP) = FTNUM(NFT) + 1
            BTBEG (CHEAP) = 0
            BTCHKP(CHEAP) = .FALSE.

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
            BTRUEX(CHEAP) = 0
            BTLB  (CHEAP) = DPMIN()
            BTUB  (CHEAP) = DPMAX()
            BTPRVH(CHEAP) = 0
            BTPRVI(CHEAP) = ' '
            CALL CLEARD ( DSCSIZ, BTPRVD(1,CHEAP) )

!
!           BINDEX is the body table index of the new entry.
!
            BINDEX = CHEAP

!
!           Now search the loaded SPK files for segments relating to
!           this body.  We start with the last-loaded files and
!           work backwards.
!
            STATUS = 'OLD FILES'



         ELSE IF ( STATUS .EQ. 'NEW FILES' ) THEN
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
            FINDEX = 1

            DO WHILE ( BTHFS( BINDEX ) .GE. FTNUM( FINDEX ) )
               FINDEX = FINDEX + 1
            END DO

            BTHFS( BINDEX ) = FTNUM( FINDEX )

            CALL DAFBFS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SSFS' )
               RETURN
            END IF

            STATUS = 'NEW SEGMENTS'

!
!           The cost of the list contributed by the new file is
!           zero so far.
!
            COST = 0


         ELSE IF ( STATUS .EQ. 'NEW SEGMENTS' ) THEN
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

            CALL DAFFNA ( FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SSFS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
!
!              We're out of segments in the current file.  Decide
!              whether we need to examine another new file, or
!              whether we're ready to check the list.
!
               STATUS = '?'
               BTEXP( BINDEX ) = BTEXP( BINDEX ) + COST

            ELSE

               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_SSFS' )
                  RETURN
               END IF

               IF (       ( ICD(1) .EQ. BODY   ) &
                    .AND. ( DCD(1) .LE. DCD(2) )  ) THEN

                  DOING  = 'NEW SEGMENTS'
                  URGENT = 'ADD TO FRONT'
                  STATUS = 'SUSPEND'

               END IF

               COST = COST + 1

            END IF
!
!           If we haven't reset the status, we'll return for another
!           'NEW SEGMENTS' pass.
!

         ELSE IF ( STATUS .EQ. 'OLD FILES' ) THEN
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

            FINDEX = NFT

            DO WHILE ( BTLFS( BINDEX ) .LE. FTNUM( FINDEX ) )
               FINDEX = FINDEX - 1
            END DO

            CALL DAFBBS ( FTHAN( FINDEX ) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SSFS' )
               RETURN
            END IF


            STATUS = 'OLD SEGMENTS'
!
!           The next thing we'll do is search through all the segments
!           of this file for those that applicable to this body.
!           The cost of the list contributed by the current file is
!           zero so far.
!
            COST = 0


         ELSE IF ( STATUS .EQ. 'OLD SEGMENTS' ) THEN
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
            CALL DAFFPA ( FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_SSFS' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN
!
!              We've been through all of the segments in this file.
!              Change the lowest file searched indicator for this body
!              to be the current file, and go check the current list.
!
               BTLFS( BINDEX ) =  FTNUM( FINDEX )
               BTEXP( BINDEX ) =  BTEXP( BINDEX ) + COST
               STATUS          = 'CHECK LIST'

            ELSE

               CALL DAFGS ( DESCR )
               CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_SSFS' )
                  RETURN
               END IF

               IF (       ( ICD(1) .EQ. BODY   ) &
                    .AND. ( DCD(1) .LE. DCD(2) )  )  THEN

                  DOING  = 'OLD SEGMENTS'
                  URGENT = 'ADD TO END'
                  STATUS = 'SUSPEND'

               END IF

               COST = COST + 1

            END IF
!
!           If we haven't reset the status, we'll return for another
!           'OLD SEGMENTS' pass.
!

         ELSE IF ( STATUS .EQ. 'CHECK LIST'   ) THEN
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
            BTLB(BINDEX) = DPMIN()
            BTUB(BINDEX) = DPMAX()
            P            = BTBEG( BINDEX )

            DO WHILE ( P .GT. 0 )

               IF ( ET .GT. STDES(2,P) ) THEN
!
!                 ET is to the right of the coverage interval of this
!                 segment.
!
                  BTLB(BINDEX) = MAX ( BTLB(BINDEX), STDES(2,P) )


               ELSE IF ( ET .LT. STDES(1,P) ) THEN
!
!                 ET is to the left of the coverage interval of this
!                 segment.
!
                  BTUB(BINDEX) = MIN ( BTUB(BINDEX), STDES(1,P) )

               ELSE
!
!                 The segment coverage interval includes ET.
!
                  CALL MOVED ( STDES( 1,P ), DSCSIZ, DESCR )
                  IDENT  = STIDNT( P )
                  HANDLE = STHAN ( P )
                  FOUND  = .TRUE.

!
!                 Set the re-use interval for the current body.
!
                  BTLB(BINDEX) = MAX ( BTLB(BINDEX), STDES(1,P) )
                  BTUB(BINDEX) = MIN ( BTUB(BINDEX), STDES(2,P) )

!
!                 Save the returned output items, in case this segment
!                 may satisfy the next request.
!
                  BTPRVH(BINDEX) =  HANDLE
                  BTPRVI(BINDEX) =  IDENT
                  CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                  BTCHKP(BINDEX) =  .TRUE.

                  CALL CHKOUT ( 'T_SSFS' )
                  RETURN

               END IF

!
!              Get the next node.  We avoid LNKNXT here in order
!              to speed up the operation.
!
               P = STPOOL ( FORWRD, P )

            END DO

!
!           If we're still here we didn't have information for this
!           body in the segment list.
!
!           If there are more files, search them.
!           Otherwise, things are hopeless, set the status that way.
!
            IF ( BTLFS( BINDEX ) .GT. FTNUM( 1 ) ) THEN
               STATUS = 'OLD FILES'
            ELSE
               STATUS = 'HOPELESS'
            END IF



         ELSE IF ( STATUS .EQ. 'MAKE ROOM' ) THEN
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
            MINEXP = INTMAX()
            CHEAP  = 0

            DO I = 1, NBT

               IF ( I .NE. BINDEX ) THEN
!
!                 This list is for a body other than the current
!                 one.
!
                  IF (      ( BTEXP(I) .LT. MINEXP   ) &
                       .OR. ( CHEAP    .EQ. 0        )  )THEN
!
!                    This list is the cheapest seen so far,
!                    possibly because it's the first one
!                    considered.  At the moment, it's as good
!                    a candidate for removal as any.
!
                     CHEAP  = I
                     MINEXP = BTEXP(I)

                  END IF

               END IF

            END DO


           IF ( CHEAP .EQ. 0 ) THEN
!
!              What we do if there are no delete-able segments
!              depends on the task that was suspended before entering
!              'MAKE ROOM'.
!
               IF ( STACK( TOP ) .EQ. 'ADD TO END' ) THEN
!
!                 There's nothing left to do but search the remaining
!                 files and segments without buffering them.
!
                  STATUS = 'SEARCH W/O BUFF'


               ELSE
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
                  P    =   BTBEG ( BINDEX )
                  TAIL = - LNKPRV( P, STPOOL )

                  CALL LNKFSL ( P, TAIL, STPOOL )

!
!                 Re-initialize the table for this body, and initiate
!                 an 'OLD FILES' search, just as in 'NEW BODY'.
!                 Also, reset the suspended task stack to be empty.
!
                  BTBEG( BINDEX ) = 0
                  BTEXP( BINDEX ) = 0
                  BTHFS( BINDEX ) = FTNUM( NFT )
                  BTLFS( BINDEX ) = FTNUM( NFT ) + 1
                  STATUS          = 'OLD FILES'
                  TOP             = 0

               END IF

            ELSE
!
!              Return this cheapest list to the segment pool.
!
               P = BTBEG  ( CHEAP )

               IF ( P .GT. 0 ) THEN

                  TAIL = - LNKPRV ( P, STPOOL )
                  CALL LNKFSL ( P, TAIL, STPOOL )

               END IF

!
!              Fill the deleted body's space in the table with
!              the final entry in the table.
!
               IF ( CHEAP .NE. NBT ) THEN

                  BTBOD ( CHEAP ) = BTBOD ( NBT )
                  BTEXP ( CHEAP ) = BTEXP ( NBT )
                  BTHFS ( CHEAP ) = BTHFS ( NBT )
                  BTLFS ( CHEAP ) = BTLFS ( NBT )
                  BTBEG ( CHEAP ) = BTBEG ( NBT )
                  BTLB  ( CHEAP ) = BTLB  ( NBT )
                  BTUB  ( CHEAP ) = BTUB  ( NBT )
                  BTPRVH( CHEAP ) = BTPRVH( NBT )
                  BTPRVI( CHEAP ) = BTPRVI( NBT )
                  BTRUEX( CHEAP ) = BTRUEX( NBT )
                  BTCHKP( CHEAP ) = BTCHKP( NBT )


                  CALL MOVED ( BTPRVD(1,NBT), DSCSIZ, BTPRVD(1,CHEAP) )

               END IF

!
!              If the final entry in the table happened to be the
!              current body of interest, then we also have to change
!              the current body index.
!
               IF ( BINDEX .EQ. NBT ) THEN
                  BINDEX = CHEAP
               END IF

!
!              One less body now.
!
               NBT    = NBT - 1
               STATUS = 'RESUME'

            END IF
!
!           Either we made room by freeing a non-empty segment list,
!           or we're going to work without additional space.  In the
!           latter case, the state is now 'OLD FILES' or
!           'SEARCH W/O BUFF'.
!


         ELSE IF ( STATUS .EQ. 'ADD TO FRONT' ) THEN
!
!           The current segment information should be linked in at
!           the head of the segment list for the current body, and
!           the pertinent body table entry should point to the new
!           head of the list.
!
!           The only way to get here is from the block NEW SEGMENTS
!           after suspending that task.
!
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN
!
!              There's no room left in the segment pool.  We must make
!              room before continuing.
!
               DOING  = 'ADD TO FRONT'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'

            ELSE
!
!              Allocate a node and link it to the front of the list
!              for the current body.
!
               CALL LNKAN ( STPOOL, NEW )

               STHAN( NEW ) = FTHAN( FINDEX )
               CALL MOVED ( DESCR, DSCSIZ, STDES(1,NEW) )
               CALL DAFGN ( STIDNT(NEW) )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_SSFS' )
                  RETURN
               END IF

!
!              If the current list is empty, this append operation
!              is a no-op.
!
               CALL LNKILB ( NEW, BTBEG(BINDEX), STPOOL )
               BTBEG( BINDEX ) = NEW

               STATUS  = 'RESUME'

            END IF



         ELSE IF ( STATUS .EQ. 'ADD TO END' ) THEN
!
!           The current segment information should be linked in at
!           the tail of the segment list for the current body.
!
!           The only way to get to this task is from the OLD SEGMENTS
!           block after suspending that task.
!
            IF ( LNKNFN(STPOOL) .EQ. 0 ) THEN
!
!              There's no room left in the segment pool.  We must make
!              room before continuing.
!
               DOING  = 'ADD TO END'
               URGENT = 'MAKE ROOM'
               STATUS = 'SUSPEND'

            ELSE
!
!              Allocate a new node in the segment table pool.
!
               CALL LNKAN ( STPOOL, NEW )

               STHAN( NEW ) = FTHAN( FINDEX )
               CALL MOVED ( DESCR, DSCSIZ, STDES( 1,NEW ) )
               CALL DAFGN ( STIDNT( NEW ) )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'T_SSFS' )
                  RETURN
               END IF

               IF ( BTBEG(BINDEX) .LE. 0 ) THEN
!
!                 This is the first node in the list for this body.
!
                  BTBEG( BINDEX ) = NEW

               ELSE
!
!                 Link the new node to the tail of the list.
!
                  TAIL = - LNKPRV ( BTBEG(BINDEX), STPOOL )
                  CALL LNKILA ( TAIL, NEW, STPOOL )

               END IF

               STATUS = 'RESUME'

            END IF



         ELSE IF ( STATUS .EQ. 'SEARCH W/O BUFF' ) THEN
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
            BTRUEX(BINDEX) = 0

!
!           Need to find the portion of the current body's segment
!           list which comes from the current file of interest.  It
!           will be returned to the segment table pool, since the
!           remainder of the file's segments can't be added to the list.
!
            CRFLBG = BTBEG( BINDEX )
            FNDHAN = .FALSE.

            DO WHILE (  ( .NOT. FNDHAN ) .AND. ( CRFLBG .GT. 0 )  )

               FNDHAN = STHAN( CRFLBG ) .EQ. FTHAN( FINDEX )

               IF ( .NOT. FNDHAN ) THEN
!
!                 Get the next node.  We avoid LNKNXT here in order
!                 to speed up the operation.
!
                  CRFLBG = STPOOL ( FORWRD, CRFLBG )

               END IF

            END DO

            IF ( CRFLBG .GT. 0 ) THEN
!
!              The sub-list from the current node onwards is to be
!              returned to the segment table pool.  Save this node,
!              since we'll finish searching the list before freeing
!              the sub-list.
!
               P = CRFLBG

!
!              It may be that the sub-list we're deleting is the
!              entire segment list for this body.  If so, the
!              corresponding body table entry should be set to
!              a non-positive value to indicate an empty segment list.
!
               IF ( P .EQ. BTBEG(BINDEX) ) THEN

                  BTBEG(BINDEX) = 0
!
!                 Also in this case, we must initialize the re-use
!                 interval for this body.
!
                  BTLB(BINDEX) = DPMIN()
                  BTUB(BINDEX) = DPMAX()

               END IF

!
!              Finish searching through the incomplete list for the
!              desired segment.
!
               DO WHILE ( CRFLBG .GT. 0 )
!
!                 Every segment seen from the current file contributes
!                 to the expense of the re-use interval.
!
                  BTRUEX(BINDEX) = BTRUEX(BINDEX) + 1


                  IF ( ET .GT. STDES(2,CRFLBG) ) THEN
!
!                    ET is to the right of the coverage interval of this
!                    segment.
!
                     BTLB(BINDEX) = MAX( BTLB(BINDEX), STDES(2,CRFLBG) )


                  ELSE IF ( ET .LT. STDES(1,CRFLBG) ) THEN
!
!                    ET is to the left of the coverage interval of this
!                    segment.
!
                     BTUB(BINDEX) = MIN( BTUB(BINDEX), STDES(1,CRFLBG) )

                  ELSE
!
!                    The segment coverage interval includes ET.
!
                     CALL MOVED ( STDES( 1,CRFLBG ), DSCSIZ, DESCR )

                     IDENT  = STIDNT( CRFLBG )
                     HANDLE = STHAN ( CRFLBG )
                     FOUND  = .TRUE.

!
!                    Set the re-use interval for the current body.
!
                     BTLB(BINDEX) = MAX( BTLB(BINDEX), STDES(1,CRFLBG) )
                     BTUB(BINDEX) = MIN( BTUB(BINDEX), STDES(2,CRFLBG) )

!
!                    Save the output items, in case this
!                    segment may be satisfy the next request.
!
                     BTPRVH(BINDEX) =  HANDLE
                     BTPRVI(BINDEX) =  IDENT
                     CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                     BTCHKP(BINDEX) =  .TRUE.

!
!                    Update the expense of the list to reflect
!                    the cost of locating this segment.
!
                     BTEXP(BINDEX) = BTEXP(BINDEX) + BTRUEX(BINDEX)

!
!                    Free the sub-list we were searching.
!
                     TAIL = LNKTL ( CRFLBG, STPOOL )
                     CALL LNKFSL ( P, TAIL, STPOOL )

                     CALL CHKOUT ( 'T_SSFS' )
                     RETURN

                  END IF

!                 Get the next node.  We avoid LNKNXT here in order
!                 to speed up the operation.
!
                  CRFLBG = STPOOL ( FORWRD, CRFLBG )

               END DO

!
!              Return the sub-list to the segment table pool.
!              CRFLBG at this point is the negative of the list head.
!              The list tail is (by the spec of the SPICELIB doubly
!              linked list routines) the negative of the predecessor
!              of the head.
!
!              Note the list is always non-empty.
!
               TAIL = -LNKPRV( -CRFLBG, STPOOL )

               CALL LNKFSL ( P, TAIL, STPOOL )

            END IF

!
!           Search through the remaining files without buffering.
!           Recall that a search is already in progress and that a
!           segment is currently under consideration (FND = .TRUE.).
!
            DO WHILE ( FINDEX .GT. 0 )

               DO WHILE ( FND )
!
!                 Each segment found contributes to the expense of the
!                 re-use interval.
!
                  BTRUEX(BINDEX) = BTRUEX(BINDEX) + 1

                  CALL DAFGS ( DESCR )
                  CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'T_SSFS' )
                     RETURN
                  END IF

                  IF ( BODY .EQ. ICD(1) ) THEN
!
!                    This is a segment for the body of interest.
!                    Update the re-use interval for this body.
!
                     IF ( ET .GT. DCD(2) ) THEN
!
!                       ET is to the right of the coverage interval
!                       of this segment.
!
                        BTLB(BINDEX) = MAX( BTLB(BINDEX), DCD(2) )


                     ELSE IF ( ET .LT. DCD(1) ) THEN
!
!                       ET is to the left of the coverage interval
!                       of this segment.
!
                        BTUB(BINDEX) = MIN( BTUB(BINDEX), DCD(1) )

                     ELSE
!
!                       The segment coverage interval includes ET.
!
                        CALL DAFGN ( IDENT )

                        IF ( FAILED() ) THEN
                           CALL CHKOUT ( 'T_SSFS' )
                           RETURN
                        END IF

                        HANDLE = FTHAN( FINDEX )
                        FOUND  = .TRUE.

!
!                       Set the re-use interval for the current body.
!
                        BTLB(BINDEX) = MAX( BTLB(BINDEX), DCD(1) )
                        BTUB(BINDEX) = MIN( BTUB(BINDEX), DCD(2) )

!
!                       Save the output items, in case this
!                       segment may satisfy the next request.
!
                        BTPRVH(BINDEX) =  HANDLE
                        BTPRVI(BINDEX) =  IDENT
                        CALL MOVED ( DESCR, DSCSIZ, BTPRVD(1,BINDEX) )
                        BTCHKP(BINDEX) =  .TRUE.

!
!                       Update the expense of the list to reflect
!                       the cost of locating this segment.
!
                        BTEXP(BINDEX) = BTEXP(BINDEX) + BTRUEX(BINDEX)

                        CALL CHKOUT ( 'T_SSFS' )
                        RETURN

                     END IF

                  END IF

                  CALL DAFFPA ( FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'T_SSFS' )
                     RETURN
                  END IF

               END DO

!
!              Try the next oldest file.
!
               FINDEX = FINDEX -1

               IF ( FINDEX .GT. 0 ) THEN

                  CALL DAFBBS ( FTHAN( FINDEX ) )
                  CALL DAFFPA ( FND             )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'T_SSFS' )
                     RETURN
                  END IF

               END IF

            END DO

!
!           If you get to here, sorry.
!
            BTRUEX(BINDEX) = 0
            STATUS         = 'HOPELESS'

!
!        When a task is suspended, the current activity is placed on
!        a stack, to be restored later. Two levels are provided, since
!        some interrupts can be interrupted by others.
!
         ELSE IF ( STATUS .EQ. 'SUSPEND' ) THEN

            TOP          = TOP + 1
            STACK( TOP ) = DOING
            STATUS       = URGENT

         ELSE IF ( STATUS .EQ. 'RESUME' ) THEN
!
!           Pop the status stack.
!
            STATUS = STACK( TOP )
            TOP    = TOP - 1

         END IF

      END DO

!
!     If we didn't find a segment, don't attempt to use saved
!     outputs from a previous call.  BINDEX will always be set
!     at this point.  Also clear the re-use interval's expense.
!
      IF ( BINDEX .GT. 0 ) THEN

         BTCHKP(BINDEX) = .FALSE.
         BTRUEX(BINDEX) =  0

      END IF


      CALL CHKOUT ( 'T_SSFS' )
      RETURN
      END
