!> Segment buffers of SPK, PCK, CK and DSK files: the tables of loaded files (per instance) and the per-context caches of the segments found for bodies and instruments.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_segments.f90.
!>
!>## SPKBSR (S/P Kernel, Buffer segments for readers)
!>
!> PEPPER: SPICELIB's umbrella routine SPKBSR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_spkbsr. Its documentation follows.
!>
!>### Abstract
!>
!>     Load and unload files for use by the readers. Buffer segments
!>     for readers.
!>
!>### Detailed Input
!>
!>     FNAME    is the name of an SPK file to be loaded.
!>
!>     HANDLE   on input is the handle of an SPK file to be
!>              unloaded.
!>
!>     BODY     is the NAIF integer code of an ephemeris object,
!>              typically a solar system body.
!>
!>     ET       is a time, in seconds past the epoch J2000 TDB.
!>
!>### Detailed Output
!>
!>     HANDLE   on output is the handle of the S/P-kernel file
!>              containing a located segment.
!>
!>     DESCR    is the descriptor of a located segment.
!>
!>     IDENT    is the identifier of a located segment.
!>
!>     FOUND    indicates whether a requested segment was found or not.
!>
!>### Parameters
!>
!>     FTSIZE   is the maximum number of ephemeris files that can be
!>              loaded by SPKLEF at any given time for use by the
!>              readers.
!>
!>     BTSIZE   is the maximum number of bodies whose segments can be
!>              buffered by SPKSFS.
!>
!>     STSIZE   is the maximum number of segments that can be buffered at
!>              any given time by SPKSFS.
!>
!>### Exceptions
!>
!>     1)  If SPKBSR is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points SPKLEF, SPKUEF, and SPKSFS for exceptions
!>         specific to them.
!>
!>### Files
!>
!>     S/P-kernel ephemeris files are indicated by filename before
!>     loading (see SPKLEF) and handle after loading (all other places).
!>
!>### Particulars
!>
!>     SPKBSR serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        SPKLEF       Load ephemeris file.
!>        SPKUEF       Unload ephemeris file.
!>        SPKSFS       Select file and segment.
!>
!>     Before a file can be read by the S/P-kernel readers, it must be
!>     loaded by SPKLEF, which among other things, loads the file into
!>     the DAF system.
!>
!>     Up to FTSIZE files may be loaded for use simultaneously, and a
!>     file only has to be loaded once to become a potential search
!>     target for any number of subsequent reads.
!>
!>     Once an SPK file has been loaded, it is assigned a file
!>     handle, which is used to keep track of the file internally,
!>     and which is used by the calling program to refer to the file
!>     in all subsequent calls to SPK routines.
!>
!>     A file may be removed from the list of files for potential
!>     searching by unloading it via a call to SPKUEF.
!>
!>     SPKSFS performs the search for segments within a file for the
!>     S/P-kernel readers. It searches through last-loaded files first.
!>     Within a single file, it searches through last-inserted segments
!>     first, thus assuming that "newest data is best".
!>
!>     Information on loaded files is used by SPKSFS to manage a buffer
!>     of saved segment descriptors and identifiers to speed up access
!>     time without having to necessarily perform file reads.
!>
!>### Examples
!>
!>     Suppose that ephemeris data for the Mars Global Surveyor
!>     spacecraft relative to Mars are contained in three separate files:
!>     PREDICT.SPK contains complete predict ephemeris data for several
!>     successive orbits, and UPDATE_1.SPK and UPDATE_2.SPK contain two
!>     separate updates to selected intervals within those orbits, based
!>     on altimeter fits.
!>
!>     In the following example, states of the spacecraft are computed
!>     in two different ways:
!>
!>     First, the predict file and one of the update files are both
!>     loaded and states are requested for regular intervals within
!>     the orbits. The update file is searched through first, and if no
!>     data for the requested time is available, the predict file is
!>     used.
!>
!>     Then, the first update file is unloaded, the second update file
!>     is loaded, and the same requests are made as above.
!>
!>     Throughout the two searches, a table is written which contains
!>     the state (position and velocity) of the spacecraft, and the
!>     file from which the data came, if such data was found, and an
!>     error message otherwise.
!>
!>     It is assumed that the beginning and ending ephemeris times
!>     (BEG_ET, END_ET) for the entire span have already been
!>     initialized, along with the step-size for each measurement
!>     (DELTA). The two routines WRITE_TABLE and WRITE_ERROR do not
!>     exist in SPICELIB.
!>
!>           INTEGER               PRED_HNDL
!>           INTEGER               UPD1_HNDL
!>           INTEGER               UPD2_HNDL
!>           INTEGER               HANDLE
!>           INTEGER               BODY
!>           INTEGER               CENTER
!>
!>           DOUBLE PRECISION      BEG_ET
!>           DOUBLE PRECISION      END_ET
!>           DOUBLE PRECISION      DELTA
!>           DOUBLE PRECISION      ET
!>           DOUBLE PRECISION      DESCR ( 5 )
!>           DOUBLE PRECISION      STATE ( 6 )
!>
!>           CHARACTER*40          IDENT
!>           CHARACTER*25          FNAME
!>
!>           LOGICAL               FOUND
!>
!>     C
!>     C     Load the predict file and the first update file. Since
!>     C     last-loaded files get searched first, we want to load the
!>     C     update file second.
!>     C
!>           CALL SPKLEF ( 'PREDICT.SPK',  PRED_HNDL )
!>           CALL SPKLEF ( 'UPDATE_1.SPK', UPD1_HNDL )
!>
!>     C
!>     C     NAIF code for the Mars Global Surveyor spacecraft is -94.
!>     C
!>           BODY = -94
!>
!>     C
!>     C     Compute states for regular intervals between BEG_ET and
!>     C     END_ET.
!>     C
!>           ET = BEG_ET
!>
!>           DO WHILE ( ET .LE. END_ET )
!>
!>     C
!>     C        Locate the applicable segment (handle and descriptor).
!>     C
!>              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!>
!>              IF ( FOUND ) THEN
!>     C
!>     C           Evaluate the state, get the name of the file from
!>     C           whence the data came, and write the results to the
!>     C           table.
!>     C
!>                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE,
!>             .                CENTER )
!>
!>                 CALL DAFHFN ( HANDLE, FNAME )
!>
!>                 CALL WRITE_TABLE ( ET, STATE, FNAME )
!>
!>              ELSE
!>
!>                 CALL WRITE_ERROR ( ET )
!>
!>              END IF
!>
!>     C
!>     C        The next time.
!>     C
!>              ET = ET + DELTA
!>
!>           END DO
!>
!>     C
!>     C     Unload the first update file, load the second, and do
!>     C     everything over again. Since the original file stays
!>     C     loaded, the update file once again gets searched first.
!>     C
!>           CALL SPKUEF (  UPD1_HNDL )
!>           CALL SPKLEF ( 'UPDATE_2.SPK', UPD2_HNDL )
!>
!>           ET = BEG_ET
!>
!>           DO WHILE ( ET .LE. END_ET )
!>
!>     C
!>     C        Locate the applicable segment.
!>     C
!>              CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!>
!>              IF ( FOUND ) THEN
!>     C
!>     C           Evaluate the state, get the name of the file from
!>     C           whence the data came, and write the results to the
!>     C           table.
!>     C
!>                 CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE,
!>             .                CENTER )
!>
!>                 CALL DAFHFN ( HANDLE, FNAME )
!>
!>                 CALL WRITE_TABLE ( ET, STATE, FNAME )
!>
!>              ELSE
!>
!>                 CALL WRITE_ERROR ( ET )
!>
!>              END IF
!>
!>     C
!>     C        The next time.
!>     C
!>              ET = ET + DELTA
!>
!>           END DO
!>
!>### Restrictions
!>
!>     1)  If Fortran I/O errors occur while searching a loaded SPK
!>         file, the internal state of this suite of routines may
!>         be corrupted. It may be possible to correct the state
!>         by unloading the pertinent SPK files and then re-loading
!>         them.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     J.M. Lynch         (JPL)
!>     H.A. Neilan        (JPL)
!>     B.V. Semenov       (JPL)
!>     W.L. Taber         (JPL)
!>     R.E. Thurman       (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 6.1.0, 13-OCT-2021 (JDR) (BVS) (NJB)
!>
!>        Increased BTSIZE (from 200 to 10000).
!>
!>        Updated entry point SPKSFS to always initialize FOUND.
!>
!>        Edited the header of SPKBSR umbrella routine and its entry
!>        points SPKLEF, SPKUEF and SPKSFS.
!>
!>        Changed SAVE statements to save each variable individually.
!>
!>-    SPICELIB Version 6.0.1, 15-MAR-2017 (NJB)
!>
!>        Corrected various spelling errors within comments.
!>
!>-    SPICELIB Version 6.0.0, 17-MAR-2014 (NJB)
!>
!>        Updated segment pool initialization condition in entry
!>        point SPKLEF so that the pool is initialized only if the file
!>        table is empty.
!>
!>-    SPICELIB Version 5.4.0, 13-JUN-2013 (BVS)
!>
!>        Increased FTSIZE (from 1000 to 5000).
!>
!>        Increased STSIZE (from 50000 to 100000).
!>
!>-    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB)
!>
!>        Bug fix:
!>
!>          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!>          is 'ADD TO FRONT' and no segment table room is available,
!>          the body table's pointer to the current segment list
!>          is now set to null. Previously the pointer was allowed to go
!>          stale.
!>
!>-    SPICELIB Version 5.2.0, 07-APR-2010 (NJB)
!>
!>        Increased segment table buffer size to 50000 entries.
!>
!>-    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB)
!>
!>        Updated to remove non-standard use of duplicate arguments
!>        in MOVED calls in entry points SPKUEF and SPKSFS.
!>
!>        Increased segment table buffer size to 30000 entries.
!>
!>-    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB)
!>
!>        Increased segment table buffer size to 10000 entries.
!>
!>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!>
!>        Bug fixes:
!>
!>           1) When a segment list is freed because the entire list
!>              is contributed by a single SPK file, and the list is
!>              too large to be buffered, the corresponding body table
!>              pointer is now set to null.
!>
!>           2) An algorithm change has eliminated a bug caused by not
!>              updating the current body index when body table entries
!>              having empty segment lists were compressed out of the
!>              body table. Previously the body table pointer BINDEX
!>              could go stale after the compression.
!>
!>           3) When a already loaded kernel is re-opened with DAFOPR,
!>              it now has its link count reset to 1 via a call to
!>              DAFCLS.
!>
!>           4) The load routine SPKLEF now resets all file numbers when
!>              the next file number reaches INTMAX()-1, thereby
!>              avoiding arithmetic overflow.
!>
!>           5) The unload routine SPKUEF now calls RETURN() on entry and
!>              returns if so directed.
!>
!>           6) In SPKSFS, DAF calls are followed by tests of FAILED()
!>              in order to ensure that the main state loop terminates.
!>
!>           7) In SPKSFS, a subscript bound violation in a loop
!>              termination test was corrected.
!>
!>        The "re-use interval" feature was introduced to improve speed
!>        in the case where repeated, consecutive requests are satisfied
!>        by the same segment.
!>
!>        The segment list cost algorithm was modified slightly:
!>        the contribution of a file search to the cost of a list
!>        is included only when the file search is completed. The
!>        cost of finding the re-use interval is accounted for when
!>        unbuffered searches are required.
!>
!>        The file table size has been increased to 1000, in order
!>        to take advantage of the DAF system's new ability to load
!>        1000 files.
!>
!>        The body table size has been increased to 200 in order to
!>        decrease the chance of thrashing due to swapping segment
!>        lists for different bodies.
!>
!>        Various small updates and corrections were made to the
!>        comments throughout the file.
!>
!>-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
!>
!>        An interim fix to a bug in SPKBSR was made. The parameters
!>        STSIZE and BTSIZE were increased to be much larger than before
!>        (from 100 and 20 to 2000 and 40 respectively). This should
!>        keep the boundary errors experienced by Cassini users from
!>        occurring again. Version 4.0.0 with a real fix to the
!>        boundary problem should be installed in SPICELIB by
!>        October 1995
!>
!>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!>
!>        1) When loading a file, SPKLEF now checks if the file table is
!>           full only after determining that the file is not currently
!>           loaded. Previously, if the file table was full and an
!>           attempt was made to reload a file, an error was signaled. A
!>           new exception was added as a result of this change.
!>
!>        2) A bug in the way that SPKLEF and SPKUEF clean up the body
!>           tables after a file is unloaded was fixed.
!>
!>        3) Variable declarations were added to the example program
!>           so that it can now be compiled.
!>
!>        4) A cut and paste error in the description of the segment
!>           table was corrected.
!>
!>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 1.0.2, 09-SEP-1991 (HAN)
!>
!>        The declaration of the variable STATE in the $Examples section
!>        was changed from a 3 dimensional vector to a 6 dimensional
!>        vector, and the term state was specified to be the position
!>        and velocity of a body relative to another body.
!>
!>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!>
!>        Literature references added to the header.
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!>
!>### Revisions
!>
!>-    SPICELIB Version 5.3.0, 01-MAR-2011 (NJB)
!>
!>        Bug fix:
!>
!>          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
!>          is 'ADD TO FRONT' and no segment table room is available,
!>          the body table's pointer to the current segment list
!>          is now set to null. Previously the pointer was allowed to go
!>          stale.
!>
!>-    SPICELIB Version 5.1.0, 08-SEP-2005 (NJB)
!>
!>        Updated to remove non-standard use of duplicate arguments
!>        in MOVED calls in entry points SPKUEF and SPKSFS.
!>
!>        Increased segment table buffer size to 30000 entries.
!>
!>-    SPICELIB Version 5.0.0, 21-FEB-2003 (NJB)
!>
!>        Increased segment table buffer size to 10000 entries.
!>
!>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
!>
!>        Bug fixes:
!>
!>           1) When a segment list is freed because the entire list
!>              is contributed by a single SPK file, and the list is
!>              too large to be buffered, the corresponding body table
!>              pointer is now set to null.
!>
!>           2) An algorithm change has eliminated a bug caused by not
!>              updating the current body index when body table entries
!>              having empty segment lists were compressed out of the
!>              body table. Previously the body table pointer BINDEX
!>              could go stale after the compression.
!>
!>           3) When a already loaded kernel is re-opened with DAFOPR,
!>              it now has its link count reset to 1 via a call to
!>              DAFCLS.
!>
!>           4) The load routine SPKLEF now resets all file numbers when
!>              the next file number reaches INTMAX()-1, thereby
!>              avoiding arithmetic overflow.
!>
!>           5) The unload routine SPKUEF now calls RETURN() on entry and
!>              returns if so directed.
!>
!>           6) In SPKSFS, DAF calls are followed by tests of FAILED()
!>              in order to ensure that the main state loop terminates.
!>
!>           7) In SPKSFS, a subscript bound violation in a loop
!>              termination test was corrected.
!>
!>        The "re-use interval" feature was introduced to improve speed
!>        in the case where repeated, consecutive requests are satisfied
!>        by the same segment. For each body, the associated re-use
!>        interval marks the time interval containing the previous
!>        request time for which the previously returned segment provides
!>        the  highest-priority data available.
!>
!>        The segment list cost algorithm was modified slightly:
!>        the contribution of a file search to the cost of a list
!>        is included only when the file search is completed. The
!>        cost of finding the re-use interval is accounted for when
!>        unbuffered searches are required.
!>
!>        The file table size has been increased to 1000, in order
!>        to take advantage of the DAF system's new ability to load
!>        1000 files.
!>
!>        The body table size has been increased to 200 in order to
!>        decrease the chance of thrashing due to swapping segment
!>        lists for different bodies.
!>
!>        Various small updates and corrections were made to the
!>        comments throughout the file.
!>
!>        In order to simplify the source code, the in-line singly
!>        linked list implementation of the segment table has been
!>        replaced by an implementation relying on the SPICELIB
!>        doubly linked list routines.
!>
!>### Implementation notes
!>
!>     Constants used in the doubly linked list structure:
!>
!>     The file table contains the handle and file number of each file
!>     that has been loaded for use with the SPK readers. File
!>     numbers begin at one, and are incremented until they reach a
!>     value of INTMAX() - 1, at which point they are mapped to the
!>     range 1:NFT, where NFT is the number of loaded SPK files.
!>
!>     (A file number is similar to a file handle, but it is assigned
!>     and used exclusively by this module. The purpose of file numbers
!>     is to keep track of the order in which files are loaded and the
!>     order in which they are searched.)
!>
!>     All names begin with FT.
!>
!>        HAN      Handle
!>        NUM      File number
!>
!>     NFT is the number of files that have been loaded. NEXT is
!>     incremented whenever a new file is loaded to give the file
!>     number of the file. FINDEX is the index of whatever file is
!>     of current interest at any given time.
!>
!>     New files are added at the end of the table. As files are
!>     removed, succeeding files are moved forward to take up the
!>     slack. This keeps the table ordered by file number.
!>
!>     The body table contains the beginning of the list of the stored
!>     segments for each body, and the expense at which that list
!>     was constructed. (The expense of a body list is the number of
!>     segment descriptors examined during the construction of the list.)
!>     It also contains the highest and lowest file numbers searched
!>     during the construction of the list.
!>
!>     For each body, the time bounds of the "re-use interval" of the
!>     last segment found are stored.  This interval is the maximal
!>     interval containing the epoch of the last request for data for
!>     this body, such that the interval is not masked by higher-priority
!>     segments.  The handle, segment descriptor, and segment identifier
!>     returned on the last request are also stored.
!>
!>     All names begin with BT.
!>
!>        BOD      Body
!>        EXP      Expense
!>        HFS      Highest file (number) searched
!>        LFS      Lowest  file (number) searched
!>        BEG      Beginning of segment list
!>        LB       Lower bound of the re-use interval of
!>                 previous segment returned.
!>        UB       Upper bound of the re-use interval of
!>                 previous segment returned.
!>        PRVD     Previous descriptor returned.
!>        PRVI     Previous segment identifier returned.
!>        PRVH     Previous handle returned.
!>        CHKP     Logical indicating that previous segment should
!>                 be checked to see whether it satisfies a request.
!>        RUEX     Expense of the re-use interval.
!>
!>     NBT is the number of bodies for which segments are currently
!>     being stored in the table. BINDEX is the index of whatever
!>     body is of current interest at any given time.
!>
!>     New bodies are added at the end of the table. As bodies are
!>     removed, the last body is moved forward to take up the slack.
!>     This keeps the entries in the table contiguous.
!>
!>     The segment table contains the handle, descriptor, and identifier
!>     for each segment that has been found so far.
!>
!>     The segment table is implemented as a set of arrays indexed by
!>     a SPICE doubly linked list structure.  For each body in the
!>     body table, there is a segment table list; each node of a list
!>     points to data associated with a segment.  In each list, the head
!>     node corresponds to the highest-priority segment in that list,
!>     and segment priority decreases in the forward direction.
!>
!>     All names begin with ST.
!>
!>        POOL     Doubly linked list pool.
!>        HAN      Handle
!>        DES      Descriptor
!>        IDNT     Identifier
!>
!>     New segments are added to the front or end of a body list
!>     as appropriate, according to the rules spelled out under
!>     entry point SPKSFS.
!>
!>     Nobody has any business calling SPKBSR directly.
!>
!>## PCKBSR (PCK, buffer segments for readers)
!>
!> PEPPER: SPICELIB's umbrella routine PCKBSR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_pckbsr. Its documentation follows.
!>
!>### Abstract
!>
!>     Load and unload PCK binary files for use by the readers.
!>     Buffer segments for readers.
!>
!>### Detailed Input
!>
!>     FNAME    is the name of an PCK file to be loaded.
!>
!>     HANDLE   on input, is the handle of an PCK file to be
!>              unloaded.
!>
!>     BODY     is the NAIF integer code of an ephemeris object,
!>              typically a solar system body.
!>
!>     ET       is a time, in seconds past the epoch J2000 TDB.
!>
!>### Detailed Output
!>
!>     HANDLE   on output, is the handle of the binary PCK file
!>              containing a located segment.
!>
!>     DESCR    is the descriptor of a located segment.
!>
!>     IDENT    is the identifier of a located segment.
!>
!>     FOUND    is a logical flag indicating whether a segment meeting
!>              the search criteria was found. FOUND will have the
!>              value .TRUE. if an appropriate segment was found during
!>              the search; it will have the value of .FALSE.
!>              otherwise. If FOUND has the value .FALSE., then either
!>              an appropriate segment could not be found in any of the
!>              loaded files or there were no PCK kernel files loaded
!>              when the request for a segment was made.
!>
!>### Parameters
!>
!>     FTSIZE   is the maximum number of files that may be loaded
!>              by PCKLOF at any given time for use by the PCK readers.
!>
!>     BTSIZE   is the maximum number of bodies whose segments can be
!>              buffered by PCKSFS.
!>
!>     STSIZE   is the maximum number of segments that can be buffered at
!>              any given time by PCKSFS.
!>
!>### Exceptions
!>
!>     1)  If PCKBSR is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points PCKLOF, PCKUOF, and PCKSFS for exceptions
!>         specific to them.
!>
!>### Files
!>
!>     PCK kernel files are indicated by filename before loading
!>     (see PCKLOF) and handle after loading (all other places).
!>
!>### Particulars
!>
!>     PCKBSR serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        PCKLOF       Load PCK binary file.
!>        PCKUOF       Unload PCK binary file.
!>        PCKSFS       Select file and segment.
!>
!>     Before a file can be read by the PCK kernel readers, it must be
!>     loaded by PCKLOF, which among other things, calls routines to
!>     open the specified file.
!>
!>     Multiple files may be loaded for use simultaneously, and a file
!>     need only be loaded once to become a potential search target
!>     for any number of subsequent reads.
!>
!>     Once a PCK kernel file is loaded and opened, it is assigned a file
!>     handle, which is used by the calling program to refer to the file
!>     in all subsequent calls to PCK routines.
!>
!>     A file may be removed from the list of files searched by using
!>     PCKUOF to unload it.
!>
!>     PCKSFS performs the search for segments within a file for the
!>     PCK kernel readers. It searches through the most recently loaded
!>     files first. Within a single file, PCKSFS searches through
!>     the segments in reverse order, beginning with the last segment in
!>     the file. The search stops when the first appropriate segment is
!>     found or all files and segments have been searched without a
!>     match.
!>
!>     PCKSFS buffers information from loaded PCK files to improve access
!>     time by preventing unnecessary file reads during segment searches.
!>
!>### Examples
!>
!>     The numerical results shown for these examples may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     1) Suppose that the data of interest are contained in a PCK file,
!>        and that we want to generate a table containing the
!>        descriptors of the PCK segments, or a message indicating that
!>        no segment was found, for various request times. We are
!>        interested in the data coverage of the segments in the file.
!>
!>        The code example below loads PCK files and performs searches
!>        for various epochs, generating a table containing the segment
!>        descriptors, if found, or a message indicating that a segment
!>        descriptor was not found.
!>
!>        Use the PCK kernel below as input file for the code example.
!>
!>           earth_latest_high_prec.bpc
!>
!>        Example code begins here.
!>
!>              PROGRAM PCKBSR_EX1
!>              IMPLICIT NONE
!>
!>        C
!>        C     Local parameters
!>        C
!>              DOUBLE PRECISION      DELTA
!>              PARAMETER           ( DELTA  = 50000000.D0 )
!>
!>              INTEGER               BODY
!>              PARAMETER           ( BODY   = 3000        )
!>
!>              INTEGER               DESCSZ
!>              PARAMETER           ( DESCSZ = 5           )
!>
!>              INTEGER               IDSIZE
!>              PARAMETER           ( IDSIZE = 40          )
!>
!>              INTEGER               NEPOCH
!>              PARAMETER           ( NEPOCH = 10          )
!>
!>        C
!>        C     Local variables
!>        C
!>              CHARACTER*(IDSIZE)    SEGID
!>
!>              DOUBLE PRECISION      BEGET
!>              DOUBLE PRECISION      DESCR ( DESCSZ )
!>              DOUBLE PRECISION      ENDET
!>              DOUBLE PRECISION      ET
!>
!>              INTEGER               BADDR
!>              INTEGER               BODYID
!>              INTEGER               EADDR
!>              INTEGER               FRAMID
!>              INTEGER               HANDLE
!>              INTEGER               I
!>              INTEGER               PCKHDL
!>              INTEGER               PCKTYP
!>
!>              LOGICAL               FOUND
!>
!>        C
!>        C     Load the PCK file.
!>        C
!>              CALL PCKLOF ( 'earth_latest_high_prec.bpc', PCKHDL )
!>
!>        C
!>        C     Search for segments using evenly spaced epochs.
!>        C
!>              ET = -86400.D0
!>
!>              DO I = 1, NEPOCH
!>
!>                 WRITE(*,*) 'Epoch = ', ET
!>
!>        C
!>        C        Locate the applicable segment (handle and
!>        C        descriptor).
!>        C
!>                 CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
!>
!>                 IF ( FOUND ) THEN
!>
!>        C
!>        C           Unpack the segment.
!>        C
!>                    CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
!>             .                    BEGET, ENDET,  BADDR,  EADDR  )
!>
!>                    WRITE(*,*) '   Segment ID: ', SEGID
!>                    WRITE(*,*) '   Body ID   : ', BODYID
!>                    WRITE(*,*) '   Frame ID  : ', FRAMID
!>                    WRITE(*,*) '   PCK Type  : ', PCKTYP
!>                    WRITE(*,*) '   Start ET  : ', BEGET
!>                    WRITE(*,*) '   End ET    : ', ENDET
!>
!>                 ELSE
!>
!>                    WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
!>
!>                 END IF
!>
!>                 WRITE(*,*) ' '
!>
!>        C
!>        C        Increment the epoch.
!>        C
!>                 ET = ET + DELTA
!>
!>              END DO
!>
!>        C
!>        C     Unload the PCK file.
!>        C
!>              CALL PCKUOF ( PCKHDL )
!>
!>              END
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, the output was:
!>
!>         Epoch =   -86400.000000000000
!>            ***** SEGMENT NOT FOUND *****
!>
!>         Epoch =    49913600.000000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :   -43135.816087188054
!>            End ET    :    86343752.179112613
!>
!>         Epoch =    99913600.000000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    86343752.179112613
!>            End ET    :    172730640.17431438
!>
!>         Epoch =    149913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    86343752.179112613
!>            End ET    :    172730640.17431438
!>
!>         Epoch =    199913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    172730640.17431438
!>            End ET    :    259117528.16951615
!>
!>         Epoch =    249913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    172730640.17431438
!>            End ET    :    259117528.16951615
!>
!>         Epoch =    299913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    259117528.16951615
!>            End ET    :    345504416.16470283
!>
!>         Epoch =    349913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    345504416.16470283
!>            End ET    :    431891304.15988630
!>
!>         Epoch =    399913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    345504416.16470283
!>            End ET    :    431891304.15988630
!>
!>         Epoch =    449913600.00000000
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    431891304.15988630
!>            End ET    :    518278192.15506977
!>
!>     2) In this example multiple PCK files are loaded and searched for
!>        segments.
!>
!>        Use the PCK kernel below as the first input file for the code
!>        example.
!>
!>           earth_latest_high_prec.bpc
!>
!>        Use the PCK kernel below as the second input file for the code
!>        example.
!>
!>           earth_720101_070426.bpc
!>
!>        Example code begins here.
!>
!>              PROGRAM PCKBSR_EX2
!>              IMPLICIT NONE
!>
!>        C
!>        C     Local parameters
!>        C
!>              DOUBLE PRECISION      DELTA
!>              PARAMETER           ( DELTA  = 50000000.D0 )
!>
!>              INTEGER               BODY
!>              PARAMETER           ( BODY   = 3000        )
!>
!>              INTEGER               DESCSZ
!>              PARAMETER           ( DESCSZ = 5           )
!>
!>              INTEGER               IDSIZE
!>              PARAMETER           ( IDSIZE = 40          )
!>
!>              INTEGER               NEPOCH
!>              PARAMETER           ( NEPOCH = 10          )
!>
!>        C
!>        C     Local variables
!>        C
!>              CHARACTER*(IDSIZE)    SEGID
!>
!>              DOUBLE PRECISION      BEGET
!>              DOUBLE PRECISION      DESCR ( DESCSZ )
!>              DOUBLE PRECISION      ENDET
!>              DOUBLE PRECISION      ET
!>
!>              INTEGER               BADDR
!>              INTEGER               BODYID
!>              INTEGER               EADDR
!>              INTEGER               FRAMID
!>              INTEGER               HANDLE
!>              INTEGER               I
!>              INTEGER               PCKHD1
!>              INTEGER               PCKHD2
!>              INTEGER               PCKTYP
!>
!>              LOGICAL               FOUND
!>
!>        C
!>        C     Load the PCK files.
!>        C
!>              CALL PCKLOF ( 'earth_latest_high_prec.bpc', PCKHD1 )
!>              CALL PCKLOF ( 'earth_720101_070426.bpc',    PCKHD2 )
!>
!>        C
!>        C     Search for segments using evenly spaced epochs.
!>        C
!>              ET = -86400.D0
!>
!>              DO I = 1, NEPOCH
!>
!>                 WRITE(*,*) 'Epoch = ', ET
!>
!>        C
!>        C        Locate the applicable segment (handle and
!>        C        descriptor).
!>        C
!>                 CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
!>
!>                 IF ( FOUND ) THEN
!>
!>        C
!>        C           Unpack the segment.
!>        C
!>                    CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
!>             .                    BEGET, ENDET,  BADDR,  EADDR  )
!>
!>                    WRITE(*,*) '   Handle    : ', HANDLE
!>                    WRITE(*,*) '   Segment ID: ', SEGID
!>                    WRITE(*,*) '   Body ID   : ', BODYID
!>                    WRITE(*,*) '   Frame ID  : ', FRAMID
!>                    WRITE(*,*) '   PCK Type  : ', PCKTYP
!>                    WRITE(*,*) '   Start ET  : ', BEGET
!>                    WRITE(*,*) '   End ET    : ', ENDET
!>
!>                 ELSE
!>
!>                    WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
!>
!>                 END IF
!>
!>                 WRITE(*,*) ' '
!>
!>        C
!>        C        Increment the epoch.
!>        C
!>                 ET = ET + DELTA
!>
!>              END DO
!>
!>        C
!>        C     Unload the PCK files.
!>        C
!>              CALL PCKUOF ( PCKHD1 )
!>              CALL PCKUOF ( PCKHD2 )
!>
!>              END
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, the output was:
!>
!>         Epoch =   -86400.000000000000
!>            Handle    :            2
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :   -74415076.797098771
!>            End ET    :    11979698.742793124
!>
!>         Epoch =    49913600.000000000
!>            Handle    :            2
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    11979698.742793124
!>            End ET    :    98374474.282683983
!>
!>         Epoch =    99913600.000000000
!>            Handle    :            2
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    98374474.282683983
!>            End ET    :    184769249.82257757
!>
!>         Epoch =    149913600.00000000
!>            Handle    :            2
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    98374474.282683983
!>            End ET    :    184769249.82257757
!>
!>         Epoch =    199913600.00000000
!>            Handle    :            2
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    184769249.82257757
!>            End ET    :    230817665.18534085
!>
!>         Epoch =    249913600.00000000
!>            Handle    :            1
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    172730640.17431438
!>            End ET    :    259117528.16951615
!>
!>         Epoch =    299913600.00000000
!>            Handle    :            1
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    259117528.16951615
!>            End ET    :    345504416.16470283
!>
!>         Epoch =    349913600.00000000
!>            Handle    :            1
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    345504416.16470283
!>            End ET    :    431891304.15988630
!>
!>         Epoch =    399913600.00000000
!>            Handle    :            1
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    345504416.16470283
!>            End ET    :    431891304.15988630
!>
!>         Epoch =    449913600.00000000
!>            Handle    :            1
!>            Segment ID: Earth PCK, ITRF93 Frame
!>            Body ID   :         3000
!>            Frame ID  :           17
!>            PCK Type  :            2
!>            Start ET  :    431891304.15988630
!>            End ET    :    518278192.15506977
!>
!>### Restrictions
!>
!>     1)  If Fortran I/O errors occur while searching a loaded PCK
!>         file, the internal state of this suite of routines may
!>         be corrupted. It may be possible to correct the state
!>         by unloading the pertinent PCK files and then re-loading
!>         them.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     B.V. Semenov       (JPL)
!>     E.D. Wright        (JPL)
!>     K.S. Zukor         (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR) (BVS) (NJB)
!>
!>        Updated entry point PCKSFS to always initialize FOUND.
!>
!>        Edited the header of the PCKBSR umbrella and all its entry
!>        points to comply with NAIF standard. Added complete code
!>        examples from existing fragments.
!>
!>        Changed SAVE statements to save each variable individually.
!>
!>-    SPICELIB Version 2.0.1, 30-JAN-2017 (NJB)
!>
!>        Corrected various spelling errors within comments.
!>
!>-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
!>
!>        Updated segment pool initialization condition in entry
!>        point PCKLOF so that the pool is initialized only if the file
!>        table is empty.
!>
!>-    SPICELIB Version 1.4.0, 03-JAN-2014 (BVS) (EDW)
!>
!>        Minor edits to $Procedure; clean trailing whitespace.
!>
!>        Increased FTSIZE (from 1000 to 5000).
!>
!>        Increased STSIZE (from 100 to 5000).
!>
!>-    SPICELIB Version 1.3.0, 01-MAR-2011 (NJB)
!>
!>        Bug fix:
!>
!>           In the PCKSFS 'MAKE ROOM' state, when the suspended
!>           activity is 'ADD TO FRONT' and no segment table room is
!>           available, the body table's pointer to the current segment
!>           list is now set to null. Previously the pointer was allowed
!>           to go stale.
!>
!>-    SPICELIB Version 1.2.0, 08-SEP-2005 (NJB)
!>
!>        Updated to remove non-standard use of duplicate arguments
!>        in MOVED calls in entry points PCKUOF and PCKSFS.
!>
!>-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
!>
!>        Bug fixes:
!>
!>           1) When a segment list is freed because the entire list
!>              is contributed by a single PCK file, and the list is
!>              too large to be buffered, the corresponding body table
!>              pointer is now set to null.
!>
!>           2) An algorithm change has eliminated a bug caused by not
!>              updating the current body index when body table entries
!>              having empty segment lists were compressed out of the
!>              body table. Previously the body table pointer BINDEX
!>              could go stale after the compression.
!>
!>           3) When a already loaded kernel is re-opened with DAFOPR,
!>              it now has its link count reset to 1 via a call to
!>              DAFCLS.
!>
!>           4) The load routine PCKLOF now resets all file numbers when
!>              the next file number reaches INTMAX()-1, thereby
!>              avoiding arithmetic overflow.
!>
!>           5) The unload routine PCKUOF now calls RETURN() on entry and
!>              returns if so directed.
!>
!>           6) In PCKSFS, DAF calls are followed by tests of FAILED()
!>              in order to ensure that the main state loop terminates.
!>
!>           7) In PCKSFS, a subscript bound violation in a loop
!>              termination test was corrected.
!>
!>        The "re-use interval" feature was introduced to improve speed
!>        in the case where repeated, consecutive requests are satisfied
!>        by the same segment. For each body, the associated re-use
!>        interval marks the time interval containing the previous
!>        request time for which the previously returned segment provides
!>        the  highest-priority data available.
!>
!>        The segment list cost algorithm was modified slightly:
!>        the contribution of a file search to the cost of a list
!>        is included only when the file search is completed. The
!>        cost of finding the re-use interval is accounted for when
!>        unbuffered searches are required.
!>
!>        The file table size has been increased to 1000, in order
!>        to take advantage of the DAF system's new ability to load
!>        1000 files.
!>
!>        Various small updates and corrections were made to the
!>        comments throughout the file.
!>
!>        In order to simplify the source code, the in-line singly
!>        linked list implementation of the segment table has been
!>        replaced by an implementation relying on the SPICELIB
!>        doubly linked list routines.
!>
!>-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
!>
!>        This differs only slightly from the SPKXXX code.
!>        The main difference is that the SFS subroutine returns
!>        FOUND = .FALSE. if no files are found, rather than returning
!>        an error.
!>
!>### Implementation notes
!>
!>     Constants used in the doubly linked list structure:
!>
!>     The file table contains the handle and file number of each file
!>     that has been loaded for use with the PCK readers. File
!>     numbers begin at one, and are incremented until they reach a
!>     value of INTMAX() - 1, at which point they are mapped to the
!>     range 1:NFT, where NFT is the number of loaded PCK files.
!>
!>     (A file number is similar to a file handle, but it is assigned
!>     and used exclusively by this module. The purpose of file numbers
!>     is to keep track of the order in which files are loaded and the
!>     order in which they are searched.)
!>
!>     All names begin with FT.
!>
!>        HAN      Handle
!>        NUM      File number
!>
!>     NFT is the number of files that have been loaded. NEXT is
!>     incremented whenever a new file is loaded to give the file
!>     number of the file. FINDEX is the index of whatever file is
!>     of current interest at any given time.
!>
!>     New files are added at the end of the table. As files are
!>     removed, succeeding files are moved forward to take up the
!>     slack. This keeps the table ordered by file number.
!>
!>     The body table contains the beginning of the list of the stored
!>     segments for each body, and the expense at which that list
!>     was constructed. (The expense of a body list is the number of
!>     segment descriptors examined during the construction of the list.)
!>     It also contains the highest and lowest file numbers searched
!>     during the construction of the list.
!>
!>     For each body, the time bounds of the "re-use interval" of the
!>     last segment found are stored.  This interval is the maximal
!>     interval containing the epoch of the last request for data for
!>     this body, such that the interval is not masked by higher-priority
!>     segments.  The handle, segment descriptor, and segment identifier
!>     returned on the last request are also stored.
!>
!>     All names begin with BT.
!>
!>        BOD      Body
!>        EXP      Expense
!>        HFS      Highest file (number) searched
!>        LFS      Lowest  file (number) searched
!>        BEG      Beginning of segment list
!>        LB       Lower bound of the re-use interval of
!>                 previous segment returned.
!>        UB       Upper bound of the re-use interval of
!>                 previous segment returned.
!>        PRVD     Previous descriptor returned.
!>        PRVI     Previous segment identifier returned.
!>        PRVH     Previous handle returned.
!>        CHKP     Logical indicating that previous segment should
!>                 be checked to see whether it satisfies a request.
!>        RUEX     Expense of the re-use interval.
!>
!>     NBT is the number of bodies for which segments are currently
!>     being stored in the table. BINDEX is the index of whatever
!>     body is of current interest at any given time.
!>
!>     New bodies are added at the end of the table. As bodies are
!>     removed, the last body is moved forward to take up the slack.
!>     This keeps the entries in the table contiguous.
!>
!>     The segment table contains the handle, descriptor, and identifier
!>     for each segment that has been found so far.
!>
!>     The segment table is implemented as a set of arrays indexed by
!>     a SPICE doubly linked list structure.  For each body in the
!>     body table, there is a segment table list; each node of a list
!>     points to data associated with a segment.  In each list, the head
!>     node corresponds to the highest-priority segment in that list,
!>     and segment priority decreases in the forward direction.
!>
!>     All names begin with ST.
!>
!>        POOL     Doubly linked list pool.
!>        HAN      Handle
!>        DES      Descriptor
!>        IDNT     Identifier
!>
!>     New segments are added to the front or end of a body list
!>     as appropriate, according to the rules spelled out under
!>     entry point PCKSFS.
!>
!>     Nobody has any business calling PCKBSR directly.
!>
!>## CKBSR (C-kernel, buffer segments for readers)
!>
!> PEPPER: SPICELIB's umbrella routine CKBSR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_ckbsr. Its documentation follows.
!>
!>### Abstract
!>
!>     Load and unload files for use by the readers. Buffer segments
!>     for readers.
!>
!>### Detailed Input
!>
!>     FNAME    is the name of a binary C-kernel file to be loaded.
!>
!>     HANDLE   on input, is the handle of a binary C-kernel file to be
!>              unloaded.
!>
!>     INST     is the NAIF ID of an instrument.
!>
!>     SCLKDP   is an encoded spacecraft clock time.
!>
!>     TOL      is a time tolerance, measured in the same units as
!>              encoded spacecraft clock.
!>
!>     NEEDAV   indicates whether or not angular velocity data are
!>              required.
!>
!>              If .TRUE., only segments containing pointing and angular
!>              velocity data will be checked. If .FALSE., segments
!>              containing just pointing data will also be considered.
!>
!>### Detailed Output
!>
!>     HANDLE   on output, is the handle of the C-kernel file
!>              containing a located segment.
!>
!>     DESCR    is the packed descriptor of a located segment.
!>
!>     SEGID    is the identifier of a located segment.
!>
!>     FOUND    indicates whether a requested segment was found or not.
!>
!>### Parameters
!>
!>     FTSIZE   is the maximum number of pointing files that can
!>              be loaded by CKLPF at any given time for use by the
!>              readers.
!>
!>     ITSIZE   is the maximum number of instruments whose segments
!>              are buffered by CKSNS.
!>
!>     STSIZE   is the maximum number of segments that can be buffered
!>              at any given time by CKSNS.
!>
!>### Exceptions
!>
!>     1)  If CKBSR is called directly, the error SPICE(CKBOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points CKLPF, CKUPF, CKBSS, and CKSNS for
!>         exceptions specific to them.
!>
!>### Files
!>
!>     C-kernel pointing files are indicated by filename before loading
!>     (see CKLPF) and handle after loading (all other places).
!>
!>### Particulars
!>
!>     CKBSR serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        CKLPF       Load pointing file.
!>        CKUPF       Unload pointing file.
!>        CKBSS       Begin search for segment.
!>        CKSNS       Select next segment.
!>        CKHAVE      Determine whether or not any CKs are loaded.
!>
!>     Before a file can be read by the C-kernel readers, it must be
!>     loaded by CKLPF, which among other things load the file into
!>     the DAF subsystem.
!>
!>     Up to FTSIZE files may be loaded for use simultaneously, and a
!>     file only has to be loaded once to become a potential search
!>     target for any number of subsequent reads.
!>
!>     Once a C-kernel has been loaded, it is assigned a file
!>     handle, which is used to keep track of the file internally, and
!>     which is used by the calling program to refer to the file in all
!>     subsequent calls to CK routines.
!>
!>     A file may be removed from the list of files for potential
!>     searching by unloading it via a call to CKUPF.
!>
!>     The purpose of entry points CKBSS and CKSNS is to search for
!>     segments in CK files matching certain criteria, which is
!>     established based on CKBSS input arguments INST, SCLKDP, TOL and
!>     NEEDAV. These two routines are used together to search through
!>     all loaded CK files for segments.
!>
!>     CKBSS sets up a search for segments by CKSNS. It records the
!>     instrument and time to be searched for, and whether to require
!>     segments containing angular velocity data. If angular velocity
!>     data are required, only segments containing angular velocity
!>     data will be returned by CKSNS. If angular velocity data are
!>     not required, segments returned by CKSNS may or may not contain
!>     angular velocity data.
!>
!>     CKBSS determines the first task that CKSNS will have to perform
!>     if it is called to get an applicable segment.
!>
!>     CKSNS finds segments matching the search criteria set up by
!>     CKBSS. Last-loaded files get searched first, and individual files
!>     are searched backwards.
!>
!>     A segment matches the CKBSS/CKSNS search criteria when the
!>     following statements are true.
!>
!>        1) INST matches the instrument number for the segment.
!>
!>        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects
!>           the time interval of the segment.
!>
!>        3) If angular velocity data are required, as indicated by
!>           NEEDAV, the segment contains angular velocity data.
!>
!>     When an applicable segment is found, CKSNS returns that segment's
!>     descriptor and identifier, along with the handle of the file
!>     containing the segment.
!>
!>     Subsequent calls to CKSNS continue the search, picking up where
!>     the previous call to this routine left off.
!>
!>     CKSNS uses information on loaded files to manage a buffer
!>     of saved segment descriptors and identifiers. The buffer is used
!>     to speed up access time by minimizing file reads.
!>
!>### Examples
!>
!>     The numerical results shown for this example may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     1) Suppose that pointing data for the Viking Orbiter 2 scan
!>        platform orientation for a certain interval of time are
!>        contained in three separate files, one containing data for the
!>        original SEDR (Supplemental Experiment Data Record) files,
!>        contains the complete set of pointing data and another two
!>        which contain two separate pointing updates based on
!>        reconstruction, one of them containing discrete data, and the
!>        other continuous pointing data.
!>
!>        In the following example, pointing from the C-kernel is
!>        extracted in two different ways for the purpose of comparing
!>        the two updates:
!>
!>        First, the original pointing file and one of the update files
!>        are both loaded and pointing is retrieved for all of the
!>        pictures. The update file is searched through first, and if
!>        no data for the desired picture is located, then the original
!>        file provides the requested pointing.
!>
!>        Then, the first update file is unloaded, the second update
!>        file is loaded, and the same search is performed, as above.
!>
!>        Use the CK kernel below to load the Viking Orbiter 2 scan
!>        platform orientation containing a combination of data from
!>        SEDR files.
!>
!>           vo2_sedr_ck2.bc
!>
!>        Use the CK kernel below to load the Viking Orbiter 2 scan
!>        platform orientation discrete data reconstructed during
!>        cartographic image registration by S. Wu, USGS.
!>
!>           vo2_swu.bc
!>
!>        Use the CK kernel below to load the Viking Orbiter 2 scan
!>        platform orientation discrete data reconstructed during
!>        cartographic image registration by S. Wu, USGS, where each
!>        discrete pointing instance was "expanded" into a 2 second
!>        window.
!>
!>           vo2_swu_ck2.bc
!>
!>        Use the meta-kernel shown below to load the required SPICE
!>        kernels.
!>
!>           KPL/MK
!>
!>           File name: ckbsr_ex1.tm
!>
!>           This meta-kernel is intended to support operation of SPICE
!>           example programs. The kernels shown here should not be
!>           assumed to contain adequate or correct versions of data
!>           required by SPICE-based user applications.
!>
!>           In order for an application to use this meta-kernel, the
!>           kernels referenced here must be present in the user's
!>           current working directory.
!>
!>           The names and contents of the kernels referenced
!>           by this meta-kernel are as follows:
!>
!>              File name                     Contents
!>              ---------                     --------
!>              vo2_fict.tsc                  Viking 2 SCLK
!>              naif0012.tls                  Leapseconds
!>
!>           \begindata
!>
!>              KERNELS_TO_LOAD = ( 'vo2_fict.tsc',
!>                                  'naif0012.tls'  )
!>
!>           \begintext
!>
!>           End of meta-kernel
!>
!>        Example code begins here.
!>
!>              PROGRAM CKBSR_EX1
!>              IMPLICIT NONE
!>
!>        C
!>        C     Local parameters.
!>        C
!>              INTEGER               NPICS
!>              PARAMETER           ( NPICS  = 5  )
!>
!>              INTEGER               TIMLEN
!>              PARAMETER           ( TIMLEN = 24 )
!>
!>        C
!>        C     Local variables.
!>        C
!>              INTEGER               HANDLE
!>              INTEGER               HNORIG
!>              INTEGER               HUPDT
!>              INTEGER               UPDATE
!>              INTEGER               INST
!>              INTEGER               SC
!>              INTEGER               I
!>
!>              DOUBLE PRECISION      ET
!>              DOUBLE PRECISION      DESCR    (    5 )
!>              DOUBLE PRECISION      SCLKDP
!>              DOUBLE PRECISION      TOL
!>              DOUBLE PRECISION      CLKOUT
!>              DOUBLE PRECISION      CMAT     ( 3, 3 )
!>              DOUBLE PRECISION      AV       (    3 )
!>
!>              CHARACTER*(TIMLEN)    FDS      ( NPICS )
!>              CHARACTER*(25)        FNAME
!>              CHARACTER*(40)        SEGID
!>              CHARACTER*(16)        OUTFDS
!>              CHARACTER*(14)        TOLSTR
!>              CHARACTER*(25)        UDFILE   (    2 )
!>
!>              LOGICAL               PFOUND
!>              LOGICAL               SFOUND
!>              LOGICAL               NEEDAV
!>
!>        C
!>        C     Set the times for the pictures.
!>        C
!>              DATA                  FDS /
!>             .                 '1976 OCT 31 22:07:21.000',
!>             .                 '1977-JAN-09 18:33:13.707',
!>             .                 '1977 APR 24 11:48:05.000',
!>             .                 '1977 JUN 07 00:13:15.000',
!>             .                 '1977-AUG-07 14:55:12.019' /
!>
!>              UDFILE ( 1 ) = 'vo2_swu.bc'
!>              UDFILE ( 2 ) = 'vo2_swu_ck2.bc'
!>
!>        C
!>        C     The NAIF integer ID codes for the Viking Orbiter 2
!>        C     spacecraft and scan platform on Viking Orbiter 2 are
!>        C     -30 and -30000, respectively.
!>        C
!>              SC           = -30
!>              INST         = -30000
!>
!>        C
!>        C     Load the LSK and Viking 2 SCLK files.
!>        C
!>              CALL FURNSH ( 'ckbsr_ex1.tm' )
!>
!>        C
!>        C     Allow a time tolerance of 500 milliseconds.  Convert
!>        C     the tolerance to 'ticks', the units of encoded
!>        C     spacecraft clock time.
!>        C
!>              TOLSTR  = '0.500'
!>              CALL SCTIKS ( SC, TOLSTR, TOL )
!>
!>        C
!>        C     Don't care about angular velocity data.
!>        C
!>              NEEDAV = .FALSE.
!>
!>        C
!>        C     Load the original CK file first.
!>        C
!>              CALL CKLPF ( 'vo2_sedr_ck2.bc', HNORIG )
!>
!>        C
!>        C     Write banner.
!>        C
!>              WRITE(*,'(A)') '     Input UTC time       '
!>             .            // 'Pointing found in      SCLK time'
!>              WRITE(*,'(A)') '------------------------  '
!>             .            // '-----------------  ----------------'
!>
!>              DO UPDATE = 1, 2
!>        C
!>        C        Load the update file.  Last-loaded files get searched
!>        C        first, so the update file will be searched before
!>        C        the original file.
!>        C
!>                 CALL CKLPF ( UDFILE ( UPDATE ), HUPDT )
!>
!>                 DO I = 1, NPICS
!>
!>        C
!>        C           Encode the character string representation of
!>        C           spacecraft clock time in FDS.
!>        C
!>                    CALL STR2ET ( FDS( I ) , ET  )
!>                    CALL SCE2C  ( SC, ET, SCLKDP )
!>
!>        C
!>        C           Begin a search for this instrument and time, and
!>        C           get the first applicable segment.
!>        C
!>                    CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV  )
!>                    CALL CKSNS ( HANDLE, DESCR,  SEGID, SFOUND  )
!>
!>        C
!>        C           Keep trying candidate segments until a segment can
!>        C           produce a pointing instance within the specified
!>        C           time tolerance of SCLKDP, the encoded spacecraft
!>        C           clock time.
!>        C
!>                    PFOUND = .FALSE.
!>                    DO WHILE (  SFOUND .AND. ( .NOT. PFOUND )  )
!>
!>                       CALL CKPFS ( HANDLE, DESCR,  SCLKDP,
!>             .                      TOL,    NEEDAV, CMAT,
!>             .                      AV,     CLKOUT, PFOUND )
!>
!>                       IF ( PFOUND ) THEN
!>
!>        C
!>        C                 Get the name of the file from whence the
!>        C                 pointing instance came, decode the
!>        C                 spacecraft clock time associated with the
!>        C                 instance, and write the results to the
!>        C                 table.
!>        C
!>                          CALL DAFHFN ( HANDLE, FNAME          )
!>                          CALL SCDECD ( SC,     CLKOUT, OUTFDS )
!>
!>                          WRITE(*,'(A,2X,A17,2X,A)') FDS( I ), FNAME,
!>             .                                       OUTFDS
!>
!>                       ELSE
!>
!>        C
!>        C                 Look for another candidate segment.
!>        C
!>                          CALL CKSNS ( HANDLE, DESCR, SEGID, SFOUND )
!>
!>                       END IF
!>
!>                    END DO
!>
!>                    IF ( .NOT. PFOUND ) THEN
!>
!>                       WRITE(*,'(A)') FDS( I ) // '  pointing not '
!>             .                     // 'found in any file.'
!>
!>                    END IF
!>
!>                 END DO
!>
!>                 WRITE(*,*) ' '
!>
!>        C
!>        C        Unload the update file. The original file stays
!>        C        loaded.
!>        C
!>                 CALL CKUPF  ( HUPDT )
!>
!>              END DO
!>
!>              END
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, the output was:
!>
!>             Input UTC time       Pointing found in      SCLK time
!>        ------------------------  -----------------  ----------------
!>        1976 OCT 31 22:07:21.000  vo2_sedr_ck2.bc    1/0026345241.000
!>        1977-JAN-09 18:33:13.707  vo2_swu.bc         1/0032380394.707
!>        1977 APR 24 11:48:05.000  vo2_sedr_ck2.bc    1/0041428086.000
!>        1977 JUN 07 00:13:15.000  pointing not found in any file.
!>        1977-AUG-07 14:55:12.019  vo2_sedr_ck2.bc    1/0050511313.019
!>
!>        1976 OCT 31 22:07:21.000  vo2_sedr_ck2.bc    1/0026345241.000
!>        1977-JAN-09 18:33:13.707  vo2_swu_ck2.bc     1/0032380394.707
!>        1977 APR 24 11:48:05.000  vo2_sedr_ck2.bc    1/0041428086.000
!>        1977 JUN 07 00:13:15.000  pointing not found in any file.
!>        1977-AUG-07 14:55:12.019  vo2_swu_ck2.bc     1/0050511313.019
!>
!>### Restrictions
!>
!>     1)  If Fortran I/O errors occur while searching a loaded CK
!>         file, the internal state of this suite of routines may
!>         be corrupted. It may be possible to correct the state
!>         by unloading the pertinent CK files and then re-loading
!>         them.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     J.M. Lynch         (JPL)
!>     B.V. Semenov       (JPL)
!>     W.L. Taber         (JPL)
!>     R.E. Thurman       (JPL)
!>     I.M. Underwood     (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 5.1.0, 25-OCT-2021 (JDR) (BVS) (NJB)
!>
!>        Increased ITSIZE (from 100 to 5000).
!>
!>        Updated entry point CKSNS to always initialize FOUND.
!>
!>        Edited the header of umbrella routine CKBSR, and all entry
!>        points to comply with NAIF standard. Created complete code
!>        example from existing fragments in CKBSR $Examples section.
!>
!>        Added references to CKHAVE entry point in CKBSR header.
!>
!>        Moved details related to search criteria and conditions to meet
!>        it from the $Detailed_Input to $Particulars.
!>
!>-    SPICELIB Version 5.0.1, 30-JAN-2017 (NJB)
!>
!>        Corrected various spelling errors within comments.
!>
!>-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
!>
!>        Updated segment pool initialization condition in entry
!>        point CKLPF so that the pool is initialized only if the file
!>        table is empty.
!>
!>-    SPICELIB Version 4.6.0, 13-JUN-2013 (BVS)
!>
!>        Increased FTSIZE (from 1000 to 5000).
!>
!>        Increased STSIZE (from 50000 to 100000).
!>
!>-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
!>
!>        Bug fixes:
!>
!>          1) In the CKSNS 'MAKE ROOM' state, when the
!>             suspended activity is 'ADD TO FRONT' and no segment table
!>             room is available, the instrument table's pointer to the
!>             current segment list is now set to null. Previously the
!>             pointer was allowed to go stale.
!>
!>          2) In CKUPF, the null pointer test used to determine
!>             eligibility for segment list deletion now uses the .LE.
!>             operator instead of the .EQ. operator.
!>
!>-    SPICELIB Version 4.4.0, 07-APR-2010 (NJB)
!>
!>        Increased STSIZE to 50000.
!>
!>-    SPICELIB Version 4.3.1, 28-FEB-2008 (BVS)
!>
!>        Corrected the contents of the $Required_Reading section
!>        of the CKHAVE entry point header.
!>
!>-    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB)
!>
!>        Updated to remove non-standard use of duplicate arguments in
!>        MOVED calls in entry points CKUPF and CKSNS. Replaced header
!>        reference to LDPOOL with reference to FURNSH.
!>
!>-    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB)
!>
!>        Increased STSIZE to 20000.
!>
!>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
!>
!>        Bug fixes:
!>
!>           1) When a segment list is freed because the entire list
!>              is contributed by a single CK file, and the list is
!>              too large to be buffered, the corresponding instrument
!>              table pointer is now set to null.
!>
!>           2) An algorithm change has eliminated a bug caused by not
!>              updating the current instrument index when instrument
!>              table entries having empty segment lists were compressed
!>              out of the instrument table. Previously the instrument
!>              table pointer IINDEX could go stale after the
!>              compression.
!>
!>           3) When a already loaded kernel is re-opened with DAFOPR,
!>              it now has its link count reset to 1 via a call to
!>              DAFCLS.
!>
!>           4) The load routine CKLPF now resets all file numbers when
!>              the next file number reaches INTMAX()-1, thereby
!>              avoiding arithmetic overflow.
!>
!>           5) The unload routine CKUPF now calls RETURN() on entry and
!>              returns if so directed.
!>
!>           6) In CKSNS, DAF calls are followed by tests of FAILED()
!>              in order to ensure that the main state loop terminates.
!>
!>        The "re-use interval" feature was introduced to improve speed
!>        in the case where repeated, consecutive requests are satisfied
!>        by the same segment.
!>
!>        The segment list cost algorithm was modified slightly:
!>        the contribution of a file search to the cost of a list
!>        is included only when the file search is completed. The
!>        cost of finding the re-use interval is accounted for when
!>        unbuffered searches are required.
!>
!>        The file table size has been increased to 1000, in order
!>        to take advantage of the DAF system's new ability to load
!>        1000 files.
!>
!>        The instrument table size has been increased to 100 in order
!>        to decrease the chance of thrashing due to swapping segment
!>        lists for different bodies.
!>
!>        Various small updates and corrections were made to the
!>        comments throughout the file.
!>
!>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
!>
!>        Added the Entry point CKHAVE
!>
!>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
!>
!>        The parameter STSIZE was increased from 1000 to 4000 to
!>        avoid the buffering error that exists in the CKBSR.
!>
!>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!>
!>     1) When loading a file, CKLPF now checks if the file table is
!>        full only after determining that the file is not currently
!>        loaded. Previously, if the file table was full and an attempt
!>        was made to reload a file, an error was signaled. A new
!>        exception was added as a result of this change.
!>
!>     2) A bug in the way that CKLPF and CKUPF clean up the instrument
!>        tables after a file is unloaded was fixed.
!>
!>     3) Variable declarations were added to the example program
!>        so that it can now be compiled.
!>
!>     4) The length of the elements in the array of segment
!>        identifiers ( STIDNT ) was changed from 56 to 40.
!>
!>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
!>
!>        An initial value was assigned to the variable STATUS so
!>        that an error will be signaled if CKSNS is called
!>        without CKBSS ever having been called to initiate the
!>        search.
!>
!>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
!>
!>### Revisions
!>
!>-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
!>
!>        Bug fixes:
!>
!>          1) In the CKSNS 'MAKE ROOM' state, when the
!>             suspended activity is 'ADD TO FRONT' and no segment table
!>             room is available, the instrument table's pointer to the
!>             current segment list is now set to null. Previously the
!>             pointer was allowed to go stale.
!>
!>          2) In CKUPF, the null pointer test used to determine
!>             eligibility for segment list deletion now uses the .LE.
!>             operator instead of the .EQ. operator.
!>
!>-    SPICELIB Version 4.3.0, 23-OCT-2005 (NJB)
!>
!>        Updated to remove non-standard use of duplicate arguments in
!>        MOVED calls in entry points CKUPF and CKSNS. Replaced header
!>        reference to LDPOOL with reference to FURNSH.
!>
!>-    SPICELIB Version 4.2.0, 30-DEC-2004 (NJB)
!>
!>        Increased STSIZE to 20000.
!>
!>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
!>
!>        Bug fixes:
!>
!>           1) When a segment list is freed because the entire list
!>              is contributed by a single CK file, and the list is
!>              too large to be buffered, the corresponding instrument
!>              table pointer is now set to null.
!>
!>           2) An algorithm change has eliminated a bug caused by not
!>              updating the current instrument index when instrument
!>              table entries having empty segment lists were compressed
!>              out of the instrument. Previously the instrument table
!>              pointer IINDEX could go stale after the compression.
!>
!>           3) When a already loaded kernel is re-opened with DAFOPR,
!>              it now has its link count reset to 1 via a call to
!>              DAFCLS.
!>
!>           4) The load routine CKLPF now resets all file numbers when
!>              the next file number reaches INTMAX()-1, thereby
!>              avoiding arithmetic overflow.
!>
!>           5) The unload routine CKUPF now calls RETURN() on entry and
!>              returns if so directed.
!>
!>           6) In CKSNS, DAF calls are followed by tests of FAILED()
!>              in order to ensure that the main state loop terminates.
!>
!>        The "re-use interval" feature was introduced to improve speed
!>        in the case where repeated, consecutive requests are satisfied
!>        by the same segment. For each instrument, the associated
!>        re-use interval marks the time interval containing the previous
!>        request time for which the previously returned segment provides
!>        the  highest-priority data available.
!>
!>        The segment list cost algorithm was modified slightly:
!>        the contribution of a file search to the cost of a list
!>        is included only when the file search is completed. The
!>        cost of finding the re-use interval is accounted for when
!>        unbuffered searches are required.
!>
!>        The file table size has been increased to 1000, in order
!>        to take advantage of the DAF system's new ability to load
!>        1000 files.
!>
!>        The instrument table size has been increased to 100 in order
!>        to decrease the chance of thrashing due to swapping segment
!>        lists for different instruments.
!>
!>        Various small updates and corrections were made to the
!>        comments throughout the file.
!>
!>        In order to simplify the source code, the in-line singly
!>        linked list implementation of the segment table has been
!>        replaced by an implementation relying on the SPICELIB
!>        doubly linked list routines.
!>
!>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
!>
!>        1) When loading a file, CKLPF now checks if the file table is
!>           full only after determining that the file is not currently
!>           loaded. Previously, if the file table was full and an
!>           attempt was made to reload a file, an error was signaled. A
!>           new exception was added as a result of this change.
!>
!>        2) A bug in the way that CKLPF and CKUPF clean up the
!>           instrument tables after a file is unloaded was fixed.
!>
!>        3) Variable declarations were added to the example program
!>           so that it can now be compiled.
!>
!>        4) The length of the elements in the array of segment
!>           identifiers ( STIDNT ) was changed from 56 to 40.
!>
!>-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
!>
!>        An initial value was assigned to the variable STATUS so
!>        that an error will be signaled if CKSNS is called
!>        without CKBSS ever having been called to initiate the
!>        search.
!>
!>-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
!>
!>        The following changes were made as a result of the
!>        NAIF CK Code and Documentation Review:
!>
!>           1) The variable SCLK  was changed to SCLKDP.
!>           2) The variable IDENT was changed to SEGID.
!>           3) The parameterized values for FTSIZE and ITSIZE were
!>               increased from 5 to 20.
!>           4) The parameterized value for STSIZE was increased from 100
!>              to 1000.
!>           5) The local variables INTDES and DPDES were changed to
!>              ICD and DCD.
!>           6) The extended SAVE statement was broken in to single
!>              SAVE statements.
!>           7) Header and internal documentation was corrected and
!>              updated.
!>
!>-    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU)
!>
!>### Implementation notes
!>
!>        ND         is the number of double precision components in an
!>                   unpacked C-kernel descriptor.
!>
!>        NI         is the number of integer components in an unpacked
!>                   C-kernel descriptor.
!>
!>        DSCSIZ     is the number of components in a packed C-kernel
!>                   descriptor.  All DAF summaries have this formulaic
!>                   relationship between the number of its integer and
!>                   double precision components and the number of packed
!>                   components.
!>
!>     Constants used in the doubly linked list structure:
!>
!>     The file table contains the handle and file number of each file
!>     that has been loaded for use with the CK readers. File
!>     numbers begin at one, and are incremented until they reach a
!>     value of INTMAX() - 1, at which point they are mapped to the
!>     range 1:NFT, where NFT is the number of loaded CK files.
!>
!>     A file number is similar to a file handle, but it is assigned
!>     and used exclusively by this module. The purpose of file numbers
!>     is to keep track of the order in which files are loaded and the
!>     order in which they are searched.
!>
!>     All names begin with FT.
!>
!>        HAN      Handle
!>        NUM      File number
!>
!>     NFT is the number of currently loaded CK files. NEXT is
!>     incremented whenever a new file is loaded to give the file
!>     number for that file. FINDEX is the index of whatever file is
!>     of current interest.
!>
!>     New files are added at the end of the table. As files are
!>     removed, succeeding files are moved forward to take up the
!>     slack. This keeps the table ordered by file number.
!>
!>     The instrument table contains the beginning of the list of the
!>     stored segments for each spacecraft/instrument pair, and the
!>     expense at which that list was constructed. (The expense of an
!>     instrument list is the number of segment descriptors examined
!>     during the construction of the list.) It also contains the
!>     highest and lowest file numbers searched during the construction
!>     of the list.
!>
!>     For each instrument, the time bounds of the "re-use interval"
!>     of the last segment found are stored.  This interval is the
!>     maximal interval containing the epoch of the last request for
!>     data for this instrument, such that the interval is not masked
!>     by higher-priority segments.  The handle, segment descriptor,
!>     and segment identifier returned on the last request are also
!>     stored.
!>
!>     The reuse-interval is computed without regard to presence of
!>     angular velocity:  all segments seen while searching for
!>     a segment satisfying a request are used to define the bounds
!>     of the re-use interval.
!>
!>     Re-use intervals are defined on the *first* search following
!>     a setup call to CKBSS.  If a search is resumed (multiple calls
!>     to CKSNS are made consecutively), the re-use interval becomes
!>     invalid after the first CKSNS call.
!>
!>     All names begin with IT.
!>
!>        INS      Spacecraft/instrument number
!>        EXP      Expense
!>        HFS      Highest file (number) searched
!>        LFS      Lowest  file (number) searched
!>        BEG      Beginning of segment list
!>        LB       Lower bound of effective coverage interval of
!>                 previous segment returned.
!>        UB       Upper bound of effective coverage interval of
!>                 previous segment returned.
!>        PRVD     Previous descriptor.
!>        PRVF     Previous descriptor angular velocity flag.  Angular
!>                 velocity is present when ITPRVF is non-zero.
!>        PRVI     Previous segment identifier returned.
!>        PRVH     Previous handle returned.
!>        CHKP     Logical indicating that previous segment should
!>                 be checked to see whether it satisfies a request.
!>        RUEX     Expense of the re-use interval.
!>
!>     NIT is the number of instruments for which segments are currently
!>     being stored in the table. IINDEX is the index of whatever
!>     instrument is of current interest at any given time.
!>
!>     New instruments are added at the end of the table. As instruments
!>     are removed, the last instrument is moved forward to take up the
!>     slack. This keeps the entries in the table contiguous.
!>
!>     The segment table contains the handle, descriptor, and identifier
!>     for each segment that has been found so far.
!>
!>     The segment table is implemented as a set of arrays indexed by
!>     a SPICE doubly linked list structure.  For each instrument
!>     in the instrument table, there is a segment table list; each
!>     node of a list points to data associated with a segment.  In
!>     each list, the head node corresponds to the highest-priority
!>     segment in that list, and segment priority decreases in the
!>     forward direction.
!>
!>     All names begin with ST.
!>
!>        IDNT     Identifier
!>        DCD      Double Precision component of descriptor
!>        HAN      Handle
!>        ICD      Integer component of descriptor
!>        POOL     Doubly linked list pool.
!>
!>     New segments are added to the front or end of an instrument list
!>     as appropriate, according to the rules spelled out under
!>     entry point CKSNS.
!>
!>     Nobody has any business calling CKBSR directly.
!>
!>## ZZDSKBSR (DSK, buffer segments for readers)
!>
!> PEPPER: SPICELIB's umbrella routine ZZDSKBSR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzdskbsr. Its documentation follows.
!>
!>### Abstract
!>
!>     Load and unload DSK files for use by the readers. Buffer segments
!>     for readers.
!>
!>### Detailed Input
!>
!>     FNAME      is the name of a binary DSK file to be loaded.
!>
!>     HANDLE     on input is the handle of a binary DSK file to be
!>                unloaded.
!>
!>     The purpose of entry points ZZDSKBSS and ZZDSKSNS is to search for
!>     segments in DSK files matching certain criteria.
!>
!>     USRCTR     on input is the value of a DSK loaded kernel counter
!>                maintained by a DSK routine or subsystem.
!>
!>### Detailed Output
!>
!>     HANDLE     on output is the handle of the DSK file
!>                containing a located segment.
!>
!>     USRCTR     on output is the value of a DSK loaded kernel counter
!>                maintained by the ZZDSKBSR subsystem.
!>
!>     UPDATE     is a logical flag indicating whether the state of
!>                the loaded DSK set has changed since the ZZDSKBSR
!>                state counter was equal to a given user counter.
!>
!>     DESCR      is the packed descriptor of a located segment.
!>
!>     FOUND      indicates whether a requested segment was found or not.
!>
!>### Parameters
!>
!>     FTSIZE     is the maximum number of shape files that can
!>                be loaded by ZZDSKLSF at any given time for use by the
!>                readers.
!>
!>     BTSIZE     is the maximum number of bodies whose segments
!>                are buffered by ZZDSKSNS.
!>
!>     STSIZE     is the maximum number of segments that can be buffered
!>                at any given time by ZZDSKSNS.
!>
!>### Exceptions
!>
!>     1) If ZZDSKBSR is called directly, the error SPICE(DSKBOGUSENTRY)
!>        is signaled.
!>
!>     2) See entry points ZZDSKLSF, ZZDSKUPF, ZZDSKBSS, and ZZDSKSNS
!>        for exceptions specific to them.
!>
!>### Files
!>
!>     DSK shape files are indicated by filename before loading
!>     (see ZZDSKLSF) and handle after loading (all other places).
!>
!>### Particulars
!>
!>     ZZDSKBSR serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        ZZDSKLSF       Load shape file.
!>        ZZDSKUPF       Unload shape file.
!>        ZZDSKBSS       Begin search for segment.
!>        ZZDSKSNS       Select next segment.
!>        ZZDSKCHK       Check for change in loaded kernel set.
!>
!>     Before a file can be read by the DSK readers, it must be
!>     loaded by ZZDSKLSF, which among other things load the file into
!>     the DAS subsystem.
!>
!>     Up to FTSIZE files may be loaded for use simultaneously, and a
!>     file only has to be loaded once to become a potential search
!>     target for any number of subsequent reads.
!>
!>     Once a DSK has been loaded, it is assigned a file
!>     handle, which is used to keep track of the file internally, and
!>     which is used by the calling program to refer to the file in all
!>     subsequent calls to DSK routines.
!>
!>     A file may be removed from the list of files for potential
!>     searching by unloading it via a call to ZZDSKUPF.
!>
!>     ZZDSKBSS and ZZDSKSNS are used together to search through loaded
!>     files for segments.
!>
!>     ZZDSKBSS sets up the search.
!>
!>     ZZDSKSNS finds segments matching the search criteria set up by
!>     ZZDSKBSS. Last-loaded files get searched first, and individual
!>     files are searched backwards.
!>
!>     When an applicable segment is found, ZZDSKSNS returns that
!>     segment's descriptor and identifier, along with the handle of the
!>     file containing the segment.
!>
!>     Subsequent calls to ZZDSKSNS continue the search, picking up
!>     where the previous call to this routine left off.
!>
!>     ZZDSKSNS uses information on loaded files to manage a buffer of
!>     saved segment descriptors and identifiers. The buffer is used to
!>     speed up access time by minimizing file reads.
!>
!>### Examples
!>
!>     None.
!>
!>### Restrictions
!>
!>     1) If Fortran I/O errors occur while searching a loaded DSK
!>        file, the internal state of this suite of routines may
!>        be corrupted.  It may be possible to correct the state
!>        by unloading the pertinent DSK files and then re-loading
!>        them.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman   (JPL)
!>     R.E. Thurman   (JPL)
!>     I.M. Underwood (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.1.0, 13-OCT-2021 (NJB) (BVS)
!>
!>        Updated entry point ZZDSKSNS to always initialize FOUND.
!>
!>        Corrected typo in long error message in entry point ZZDSKBSS.
!>        Corrected comments in ZZDSKBSS and in ZZDSKSNS
!>
!>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!>
!>        Updated version info.
!>
!>        31-MAR-2016 (NJB)
!>
!>           Deleted unused variable references.
!>           Added header for ZZDSKCHK.
!>
!>           Cleaned up in-line comments.
!>
!>        19-SEP-2014 (NJB)
!>
!>           Specification change: now treats the target body as the
!>           primary search key. Segments lists are associated with
!>           bodies rather than surfaces.
!>
!>           The input argument SURFID has been replaced with BODYID.
!>
!>           A counter system has been implemented to enable SPICE
!>           routines to detect changes in the state of the loaded
!>           kernel set. The user interface entry point for checking for
!>           state updates in ZZDSKCHK.
!>
!>        21-MAY-2013 (NJB)
!>
!>           Removed debugging WRITE statement from entry point
!>           ZZDSKSNS. Edited headers to remove long lines.
!>
!>        02-APR-2010 (NJB) (RET) (IMU)
!>
!>           Original version.
!>
!>### Revisions
!>
!>     None.
!>
!>### Implementation notes
!>
!>     Constants used in the doubly linked list structure:
!>
!>     DSK loaded kernel set state change counter:
!>
!>     The file table contains the handle and file number of each file
!>     that has been loaded for use with the DSK readers. File
!>     numbers begin at one, and are incremented until they reach a
!>     value of INTMAX() - 1, at which point they are mapped to the
!>     range 1:NFT, where NFT is the number of loaded DSK files.
!>
!>     A file number is similar to a file handle, but it is assigned
!>     and used exclusively by this module. The purpose of file numbers
!>     is to keep track of the order in which files are loaded and the
!>     order in which they are searched.
!>
!>     All names begin with FT.
!>
!>        HAN      Handle
!>        NUM      File number
!>
!>     NFT is the number of currently loaded DSK files. NEXT is
!>     incremented whenever a new file is loaded to give the file
!>     number for that file. FINDEX is the index of whatever file is
!>     of current interest.
!>
!>     New files are added at the end of the table. As files are
!>     removed, succeeding files are moved forward to take up the
!>     slack. This keeps the table ordered by file number.
!>
!>     The body table contains the beginning of the list of the stored
!>     segments for each body and the expense at which that list was
!>     constructed. (The expense of a body list is the number of segment
!>     descriptors examined during the construction of the list.) It
!>     also contains the highest and lowest file numbers searched during
!>     the construction of the list.
!>
!>     All names begin with BT.
!>
!>        BOD      Body ID code
!>        EXP      Expense
!>        HFS      Highest file (number) searched
!>        LFS      Lowest  file (number) searched
!>        BEG      Beginning of segment list
!>
!>     NBT is the number of bodies for which segments are currently
!>     being stored in the table. BINDEX is the index of whatever
!>     body is of current interest at any given time.
!>
!>     New bodies are added at the end of the table. As bodies
!>     are removed, the last body is moved forward to take up the
!>     slack. This keeps the entries in the table contiguous.
!>
!>     The segment table contains the handle, descriptor, and identifier
!>     for each segment that has been found so far.
!>
!>     The segment table is implemented as a set of arrays indexed by
!>     a SPICE doubly linked list structure.  For each body
!>     in the body table, there is a segment table list; each
!>     node of a list points to data associated with a segment.  In
!>     each list, the head node corresponds to the highest-priority
!>     segment in that list, and segment priority decreases in the
!>     forward direction.
!>
!>     All names begin with ST.
!>
!>        DLAD     DLA segment descriptor
!>        DSKD     DSK segment descriptor
!>        HAN      Handle
!>        POOL     Doubly linked list pool.
!>
!>     New segments are added to the front or end of an body list
!>     as appropriate, according to the rules spelled out under
!>     entry point ZZDSKSNS.
!>
!>     Nobody has any business calling ZZDSKBSR directly.
submodule (pepper_instance_module) pepper_segments

   use pepper_segment_index, only: segment_index, segment_reuse, segid_length

   implicit none

contains

      !>## Detailed Input
      !>
      !>     FNAME    is a string containing the name of the file to be loaded.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is an integer handle assigned to the file upon loading.
      !>              Almost every other SPK routine will subsequently use this
      !>              number to refer to the file.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of SPK files that may
      !>              be loaded simultaneously under any circumstances.
      !>              FTSIZE is currently set to match the maximum number
      !>              of DAF files that may be loaded simultaneously.
      !>
      !>## Exceptions
      !>
      !>     1)  If an attempt is made to open more DAF files than is specified
      !>         by the parameter FTSIZE in DAFAH, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If an attempt is made to load more files than is specified by
      !>         the local parameter FTSIZE, and if the DAF system has room to
      !>         load another file, the error SPICE(SPKFILETABLEFULL) is
      !>         signaled. The current setting of FTSIZE does not allow this
      !>         situation to arise: the DAF system will trap the error before
      !>         this routine has the chance.
      !>
      !>## Files
      !>
      !>     A file specified by FNAME, to be loaded. The file is assigned a
      !>     handle by SPKLEF, which will be used by most other routines to
      !>     refer to it.
      !>
      !>## Particulars
      !>
      !>     If there is room for a new file in the file table, SPKLEF creates
      !>     an entry for it and loads the file for reading using DAFOPR.
      !>
      !>## Examples
      !>
      !>     See $Examples in SPKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.2, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 5.0.1, 15-MAR-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
      !>
      !>        Updated segment pool initialization condition in entry
      !>        point SPKLEF so that the pool is initialized only if the file
      !>        table is empty.
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) When an already loaded kernel is opened with DAFOPR,
      !>           it now has its link count reset to 1 via a call to
      !>           DAFCLS.
      !>
      !>        2) This routine now resets all file numbers when
      !>           the next file number reaches INTMAX()-1, thereby avoiding
      !>           arithmetic overflow. The numbers in the file table
      !>           are replaced with consecutive integers in the range
      !>           1 : NFT, such that the ordering of the numbers is not
      !>           changed. The HFS and LFS arrays are updated accordingly.
      !>
      !>        Also, the flags indicating validity of the re-use intervals
      !>        are set to .FALSE. here.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        When loading a file, SPKLEF now checks if the file table is
      !>        full only after determining that the file is not currently
      !>        loaded. Previously, if the file table was full and an attempt
      !>        was made to reload a file, an error was signaled. A new
      !>        exception was added as a result of this change.
      !>
      !>        A bug in the way that SPKLEF and SPKUEF clean up the body
      !>        tables after a file is unloaded was fixed.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) When a loaded kernel is opened with DAFOPR,
      !>           it now has its link count reset to 1 via a call to
      !>           DAFCLS.
      !>
      !>        2) This routine now resets all file numbers when
      !>           the next file number reaches INTMAX()-1, thereby avoiding
      !>           arithmetic overflow. The numbers in the file table
      !>           are replaced with consecutive integers in the range
      !>           1 : NFT, such that the ordering of the numbers is not
      !>           changed. The HFS and LFS arrays are updated accordingly.
      !>           HFS and LFS entries that have gone stale are set to zero.
      !>
      !>        Also, the flags indicating validity of the re-use intervals
      !>        are set to .FALSE. here.
      !>
      !>-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
      !>
      !>        An interim fix to a bug in SPKBSR was made. The parameters
      !>        STSIZE and BTSIZE were increased to be much larger than before
      !>        (from 100 and 20 to 2000 and 40 respectively). This should
      !>        keep the boundary errors experienced by Cassini users from
      !>        occurring again. Version 4.0.0 with a real fix to the
      !>        boundary problem should be installed in SPICELIB by
      !>        October 1995
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        When loading a file, SPKLEF now checks if the file table is
      !>        full only after determining that the file is not currently
      !>        loaded. Previously, if the file table was full and an attempt
      !>        was made to reload a file, an error was signaled. A new
      !>        exception was added as a result of this change.
      !>
      !>        A bug in the way that SPKLEF and SPKUEF clean up the body
      !>        tables after a file is unloaded was fixed.
      !>
      !>        If as the result of loading a file that was previously loaded,
      !>        there are no more segments buffered for a particular body,
      !>        the counter variable for the bodies is no longer incremented.
      !>
      !>        The following code fragment changed:
      !>
      !>           IF ( BTBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NBT = NBT - 1
      !>
      !>           END IF
      !>
      !>           I = I + 1
      !>
      !>        This is the fix:
      !>
      !>           IF ( BTBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NBT = NBT - 1
      !>
      !>           ELSE
      !>
      !>              I = I + 1
      !>
      !>           END IF
      !>
      !>-    Beta Version 1.1.0, 25-JAN-1990 (IMU)
      !>
      !>        If a file that has already been loaded is loaded a second
      !>        (or third or fourth) time, it should be removed from the
      !>        file table, and any segments from the file must be removed
      !>        from the segment lists, just as if the user had unloaded
      !>        the file before loading it again. This means that a single
      !>        file cannot occur more than once in the file table.
      !>
      !>     Standard SPICE error handling.
      module subroutine spklef ( ctx, fname, handle )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the file to be loaded.
      integer(ip), intent(out) :: handle  !! Loaded file's handle.
!
!     PEPPER: the segments of the file are indexed now (SPICELIB found
!     them when a body was first searched for). A file loaded again is
!     searched first, as in SPICELIB.
!
      if ( ctx%failed() ) return
      if ( ctx%spice%spkbsr%index%ndescr == 0 ) &
         call ctx%spice%spkbsr%index%init ( 5_ip, 6_ip )
      call load_daf_file ( ctx, ctx%spice%spkbsr%index, fname, 2_ip, 6_ip, handle )
      return
      end subroutine spklef

      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle assigned to the file upon loading.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Unloading a file that has not been loaded is a no-op.
      !>         No error is signaled.
      !>
      !>## Files
      !>
      !>     The file referred to by HANDLE is unloaded.
      !>
      !>## Particulars
      !>
      !>     A file is removed from consideration by the readers by a call to
      !>     SPKUEF.
      !>
      !>     The file table entry corresponding to the file referenced by
      !>     HANDLE, is removed. Any segment table entry which came from the
      !>     specified file is also deleted.
      !>
      !>     If the file specified by HANDLE is not currently loaded in the
      !>     SPK system, no action is taken.
      !>
      !>## Examples
      !>
      !>     See $Examples in SPKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.1.2, 17-JUN-2021 (JDR)
      !>
      !>        Updated the header to comply with NAIF standard.
      !>
      !>        Removed the reference to SPK required reading from
      !>        $Literature_References section. Extended $Particulars
      !>        section.
      !>
      !>-    SPICELIB Version 4.1.1, 15-MAR-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) This routine now calls RETURN() on entry and
      !>           returns if so directed.
      !>
      !>        Also, the flags indicating validity of those re-use intervals
      !>        whose data comes from the unloaded file are set to .FALSE.
      !>
      !>-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
      !>
      !>        An interim fix to a bug in SPKBSR was made. The parameters
      !>        STSIZE and BTSIZE were increased to be much larger than before
      !>        (from 100 and 20 to 2000 and 40 respectively). This should
      !>        keep the boundary errors experienced by Cassini users from
      !>        occurring again. Version 4.0.0 with a real fix to the
      !>        boundary problem should be installed in SPICELIB by
      !>        October 1995
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        A bug in the way that SPKLEF and SPKUEF clean up the body
      !>        tables after a file is unloaded was fixed.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 02-MAY-1990 (RET)
      !>
      !>        If unloading a file causes all segments in the list for a
      !>        body to go away, delete that body from the body list.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) This routine now calls RETURN() on entry and
      !>           returns if so directed.
      !>
      !>        Also, the flags indicating validity of those re-use intervals
      !>        whose data comes from the unloaded file are set to .FALSE.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        A bug in the way that SPKLEF and SPKUEF clean up the body
      !>        tables after a file is unloaded was fixed.
      !>
      !>        If as the result of unloading a file there are no more
      !>        segments buffered for a particular body, the counter variable
      !>        for the bodies is no longer incremented.
      !>
      !>        The following code fragment changed:
      !>
      !>           IF ( BTBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NBT = NBT - 1
      !>
      !>           END IF
      !>
      !>           I = I + 1
      !>
      !>        This is the fix:
      !>
      !>           IF ( BTBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NBT = NBT - 1
      !>
      !>           ELSE
      !>
      !>              I = I + 1
      !>
      !>           END IF
      !>
      !>-    SPICELIB Version 1.1.0, 2-MAY-1990 (RET)
      !>
      !>        If unloading a file causes all segments in the list for a
      !>        body to go away, delete that body from the body list.
      !>
      !>-    Beta Version 1.1.0, 25-JAN-1990 (IMU)
      !>
      !>        When unloading a file, close it.
      module subroutine spkuef ( ctx, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of file to be unloaded
      if ( ctx%failed() ) return
      if ( ctx%spice%spkbsr%index%ndescr == 0 ) return
      call unload_daf_file ( ctx, ctx%spice%spkbsr%index, handle )
      return
      end subroutine spkuef

      !>## Detailed Input
      !>
      !>     BODY     is the NAIF integer code of an ephemeris object,
      !>              typically a solar system body.
      !>
      !>     ET       is a time, in seconds past the epoch J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the handle of the SPK file containing a located
      !>              segment.
      !>
      !>     DESCR    is the descriptor of a located segment.
      !>
      !>     IDENT    is the identifier of a located segment.
      !>
      !>     FOUND    is a logical flag indicating whether a requested segment
      !>              was found or not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an attempt is made to call SPKSFS when there aren't any
      !>         files loaded, the error SPICE(NOLOADEDFILES) is signaled.
      !>
      !>     2)  If an error occurs while this routine attempts to extract
      !>         segment descriptors from loaded SPK files, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>         Note however that I/O errors occurring during reads of DAF
      !>         double precision records are NOT treated as SPICE errors
      !>         and are not signaled.
      !>
      !>## Files
      !>
      !>     All SPK files loaded by FURNSH or SPKLEF are potential search
      !>     targets for SPKSFS.
      !>
      !>## Particulars
      !>
      !>     This routine finds the highest-priority segment, in any loaded
      !>     SPK file, such that the segment provides data for the specified
      !>     body and epoch.
      !>
      !>## Examples
      !>
      !>     See $Examples in SPKBSR.
      !>
      !>## Restrictions
      !>
      !>     1)  If Fortran I/O errors occur while searching a loaded SPK
      !>         file, the internal state of this suite of routines may
      !>         be corrupted. It may be possible to correct the state
      !>         by unloading the pertinent SPK files and then re-loading
      !>         them.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.3.0, 13-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard. Improved
      !>        $Abstract description.
      !>
      !>        Relocated initialization of FOUND so it is always
      !>        executed, even if an error state is indicated by RETURN().
      !>
      !>        Added entry #2 to $Exceptions section. Added reference to
      !>        FURNSH routine in $Files section.
      !>
      !>-    SPICELIB Version 4.2.1, 15-MAR-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
      !>
      !>        Bug fix:
      !>
      !>          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
      !>          is 'ADD TO FRONT' and no segment table room is available,
      !>          the body table's pointer to the current segment list
      !>          is now set to null. Previously the pointer was allowed to go
      !>          stale.
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When a segment list is freed because the entire list
      !>              is contributed by a single SPK file, and the list is
      !>              too large to be buffered, the corresponding body table
      !>              pointer is now set to null.
      !>
      !>           2) An algorithm change has eliminated a bug caused by not
      !>              updating the current body index when body table entries
      !>              having empty segment lists were compressed out of the
      !>              body table. Previously the body table pointer BINDEX
      !>              could go stale after the compression.
      !>
      !>           3) DAF calls are now followed by tests of FAILED()
      !>              in order to ensure that the main state loop terminates.
      !>
      !>           4) A subscript bound violation in a loop termination test
      !>              was corrected.
      !>
      !>        The "re-use interval" feature was introduced to improve speed
      !>        in the case where repeated, consecutive requests are satisfied
      !>        by the same segment.
      !>
      !>        The segment list cost algorithm was modified slightly:
      !>        the contribution of a file search to the cost of a list
      !>        is included only when the file search is completed. The
      !>        cost of finding the re-use interval is accounted for when
      !>        unbuffered searches are required.
      !>
      !>        The file table size has been increased to 1000, in order
      !>        to take advantage of the DAF system's new ability to load
      !>        1000 files.
      !>
      !>        The body table size has been increased to 200 in order to
      !>        decrease the chance of thrashing due to swapping segment
      !>        lists for different bodies.
      !>
      !>        Various small updates and corrections were made to the
      !>        comments throughout the file.
      !>
      !>-    SPICELIB Version 3.0.0, 14-AUG-1995 (WLT)
      !>
      !>        An interim fix to a bug in SPKBSR was made. The parameters
      !>        STSIZE and BTSIZE were increased to be much larger than before
      !>        (from 100 and 20 to 2000 and 40 respectively). This should
      !>        keep the boundary errors experienced by Cassini users from
      !>        occurring again. Version 4.0.0 with a real fix to the
      !>        boundary problem should be installed in SPICELIB by
      !>        October 1995
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 02-MAY-1990 (RET)
      !>
      !>        New error detected.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
      !>
      !>        Bug fix:
      !>
      !>          In the SPKSFS 'MAKE ROOM' state, when the suspended activity
      !>          is 'ADD TO FRONT' and no segment table room is available,
      !>          the body table's pointer to the current segment list
      !>          is now set to null. Previously the pointer was allowed to go
      !>          stale.
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.0.0, 28-DEC-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When a segment list is freed because the entire list
      !>              is contributed by a single SPK file, and the list is
      !>              too large to be buffered, the corresponding body table
      !>              pointer is now set to null.
      !>
      !>           2) An algorithm change has eliminated a bug caused by not
      !>              updating the current body index when body table entries
      !>              having empty segment lists were compressed out of the
      !>              body table. Previously the body table pointer BINDEX
      !>              could go stale after the compression.
      !>
      !>           3) DAF calls are now followed by tests of FAILED()
      !>              in order to ensure that the main state loop terminates.
      !>
      !>           4) A subscript bound violation in a loop termination test
      !>              was corrected. The loop is located in the
      !>              'SEARCH W/O BUFFERING' block; it finds the start of a
      !>              partial list that is to be freed.
      !>
      !>        The "re-use interval" feature was introduced to improve speed
      !>        in the case where repeated, consecutive requests are satisfied
      !>        by the same segment.
      !>
      !>        The segment list cost algorithm was modified slightly:
      !>        the contribution of a file search to the cost of a list
      !>        is included only when the file search is completed. The
      !>        cost of finding the re-use interval is accounted for when
      !>        unbuffered searches are required.
      !>
      !>        The file table size has been increased to 1000, in order
      !>        to take advantage of the DAF system's new ability to load
      !>        1000 files.
      !>
      !>        The body table size has been increased to 200 in order to
      !>        decrease the chance of thrashing due to swapping segment
      !>        lists for different bodies.
      !>
      !>        Various small updates and corrections were made to the
      !>        comments throughout the file.
      !>
      !>        In order to simplify the source code, the in-line singly
      !>        linked list implementation of the segment table has been
      !>        replaced by an implementation relying on the SPICELIB
      !>        doubly linked list routines.
      !>
      !>-    SPICELIB Version 1.1.0, 2-MAY-1990 (RET)
      !>
      !>        If an attempt is made to call SPKSFS when there are no files
      !>        loaded, an error is now signaled.
      !>
      !>     Assume the segment is not found, until it actually is.
      module subroutine spksfs ( ctx, body, et, handle, descr, ident, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin, intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
      integer(ip), intent(in) :: body  !! Body ID.
      real(wp), intent(in) :: et  !! Ephemeris time.
      real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*), intent(out) :: ident  !! Identifier of the applicable segment.
      logical, intent(out) :: found  !! Indicates whether or not a segment was found.
      found = .false.
      if ( ctx%failed() ) return
      if ( ctx%spice%spkbsr%index%nfiles > 0 ) then
         if ( .not. allocated ( ctx%spkbsr_cache ) ) allocate ( ctx%spkbsr_cache )
         call find_segment ( ctx, ctx%spice%spkbsr%index, ctx%spkbsr_cache%reuse, &
                             body, et, handle, descr, ident, found )
         return
      end if
      call ctx%setmsg ( 'At least one SPK file needs to be loaded '   // &
                        'by SPKLEF before beginning a search.'        )
      call ctx%sigerr ( 'SPICE(NOLOADEDFILES)'                       )
      return
      end subroutine spksfs

      !>## Detailed Input
      !>
      !>     FNAME    is the character name of the file to be loaded.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the integer handle assigned to the file upon loading.
      !>              Other PCK routines will subsequently use this number to
      !>              refer to the file.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of PCK files that may be loaded
      !>              simultaneously under any circumstances. FTSIZE is
      !>              currently set to match the maximum number of DAF files
      !>              that may be loaded simultaneously.
      !>
      !>## Exceptions
      !>
      !>     1)  If an attempt is made to open more DAF files than is
      !>         specified by the parameter FTSIZE in DAF system, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If an attempt is made to load more files than is specified
      !>         by the parameter FTSIZE in the PCK subsystem, and if the DAF
      !>         system has room to load another file, the error
      !>         SPICE(PCKFILETABLEFULL) is signaled. The current setting of
      !>         FTSIZE does not allow this situation to arise: the DAF system
      !>         will trap the error before this routine has the chance.
      !>
      !>     3)  This routine makes use of DAF file system routines and is
      !>         subject to all of the constraints imposed by the DAF file
      !>         system. See the DAF Required Reading daf.req or individual DAF
      !>         routines for details.
      !>
      !>## Files
      !>
      !>     A file specified by FNAME, to be loaded. The file is assigned a
      !>     handle by PCKLOF, which will be used by other routines to refer
      !>     to it.
      !>
      !>## Particulars
      !>
      !>     If there is room for a new file in the file table, PCKLOF creates
      !>     an entry for it, and opens the file for reading.
      !>
      !>     Also, if the file table is empty, PCKLOF initializes it.
      !>
      !>## Examples
      !>
      !>     See $Examples in PCKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.2, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 2.0.0, 17-MAR-2014 (NJB)
      !>
      !>        Updated segment pool initialization condition in entry
      !>        point PCKLOF so that the pool is initialized only if the file
      !>        table is empty.
      !>
      !>-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When an already loaded kernel is opened with DAFOPR,
      !>              it now has its link count reset to 1 via a call to
      !>              DAFCLS.
      !>
      !>           2) This routine now resets all file numbers when
      !>              the next file number reaches INTMAX()-1, thereby avoiding
      !>              arithmetic overflow. The numbers in the file table
      !>              are replaced with consecutive integers in the range
      !>              1 : NFT, such that the ordering of the numbers is not
      !>              changed. The HFS and LFS arrays are updated accordingly.
      !>
      !>        Also, the flags indicating validity of the re-use intervals
      !>        are set to .FALSE. here.
      !>
      !>-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
      module subroutine pcklof ( ctx, fname, handle )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the file to be loaded.
      integer(ip), intent(out) :: handle  !! Loaded file's handle.
!
!     PEPPER: the segments of the file are indexed now (SPICELIB found
!     them when a body was first searched for). A file loaded again is
!     searched first, as in SPICELIB.
!
      if ( ctx%failed() ) return
      if ( ctx%spice%pckbsr%index%ndescr == 0 ) &
         call ctx%spice%pckbsr%index%init ( 5_ip, 5_ip )
      call load_daf_file ( ctx, ctx%spice%pckbsr%index, fname, 2_ip, 5_ip, handle )
      return
      end subroutine pcklof

      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle assigned to the PCK file upon
      !>              loading.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Unloading a file that has not been loaded is a no-op.
      !>         No error is signaled.
      !>
      !>## Files
      !>
      !>     The file referred to by HANDLE is unloaded.
      !>
      !>## Particulars
      !>
      !>     A PCK file is removed from consideration during a search by the
      !>     readers by a call to PCKUOF.
      !>
      !>     The file table entry corresponding to the file referenced by
      !>     HANDLE is removed and the file is closed. Any segment table
      !>     entry which came from the specified file is also deleted.
      !>
      !>     If the file specified by HANDLE is not currently loaded in the
      !>     PCK system, no action is taken.
      !>
      !>## Examples
      !>
      !>     See $Examples in PCKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.1.3, 09-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Improved documentation of $Detailed_Input and $Particulars
      !>        sections.
      !>
      !>-    SPICELIB Version 4.1.2, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.1.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) This routine now calls RETURN() on entry and
      !>           returns if so directed.
      !>
      !>        Also, the flags indicating validity of those re-use intervals
      !>        whose data comes from the unloaded file are set to .FALSE.
      !>
      !>-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
      module subroutine pckuof ( ctx, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of file to be unloaded
      if ( ctx%failed() ) return
      if ( ctx%spice%pckbsr%index%ndescr == 0 ) return
      call unload_daf_file ( ctx, ctx%spice%pckbsr%index, handle )
      return
      end subroutine pckuof

      !>## Detailed Input
      !>
      !>     BODY     is the NAIF integer code of an ephemeris object,
      !>              typically a solar system body.
      !>
      !>     ET       is a time, in seconds past the epoch J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the handle of the binary PCK file containing a
      !>              located segment.
      !>
      !>     DESCR    is the descriptor of a located segment.
      !>
      !>     IDENT    is the identifier of a located segment.
      !>
      !>     FOUND    is a logical flag indicating whether a requested segment
      !>              was found or not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an attempt is made to call PCKSFS when there aren't any
      !>         files loaded, the error SPICE(NOLOADEDFILES) is signaled.
      !>
      !>## Files
      !>
      !>     All files loaded by PCKLOF are potential search targets for
      !>     PCKSFS.
      !>
      !>## Particulars
      !>
      !>     This routine finds the highest-priority segment, in any loaded
      !>     PCK file, such that the segment provides data for the specified
      !>     body and epoch.
      !>
      !>## Examples
      !>
      !>     See $Examples in PCKBSR.
      !>
      !>## Restrictions
      !>
      !>     1)  If Fortran I/O errors occur while searching a loaded PCK
      !>         file, the internal state of this suite of routines may
      !>         be corrupted. It may be possible to correct the state
      !>         by unloading the pertinent PCK files and then re-loading
      !>         them.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.3.0, 13-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Relocated initialization of FOUND so it is always
      !>        executed, even if an error state is indicated by RETURN().
      !>
      !>-    SPICELIB Version 4.2.2, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.2.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 4.2.0, 01-MAR-2011 (NJB)
      !>
      !>        Bug fix:
      !>
      !>           In the PCKSFS 'MAKE ROOM' state, when the suspended
      !>           activity is 'ADD TO FRONT' and no segment table room is
      !>           available, the body table's pointer to the current segment
      !>           list is now set to null. Previously the pointer was allowed
      !>           to go stale.
      !>
      !>-    SPICELIB Version 4.1.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 1.1.0, 08-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When a segment list is freed because the entire list
      !>              is contributed by a single PCK file, and the list is
      !>              too large to be buffered, the corresponding body table
      !>              pointer is now set to null.
      !>
      !>           2) An algorithm change has eliminated a bug caused by not
      !>              updating the current body index when body table entries
      !>              having empty segment lists were compressed out of the
      !>              body table. Previously the body table pointer BINDEX
      !>              could go stale after the compression.
      !>
      !>           3) DAF calls are now followed by tests of FAILED()
      !>              in order to ensure that the main state loop terminates.
      !>
      !>           4) A subscript bound violation in a loop termination test
      !>              was corrected. The loop is located in the
      !>              'SEARCH W/O BUFFERING' block; it finds the start of a
      !>              partial list that is to be freed.
      !>
      !>        The "re-use interval" feature was introduced to improve speed
      !>        in the case where repeated, consecutive requests are satisfied
      !>        by the same segment.
      !>
      !>        The segment list cost algorithm was modified slightly:
      !>        the contribution of a file search to the cost of a list
      !>        is included only when the file search is completed. The
      !>        cost of finding the re-use interval is accounted for when
      !>        unbuffered searches are required.
      !>
      !>        The file table size has been increased to 1000, in order
      !>        to take advantage of the DAF system's new ability to load
      !>        1000 files.
      !>
      !>        The body table size has been increased to 200 in order to
      !>        decrease the chance of thrashing due to swapping segment
      !>        lists for different bodies.
      !>
      !>        Various small updates and corrections were made to the
      !>        comments throughout the file.
      !>
      !>        In order to simplify the source code, the in-line singly
      !>        linked list implementation of the segment table has been
      !>        replaced by an implementation relying on the SPICELIB
      !>        doubly linked list routines.
      !>
      !>-    SPICELIB Version 1.0.0, 16-MAR-1994 (KSZ)
      !>
      !>        This differs only slightly from the SPKXXX code.
      !>        The main difference is that the SFS subroutine returns
      !>        FOUND = .FALSE. if no files are found, rather than returning
      !>        an error.
      module subroutine pcksfs ( ctx, body, et, handle, descr, ident, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin, intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
      integer(ip), intent(in) :: body  !! Body ID.
      real(wp), intent(in) :: et  !! Ephemeris time.
      real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*), intent(out) :: ident  !! Identifier of the applicable segment.
      logical, intent(out) :: found  !! Indicates whether or not a segment was found.
      found = .false.
      if ( ctx%failed() ) return
      if ( ctx%spice%pckbsr%index%ndescr == 0 ) return
      if ( ctx%spice%pckbsr%index%nfiles == 0 ) return
      if ( .not. allocated ( ctx%pckbsr_cache ) ) allocate ( ctx%pckbsr_cache )
      call find_segment ( ctx, ctx%spice%pckbsr%index, ctx%pckbsr_cache%reuse, &
                          body, et, handle, descr, ident, found )
      return
      end subroutine pcksfs

      !>## Detailed Input
      !>
      !>     FNAME    is the name of a C-kernel file to be loaded.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is an integer handle assigned to the file upon loading.
      !>              Almost every other CK routine will subsequently use
      !>              this number to refer to the file.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of CK files that may
      !>              be loaded simultaneously under any circumstances.
      !>              FTSIZE is currently set to match the maximum number
      !>              of DAF files that may be loaded simultaneously.
      !>
      !>## Exceptions
      !>
      !>     1)  If an attempt is made to load more DAF files than is
      !>         specified by the parameter FTSIZE in DAF system, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If an attempt is made to load more files than is specified
      !>         by the parameter FTSIZE in the CK subsystem, and if the DAF
      !>         system has room to load another file, the error
      !>         SPICE(CKTOOMANYFILES) is signaled. The current setting of
      !>         FTSIZE does not allow this situation to arise: the DAF
      !>         system will trap the error before this routine has the
      !>         chance.
      !>
      !>     3)  If the file specified by FNAME can not be opened, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     4)  If the file specified by FNAME has already been loaded,
      !>         it will become the "last-loaded" file. The readers
      !>         search the last-loaded file first.
      !>
      !>## Files
      !>
      !>     The C-kernel file specified by FNAME is loaded. The file is
      !>     assigned an integer handle by CKLPF. Other CK routines will refer
      !>     to this file by its handle.
      !>
      !>## Particulars
      !>
      !>     Before a file can be read by the C-kernel readers, it must be
      !>     loaded by CKLPF, which among other things load the file into
      !>     the DAF subsystem.
      !>
      !>     Up to FTSIZE files may be loaded for use simultaneously, and a
      !>     file only has to be loaded once to become a potential search
      !>     target for any number of subsequent reads.
      !>
      !>     Once a C-kernel has been loaded, it is assigned a file
      !>     handle, which is used to keep track of the file internally, and
      !>     which is used by the calling program to refer to the file in all
      !>     subsequent calls to CK routines.
      !>
      !>     If there is room for a new file, CKLPF opens the file for
      !>     reading. This routine must be called prior to a call to CKGP or
      !>     CKGPAV.
      !>
      !>     CK readers search files loaded with CKLPF in the reverse order
      !>     in which they were loaded. That is, last-loaded files are
      !>     searched first.
      !>
      !>## Examples
      !>
      !>     See $Examples in CKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.2, 25-OCT-2021 (JDR)
      !>
      !>        Updated the header to comply with NAIF standard. Extended
      !>        $Particulars section.
      !>
      !>-    SPICELIB Version 5.0.1, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 5.0.0, 17-MAR-2014 (NJB)
      !>
      !>        Updated segment pool initialization condition in entry
      !>        point CKLPF so that the pool is initialized only if the file
      !>        table is empty.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) When an already loaded kernel is opened with DAFOPR,
      !>           it now has its link count reset to 1 via a call to
      !>           DAFCLS.
      !>
      !>        2) This routine now resets all file numbers when
      !>           the next file number reaches INTMAX()-1, thereby avoiding
      !>           arithmetic overflow. The numbers in the file table
      !>           are replaced with consecutive integers in the range
      !>           1 : NFT, such that the ordering of the numbers is not
      !>           changed. The HFS and LFS arrays are updated accordingly.
      !>
      !>        Also, the flags indicating validity of the re-use intervals
      !>        are set to .FALSE. here.
      !>
      !>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
      !>
      !>        Added the Entry point CKHAVE
      !>
      !>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
      !>
      !>        The parameter STSIZE was increased from 1000 to 4000 to
      !>        avoid the buffering error that exists in the CKBSR.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        When loading a file, CKLPF now checks if the file table is
      !>        full only after determining that the file is not currently
      !>        loaded. Previously, if the file table was full and an attempt
      !>        was made to reload a file, an error was signaled. A new
      !>        exception was added as a result of this change.
      !>
      !>        A bug in the way that CKLPF and CKUPF clean up the instrument
      !>        tables after a file is unloaded was fixed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>        1) When a loaded kernel is opened with DAFOPR,
      !>           it now has its link count reset to 1 via a call to
      !>           DAFCLS.
      !>
      !>        2) This routine now resets all file numbers when
      !>           the next file number reaches INTMAX()-1, thereby avoiding
      !>           arithmetic overflow. The numbers in the file table
      !>           are replaced with consecutive integers in the range
      !>           1 : NFT, such that the ordering of the numbers is not
      !>           changed. The HFS and LFS arrays are updated accordingly.
      !>           HFS and LFS entries that have gone stale are set to zero.
      !>
      !>        Also, the flags indicating validity of the re-use intervals
      !>        are set to .FALSE. here.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        Temp version for testing purposes.
      !>
      !>        When loading a file, CKLPF now checks if the file table is
      !>        full only after determining that the file is not currently
      !>        loaded. Previously, if the file table was full and an attempt
      !>        was made to reload a file, an error was signaled. A new
      !>        exception was added as a result of this change.
      !>
      !>        A bug in the way that CKLPF and CKUPF clean up the instrument
      !>        tables after a file is unloaded was fixed.
      !>
      !>        If as the result of loading a file that was previously loaded,
      !>        there are no more segments buffered for a particular
      !>        instrument, the counter variable for the instruments is no
      !>        longer incremented.
      !>
      !>        The following code fragment changed:
      !>
      !>           IF ( ITBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NIT = NIT - 1
      !>
      !>           END IF
      !>
      !>           I = I + 1
      !>
      !>        This is the fix:
      !>
      !>           IF ( ITBEG( I ) .EQ. 0 ) THEN
      !>
      !>              .
      !>              .
      !>              .
      !>              NIT = NIT - 1
      !>
      !>           ELSE
      !>
      !>              I = I + 1
      !>
      !>           END IF
      !>
      !>-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
      !>
      !>        Header documentation was updated, and error handling was
      !>        modified.
      !>
      !>-    Beta Version 1.0.0, 14-MAR-1990 (RET) (IMU)
      !>
      !>     Standard SPICE error handling.
      module subroutine cklpf ( ctx, fname, handle )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the CK file to be loaded.
      integer(ip), intent(out) :: handle  !! Loaded file's handle.
!
!     PEPPER: the segments of the file are indexed now (SPICELIB found
!     them when an instrument was first searched for). A file loaded
!     again is searched first, as in SPICELIB.
!
      if ( ctx%failed() ) return
      if ( ctx%spice%ckbsr%index%ndescr == 0 ) &
         call ctx%spice%ckbsr%index%init ( 5_ip, 6_ip )
      call load_daf_file ( ctx, ctx%spice%ckbsr%index, fname, 2_ip, 6_ip, handle )
      return
      end subroutine cklpf

      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle assigned to the CK file upon
      !>              loading.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Unloading a file that has not been loaded is a no-op.
      !>         No error is signaled.
      !>
      !>## Files
      !>
      !>     The file referred to by HANDLE is unloaded.
      !>
      !>## Particulars
      !>
      !>     Unloading a file with CKUPF removes that file from consideration
      !>     by the CK readers. In doing so, it frees up space for another
      !>     file to be loaded.
      !>
      !>## Examples
      !>
      !>     See $Examples in CKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.3.2, 16-JUL-2020 (JDR)
      !>
      !>        Updated the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 4.3.1, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB)
      !>
      !>        Bug fix: the null pointer test used to determine eligibility
      !>        for segment list deletion now uses the .LE. operator instead
      !>        of the .EQ. operator.
      !>
      !>-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) This routine now calls RETURN() on entry and
      !>              returns if so directed.
      !>
      !>        Also, the flags indicating validity of those re-use intervals
      !>        whose data comes from the unloaded file are set to .FALSE.
      !>
      !>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
      !>
      !>        Added the Entry point CKHAVE.
      !>
      !>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
      !>
      !>        The parameter STSIZE was increased from 1000 to 4000 to
      !>        avoid the buffering error that exists in the CKBSR.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        A bug in the way that CKLPF and CKUPF clean up the instrument
      !>        tables after a file is unloaded was fixed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.3.0, 24-FEB-2011 (NJB)
      !>
      !>        Bug fix: the null pointer test used to determine eligibility
      !>        for segment list deletion now uses the .LE. operator instead
      !>        of the .EQ. operator.
      !>
      !>-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) This routine now calls RETURN() on entry and
      !>              returns if so directed.
      !>
      !>        Also, the flags indicating validity of those re-use intervals
      !>        whose data comes from the unloaded file are set to .FALSE.
      !>
      !>-    SPICELIB Version 2.0.0, 25-NOV-1992 (JML)
      !>
      !>        1) A bug in the way that CKLPF and CKUPF clean up the
      !>           instrument tables after a file is unloaded was fixed.
      !>
      !>           If as the result of unloading a file there are no more
      !>           segments buffered for a particular instrument, the counter
      !>           variable for the instruments in the instrument table is no
      !>           longer incremented.
      !>
      !>           The following code fragment changed:
      !>
      !>              IF ( ITBEG( I ) .EQ. 0 ) THEN
      !>
      !>                 .
      !>                 .
      !>                 .
      !>                 NIT = NIT - 1
      !>
      !>              END IF
      !>
      !>              I = I + 1
      !>
      !>           This is the fix:
      !>
      !>              IF ( ITBEG( I ) .EQ. 0 ) THEN
      !>
      !>                 .
      !>                 .
      !>                 .
      !>                 NIT = NIT - 1
      !>
      !>              ELSE
      !>
      !>                 I = I + 1
      !>
      !>              END IF
      !>
      !>-    Beta Version 1.0.1, 29-AUG-1990 (MJS) (JEM)
      !>
      !>         Comments were updated.
      !>
      !>-    Beta Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      module subroutine ckupf ( ctx, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of CK file to be unloaded
      if ( ctx%failed() ) return
      if ( ctx%spice%ckbsr%index%ndescr == 0 ) return
      call unload_daf_file ( ctx, ctx%spice%ckbsr%index, handle )
      return
      end subroutine ckupf

      !>## Detailed Input
      !>
      !>     INST     is the NAIF ID of an instrument.
      !>
      !>     SCLKDP   is an encoded spacecraft clock time.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>     NEEDAV   indicates whether or not angular velocity data is
      !>              required.
      !>
      !>              If .TRUE., only segments containing pointing and angular
      !>              velocity data will be checked. If .FALSE., segments
      !>              containing just pointing data will also be considered.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If no files have been loaded, the error SPICE(NOLOADEDFILES)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     All files loaded by CKLPF are potential search targets for
      !>     CKSNS.
      !>
      !>## Particulars
      !>
      !>     CKBSS sets up a search for segments by CKSNS. It records the
      !>     instrument and time to be searched for, and whether to require
      !>     segments containing angular velocity data. If angular velocity
      !>     data are required, only segments containing angular velocity
      !>     data will be returned by CKSNS. If angular velocity data are
      !>     not required, segments returned by CKSNS may or may not contain
      !>     angular velocity data.
      !>
      !>     CKBSS determines the first task that CKSNS will have to perform
      !>     if it is called to get an applicable segment.
      !>
      !>     A segment matches the CKBSS/CKSNS search criteria when the
      !>     following statements are true.
      !>
      !>        1) INST matches the instrument number for the segment.
      !>
      !>        2) The time interval [SCLKDP - TOL, SCLKDP + TOL] intersects
      !>           the time interval of the segment.
      !>
      !>        3) If angular velocity data is required, as indicated by
      !>           NEEDAV, the segment contains angular velocity data.
      !>
      !>## Examples
      !>
      !>     See $Examples in CKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.1.2, 13-JAN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Moved details related to search criteria and conditions to meet
      !>        it from the $Detailed_Input to $Particulars.
      !>
      !>        Fixed typo in the long error message ('needs must' -> 'must').
      !>
      !>-    SPICELIB Version 4.1.1, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Updated to support new doubly-linked list implementation:
      !>        partial segment list that cannot be buffered is now
      !>        deallocated here rather than in CKSNS. Minor changes to
      !>        comments were made as well.
      !>
      !>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
      !>
      !>        Added the Entry point CKHAVE.
      !>
      !>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
      !>
      !>        The parameter STSIZE was increased from 1000 to 4000 to
      !>        avoid the buffering error that exists in the CKBSR.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Updated to support new doubly-linked list implementation:
      !>        partial segment list that cannot be buffered is now
      !>        deallocated here rather than in CKSNS. Minor changes to
      !>        comments were made as well.
      !>
      !>-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>          1) The variable SCLK was changed to SCLKDP.
      !>          2) Header documentation was updated.
      !>
      !>-    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU)
      !>
      !>     Standard SPICE error handling.
      module subroutine ckbss ( ctx, inst, sclkdp, tol, needav )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inst  !! Spacecraft and instrument ID.
      real(wp), intent(in) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      logical, intent(in) :: needav  !! Is there a need for angular velocity?
      if ( .not. allocated ( ctx%ckbsr_cache ) ) allocate ( ctx%ckbsr_cache )
      associate ( cs => ctx%ckbsr_cache )
      if ( ctx%failed() ) return
      cs%active = .false.
      cs%scinst = inst
      cs%alpha  = sclkdp - tol
      cs%omega  = sclkdp + tol
      cs%avneed = needav
      cs%reqt   = sclkdp
      cs%savtol = tol
      if ( ctx%spice%ckbsr%index%nfiles == 0 ) then
         call no_ck_files ( ctx )
         return
      end if
!
!     The search runs down the instrument's list in the index, from the
!     segment searched first.
!
      cs%active = .true.
      cs%epoch  = ctx%spice%segment_epoch
      cs%list   = ctx%spice%ckbsr%index%list ( inst )
      cs%pos    = ctx%spice%ckbsr%index%count ( cs%list )
      cs%newsch = .true.
      cs%fresh  = .true.
      return
      end associate
      contains
         !> Signal SPICE(NOLOADEDFILES).
         subroutine no_ck_files ( ctx )
            class(pepper_context), intent(inout) :: ctx  !! the context
            call ctx%setmsg ( 'At least one CK file must be loaded ' // &
                              'by CKLPF before beginning a search.'       )
            call ctx%sigerr ( 'SPICE(NOLOADEDFILES)'                      )
         end subroutine no_ck_files
      end subroutine ckbss

      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is an integer handle of the file containing the
      !>              segment matching the instrument and time
      !>              specifications made in the last call to CKBSS.
      !>
      !>     DESCR,
      !>     SEGID    are the descriptor and identifier of the segment found
      !>              which matches the instrument and time specifications
      !>              made in the last call to CKBSS.
      !>
      !>     FOUND    is .TRUE. if an applicable segment was found, .FALSE.
      !>              otherwise. If FOUND is .FALSE., the values of the
      !>              other arguments are meaningless.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If CKSNS is called without CKBSS ever having been called,
      !>         the error SPICE(CALLCKBSSFIRST) is signaled.
      !>
      !>     2)  If no segment is found that matches the search criteria,
      !>         FOUND is set to .FALSE., but the values of HANDLE, DESCR,
      !>         and SEGID will be meaningless.
      !>
      !>## Files
      !>
      !>     All files loaded by CKLPF are potential search targets for
      !>     CKSNS. The files are all referred to by their integer handles.
      !>
      !>## Particulars
      !>
      !>     CKSNS is used to locate segments based on the search criteria
      !>     established by the most recent call to CKBSS. When a segment
      !>     is found it will have the following characteristics:
      !>
      !>        1) Its instrument will match the instrument specified in the
      !>           call to CKBSS.
      !>
      !>        2) Its time interval will intersect the time interval
      !>
      !>              [SCLKDP - TOL, SCLKDP + TOL],
      !>
      !>           where SCLKDP and TOL were specified in the call to CKBSS.
      !>
      !>        3) If there is a need for angular velocity data, as specified
      !>           by NEEDAV in the call to CKBSS, a returned segment
      !>           will contain angular velocity data. If there is no need
      !>           for such data, the returned segment may or may not contain
      !>           angular velocity data.
      !>
      !>     The first call to CKSNS following a call to CKBSS starts a search
      !>     through loaded files and either returns the first applicable
      !>     segment, or indicates that no segment was found.
      !>
      !>     CKSNS searches through last-loaded files first. Individual
      !>     files are searched backwards, so that segments that were inserted
      !>     last into the file get checked first.
      !>
      !>     Subsequent calls to CKSNS pick up the search exactly where the
      !>     previous calls left off. If a segment is not found, future calls
      !>     will also indicate that no segment could be found, until a new
      !>     search is begun.
      !>
      !>     CKSNS also buffers segment descriptors and identifiers, to
      !>     attempt to minimize file reads.
      !>
      !>## Examples
      !>
      !>     See $Examples in CKBSR.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine assumes that a search has been initiated by
      !>         a call to CKBSS.
      !>
      !>     2)  When a CK file is loaded or unloaded, a new search must
      !>         be started via a call to CKBSS before this routine may
      !>         be called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.6.0, 13-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Relocated initialization of FOUND so it is always
      !>        executed, even if an error state is indicated by RETURN().
      !>
      !>-    SPICELIB Version 4.5.1, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
      !>
      !>        Bug fix: in the 'MAKE ROOM' state, when the suspended activity
      !>        is 'ADD TO FRONT' and no segment table room is available, the
      !>        instrument table's pointer to the current segment list is now
      !>        set to null. Previously the pointer was allowed to go stale.
      !>
      !>-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When a segment list is freed because the entire list
      !>              is contributed by a single CK file, and the list is
      !>              too large to be buffered, the corresponding instrument
      !>              table pointer is now set to null.
      !>
      !>           2) An algorithm change has eliminated a bug caused by not
      !>              updating the current instrument index when instrument
      !>              table entries  having empty segment lists were compressed
      !>              out of the instrument table. Previously the instrument
      !>              table pointer IINDEX could go stale after the
      !>              compression.
      !>
      !>           3) DAF calls are now followed by tests of FAILED()
      !>              in order to ensure that the main state loop terminates.
      !>
      !>        The "re-use interval" feature was introduced to improve speed
      !>        in the case where repeated, consecutive requests are satisfied
      !>        by the same segment.
      !>
      !>        The segment list cost algorithm was modified slightly:
      !>        the contribution of a file search to the cost of a list
      !>        is included only when the file search is completed. The
      !>        cost of finding the re-use interval is accounted for when
      !>        unbuffered searches are required.
      !>
      !>        The file table size has been increased to 1000, in order
      !>        to take advantage of the DAF system's new ability to load
      !>        1000 files.
      !>
      !>        The instrument table size has been increased to 100 in order to
      !>        decrease the chance of thrashing due to swapping segment
      !>        lists for different bodies.
      !>
      !>        Various small updates and corrections were made to the
      !>        comments throughout the file.
      !>
      !>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
      !>
      !>        Added the Entry point CKHAVE.
      !>
      !>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
      !>
      !>        The parameter STSIZE was increased from 1000 to 4000 to
      !>        avoid the buffering error that exists in the CKBSR.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
      !>
      !>        A check on the initial value of the variable STATUS
      !>        was added in order to detect the situation in which
      !>        CKBSS was never called to initiate a search.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.5.0, 24-FEB-2011 (NJB)
      !>
      !>        Bug fix: in the 'MAKE ROOM' state, when the suspended activity
      !>        is 'ADD TO FRONT' and no segment table room is available, the
      !>        instrument table's pointer to the current segment list is now
      !>        set to null. Previously the pointer was allowed to go stale.
      !>
      !>-    SPICELIB Version 4.2.0, 08-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MOVED call.
      !>
      !>-    SPICELIB Version 4.1.0, 20-NOV-2001 (NJB)
      !>
      !>        Bug fixes:
      !>
      !>           1) When a segment list is freed because the entire list
      !>              is contributed by a single CK file, and the list is
      !>              too large to be buffered, the corresponding instrument
      !>              table pointer is now set to null.
      !>
      !>           2) An algorithm change has eliminated a bug caused by not
      !>              updating the current instrument index when instrument
      !>              table entries  having empty segment lists were compressed
      !>              out of the instrument table. Previously the instrument
      !>              table pointer IINDEX could go stale after the
      !>              compression.
      !>
      !>           3) DAF calls are now followed by tests of FAILED()
      !>              in order to ensure that the main state loop terminates.
      !>
      !>        The "re-use interval" feature was introduced to improve speed
      !>        in the case where repeated, consecutive requests are satisfied
      !>        by the same segment.
      !>
      !>        The segment list cost algorithm was modified slightly:
      !>        the contribution of a file search to the cost of a list
      !>        is included only when the file search is completed. The
      !>        cost of finding the re-use interval is accounted for when
      !>        unbuffered searches are required.
      !>
      !>        The file table size has been increased to 1000, in order
      !>        to take advantage of the DAF system's new ability to load
      !>        1000 files.
      !>
      !>        The instrument table size has been increased to 100 in order to
      !>        decrease the chance of thrashing due to swapping segment
      !>        lists for different instruments.
      !>
      !>        Various small updates and corrections were made to the
      !>        comments throughout the file.
      !>
      !>-    SPICELIB Version 1.1.0, 01-NOV-1990 (JML)
      !>
      !>        A check on the initial value of the variable STATUS
      !>        was added in order to detect the situation in which
      !>        CKBSS was never called to initiate a search.
      !>
      !>-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>           1) The variable IDENT was changed to SEGID.
      !>           2) The local variables INTDES and DPDES were changed to
      !>              ICD and DCD.
      !>           3) Header and internal documentation was corrected and
      !>              updated.
      !>
      !>-    Beta Version 1.0.0, 20-APR-1990 (RET) (IMU)
      !>
      !>     Nothing's been found yet.
      module subroutine cksns ( ctx, handle, descr, segid, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin, intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
      real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*), intent(out) :: segid  !! Identifier of the applicable segment.
      logical, intent(out) :: found  !! .TRUE. if a segment was found.
      integer(ip) :: e
      integer(ip) :: s
      found = .false.
      if ( ctx%failed() ) return
      if ( .not. allocated ( ctx%ckbsr_cache ) ) allocate ( ctx%ckbsr_cache )
      if ( ctx%ckbsr_cache%active ) then
         if ( ctx%ckbsr_cache%epoch /= ctx%spice%segment_epoch ) ctx%ckbsr_cache%active = .false.
      end if
      if ( .not. ctx%ckbsr_cache%active ) then
         call ctx%setmsg ( 'Must begin a search by calling CKBSS first.' )
         call ctx%sigerr ( 'SPICE(CALLCKBSSFIRST)' )
         return
      end if
      associate ( cs => ctx%ckbsr_cache, ix => ctx%spice%ckbsr%index )
      if ( cs%reuse%epoch /= ctx%spice%segment_epoch ) then
         call cs%reuse%reset ( ctx%spice%segment_epoch )
      end if
!
!     At the start of a search, the segment returned first by the last
!     search for the instrument is the answer if the request is inside its
!     re-use interval (by more than the tolerance). As in SPICELIB, the
!     search then continues from the top of the list (checked once per
!     search).
!
      if ( cs%fresh ) then
         cs%fresh = .false.
         e = cs%reuse%find ( cs%scinst )
         if ( e > 0 ) then
            s = cs%reuse%seg(e)
            if ( s > 0 ) then
               if (       ( cs%reqt > ( cs%reuse%lb(e) + cs%savtol ) ) &
                    .and. ( cs%reqt < ( cs%reuse%ub(e) - cs%savtol ) ) &
                    .and. ( ( .not. cs%avneed ) .or. ( ix%idescr(4,s) /= 0 ) ) ) then
                  handle = ix%handle(s)
                  segid  = ix%ident(s)
                  descr(1:ix%ndescr) = ix%descr(1:ix%ndescr,s)
                  found  = .true.
                  return
               end if
               call cs%reuse%forget ( e )
            end if
         end if
      end if
      if ( cs%newsch .and. cs%pos == ix%count ( cs%list ) ) then
         cs%lb = dpmin
         cs%ub = dpmax
      end if
!
!     Search the rest of the list, highest priority first. The first
!     segment returned by a search, if the request time is inside it,
!     gets a re-use interval: the part of it not covered by segments of
!     higher priority (those that do not contain the request time).
!
      do while ( cs%pos >= 1 )
         s = ix%segment ( cs%list, cs%pos )
         cs%pos = cs%pos - 1
         if ( cs%newsch ) then
            if ( cs%reqt > ix%tend(s) ) then
               cs%lb = max ( cs%lb, ix%tend(s) )
            else if ( cs%reqt < ix%tbeg(s) ) then
               cs%ub = min ( cs%ub, ix%tbeg(s) )
            end if
         end if
         if (       ( cs%omega >= ix%tbeg(s) ) &
              .and. ( cs%alpha <= ix%tend(s) )  ) then
            if (  ( .not. cs%avneed ) .or. ( ix%idescr(4,s) /= 0 )  ) then
               handle = ix%handle(s)
               segid  = ix%ident(s)
               descr(1:ix%ndescr) = ix%descr(1:ix%ndescr,s)
               found  = .true.
               if (         cs%newsch &
                    .and. ( cs%reqt > ix%tbeg(s) ) &
                    .and. ( cs%reqt < ix%tend(s) )  ) then
                  call cs%reuse%store ( cs%scinst, s, max ( cs%lb, ix%tbeg(s) ), &
                                        min ( cs%ub, ix%tend(s) ) )
               end if
               cs%newsch = .false.
               return
            end if
         end if
      end do
      return
      end associate
      end subroutine cksns

      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    is returned with the value .TRUE. if at least one
      !>              C-kernel is currently loaded. Otherwise it returns
      !>              the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point allows the user to query the set of "loaded"
      !>     C-kernels to make sure that at least one C-kernel has been loaded.
      !>     This allows you to avoid making a search of an empty set of
      !>     loaded kernels which forces a SPICELIB error to be signaled.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you want to call one of the C-kernel readers, but wish
      !>        to handle the exceptional case of "no kernels loaded" so that
      !>        the SPICE exception handling mechanism is avoided in the case
      !>        of an empty set of loaded kernels. The code example below
      !>        shows how you might do this.
      !>
      !>        Example code begins here.
      !>
      !>              PROGRAM CKHAVE_EX1
      !>              IMPLICIT NONE
      !>
      !>        C
      !>        C     Local variables.
      !>        C
      !>              LOGICAL               LOADED
      !>
      !>        C
      !>        C     We have not loaded any CK.
      !>        C
      !>              CALL CKHAVE ( LOADED )
      !>
      !>              IF ( LOADED ) THEN
      !>
      !>        C
      !>        C        Here we could be calling CK readers. For this
      !>        C        example, just display a message indicating that
      !>        C        at least one CK has been loaded.
      !>        C
      !>                 WRITE(*,*) 'At least one CK has been loaded. '
      !>             .          //  'You may call CK readers.'
      !>
      !>              ELSE
      !>
      !>                 WRITE(*,*) 'No CK has been loaded.'
      !>                 WRITE(*,*) 'Calling CK readers will result in an '
      !>             .          //  'error.'
      !>
      !>              END IF
      !>
      !>              END
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         No CK has been loaded.
      !>         Calling CK readers will result in an error.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.0.4, 16-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example code fragment.
      !>
      !>-    SPICELIB Version 4.0.3, 30-JAN-2017 (NJB)
      !>
      !>        Corrected various spelling errors within comments.
      !>
      !>-    SPICELIB Version 4.0.2, 28-FEB-2008 (BVS)
      !>
      !>        Corrected the contents of the $Required_Reading section.
      !>
      !>-    SPICELIB Version 4.0.1, 31-OCT-2001 (NJB)
      !>
      !>        Typo corrected.
      !>
      !>-    SPICELIB Version 4.0.0, 17-FEB-2000 (WLT)
      !>
      !>        Added the Entry point CKHAVE.
      !>
      !>-    SPICELIB Version 3.0.0, 03-MAR-1999 (WLT)
      module subroutine ckhave ( ctx, found )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(out) :: found  !! .TRUE. if at least one C-kernel is loaded.
      found = .false.
      if ( ctx%failed() ) return
      found = ctx%spice%ckbsr%index%nfiles > 0
      return
      end subroutine ckhave

      !>## Detailed Input
      !>
      !>     FNAME      is the name of a DSK file to be loaded.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE     is an integer handle assigned to the file upon loading.
      !>                Almost every other DSK routine will subsequently use
      !>                this number to refer to the file.
      !>
      !>## Parameters
      !>
      !>     FTSIZE     is the maximum number of DSK files that may
      !>                be loaded simultaneously under any circumstances.
      !>                FTSIZE is currently set to match the maximum number
      !>                of DAS files that may be loaded simultaneously.
      !>
      !>## Exceptions
      !>
      !>     1) If an attempt is made to open more DAS files than is specified
      !>        by the parameter FTSIZE in DASAH, an error is signaled by a
      !>        routine in the call tree of this routine.
      !>
      !>     2) If an attempt is made to load more files than is specified
      !>        by the local parameter FTSIZE, and if the DAS system has
      !>        room to load another file, the error SPICE(DSKTOOMANYFILES)
      !>        signaled. The current setting of FTSIZE does not allow this
      !>        situation to arise:  the DAS system will trap the error
      !>        before this routine has the chance.
      !>
      !>     3) If the file specified by FNAME can not be opened, an error
      !>        is signaled by a routine that this routine calls.
      !>
      !>     4) If the file specified by FNAME has already been loaded,
      !>        it will become the "last-loaded" file.  The readers
      !>        search the last-loaded file first.
      !>
      !>## Files
      !>
      !>     The DSK file specified by FNAME is loaded. The file is assigned
      !>     an integer handle by ZZDSKLSF. Other DSK routines will refer to
      !>     this file by its handle.
      !>
      !>## Particulars
      !>
      !>     See Particulars above, in ZZDSKBSR.
      !>
      !>     If there is room for a new file, ZZDSKLSF opens the file for
      !>     reading.
      !>
      !>     DSK readers search files loaded with ZZDSKLSF in the reverse order
      !>     in which they were loaded.  That is, last-loaded files are
      !>     searched first.
      !>
      !>## Examples
      !>
      !>     See the Example above, in ZZDSKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     R.E. Thurman   (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        19-SEP-2014 (NJB)
      !>
      !>           Specification change: now treats the target body as the
      !>           primary search key. Segments lists are associated with
      !>           bodies rather than surfaces.
      !>
      !>           Now initializes the DSK state change counter on the first
      !>           pass and updates it on every subsequent call.
      !>       21-MAY-2013 (NJB)
      !>
      !>           Edited headers to remove long lines.
      !>
      !>       02-APR-2010 (NJB) (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>     None.
      !>
      !>     Standard SPICE error handling.
      module subroutine zzdsklsf ( ctx, fname, handle )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the DSK file to be loaded.
      integer(ip), intent(out) :: handle  !! Loaded file's handle.
      if ( ctx%failed() ) return
      if ( ctx%spice%zzdskbsr%index%ndescr == 0 ) &
         call ctx%spice%zzdskbsr%index%init ( int ( dskdsz, ip ), int ( dladsz, ip ) )
      if ( ctx%spice%zzdskbsr%pass1 ) then
         call zzctrsin ( ctx, ctx%spice%zzdskbsr%dskctr )
         ctx%spice%zzdskbsr%pass1 = .false.
      end if
      call zzctrinc ( ctx, ctx%spice%zzdskbsr%dskctr )
!
!     PEPPER: the segments of the file are indexed now, by central body
!     (SPICELIB found them when a body was first searched for). A file
!     loaded again is searched first, as in SPICELIB.
!
      call dasopr ( ctx, fname, handle )
      if ( ctx%failed() ) return
      ctx%spice%segment_epoch = ctx%spice%segment_epoch + 1
      associate ( ix => ctx%spice%zzdskbsr%index )
      if ( ix%file_loaded ( handle ) ) then
         call dascls ( ctx, handle )
         call ix%remove_file ( handle )
      end if
      call index_dsk_file ( ctx, ix, handle )
      if ( ctx%failed() ) then
         call ix%remove_file ( handle )
         call dascls ( ctx, handle )
         return
      end if
      call ix%add_file ( handle )
      end associate
      return
      end subroutine zzdsklsf

      !>## Detailed Input
      !>
      !>     HANDLE     Integer handle assigned to the file upon loading.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) Unloading a file that has not been loaded is a no-op.
      !>        No error is signaled.
      !>
      !>## Files
      !>
      !>     The file referred to by HANDLE is unloaded.
      !>
      !>## Particulars
      !>
      !>     See Particulars section above, in ZZDSKBSR.
      !>
      !>     Unloading a file with ZZDSKUSF removes that file from
      !>     consideration by the DSK readers. In doing so, it frees up space
      !>     for another file to be loaded.
      !>
      !>## Examples
      !>
      !>     See the Example above, in ZZDSKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     R.E. Thurman   (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        19-SEP-2014 (NJB)
      !>
      !>           Specification change: now treats the target body as the
      !>           primary search key. Segments lists are associated with
      !>           bodies rather than surfaces.
      !>
      !>           Now initializes the DSK state change counter on the first
      !>           pass and updates it on every subsequent call.
      !>        21-MAY-2013 (NJB)
      !>
      !>           Edited headers to remove long lines.
      !>           Removed debugging output.
      !>
      !>        02-APR-2010 (NJB) (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>     None.
      module subroutine zzdskusf ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DSK file to be unloaded
      if ( ctx%failed() ) return
      if ( ctx%spice%zzdskbsr%index%ndescr == 0 ) return
      if ( ctx%spice%zzdskbsr%pass1 ) then
         call zzctrsin ( ctx, ctx%spice%zzdskbsr%dskctr )
         ctx%spice%zzdskbsr%pass1 = .false.
      end if
      call zzctrinc ( ctx, ctx%spice%zzdskbsr%dskctr )
      ctx%spice%segment_epoch = ctx%spice%segment_epoch + 1
      if ( .not. ctx%spice%zzdskbsr%index%file_loaded ( handle ) ) return
      call dascls ( ctx, handle )
      call ctx%spice%zzdskbsr%index%remove_file ( handle )
      return
      end subroutine zzdskusf

      !>## Detailed Input
      !>
      !>     BODYID     is the integer ID code of the body for which a search
      !>                is to be performed.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If no files have been loaded, the error
      !>         SPICE(NOLOADEDDSKFILES) is signaled.
      !>
      !>## Files
      !>
      !>     All files loaded by ZZDSKLSF are potential search targets for
      !>     ZZDSKSNS.
      !>
      !>## Particulars
      !>
      !>     ZZDSKBSS sets up a search for segments by ZZDSKSNS. It records
      !>     the body for which the search is to be conducted.
      !>
      !>     ZZDSKBSS determines the first task that ZZDSKSNS will have to
      !>     perform if it is called to get an applicable segment.
      !>
      !>## Examples
      !>
      !>     See Examples in ZZDSKBSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     R.E. Thurman   (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 23-NOV-2020 (NJB) (BVS)
      !>
      !>        Corrected typo in long error message. Corrected descriptions
      !>        of inputs. Corrected description of saved information in the
      !>        Particulars header section.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        19-SEP-2014 (NJB)
      !>
      !>           Specification change: now treats the target body as the
      !>           primary search key. Segments lists are associated with
      !>           bodies rather than surfaces.
      !>
      !>           Now initializes the DSK state change counter on the first
      !>           pass.
      !>
      !>        21-MAY-2013 (NJB)
      !>
      !>           Edited headers to remove long lines.
      !>
      !>        02-APR-2010 (NJB) (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>     None.
      !>
      !>     Standard SPICE error handling.
      module subroutine zzdskbss ( ctx, bodyid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: bodyid  !! ID code of body.
      if ( .not. allocated ( ctx%zzdskbsr_cache ) ) allocate ( ctx%zzdskbsr_cache )
      if ( ctx%failed() ) return
      ctx%zzdskbsr_cache%active = .false.
      if ( ctx%spice%zzdskbsr%index%nfiles > 0 ) then
         associate ( cs => ctx%zzdskbsr_cache, ix => ctx%spice%zzdskbsr%index )
         cs%active = .true.
         cs%epoch  = ctx%spice%segment_epoch
         cs%list   = ix%list ( bodyid )
         cs%pos    = ix%count ( cs%list )
         end associate
         return
      end if
      call ctx%setmsg ( 'At least one DSK file must have been loaded ' &
      //            'by ZZDSKLSF before a search can be started.' )
      call ctx%sigerr ( 'SPICE(NOLOADEDDSKFILES)'                     )
      return
      end subroutine zzdskbss

      !>## Detailed Input
      !>
      !>     CMPFUN
      !>
      !>## Detailed Output
      !>
      !>     DLADSC
      !>
      !>     DSKDS
      !>
      !>     HANDLE     is an integer handle of the file containing the
      !>                segment matching the body and time
      !>                specifications made in the last call to ZZDSKBSS.
      !>
      !>     FOUND      is true if an applicable segment was found.  False
      !>                otherwise.  If FOUND is false, the values of the
      !>                other arguments are meaningless.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If ZZDSKSNS is called without ZZDSKBSS ever having been
      !>        called, the error 'SPICE(CALLZZDSKBSSFIRST)' is signaled.
      !>
      !>     2) If no segment is found that matches the search criteria,
      !>        FOUND is set to false, but the values of HANDLE, DESCR,
      !>        and SEGID will be meaningless.
      !>
      !>## Files
      !>
      !>     All files loaded by ZZDSKLSF are potential search targets for
      !>     ZZDSKSNS. The files are all referred to by their integer handles.
      !>
      !>## Particulars
      !>
      !>     ZZDSKSNS is used to locate segments based on the search criteria
      !>     established by the most recent call to ZZDSKBSS.  When a segment
      !>     is found it will have the following characteristics:
      !>
      !>        1) Its body will match the body specified in the
      !>           call to ZZDSKBSS.
      !>
      !>        2) Its time interval will intersect the time interval
      !>
      !>              [SCLKDP - TOL, SCLKDP + TOL],
      !>
      !>           where SCLKDP and TOL were specified in the call to ZZDSKBSS.
      !>
      !>        3) If there is a need for angular velocity data, as specified
      !>           by NEEDAV in the call to ZZDSKBSS, a returned segment
      !>           will contain angular velocity data. If there is no need
      !>           for such data, the returned segment may or may not contain
      !>           angular velocity data.
      !>
      !>     The first call to ZZDSKSNS following a call to ZZDSKBSS starts a
      !>     search through loaded files and either returns the first
      !>     applicable segment, or indicates that no segment was found.
      !>
      !>     ZZDSKSNS searches through last-loaded files first. Individual
      !>     files are searched backwards, so that segments that were inserted
      !>     last into the file get checked first.
      !>
      !>     Subsequent calls to ZZDSKSNS pick up the search exactly where the
      !>     previous calls left off. If a segment is not found, future calls
      !>     will also indicate that no segment could be found, until a new
      !>     search is begun.
      !>
      !>     ZZDSKSNS also buffers segment descriptors and identifiers, to
      !>     attempt to minimize file reads.
      !>
      !>## Examples
      !>
      !>     See Examples in ZZDSKBSR.
      !>
      !>## Restrictions
      !>
      !>     1) This subroutine assumes that a search has been initiated by
      !>        a call to ZZDSKBSS.
      !>
      !>     2) When a DSK file is loaded or unloaded, a new search must
      !>        be started via a call to ZZDSKBSS before this routine may
      !>        be called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     R.E. Thurman   (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-OCT-2021 (NJB)
      !>
      !>        Relocated initialization of FOUND so it is always
      !>        executed, even if an error state is indicated by RETURN().
      !>
      !>        Corrected inline comments describing segment search criteria.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        31-MAR-2016 (NJB)
      !>
      !>           References to unneeded variables were removed.
      !>
      !>        19-SEP-2014 (NJB)
      !>
      !>           Specification change: now treats the target body as the
      !>           primary search key. Segment lists are associated with
      !>           bodies rather than surfaces.
      !>
      !>           Now initializes the DSK state change counter on the first
      !>           pass.
      !>
      !>        21-MAY-2013 (NJB)
      !>
      !>            Removed debugging WRITE statement. Edited headers to
      !>            remove long lines.
      !>
      !>        02-APR-2010 (NJB) (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>     None.
      !>
      !>     Nothing's been found yet.
      module subroutine zzdsksns ( ctx, cmpfun, handle, dladsc, dskdsc, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
!     PEPPER: the full form (not MODULE PROCEDURE), with the arguments
!     declared again: gfortran ignores the interface of a dummy procedure
!     in a MODULE PROCEDURE body.
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
      procedure(zzdsk_compare) :: cmpfun  !! Comparison function.
      integer(ip), intent(out) :: dladsc ( * )  !! DLA descriptor of the applicable segment.
      real(wp), intent(out) :: dskdsc ( * )  !! DSK descriptor of the applicable segment.
      logical, intent(out) :: found  !! True if a segment was found.
      integer(ip) :: s
      integer(ip) :: lhan
      integer(ip) :: ldla ( dladsz )
      real(wp) :: ldsk ( dskdsz )
      found = .false.
      if ( ctx%failed() ) return
      if ( .not. allocated ( ctx%zzdskbsr_cache ) ) allocate ( ctx%zzdskbsr_cache )
      if ( ctx%zzdskbsr_cache%active ) then
         if ( ctx%zzdskbsr_cache%epoch /= ctx%spice%segment_epoch ) ctx%zzdskbsr_cache%active = .false.
      end if
      if ( .not. ctx%zzdskbsr_cache%active ) then
         call ctx%setmsg ( 'Must begin a search by calling ZZDSKBSS ' &
         //            'first.' )
         call ctx%sigerr ( 'SPICE(CALLZZDSKBSSFIRST)'                 )
         return
      end if
!
!     PEPPER: the body's segments come from the index, highest priority
!     first (SPICELIB signaled SPICE(BUFFEROVERFLOW) when a body had more
!     segments than its table).
!
      do while ( ctx%zzdskbsr_cache%pos >= 1 )
         s = ctx%spice%zzdskbsr%index%segment ( ctx%zzdskbsr_cache%list, ctx%zzdskbsr_cache%pos )
         ctx%zzdskbsr_cache%pos = ctx%zzdskbsr_cache%pos - 1
         lhan = ctx%spice%zzdskbsr%index%handle(s)
         ldla = ctx%spice%zzdskbsr%index%idescr(1:dladsz,s)
         ldsk = ctx%spice%zzdskbsr%index%descr(1:dskdsz,s)
         if ( cmpfun ( ctx, lhan, ldla, ldsk ) ) then
            handle = lhan
            dladsc(1:dladsz) = ldla
            dskdsc(1:dskdsz) = ldsk
            found  = .true.
            return
         end if
         if ( ctx%failed() ) return
      end do
      return
      end subroutine zzdsksns

      !>## Detailed Input
      !>
      !>     USRCTR    is, on input, the value of a counter maintained by
      !>               a calling routine. This counter is to be compared to
      !>               a local counter.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR    is, on output, the value of the local DSK load/unload
      !>               counter.
      !>
      !>     UPDATE    is a logical flag that is .TRUE. if and only if the
      !>               local DSK load/unload counter differs from the input
      !>               value of USRCTR. If the counters differ, a DSK load or
      !>               unload call has been made since the local counter last
      !>               had the value provided in USRCTR on input.
      !>
      !>## Parameters
      !>
      !>     See zzctr.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine will fail if the total count of DSK load and
      !>         unload operations exceeds the maximum accommodated by a
      !>         two-integer counter. The maximum count is about 4e19,
      !>         presuming a two-integer counter.
      !>
      !>## Files
      !>
      !>     This routine does not access DSK files, but all DSK file
      !>     load and unload operations affect the local counter used
      !>     by this routine.
      !>
      !>## Particulars
      !>
      !>     This set of routines maintains a local counter that indicates
      !>     the kernel load status. When a call is made to ZZDSKLSF or
      !>     ZZDSKUSF, the local counter is incremented. Applications can
      !>     compare counter values they save to the current count by
      !>     calling this routine. A mismatch indicates that DSK load or
      !>     unload calls have been made since an application's counter
      !>     was set.
      !>
      !>     This type of check presumes the caller's counter is initialized
      !>     by a call to
      !>
      !>        ZZCTRUIN
      !>
      !>     and is updated only by calls to this routine.
      !>
      !>     This routine is used by much of the DSK subsystem to determine
      !>     whether locally buffered information about DSK segments remains
      !>     up to date.
      !>
      !>## Examples
      !>
      !>     See usage in ZZDSKBBL.
      !>
      !>## Restrictions
      !>
      !>     This routine assumes the number of DSK load and unload operations
      !>     does not exceed the maximum value of a two-integer counter.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     B.V. Semenov   (JPL)
      !>     R.E. Thurman   (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>## Revisions
      !>
      !>     None.
      module subroutine zzdskchk ( ctx, usrctr, update )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: usrctr ( * )  !! User counter.
      logical, intent(out) :: update  !! Update flag.
      if ( ctx%failed() ) return
      call zzctrchk ( ctx, ctx%spice%zzdskbsr%dskctr, usrctr, update )
      return
      end subroutine zzdskchk

   !> PEPPER: open a DAF (SPK, PCK or CK) for reading, index its segments
   !> and record it as the file searched first (the body of SPKLEF, PCKLOF
   !> and CKLPF). A file loaded again is closed once ([[dafopr]] counted a
   !> second open) and moved to the top. If its segments cannot be read,
   !> the file is closed and not loaded.
   subroutine load_daf_file ( ctx, ix, fname, nd, ni, handle )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(segment_index), intent(inout) :: ix  !! the index of the file's kind
      character(*), intent(in) :: fname  !! the file
      integer(ip), intent(in) :: nd  !! double precision components of a summary
      integer(ip), intent(in) :: ni  !! integer components of a summary
      integer(ip), intent(out) :: handle  !! the file's handle
      call dafopr ( ctx, fname, handle )
      if ( ctx%failed() ) return
      ctx%spice%segment_epoch = ctx%spice%segment_epoch + 1
      if ( ix%file_loaded ( handle ) ) then
         call dafcls ( ctx, handle )
         call ix%remove_file ( handle )
      end if
      call index_daf_file ( ctx, ix, handle, nd, ni )
      if ( ctx%failed() ) then
         call ix%remove_file ( handle )
         call dafcls ( ctx, handle )
         return
      end if
      call ix%add_file ( handle )
   end subroutine load_daf_file

   !> PEPPER: add the segments of a DAF to an index, in the order of the
   !> file, keyed by the first integer component of their summaries.
   !> Segments whose start time is after their end time are left out, as
   !> SPICELIB's searches did.
   subroutine index_daf_file ( ctx, ix, handle, nd, ni )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(segment_index), intent(inout) :: ix  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      integer(ip), intent(in) :: nd  !! double precision components of a summary
      integer(ip), intent(in) :: ni  !! integer components of a summary
      real(wp) :: sum ( 128 )
      real(wp) :: dc ( 125 )
      integer(ip) :: ic ( 250 )
      character(segid_length) :: name
      logical :: fnd
      call dafbfs ( ctx, handle )
      if ( ctx%failed() ) return
      call daffna ( ctx, fnd )
      do while ( fnd .and. .not. ctx%failed() )
         call dafgs ( ctx, sum )
         call dafus ( ctx, sum, nd, ni, dc, ic )
         call dafgn ( ctx, name )
         if ( ctx%failed() ) return
         if ( dc(1) <= dc(2) ) then
            call ix%add ( handle, ic(1), dc(1), dc(2), sum, ic(1:ni), name )
         end if
         call daffna ( ctx, fnd )
      end do
   end subroutine index_daf_file

   !> PEPPER: add the segments of a DSK file to an index, in the order of
   !> the file, keyed by their central body.
   subroutine index_dsk_file ( ctx, ix, handle )
      use pepper_inc_dla, only: dladsz
      use pepper_inc_dskdsc, only: dskdsz, ctridx
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(segment_index), intent(inout) :: ix  !! the index
      integer(ip), intent(in) :: handle  !! the file's handle
      integer(ip) :: dladsc ( dladsz )
      integer(ip) :: nxtdsc ( dladsz )
      real(wp) :: dskdsc ( dskdsz )
      logical :: fnd
      call dlabfs ( ctx, handle, dladsc, fnd )
      do while ( fnd .and. .not. ctx%failed() )
         call dskgd ( ctx, handle, dladsc, dskdsc )
         if ( ctx%failed() ) return
         call ix%add ( handle, nint ( dskdsc(ctridx), ip ), 0.0_wp, 0.0_wp, dskdsc, dladsc, ' ' )
         call dlafns ( ctx, handle, dladsc, nxtdsc, fnd )
         dladsc = nxtdsc
      end do
   end subroutine index_dsk_file

   !> PEPPER: unload a DAF (the body of SPKUEF, PCKUOF and CKUPF): close it
   !> and drop its segments. Nothing happens if it is not loaded.
   subroutine unload_daf_file ( ctx, ix, handle )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(segment_index), intent(inout) :: ix  !! the index of the file's kind
      integer(ip), intent(in) :: handle  !! the file's handle
      if ( .not. ix%file_loaded ( handle ) ) return
      ctx%spice%segment_epoch = ctx%spice%segment_epoch + 1
      call dafcls ( ctx, handle )
      call ix%remove_file ( handle )
   end subroutine unload_daf_file

   !> PEPPER: the segment of highest priority for a body that covers a
   !> time (the search of [[spksfs]] and [[pcksfs]]). The segment returned last for
   !> the body is the answer while the time stays strictly inside its
   !> re-use interval: the part of its coverage that no segment of higher
   !> priority covers, as in SPICELIB.
   subroutine find_segment ( ctx, ix, reuse, body, et, handle, descr, ident, found )
      use pepper_constants, only: dpmax, dpmin
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(segment_index), intent(in) :: ix  !! the index
      type(segment_reuse), intent(inout) :: reuse  !! the context's re-use intervals
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the time
      integer(ip), intent(out) :: handle  !! the segment's file
      real(wp), intent(out) :: descr ( * )  !! the segment's descriptor
      character(*), intent(out) :: ident  !! the segment's identifier
      logical, intent(out) :: found  !! a segment was found
      integer(ip) :: e, i, l, s
      real(wp) :: lb, ub
      found = .false.
      if ( reuse%epoch /= ctx%spice%segment_epoch ) call reuse%reset ( ctx%spice%segment_epoch )
      e = reuse%find ( body )
      if ( e > 0 ) then
         s = reuse%seg(e)
         if ( s > 0 ) then
            if ( et > reuse%lb(e) .and. et < reuse%ub(e) ) then
               call result ( s )
               return
            end if
            call reuse%forget ( e )
         end if
      end if
      l = ix%list ( body )
      lb = dpmin
      ub = dpmax
      do i = ix%count ( l ), 1, -1
         s = ix%segment ( l, i )
         if ( et > ix%tend(s) ) then
            lb = max ( lb, ix%tend(s) )
         else if ( et < ix%tbeg(s) ) then
            ub = min ( ub, ix%tbeg(s) )
         else
            call reuse%store ( body, s, max ( lb, ix%tbeg(s) ), min ( ub, ix%tend(s) ) )
            call result ( s )
            return
         end if
      end do
   contains
      !> Return segment S.
      subroutine result ( s )
         integer(ip), intent(in) :: s  !! the segment
         handle = ix%handle(s)
         descr(1:ix%ndescr) = ix%descr(1:ix%ndescr,s)
         ident  = ix%ident(s)
         found  = .true.
      end subroutine result
   end subroutine find_segment

end submodule pepper_segments
