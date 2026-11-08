!$Procedure DASA2L ( DAS, address to physical location )

      SUBROUTINE DASA2L ( HANDLE, TYPE,   ADDRSS, &
                          CLBASE, CLSIZE, RECNO,  WORDNO )

! $ Abstract
!
!     Map a DAS address to a physical location in a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     TRANSFORMATION
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               RECNO
      INTEGER               WORDNO

      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     TYPE       I   Data type specifier.
!     ADDRSS     I   DAS address of a word of data type TYPE.
!     CLBASE,
!     CLSIZE     O   Cluster base record number and size.
!     RECNO,
!     WORDNO     O   Record/word pair corresponding to ADDRSS.
!     CHAR       P   Parameter indicating character data type.
!     INT        P   Parameter indicating integer data type.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an open DAS file.
!
!     TYPE     is a data type specifier. TYPE may be any of
!              the parameters
!
!                 CHAR
!                 DP
!                 INT
!
!              which indicate `character', `double precision',
!              and `integer' respectively.
!
!
!     ADDRSS   is the address in a DAS of a word of data
!              type TYPE. For each data type (double precision,
!              integer, or character), addresses range
!              from 1 to the maximum current value for that type,
!              which is available from DAFRFR.
!
! $ Detailed_Output
!
!     CLBASE,
!     CLSIZE   are, respectively, the base record number and
!              size, in records, of the cluster containing the
!              word corresponding to ADDRSS. The cluster spans
!              records numbered CLBASE through CLBASE +
!              CLSIZE - 1.
!
!     RECNO,
!     WORD     are, respectively, the number of the physical
!              record and the number of the word within the
!              record that correspond to ADDRSS. Word numbers
!              start at 1 and go up to NC, ND, or NI in
!              character, double precision, or integer records
!              respectively.
!
! $ Parameters
!
!     CHAR,
!     DP,
!     INT      are data type specifiers which indicate
!              `character', `double precision', and `integer'
!              respectively. These parameters are used in
!              all DAS routines that require a data type
!              specifier as input.
!
! $ Exceptions
!
!     If any of the following exceptions occur, the output arguments may
!     contain bogus information.
!
!     1)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
!         is signaled.
!
!     2)  ADDRSS must be between 1 and LAST inclusive, where LAST is
!         last address in the DAS for a word of the specified type. If
!         ADDRSS is out of range, the error SPICE(DASNOSUCHADDRESS) is
!         signaled.
!
!     3)  If this routine doesn't find an expected cluster descriptor
!         in a directory record, the error SPICE(BADDASDIRECTORY) is
!         signaled.
!
!     4)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     The DAS architecture allows a programmer to think of the data
!     within a DAS file as three one-dimensional arrays: one of
!     double precision numbers, one of integers, and one of characters.
!     This model allows a programmer to ask the DAS system for the
!     `nth double precision number (or integer, or character) in the
!     file'.
!
!     DAS files are Fortran direct access files, so to find the
!     `nth double precision number', you must have the number of the
!     record containing it and the `word number', or position, within
!     the record of the double precision number. This routine finds
!     the record/word number pair that specify the physical location
!     in a DAS file corresponding to a DAS address.
!
!     As opposed to DAFs, the mapping of addresses to physical
!     locations for a DAS file depends on the organization of data in
!     the file. For example, given a fixed set of DAS file summary
!     parameters, the physical location of the nth double precision
!     number can depend on how many integer and character records have
!     been written prior to the record containing that double precision
!     number.
!
!     The cluster information output from this routine allows the
!     caller to substantially reduce the number of directory reads
!     required to read a from range of addresses that spans
!     multiple physical records; the reading program only need call
!     this routine once per cluster read, rather than once per
!     physical record read.
!
! $ Examples
!
!     1)  Use this routine to read integers from a range of
!         addresses. This is done in the routine DASRDI.
!
!            C
!            C     Decide how many integers to read.
!            C
!                  NUMINT = LAST - FIRST + 1
!                  NREAD  = 0
!
!            C
!            C     Find out the physical location of the first
!            C     integer. If FIRST is invalid, DASA2L will take care
!            C     of the problem.
!            C
!                  CALL DASA2L (  HANDLE,  INT,     FIRST,
!                 .               CLBASE,  CLSIZE,  RECNO,  WORDNO  )
!
!            C
!            C     Read as much data from record RECNO as necessary.
!            C
!                  N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )
!
!                  CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1,
!                 .              DATA                                 )
!
!                  NREAD  =  N
!                  RECNO  =  RECNO + 1
!
!            C
!            C     Read from as many additional records as necessary.
!            C
!                  DO WHILE ( NREAD .LT. NUMINT )
!            C
!            C        At this point, RECNO if RECNO refers to
!            C        a record in the current cluster, RECNO
!            C        is the correct number of the record to read
!            C        from next. Otherwise, the next cluster of
!            C        records containing integer data must be located.
!            C        CLBASE is the number of the first record of
!            C        the cluster we're about to read from.
!            C
!                     IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
!            C
!            C           We can continue reading from the current
!            C           cluster.
!            C
!                        N  =  MIN ( NUMINT - NREAD,  NWI )
!
!                        CALL DASRRI (  HANDLE,
!                 .                     RECNO,
!                 .                     1,
!                 .                     N,
!                 .                     DATA ( NREAD + 1 )   )
!
!                        NREAD   =   NREAD + N
!                        RECNO   =   RECNO + 1
!
!
!                     ELSE
!            C
!            C           We must find the next integer cluster to
!            C           read from. The first integer in this
!            C           cluster has address FIRST + NREAD.
!            C
!                        CALL DASA2L ( HANDLE,
!                 .                    INT,
!                 .                    FIRST + NREAD,
!                 .                    CLBASE,
!                 .                    CLSIZE,
!                 .                    RECNO,
!                 .                    WORDNO  )
!
!                     END IF
!
!                  END DO
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries from $Revisions section.
!
!-    SPICELIB Version 3.0.0, 09-FEB-2015 (NJB)
!
!        Updated to use DAF/DAS handle manager subsystem.
!
!-    SPICELIB Version 2.0.0, 15-APR-2014 (NJB)
!
!        Previous update was 25-FEB-2014
!
!        Bug fix: value of variable FAST for "unknown" files with one
!        directory record is now stored in TBFAST. The routine
!        previously computed correct outputs but did so more slowly
!        than necessary when multiple "fast" files were accessed.
!
!        Functional change: new entries in the file attribute table are
!        now inserted at index 1; the existing part of the table is
!        shifted to make room. Old entries drop off the end of the
!        list. The previous algorithm simply overwrote the first entry
!        once the table became full.
!
!        The file attribute table was expanded to store values of a
!        "read only" flag for each file. This enables the routine to
!        avoid look up of maximum addresses for known, read-only,
!        non-segregated files.
!
!        Tests of FAILED and backup loop termination checks
!        were added. Logic was introduced to prevent reliance on
!        previous values of logical flags unless those flags were
!        set on a successful call. On any call that fails, the
!        table entry for the current file is marked as unused by
!        setting the handle entry to zero.
!
!        The state variables FIRST and RDONLY have been removed.
!
!        Unneeded declarations were removed.
!
!        The code was re-structured to improve clarity.
!
!-    SPICELIB Version 1.2.1, 20-NOV-2001 (NJB)
!
!        Comment fix: diagram showing directory record pointers
!        incorrectly showed element 2 of the record as a backward
!        pointer. The element is actually a forward pointer.
!
!-    SPICELIB Version 1.2.0, 03-JUL-1996 (NJB)
!
!        Bug fix: calculation to determine whether file is segregated
!        has been fixed.
!
!-    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
!
!        Re-written to optimize address calculations for segregated,
!        read-only files.
!
!-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
!
!        Fixed a typo in the $Brief_I/O section of the header.
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     map DAS logical address to physical location
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0 03-JUL-1996 (NJB)
!
!        Bug fix: calculation to determine whether file is segregated
!        has been fixed. An incorrect variable name used in a bound
!        calculation resulted in an incorrect determination of whether
!        a file was segregated, and caused arithmetic overflow for
!        files with large maximum addresses.
!
!        In the previous version, the number of DAS words in a cluster
!        was incorrectly calculated as the product of the maximum
!        address of the cluster's data type and the number of words of
!        that data type in a DAS record. The correct product involves
!        the number of records in the cluster and the number of words of
!        that data type in a DAS record.
!
!-&


!
!     Programmer's note: the TSPICE routine P_DASA2L must be
!     kept in sync with this routine. Current version of that
!     routine is
!
!        TSPICE Version 1.0.0 APR-11-2014 (NJB)
!


!
!     SPICELIB functions
!
      INTEGER               ISRCHI
      LOGICAL               FAILED

!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI =  256 )

!
!     Directory forward pointer location
!
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )

!
!     Directory address range locations
!
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )

      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )

      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )

!
!     Index of highest address in a `range array':
!
      INTEGER               HIGH
      PARAMETER           ( HIGH = 2 )

!
!     Location of first type descriptor
!
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )

!
!     Access word length
!
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 10 )

!
!     File table size
!
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 20 )

      INTEGER               MAXVEC
      PARAMETER           ( MAXVEC = 3 * MAXFIL )


!
!     Local variables
!
      CHARACTER*(ACCLEN)    ACCESS

      INTEGER               BASERC
      INTEGER               CURTYP
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCLOC
      INTEGER               FIDX
      INTEGER               FREE
      INTEGER               HIADDR
      INTEGER               I
      INTEGER               J
      INTEGER               LSTREC ( 3 )
      INTEGER               LSTWRD ( 3 )
      INTEGER               MXADDR
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDIRS
      INTEGER               NEXT   ( 3 )
      INTEGER               NFILES
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NTYPES
      INTEGER               NW     ( 3 )
      INTEGER               NXTREC
      INTEGER               PREV   ( 3 )
      INTEGER               PRVHAN
      INTEGER               PRVTYP
      INTEGER               RANGE  ( 2 )
      INTEGER               RNGLOC ( 3 )
      INTEGER               TBBASE ( 3, MAXFIL )
      INTEGER               TBFWRD (    MAXFIL )
      INTEGER               TBHAN  (    MAXFIL )
      INTEGER               TBMXAD ( 3, MAXFIL )
      INTEGER               TBSIZE ( 3, MAXFIL )
      INTEGER               UB
      LOGICAL               FAST
      LOGICAL               KNOWN
      LOGICAL               PRVOK
      LOGICAL               TBFAST ( MAXFIL )
      LOGICAL               TBRDON ( MAXFIL )
      LOGICAL               SAMFIL
      LOGICAL               SEGOK

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      DATA                  NEXT   /  2,   3,   1   /
      DATA                  PREV   /  3,   1,   2   /
      DATA                  NW     /  NWC,    NWD,   NWI    /
      DATA                  RNGLOC /  CHRRNG, DPRNG, INTRNG /

      DATA                  FAST   / .FALSE.          /
      DATA                  FIDX   / 0                /
      DATA                  KNOWN  / .FALSE.          /
      DATA                  NFILES / 0                /
      DATA                  PRVHAN / 0                /
      DATA                  PRVOK  / .FALSE.          /
      DATA                  TBBASE / MAXVEC * -1      /
      DATA                  TBFAST / MAXFIL * .FALSE. /
      DATA                  TBFWRD / MAXFIL * -1      /
      DATA                  TBHAN  / MAXFIL *  0      /
      DATA                  TBMXAD / MAXVEC * -1      /
      DATA                  TBRDON / MAXFIL * .FALSE. /
      DATA                  TBSIZE / MAXVEC * -1      /

!
!     Discovery check-in is used in this routine, even though
!     this routine calls routines that can signal errors. This
!     routine is a special case, because fast operation is very
!     important.
!
!
!     DAS files have the following general structure:
!
!           +------------------------+
!           |      file record       |
!           +------------------------+
!           |    reserved records    |
!           |                        |
!           +------------------------+
!           |     comment records    |
!           |                        |
!           |                        |
!           |                        |
!           +------------------------+
!           | first data directory   |
!           +------------------------+
!           |      data records      |
!           |                        |
!           |                        |
!           |                        |
!           |                        |
!           +------------------------+
!                       .
!                       .
!           +------------------------+
!           | last data directory    |
!           +------------------------+
!           |     data records       |
!           |                        |
!           |                        |
!           +------------------------+
!
!
!        Within each DAS data record, word numbers start at one and
!        increase up to NWI, NWD, or NWC: the number of words in an
!        integer, double precision, or character data record.
!
!
!           +--------------------------------+
!           |       |       |   ...  |       |
!           +--------------------------------+
!               1      2                NWD
!
!           +--------------------------------+
!           |   |   |       ...          |   |
!           +--------------------------------+
!             1   2                       NWI
!
!           +------------------------------------+
!           | | |           ...                | |
!           +------------------------------------+
!            1 2                               NWC
!
!
!        Directories are single records that describe the data
!        types of data records that follow. The directories
!        in a DAS file form a doubly linked list: each directory
!        contains forward and backward pointers to the next and
!        previous directories.
!
!        Each directory also contains, for each data type, the lowest
!        and highest logical address occurring in any of the records
!        described by the directory.
!
!        Following the pointers and address range information is
!        a sequence of data type descriptors. These descriptors
!        indicate the data type of data records following the
!        directory record. Each descriptor gives the data type
!        of a maximal set of contiguous data records, all having the
!        same type. By `maximal set' we mean that no data records of
!        the same type bound the set of records in question.
!
!        Pictorially, the structure of a directory is as follows:
!
!           +----------------------------------------------------+
!           | <pointers> | <address ranges> | <type descriptors> |
!           +----------------------------------------------------+
!
!        where the <pointers> section looks like
!
!           +-----------------------------------------+
!           | <backward pointer> | <forward pointer>  |
!           +-----------------------------------------+
!
!        the <address ranges> section looks like
!
!           +-------------------------------------------+
!           | <char range> | <d.p. range> | <int range> |
!           +-------------------------------------------+
!
!        and each range looks like one of:
!
!           +------------------------------------------------+
!           | <lowest char address> | <highest char address> |
!           +------------------------------------------------+
!
!           +------------------------------------------------+
!           | <lowest d.p. address> | <highest d.p. address> |
!           +------------------------------------------------+
!
!           +------------------------------------------------+
!           | <lowest int address>  | <highest int address>  |
!           +------------------------------------------------+
!
!        The type descriptors implement a run-length encoding
!        scheme. The first element of the series of descriptors
!        occupies two integers: it contains a type code and a count.
!        The rest of the descriptors are just signed counts; the data
!        types of the records they describe are deduced from the sign
!        of the count and the data type of the previous descriptor.
!        The method of finding the data type for a given descriptor
!        in terms of its predecessor is as follows: if the sign of a
!        descriptor is positive, the type of that descriptor is the
!        successor of the type of the preceding descriptor in the
!        sequence of types below. If the sign of a descriptor is
!        negative, the type of the descriptor is the predecessor of the
!        type of the preceding descriptor.
!
!           C  -->  D  -->  I  -->  C
!
!        For example, if the preceding type is `I', and a descriptor
!        contains the number 16, the type of the descriptor is `C',
!        whereas if the descriptor contained the number -800, the type
!        of the descriptor would be `D'.
!
!
!     Logic cases
!     ===========
!
!     There are three kinds of file attributes that this
!     routine distinguishes:
!
!        Attributes
!        ----------
!        "FAST"           read-only and segregated
!        "READONLY"       read-only and unsegregated
!        "WRITABLE"       writable
!
!     There are three kinds of file histories that this
!     routine distinguishes:
!
!        History
!        -------
!        "SAME"           file is the same as seen on
!                         the previous call
!
!        "KNOWN"          file is not the same as seen
!                         on the previous call, but file
!                         information is buffered
!
!        "UNKNOWN"        file information is not buffered.
!
!     All combinations of attributes and history are possible,
!     so there are nine cases.
!
!     Mapping actions to cases
!     ========================
!
!        Action                             Cases
!        ------                             -----
!        Set SAMFIL, PRVOK                  ALL
!        Data type check                    ALL
!        Set KNOWN                          not (FAST and SAME)
!        Get access method                  UNKNOWN
!        Buffer insertion                   UNKNOWN
!        Set
!            TBHAN
!            TBRDON
!            TBFAST
!            TBFWRD                         UNKNOWN
!        Get file summary                   UNKNOWN or WRITABLE
!        Set TBMXAD                         UNKNOWN or WRITABLE
!        Segregation check                  UNKNOWN and not WRITABLE
!        Set TBBASE, TBSIZE                 FAST and UNKNOWN
!        Set FAST                           not SAME
!        Address range check                ALL
!        Address search                     READONLY or WRITABLE
!        Set CLBASE, CLSIZE                 ALL
!
!     ========================
!
!
!     Make sure the data type is valid.
!
      IF (  ( TYPE .LT. CHAR ) .OR. ( TYPE .GT. INT )  ) THEN

         CALL CHKIN    ( 'DASA2L'                         )
         CALL SETMSG ( 'Invalid data type: #. File was #' )
         CALL ERRINT ( '#',  TYPE                         )
         CALL ERRHAN ( '#',  HANDLE                       )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'            )
         CALL CHKOUT ( 'DASA2L'                           )
         RETURN

      END IF

!
!     Decide whether we're looking at the same file as we did on the
!     last call. We can use data from the previous call only if that
!     call succeeded.
!
      SAMFIL =  ( HANDLE .EQ. PRVHAN )  .AND.  PRVOK
!
!     PRVOK defaults to .FALSE. and will be reset if this call
!     succeeds.
!
      PRVOK  = .FALSE.

!
!     Fast files get priority handling. If we have a fast file
!     that we saw on the previous call, skip directly to the
!     address range check.
!
      IF (  .NOT. ( FAST .AND. SAMFIL )  ) THEN
!
!        Is this a file we recognize?
!
         IF ( SAMFIL ) THEN

            KNOWN = .TRUE.

         ELSE

            FIDX  =  ISRCHI ( HANDLE, NFILES, TBHAN )
            KNOWN =  FIDX .GT. 0

         END IF

         IF ( KNOWN ) THEN

            FAST = TBFAST(FIDX)

         ELSE
!
!           This file is not in our list. We'll buffer information
!           about this file.
!
!           Shift the table and insert the new entry at the front. The
!           entry at the back will be lost if the table is full.
!
!           Note that unused entries (those for which the DAS handle is
!           0) will drop out of the list automatically.
!
            UB = MIN ( NFILES, MAXFIL-1 )

            DO I = UB, 1, -1

               TBHAN (I+1) = TBHAN (I)
               TBRDON(I+1) = TBRDON(I)
               TBFAST(I+1) = TBFAST(I)
               TBFWRD(I+1) = TBFWRD(I)

               DO J = 1, 3
                  TBBASE( J, I+1 ) = TBBASE( J, I )
                  TBSIZE( J, I+1 ) = TBSIZE( J, I )
                  TBMXAD( J, I+1 ) = TBMXAD( J, I )
               END DO

            END DO
!
!           Insert the new table entry at index 1.
!
            NFILES       =  MIN ( NFILES+1,  MAXFIL )
            FIDX         =  1
            TBHAN(FIDX)  =  HANDLE
!
!           Set FAST to .FALSE. until we find out whether the file
!           is read-only and segregated.
!
            FAST          =  .FALSE.
            TBFAST(FIDX)  =  FAST
!
!           FIDX is now set whether or not the current file is known.
!
!           TBRDON(FIDX) and TBFAST(FIDX) are set.
!
!           Find out whether the file is open for read or write access.
!           We consider the file to be `slow' until we find out
!           otherwise. The contents of the arrays TBBASE, TBSIZE, and
!           TBMXAD are left undefined for slow files.
!
            CALL DASHAM ( HANDLE, ACCESS )

            IF ( FAILED() ) THEN
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               TBHAN(FIDX) = 0
               RETURN

            END IF

!
!           TBRDON(FIDX) indicates whether the file is read-only.
!
            TBRDON(FIDX) =  ACCESS .EQ. 'READ'

         END IF

!
!        FIDX, KNOWN and TBRDON( FIDX ) are set.
!
!        Get the file summary if it isn't known already.
!
         IF (  .NOT.  ( KNOWN .AND. TBRDON(FIDX) )  ) THEN
!
!           The file is new or it's writable; in either case the
!           maximum addresses are unknown. Get the current address
!           range for the file.
!
            CALL DASHFS ( HANDLE, &
                          NRESVR, &
                          NRESVC, &
                          NCOMR, &
                          NCOMC, &
                          FREE, &
                          TBMXAD(1,FIDX), &
                          LSTREC, &
                          LSTWRD )

            IF ( FAILED() ) THEN
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               TBHAN(FIDX) = 0
               RETURN

            END IF

!
!           Set the forward cluster pointer.
!
            TBFWRD(FIDX) = NRESVR + NCOMR + 2

         END IF

!
!        TBMXAD is set.
!
!        If this is an unknown file and is read-only, determine
!        whether the file is segregated
!
         IF (  ( .NOT. KNOWN ) .AND.  TBRDON(FIDX)  ) THEN
!
!           The file is read-only; we need to know whether it is
!           segregated. If so, there are at most three cluster
!           descriptors, and the first directory record's maximum
!           address for each type matches the last logical address for
!           that type.
!
!           FAST has been initialized to .FALSE. above.
!
!           NREC is the record number of the first directory record.
!
            NREC   = TBFWRD( FIDX )

            CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )

            NXTREC = DIRREC( FWDLOC )

            IF ( NXTREC .LE. 0 ) THEN
!
!              If this file is segregated, there are at most three
!              cluster descriptors, and each one points to a cluster
!              containing all records of the corresponding data type.
!              For each data type having a non-zero maximum address,
!              the size of the corresponding cluster must be large
!              enough to hold all addresses of that type.
!
               NTYPES = 0

               DO I = 1, 3

                  IF ( TBMXAD(I,FIDX) .GT. 0 ) THEN
                     NTYPES = NTYPES + 1
                  END IF

               END DO

!
!              Now look at the first NTYPES cluster descriptors,
!              collecting cluster bases and sizes as we go.
!
               BASERC  =  NREC   + 1
               PRVTYP  =  PREV ( DIRREC(BEGDSC) )
               DSCLOC  =  BEGDSC + 1
               SEGOK   = .TRUE.

               DO WHILE (       ( DSCLOC .LE. (BEGDSC+NTYPES) ) &
                          .AND.   SEGOK                        )
!
!                 Find the type of the current descriptor.
!
                  IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                     CURTYP  =  NEXT( PRVTYP )
                  ELSE
                     CURTYP  =  PREV( PRVTYP )
                  END IF

                  PRVTYP                 =   CURTYP
                  TBBASE( CURTYP, FIDX ) =   BASERC
                  TBSIZE( CURTYP, FIDX ) =   ABS ( DIRREC( DSCLOC ) )
                  BASERC                 =   BASERC &
                                           + TBSIZE (CURTYP, FIDX )

                  SEGOK   =       TBMXAD( CURTYP, FIDX ) &
                            .LE.  TBSIZE( CURTYP, FIDX ) * NW(CURTYP)

                  DSCLOC  =  DSCLOC + 1
!
!                 This loop will terminate after at most 3
!                 iterations. No further checks are needed.
!
               END DO

!
!              Update FAST and TBFAST based on the segregation check.
!
               FAST         = SEGOK
               TBFAST(FIDX) = FAST
!
!              If the file is FAST,
!
!                 TBBASE
!                 TBSIZE
!
!              have been updated as well.
!
            END IF

         END IF
!
!        End of the segregation check.
!
      END IF
!
!     End of the NOT FAST or NOT SAME case.
!
!     At this point we have the logical address ranges for the
!     file. Check the input address against them.
!
      MXADDR = TBMXAD( TYPE, FIDX )

      IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN
!
!        Make sure the current table entry won't be found on a
!        subsequent search.
!
         TBHAN(FIDX) = 0

         CALL CHKIN    ( 'DASA2L'                                 )
         CALL SETMSG ( 'ADDRSS was #; valid range for type # is ' &
         //            '# to #.  File was #'                      )
         CALL ERRINT ( '#',  ADDRSS                               )
         CALL ERRINT ( '#',  TYPE                                 )
         CALL ERRINT ( '#',  1                                    )
         CALL ERRINT ( '#',  MXADDR                               )
         CALL ERRHAN ( '#',  HANDLE                               )
         CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'                  )
         CALL CHKOUT ( 'DASA2L'                                   )
         RETURN

      END IF

!
!     If we're looking at a "fast" file, we know the cluster base and
!     size. HIADDR is the highest address (not necessarily in use) in
!     the cluster.
!
      IF ( TBFAST(FIDX) ) THEN
!
!        The current file is "fast": read-only and segregated.
!
         CLBASE = TBBASE( TYPE, FIDX )
         CLSIZE = TBSIZE( TYPE, FIDX )
         HIADDR = CLSIZE * NW(TYPE)

      ELSE
!
!        If we're not looking at a "fast" file, find the cluster
!        containing the input address, for the input data type.
!
!        Find out which directory describes the cluster containing this
!        word. To do this, we must traverse the directory list. The
!        first directory record comes right after the last comment
!        record. (Don't forget the file record when counting the
!        predecessors of the directory record.)
!
!        Note that we don't need to worry about not finding a directory
!        record that contains the address we're looking for, since
!        we've already checked that the address is in range.
!
         NREC  = TBFWRD( FIDX )
         NDIRS = 1

         CALL DASRRI ( HANDLE, &
                       NREC, &
                       RNGLOC(TYPE), &
                       RNGLOC(TYPE)+1, &
                       RANGE )


         DO WHILE ( RANGE(HIGH) .LT. ADDRSS )
!
!           The record number of the next directory is the forward
!           pointer in the current directory record. Update NREC with
!           this pointer. Get the address range for the specified type
!           covered by this next directory record.
!
            CALL DASRRI ( HANDLE, NREC, FWDLOC, FWDLOC, NXTREC )

            NREC  = NXTREC
            NDIRS = NDIRS + 1

            CALL DASRRI ( HANDLE, &
                          NREC, &
                          RNGLOC(TYPE), &
                          RNGLOC(TYPE)+1, &
                          RANGE           )

            IF ( FAILED() ) THEN
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               TBHAN(FIDX) = 0

               RETURN

            END IF

         END DO

!
!        NREC is now the record number of the directory that contains
!        the type descriptor for the address we're looking for.
!
!        Our next task is to find the descriptor for the cluster
!        containing the input address. To do this, we must examine the
!        directory record in `left-to-right' order. As we do so, we'll
!        keep track of the highest address of type TYPE occurring in
!        the clusters whose descriptors we've seen. The variable HIADDR
!        will contain this address.
!
         CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )

         IF ( FAILED() ) THEN
!
!           Make sure the current table entry won't be found on a
!           subsequent search.
!
            TBHAN(FIDX) = 0

            RETURN

         END IF

!
!        In the process of finding the physical location corresponding
!        to ADDRSS, we'll find the record number of the base of the
!        cluster containing ADDRSS. We'll start out by initializing
!        this value with the number of the first data record of the
!        next cluster.
!
         CLBASE  =  NREC + 1

!
!        We'll initialize HIADDR with the value preceding the lowest
!        address of type TYPE described by the current directory.
!
         HIADDR  =  DIRREC(RNGLOC(TYPE)) - 1

!
!        Initialize the number of records described by the last seen
!        type descriptor. This number, when added to CLBASE, should
!        yield the number of the first record of the current cluster;
!        that's why it's initialized to 0.
!
         CLSIZE  =  0

!
!        Now find the descriptor for the cluster containing ADDRSS.
!        Read descriptors until we get to the one that describes the
!        record containing ADDRSS. Keep track of descriptor data
!        types as we go. Also count the descriptors.
!
!        At this point, HIADDR is less than ADDRSS, so the loop will
!        always be executed at least once.
!
         PRVTYP  =  PREV ( DIRREC(BEGDSC) )
         DSCLOC  =  BEGDSC + 1

         DO WHILE ( HIADDR .LT. ADDRSS )

            IF ( DSCLOC .GT. NWI ) THEN
!
!              This situation shouldn't occur, but it might if the
!              DAS file is corrupted.
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               TBHAN(FIDX) = 0

               CALL CHKIN  ( 'DASA2L'                               )
               CALL SETMSG ( 'Directory record # in DAS file with ' &
               //            'handle # is probably corrupted. No ' &
               //            'high cluster address at or above the ' &
               //            'input address # was found, though it ' &
               //            'should have been. High address was #. ' &
               //            'Data type was #.'                     )
               CALL ERRINT ( '#', NREC                              )
               CALL ERRINT ( '#', HANDLE                            )
               CALL ERRINT ( '#', ADDRSS                            )
               CALL ERRINT ( '#', HIADDR                            )
               CALL ERRINT ( '#', TYPE                              )
               CALL SIGERR ( 'SPICE(BADDASDIRECTORY)'               )
               CALL CHKOUT ( 'DASA2L'                               )
               RETURN

            END IF
!
!           Update CLBASE so that it is the record number of the
!           first record of the current cluster.
!
            CLBASE = CLBASE + CLSIZE
!
!           Find the type of the current descriptor.
!
            IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
               CURTYP  =   NEXT( PRVTYP )
            ELSE
               CURTYP  =   PREV( PRVTYP )
            END IF

!
!           Forgetting to update PRVTYP is a Very Bad Thing (VBT).
!
            PRVTYP = CURTYP

!
!           If the current descriptor is of the type we're interested
!           in, update the highest address count.
!
            IF ( CURTYP .EQ. TYPE ) THEN
               HIADDR = HIADDR + ( NW(TYPE) * ABS( DIRREC(DSCLOC) ) )
            END IF

!
!           Compute the number of records described by the current
!           descriptor. Update the descriptor location.
!
            CLSIZE  =  ABS (  DIRREC( DSCLOC )  )
            DSCLOC  =  DSCLOC + 1

         END DO
!
!        At this point, the variables
!
!           CLBASE
!           CLSIZE
!           HIADDR
!
!        are set.
!
      END IF

!
!     At this point,
!
!        -- CLBASE is properly set: it is the record number of the
!           first record of the cluster containing ADDRSS.
!
!        -- CLSIZE is properly set: it is the size of the cluster
!           containing ADDRSS.
!
!        -- HIADDR is the last logical address in the cluster
!           containing ADDRSS.
!
!     Now we must find the physical record and word corresponding
!     to ADDRSS. The structure of the cluster containing ADDRSS and
!     HIADDR is shown below:
!
!        +--------------------------------------+
!        |                                      |  Record # CLBASE
!        +--------------------------------------+
!                           .
!                           .
!                           .
!        +--------------------------------------+
!        |      |ADDRSS|                        |  Record # RECNO
!        +--------------------------------------+
!                           .
!                           .
!                           .
!        +--------------------------------------+  Record #
!        |                               |HIADDR|
!        +--------------------------------------+  CLBASE + CLSIZE - 1
!
!
      RECNO  =     ( CLBASE + CLSIZE - 1 ) &
                -  ( HIADDR - ADDRSS     ) / NW(TYPE)

      WORDNO =    ADDRSS    -   (  (ADDRSS-1) / NW(TYPE) ) * NW(TYPE)
!
!     Update PRVHAN and set PRVOK to .TRUE. only if the call succeeded.
!
      PRVHAN =  HANDLE
      PRVOK  = .TRUE.

      RETURN
      END
