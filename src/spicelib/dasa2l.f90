!$Procedure DASA2L ( DAS, address to physical location )

      subroutine dasa2l ( handle, type,   addrss, &
                          clbase, clsize, recno,  wordno )
      use pepper_kinds, only: ip

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

      implicit none

      integer(ip)           handle
      integer(ip)           type
      integer(ip)           addrss
      integer(ip)           clbase
      integer(ip)           clsize
      integer(ip)           recno
      integer(ip)           wordno

      integer(ip)           char
      parameter           ( char   =  1  )

      integer(ip)           int
      parameter           ( int    =  3  )

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
      integer(ip)           isrchi
      logical               failed

!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      integer(ip)           nwc
      parameter           ( nwc = 1024 )

      integer(ip)           nwd
      parameter           ( nwd =  128 )

      integer(ip)           nwi
      parameter           ( nwi =  256 )

!
!     Directory forward pointer location
!
      integer(ip)           fwdloc
      parameter           ( fwdloc = 2 )

!
!     Directory address range locations
!
      integer(ip)           chrrng
      parameter           ( chrrng =          3 )

      integer(ip)           dprng
      parameter           ( dprng  = chrrng + 2 )

      integer(ip)           intrng
      parameter           ( intrng = dprng  + 2 )

!
!     Index of highest address in a `range array':
!
      integer(ip)           high
      parameter           ( high = 2 )

!
!     Location of first type descriptor
!
      integer(ip)           begdsc
      parameter           ( begdsc = 9 )

!
!     Access word length
!
      integer(ip)           acclen
      parameter           ( acclen = 10 )

!
!     File table size
!
      integer(ip)           maxfil
      parameter           ( maxfil = 20 )

      integer(ip)           maxvec
      parameter           ( maxvec = 3 * maxfil )


!
!     Local variables
!
      character(acclen)     access

      integer(ip)           baserc
      integer(ip)           curtyp
      integer(ip)           dirrec ( nwi )
      integer(ip)           dscloc
      integer(ip)           fidx
      integer(ip)           free
      integer(ip)           hiaddr
      integer(ip)           i
      integer(ip)           j
      integer(ip)           lstrec ( 3 )
      integer(ip)           lstwrd ( 3 )
      integer(ip)           mxaddr
      integer(ip)           ncomc
      integer(ip)           ncomr
      integer(ip)           ndirs
      integer(ip)           next   ( 3 )
      integer(ip)           nfiles
      integer(ip)           nrec
      integer(ip)           nresvc
      integer(ip)           nresvr
      integer(ip)           ntypes
      integer(ip)           nw     ( 3 )
      integer(ip)           nxtrec
      integer(ip)           prev   ( 3 )
      integer(ip)           prvhan
      integer(ip)           prvtyp
      integer(ip)           range  ( 2 )
      integer(ip)           rngloc ( 3 )
      integer(ip)           tbbase ( 3, maxfil )
      integer(ip)           tbfwrd (    maxfil )
      integer(ip)           tbhan  (    maxfil )
      integer(ip)           tbmxad ( 3, maxfil )
      integer(ip)           tbsize ( 3, maxfil )
      integer(ip)           ub
      logical               fast
      logical               known
      logical               prvok
      logical               tbfast ( maxfil )
      logical               tbrdon ( maxfil )
      logical               samfil
      logical               segok

!
!     Saved variables
!
      save

!
!     Initial values
!
!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      data                  next   /  2,   3,   1   /
      data                  prev   /  3,   1,   2   /
      data                  nw     /  nwc,    nwd,   nwi    /
      data                  rngloc /  chrrng, dprng, intrng /

      data                  fast   / .false.          /
      data                  fidx   / 0                /
      data                  known  / .false.          /
      data                  nfiles / 0                /
      data                  prvhan / 0                /
      data                  prvok  / .false.          /
      data                  tbbase / maxvec * -1      /
      data                  tbfast / maxfil * .false. /
      data                  tbfwrd / maxfil * -1      /
      data                  tbhan  / maxfil *  0      /
      data                  tbmxad / maxvec * -1      /
      data                  tbrdon / maxfil * .false. /
      data                  tbsize / maxvec * -1      /

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
      if (  ( type < char ) .or. ( type > int )  ) then

         call chkin    ( 'DASA2L'                         )
         call setmsg ( 'Invalid data type: #. File was #' )
         call errint ( '#',  type                         )
         call errhan ( '#',  handle                       )
         call sigerr ( 'SPICE(DASINVALIDTYPE)'            )
         call chkout ( 'DASA2L'                           )
         return

      end if

!
!     Decide whether we're looking at the same file as we did on the
!     last call. We can use data from the previous call only if that
!     call succeeded.
!
      samfil =  ( handle == prvhan )  .and.  prvok
!
!     PRVOK defaults to .FALSE. and will be reset if this call
!     succeeds.
!
      prvok  = .false.

!
!     Fast files get priority handling. If we have a fast file
!     that we saw on the previous call, skip directly to the
!     address range check.
!
      if (  .not. ( fast .and. samfil )  ) then
!
!        Is this a file we recognize?
!
         if ( samfil ) then

            known = .true.

         else

            fidx  =  isrchi ( handle, nfiles, tbhan )
            known =  fidx > 0

         end if

         if ( known ) then

            fast = tbfast(fidx)

         else
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
            ub = min ( nfiles, maxfil-1 )

            do i = ub, 1, -1

               tbhan (i+1) = tbhan (i)
               tbrdon(i+1) = tbrdon(i)
               tbfast(i+1) = tbfast(i)
               tbfwrd(i+1) = tbfwrd(i)

               do j = 1, 3
                  tbbase( j, i+1 ) = tbbase( j, i )
                  tbsize( j, i+1 ) = tbsize( j, i )
                  tbmxad( j, i+1 ) = tbmxad( j, i )
               end do

            end do
!
!           Insert the new table entry at index 1.
!
            nfiles       =  min ( nfiles+1,  maxfil )
            fidx         =  1
            tbhan(fidx)  =  handle
!
!           Set FAST to .FALSE. until we find out whether the file
!           is read-only and segregated.
!
            fast          =  .false.
            tbfast(fidx)  =  fast
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
            call dasham ( handle, access )

            if ( failed() ) then
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               tbhan(fidx) = 0
               return

            end if

!
!           TBRDON(FIDX) indicates whether the file is read-only.
!
            tbrdon(fidx) =  access == 'READ'

         end if

!
!        FIDX, KNOWN and TBRDON( FIDX ) are set.
!
!        Get the file summary if it isn't known already.
!
         if (  .not.  ( known .and. tbrdon(fidx) )  ) then
!
!           The file is new or it's writable; in either case the
!           maximum addresses are unknown. Get the current address
!           range for the file.
!
            call dashfs ( handle, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc, &
                          free, &
                          tbmxad(1,fidx), &
                          lstrec, &
                          lstwrd )

            if ( failed() ) then
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               tbhan(fidx) = 0
               return

            end if

!
!           Set the forward cluster pointer.
!
            tbfwrd(fidx) = nresvr + ncomr + 2

         end if

!
!        TBMXAD is set.
!
!        If this is an unknown file and is read-only, determine
!        whether the file is segregated
!
         if (  ( .not. known ) .and.  tbrdon(fidx)  ) then
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
            nrec   = tbfwrd( fidx )

            call dasrri ( handle, nrec, 1, nwi, dirrec )

            nxtrec = dirrec( fwdloc )

            if ( nxtrec <= 0 ) then
!
!              If this file is segregated, there are at most three
!              cluster descriptors, and each one points to a cluster
!              containing all records of the corresponding data type.
!              For each data type having a non-zero maximum address,
!              the size of the corresponding cluster must be large
!              enough to hold all addresses of that type.
!
               ntypes = 0

               do i = 1, 3

                  if ( tbmxad(i,fidx) > 0 ) then
                     ntypes = ntypes + 1
                  end if

               end do

!
!              Now look at the first NTYPES cluster descriptors,
!              collecting cluster bases and sizes as we go.
!
               baserc  =  nrec   + 1
               prvtyp  =  prev ( dirrec(begdsc) )
               dscloc  =  begdsc + 1
               segok   = .true.

               do while (       ( dscloc <= (begdsc+ntypes) ) &
                          .and.   segok                        )
!
!                 Find the type of the current descriptor.
!
                  if ( dirrec(dscloc) > 0 ) then
                     curtyp  =  next( prvtyp )
                  else
                     curtyp  =  prev( prvtyp )
                  end if

                  prvtyp                 =   curtyp
                  tbbase( curtyp, fidx ) =   baserc
                  tbsize( curtyp, fidx ) =   abs ( dirrec( dscloc ) )
                  baserc                 =   baserc &
                                           + tbsize (curtyp, fidx )

                  segok   =       tbmxad( curtyp, fidx ) &
                            <=  tbsize( curtyp, fidx ) * nw(curtyp)

                  dscloc  =  dscloc + 1
!
!                 This loop will terminate after at most 3
!                 iterations. No further checks are needed.
!
               end do

!
!              Update FAST and TBFAST based on the segregation check.
!
               fast         = segok
               tbfast(fidx) = fast
!
!              If the file is FAST,
!
!                 TBBASE
!                 TBSIZE
!
!              have been updated as well.
!
            end if

         end if
!
!        End of the segregation check.
!
      end if
!
!     End of the NOT FAST or NOT SAME case.
!
!     At this point we have the logical address ranges for the
!     file. Check the input address against them.
!
      mxaddr = tbmxad( type, fidx )

      if (  ( addrss < 1 ) .or. ( addrss > mxaddr )  ) then
!
!        Make sure the current table entry won't be found on a
!        subsequent search.
!
         tbhan(fidx) = 0

         call chkin    ( 'DASA2L'                                 )
         call setmsg ( 'ADDRSS was #; valid range for type # is ' &
         //            '# to #.  File was #'                      )
         call errint ( '#',  addrss                               )
         call errint ( '#',  type                                 )
         call errint ( '#',  1                                    )
         call errint ( '#',  mxaddr                               )
         call errhan ( '#',  handle                               )
         call sigerr ( 'SPICE(DASNOSUCHADDRESS)'                  )
         call chkout ( 'DASA2L'                                   )
         return

      end if

!
!     If we're looking at a "fast" file, we know the cluster base and
!     size. HIADDR is the highest address (not necessarily in use) in
!     the cluster.
!
      if ( tbfast(fidx) ) then
!
!        The current file is "fast": read-only and segregated.
!
         clbase = tbbase( type, fidx )
         clsize = tbsize( type, fidx )
         hiaddr = clsize * nw(type)

      else
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
         nrec  = tbfwrd( fidx )
         ndirs = 1

         call dasrri ( handle, &
                       nrec, &
                       rngloc(type), &
                       rngloc(type)+1, &
                       range )


         do while ( range(high) < addrss )
!
!           The record number of the next directory is the forward
!           pointer in the current directory record. Update NREC with
!           this pointer. Get the address range for the specified type
!           covered by this next directory record.
!
            call dasrri ( handle, nrec, fwdloc, fwdloc, nxtrec )

            nrec  = nxtrec
            ndirs = ndirs + 1

            call dasrri ( handle, &
                          nrec, &
                          rngloc(type), &
                          rngloc(type)+1, &
                          range           )

            if ( failed() ) then
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               tbhan(fidx) = 0

               return

            end if

         end do

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
         call dasrri ( handle, nrec, 1, nwi, dirrec )

         if ( failed() ) then
!
!           Make sure the current table entry won't be found on a
!           subsequent search.
!
            tbhan(fidx) = 0

            return

         end if

!
!        In the process of finding the physical location corresponding
!        to ADDRSS, we'll find the record number of the base of the
!        cluster containing ADDRSS. We'll start out by initializing
!        this value with the number of the first data record of the
!        next cluster.
!
         clbase  =  nrec + 1

!
!        We'll initialize HIADDR with the value preceding the lowest
!        address of type TYPE described by the current directory.
!
         hiaddr  =  dirrec(rngloc(type)) - 1

!
!        Initialize the number of records described by the last seen
!        type descriptor. This number, when added to CLBASE, should
!        yield the number of the first record of the current cluster;
!        that's why it's initialized to 0.
!
         clsize  =  0

!
!        Now find the descriptor for the cluster containing ADDRSS.
!        Read descriptors until we get to the one that describes the
!        record containing ADDRSS. Keep track of descriptor data
!        types as we go. Also count the descriptors.
!
!        At this point, HIADDR is less than ADDRSS, so the loop will
!        always be executed at least once.
!
         prvtyp  =  prev ( dirrec(begdsc) )
         dscloc  =  begdsc + 1

         do while ( hiaddr < addrss )

            if ( dscloc > nwi ) then
!
!              This situation shouldn't occur, but it might if the
!              DAS file is corrupted.
!
!              Make sure the current table entry won't be found
!              on a subsequent search.
!
               tbhan(fidx) = 0

               call chkin  ( 'DASA2L'                               )
               call setmsg ( 'Directory record # in DAS file with ' &
               //            'handle # is probably corrupted. No ' &
               //            'high cluster address at or above the ' &
               //            'input address # was found, though it ' &
               //            'should have been. High address was #. ' &
               //            'Data type was #.'                     )
               call errint ( '#', nrec                              )
               call errint ( '#', handle                            )
               call errint ( '#', addrss                            )
               call errint ( '#', hiaddr                            )
               call errint ( '#', type                              )
               call sigerr ( 'SPICE(BADDASDIRECTORY)'               )
               call chkout ( 'DASA2L'                               )
               return

            end if
!
!           Update CLBASE so that it is the record number of the
!           first record of the current cluster.
!
            clbase = clbase + clsize
!
!           Find the type of the current descriptor.
!
            if ( dirrec(dscloc) > 0 ) then
               curtyp  =   next( prvtyp )
            else
               curtyp  =   prev( prvtyp )
            end if

!
!           Forgetting to update PRVTYP is a Very Bad Thing (VBT).
!
            prvtyp = curtyp

!
!           If the current descriptor is of the type we're interested
!           in, update the highest address count.
!
            if ( curtyp == type ) then
               hiaddr = hiaddr + ( nw(type) * abs( dirrec(dscloc) ) )
            end if

!
!           Compute the number of records described by the current
!           descriptor. Update the descriptor location.
!
            clsize  =  abs (  dirrec( dscloc )  )
            dscloc  =  dscloc + 1

         end do
!
!        At this point, the variables
!
!           CLBASE
!           CLSIZE
!           HIADDR
!
!        are set.
!
      end if

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
      recno  =     ( clbase + clsize - 1 ) &
                -  ( hiaddr - addrss     ) / nw(type)

      wordno =    addrss    -   (  (addrss-1) / nw(type) ) * nw(type)
!
!     Update PRVHAN and set PRVOK to .TRUE. only if the call succeeded.
!
      prvhan =  handle
      prvok  = .true.

      return
      end
