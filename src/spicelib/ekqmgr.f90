!$Procedure EKQMGR  ( EK, query manager )

      subroutine ekqmgr (  cindex,  elment,  eqryc,   eqryd,   eqryi, &
                           fname,   row,     selidx,  column,  handle, &
                           n,       table,   attdsc,  ccount,  found, &
                           nelt,    nmrows,  semerr,  errmsg,  cdata, &
                           ddata,   idata,   null                     )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Manage query operations on EK files.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     SEARCH
!
! $ Declarations

      implicit none

      include  'ekattdsc.inc'
      include  'ekbool.inc'
      include  'ekcnamsz.inc'
      include  'ekcoldsc.inc'
      include  'ekjrs.inc'
      include  'ekopcd.inc'
      include  'ekqlimit.inc'
      include  'ekquery.inc'
      include  'eksegdsc.inc'
      include  'ektnamsz.inc'
      include  'ektype.inc'

      integer(ip), parameter :: ftsize = 20

      integer(ip), parameter :: stsize = 200

      integer(ip), parameter :: mxtbld = 100

      integer(ip), parameter :: mxclld = 500

      integer(ip), parameter :: lbcell = -5


      integer(ip)           cindex
      integer(ip)           elment
      character(*)          eqryc
      real(wp)              eqryd  ( * )
      integer(ip)           eqryi  ( lbcell : * )
      character(*)          fname
      integer(ip)           row
      integer(ip)           selidx
      character(*)          column
      integer(ip)           handle
      integer(ip)           n
      character(*)          table
      integer(ip)           attdsc ( adscsz )
      integer(ip)           ccount
      logical               found
      integer(ip)           nelt
      integer(ip)           nmrows
      logical               semerr
      character(*)          errmsg
      character(*)          cdata
      real(wp)              ddata
      integer(ip)           idata
      logical               null

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     CINDEX     I   EKCII
!     ELMENT     I   EKGC, EKGD, EKGI
!     EQRYC      I   EKSRCH
!     EQRYD      I   EKSRCH
!     EQRYI      I   EKSRCH
!     FNAME      I   EKLEF
!     ROW        I   EKGC, EKGD, EKGI, EKNELT
!     SELIDX     I   EKGC, EKGD, EKGI, EKNELT
!     COLUMN    I-O  EKCIN, EKGC, EKGD, EKGI, EKNELT, EKCII
!     HANDLE    I-O  EKLEF, EKUEF
!     N         I-O  EKTNAM, EKNTAB
!     TABLE     I-O  EKCCNT, EKCII, EKTNAM
!     ATTDSC     O   EKCII, EKCIN
!     CCOUNT     O   EKCCNT
!     FOUND      O   EKCIN, EKGC, EKGD, EKGI
!     NELT       O   EKNELT
!     NMROWS     O   EKSRCH
!     SEMERR     O   EKSRCH
!     ERRMSG     O   EKSRCH
!     CDATA      O   EKGC
!     DDATA      O   EKGD
!     IDATA      O   EKGI
!     NULL       O   EKGC, EKGD, EKGI
!     FTSIZE     P   All
!     MAXCON     P   All
!     MXCLLD     P   All
!     STSIZE     P   All
!     MAXORD     P   All
!     CNAMSZ     P   All
!     ITSIZE     P   All
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
!     FTSIZE   is the maximum number of EK files that may be
!              loaded. Any other DAS files loaded by the calling
!              program count against this limit.
!
!     STSIZE   is the size of the segment table; this is the
!              maximum number of segments that can be loaded at
!              one time.
!
!     MXTBLD   is the maximum number of tables that can be loaded
!              at any time. A table can consist of multiple
!              segments.
!
!     MXCLLD   is the maximum number of columns that can be loaded
!              at any time. A column may be spread across
!              multiple segments; in this case, the portions of
!              the column contained in each segment count against
!              this limit.
!
!     ADSCSZ   is the size of column attribute descriptor.
!              (Defined in ekattdsc.inc.)
!
!     LBCELL   is the SPICE cell lower bound.
!
!     Many other parameters are defined in the include files referenced
!     above. See those files for details.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See the headers of the entry points for descriptions of
!         exceptions specific to those routines.
!
! $ Files
!
!     This suite of routines reads binary `sequence component' EK files.
!     In order for a binary EK file to be accessible to this routine,
!     the file must be `loaded' via a call to the entry point EKLEF.
!
!     Text format EK files cannot be used by this routine; they must
!     first be converted by binary format by the NAIF Toolkit utility
!     SPACIT.
!
! $ Particulars
!
!     EKQMGR is an umbrella routine for its entry points: all variables
!     used by the entry points are declared here.
!
!     EKQMGR supports loading and unloading EK files, executing queries,
!     and fetching the results of executed queries. The entry points
!     and their functions are:
!
!        File loading and unloading:
!
!           EKLEF  ( EK, load event file   )
!           EKUEF  ( EK, unload event file )
!
!        Query execution:
!
!           EKSRCH ( EK, search for events )
!
!        Fetching query results:
!
!           EKGC   ( EK, get event data, character        )
!           EKGD   ( EK, get event data, double precision )
!           EKGI   ( EK, get event data, integer          )
!
!        Utilities:
!
!           EKNTAB ( EK, return the number of loaded tables        )
!           EKTNAM ( EK, return the names of loaded tables         )
!           EKCCNT ( EK, return the column count of a table        )
!           EKCII  ( EK, look up column info by index              )
!           EKNELT ( EK, return number of elements in column entry )
!
!
!     To issue queries to the EK system, users would normally call the
!     high-level interface routine EKFIND. EKFIND parses queries and
!     converts them to the encoded form expected by EKSRCH. It is
!     possible to call EKSRCH directly, but this should not be attempted
!     by others than EK masters. EKFIND is not an entry point of
!     EKQMGR, but instead is a separate subroutine.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Query the EK system and fetch data matching that query.
!
!        The program shown here does not rely on advance
!        knowledge of the input query or the contents of any loaded EK
!        files.
!
!        To simplify the example, we assume that all data are scalar.
!        This assumption relieves us of the need to test the size of
!        column entries before fetching them. In the event that a
!        column contains variable-size array entries, the entry point
!        EKNELT may be called to obtain the size of column entries to
!        be fetched. See EKNELT for an example.
!
!
!        Use the EK kernel below to load the information from the
!        original Supplementary Engineering Data Record (SEDR) data
!        set generated by the Viking Project.
!
!           vo_sedr.bdb
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!
!        Example code begins here.
!
!
!              PROGRAM EKQMGR_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include EK Query Limit Parameters
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
!
!              CHARACTER*(*)         LSKNAM
!              PARAMETER           ( LSKNAM = 'naif0012.tls' )
!
!              INTEGER               DESCSZ
!              PARAMETER           ( DESCSZ = 31   )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               ITEMSZ
!              PARAMETER           ( ITEMSZ = DESCSZ + 4 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 27   )
!
!              INTEGER               TYPLEN
!              PARAMETER           ( TYPLEN = 4    )
!
!              INTEGER               XCLSLN
!              PARAMETER           ( XCLSLN = 4    )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(MAXSTR)    CDATA
!              CHARACTER*(MAXCLN)    COLS   ( MAXSEL )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(ITEMSZ)    ITEM
!              CHARACTER*(DESCSZ)    OUTSTR
!              CHARACTER*(MAXQRY)    QUERY
!              CHARACTER*(TIMLEN)    UTCSTR
!              CHARACTER*(MAXCLN)    TABS   ( MAXTAB )
!              CHARACTER*(XCLSLN)    XCLASS ( MAXSEL )
!              CHARACTER*(TYPLEN)    XTYPES ( MAXSEL )
!
!              DOUBLE PRECISION      DDATA
!              DOUBLE PRECISION      TDATA
!
!              INTEGER               B
!              INTEGER               COLNO
!              INTEGER               E
!              INTEGER               HANDLE
!              INTEGER               IDATA
!              INTEGER               N
!              INTEGER               NMROWS
!              INTEGER               ROW
!              INTEGER               XBEGS  ( MAXSEL )
!              INTEGER               XENDS  ( MAXSEL )
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               NULL
!
!        C
!        C     Load leapseconds file for time conversion.
!        C
!              CALL FURNSH ( LSKNAM )
!
!        C
!        C     Load EK.
!        C
!              CALL EKLEF  ( EKNAME, HANDLE )
!
!        C
!        C     Setup the query.  Parse the SELECT clause using
!        C     EKPSEL.
!        C
!              QUERY = 'Select IMAGE_NUMBER, IMAGE_ID, '
!             .   //          'PLATFORM_CLOCK, IMAGE_TIME '
!             .   //   'from VIKING_SEDR_DATA '
!             .   //   'where IMAGE_NUMBER < 25850000 '
!             .   //   'order by IMAGE_NUMBER'
!
!              CALL EKPSEL ( QUERY,  N,    XBEGS, XENDS, XTYPES,
!             .              XCLASS, TABS, COLS,  ERROR, ERRMSG )
!
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) ERRMSG
!
!              ELSE
!
!        C
!        C        Submit query to the EK query system.
!        C
!                 CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!                 IF ( ERROR ) THEN
!
!                    WRITE(*,*) ERRMSG
!
!                 ELSE
!
!        C
!        C           Fetch the rows that matched the query.
!        C
!                    DO ROW = 1, NMROWS
!
!        C
!        C              Fetch data from the Ith row.
!        C
!                       WRITE (*,*) ' '
!                       WRITE (*,*) 'ROW = ', ROW
!
!                       DO COLNO = 1, N
!
!        C
!        C                 Fetch the data from the Jth selected
!        C                 column.
!        C
!                          IF ( XCLASS(COLNO) .EQ. 'COL' ) THEN
!
!                             OUTSTR  =  COLS(COLNO)
!                             CALL PREFIX ( '.',         0, OUTSTR )
!                             CALL PREFIX ( TABS(COLNO), 0, OUTSTR )
!                             ITEM = '  ' // OUTSTR // ':'
!
!                          ELSE
!
!                             B  =  XBEGS(COLNO)
!                             E  =  XENDS(COLNO)
!                             ITEM = '  ITEM = ' // QUERY(B:E)
!
!                          END IF
!
!                          IF ( XTYPES(COLNO) .EQ. 'CHR' ) THEN
!
!                             CALL EKGC ( COLNO,  ROW,  1,
!             .                           CDATA, NULL, FOUND )
!
!                             IF ( NULL ) THEN
!                                WRITE(*,*) ITEM, '<Null>'
!                             ELSE
!                                WRITE(*,*) ITEM, CDATA(:RTRIM(CDATA))
!                             END IF
!
!
!                          ELSE IF ( XTYPES(COLNO) .EQ. 'DP' ) THEN
!
!                             CALL EKGD ( COLNO,  ROW,  1,
!             .                           DDATA, NULL, FOUND )
!
!                             IF ( NULL ) THEN
!                                WRITE(*,*) ITEM, '<Null>'
!                             ELSE
!                                WRITE(*,*) ITEM, DDATA
!                             END IF
!
!
!                          ELSE IF ( XTYPES(COLNO) .EQ. 'INT' ) THEN
!
!                             CALL EKGI ( COLNO,  ROW,  1,
!             .                           IDATA, NULL, FOUND )
!
!                             IF ( NULL ) THEN
!                                WRITE(*,*) ITEM, '<Null>'
!                             ELSE
!                                WRITE(*,*) ITEM, IDATA
!                             END IF
!
!
!                          ELSE
!        C
!        C                    The item is a time value.  Convert it
!        C                    to UTC for output.
!        C
!                             CALL EKGD ( COLNO,  ROW,  1,
!             .                           TDATA, NULL, FOUND )
!
!                             IF ( NULL ) THEN
!                                WRITE(*,*) ITEM, '<Null>'
!                             ELSE
!                                CALL ET2UTC ( TDATA, 'C', 3, UTCSTR )
!                                WRITE(*,*) ITEM, UTCSTR
!                             END IF
!
!                          END IF
!
!        C
!        C              We're done with the column having index COLNO.
!        C
!                       END DO
!
!        C
!        C           We're done with the row having index ROW.
!        C
!                    END DO
!
!        C
!        C        We either processed the query or had an error.
!        C
!                 END IF
!
!        C
!        C     We either parsed the SELECT clause or had an error.
!        C
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         ROW =            1
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837050
!           VIKING_SEDR_DATA.IMAGE_ID      : 168C09
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:50:55.925
!
!         ROW =            2
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25837051
!           VIKING_SEDR_DATA.IMAGE_ID      : 168C10
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 16:51:00.269
!
!         ROW =            3
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840344
!           VIKING_SEDR_DATA.IMAGE_ID      : 168C11
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:53.051
!
!         ROW =            4
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25840345
!           VIKING_SEDR_DATA.IMAGE_ID      : 168C12
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 16 20:56:57.395
!
!         ROW =            5
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843638
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C01
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.88000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:50.177
!
!         ROW =            6
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25843639
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C02
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.27000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 01:02:54.521
!
!         ROW =            7
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846934
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C03
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:08:56.263
!
!         ROW =            8
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25846935
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C04
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    119.52000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 05:09:00.607
!
!         ROW =            9
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848026
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C05
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:28.424
!
!         ROW =           10
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848030
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C09
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:46.174
!
!         ROW =           11
!           VIKING_SEDR_DATA.IMAGE_NUMBER  :     25848032
!           VIKING_SEDR_DATA.IMAGE_ID      : 169C11
!           VIKING_SEDR_DATA.PLATFORM_CLOCK:    120.14000000000000
!           VIKING_SEDR_DATA.IMAGE_TIME    : 1976 JUN 17 06:30:55.168
!
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
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header of EKQMGR umbrella routine and all its entry
!        points. Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 2.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 2.0.3, 10-FEB-2014 (BVS)
!
!        Added descriptions of ADSCSZ and LBCELL to the $Parameters
!        section of the header.
!
!-    SPICELIB Version 2.0.2, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW)
!
!        Removed from the header descriptions, all occurrences of the
!        token used to mark the $Procedure section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.3.0, 12-FEB-1999 (NJB)
!
!        Bug fix: in entry point EKNELT, there was a error handling
!        branch that called CHKOUT where CHKIN should have been called.
!        This has been fixed.
!
!-    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB)
!
!        In the entry point EKSRCH, a ZZEKJSQZ call was added after
!        the ZZEKJOIN call. This change reduces the scratch area usage
!        for intermediate results of joins. It also prevents ZZEKJOIN
!        from being handed a join row set containing a segment vector
!        having no corresponding row vectors.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Code fixes were made in routines
!
!           EKNELT, EKGC, EKGD, EKGI
!
!        Version lines were fixed in all routines: versions were
!        changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     Manage EK query operations
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardc
      integer(ip)           isrchc

      logical               eqstr
      logical               failed
      logical               return

!
!     Non-SPICELIB functions
!
      integer(ip)           eknseg
      integer(ip)           zzekesiz

      logical               zzekrmch
      logical               zzekvmch

!
!     Linked list functions:
!
!        Find next node
!        Find tail of list
!        Return number of free nodes
!
!
      integer(ip)           lnknxt
      integer(ip)           lnktl
      integer(ip)           lnknfn


!
!     Local parameters
!

!
!     Maximum number of constraints allowed in a single query:
!
      integer(ip), parameter :: maxseg = stsize

      integer(ip), parameter :: ctsize = mxclld

      integer(ip), parameter :: dtsize = maxseg * mxclld / 10


!
!     Miscellaneous parameters
!
      integer(ip), parameter :: lbpool = -5

      integer(ip), parameter :: nil = 0

!
!     Number of data types
!
      integer(ip), parameter :: ntypes = 4

!
!     Length of strings used for data type names.
!
      integer(ip), parameter :: typlen = 4

!
!     Length of status strings.
!
      integer(ip), parameter :: short = 80


!
!     Local variables
!

!
!     As do the CK and SPK `BSR' entry points, the EKQMGR entry points
!     make use of an amusing panoply of tables, linked lists, and
!     pointers.  Here's where they're declared and described.
!
!
!     The file table contains a list of handles of loaded EK files.
!     Entries in the table are organized as a doubly linked list.
!     Names of file table variables begin with the string 'FT'.
!
!        The maximum number of EK files that can be loaded is FTSIZE.
!
!        The linked list pool used to index table entries is called
!        FTPOOL.
!
!        FTHAN is an array containing file handles of loaded EKs.
!
!        FTHEAD is the head node of the file list.
!
      integer(ip)           ftpool ( 2,  lbpool : ftsize )
      integer(ip)           fthan  (              ftsize )
      integer(ip)           fthead


!
!     The table list contains table names, segment table pointers,
!     and column table pointers for every table associated with a
!     loaded segment.  The segment table pointers indicate the head node
!     of the segment list for each table.  The column table pointers
!     indicate the column names and attributes associated with each
!     table.
!
!     The entries of the table list are organized as a doubly linked
!     list.  All variables in the table list have names starting with
!     the string 'TB'.
!
!        MXTBLD is the maximum number of tables that can be
!        accommodated by the table list.
!
!        TBPOOL is the doubly linked list pool used to index the
!        table list.
!
!        TBNAMS is an array of table names.
!
!        TBSTPT is an array containing pointers to the heads of segment
!        lists corresponding to segments belonging to the table.
!
!        TBNCOL is the number of columns in each table.
!
!        TBCTPT is an array of pointers to lists of column table
!        entries giving the names and attributes of the columns in each
!        table.
!
!        TBFILS is an array containing, for each table, handles of the
!        files that contain segments belonging to that table.
!
!        TBFLSZ is an array of sizes of handle lists for each table
!        entry.
!
!        TBHEAD is the head node of the table list.
!
!
      integer(ip)           tbpool ( 2,  lbpool : mxtbld )
      integer(ip)           tbstpt (              mxtbld )
      integer(ip)           tbncol (              mxtbld )
      character(tnamsz)     tbnams (              mxtbld )
      integer(ip)           tbctpt (              mxtbld )
      integer(ip)           tbfils ( ftsize,      mxtbld )
      integer(ip)           tbflsz (              mxtbld )
      integer(ip)           tbhead


!
!
!     The segment table contains descriptive information for each
!     loaded segment.  Entries in the table are indexed by a linked
!     list pool containing a doubly linked list for each system (or
!     instrument) for which segments are loaded.
!
!     Names of segment table variables begin with the string 'ST'.
!
!        The maximum number of segments that can be loaded is MAXSEG.
!        Currently, the value of MAXSEG is just the size of the segment
!        table, STSIZE.
!
!        The linked list pool used to index segment table entries is
!        called STPOOL.
!
!        For each loaded segment, the following information is stored:
!
!           -- The file handle of the EK containing the segment.
!
!           -- The index of the segment within the EK that contains it.
!              Indices start at 1 and end with the segment count for the
!              EK file.
!
!           -- The segment descriptor.
!
!           -- The number of rows in the segment.
!
!           -- The number of columns in the segment.
!
!           -- A pointer to a list of column descriptors.  The
!              column descriptor table contains a complete descriptor
!              for every loaded column.
!
!
!
      integer(ip)           stpool ( 2, lbpool : maxseg )
      integer(ip)           sthan  (             maxseg )
      integer(ip)           stsidx (             maxseg )
      integer(ip)           stdscs ( sdscsz,     maxseg )
      integer(ip)           stnrow (             maxseg )
      integer(ip)           stncol (             maxseg )
      integer(ip)           stdtpt (             maxseg )

!
!     The column descriptor table contains a column descriptor for
!     every loaded column.  This table allows segments to share the
!     area used for buffering descriptors, making it reasonable for
!     the buffer space to have room for fewer than
!
!        MXCLLD * MAXSEG
!
!     column descriptors.
!
!     The space in the table is organized as a doubly linked list.
!
      integer(ip)           dtpool ( 2,       lbpool:dtsize )
      integer(ip)           dtdscs ( cdscsz,  dtsize        )

!
!     The column attribute table contains attribute information for
!     every column in every loaded segment.  There is one entry per
!     column name; columns with the same names and different data
!     types may not be loaded simultaneously.
!
!     The entries of the column table are organized as a doubly linked
!     list.  All variables in the column table have names starting with
!     the string 'CT'.
!
!        CTSIZE is the maximum number of distinct column declarations
!        that can be accommodated by the column table.
!
!        CTPOOL is the doubly linked list pool used to index the column
!        table.
!
!        CTNAMS is an array containing column names.
!
!        CTCLAS is an array containing column class specifiers.
!
!        CTTYPS is an array containing column data types.
!
!        CTLENS is an array containing column string length specifiers.
!
!        CTFIXD is an array of logical flags indicating whether the
!        columns they correspond to have fixed size.
!
!        CTSIZS is an array of integers indicating the number of array
!        elements per column entry, for fixed-size columns.
!
!        CTINDX is an array of logical flags that indicate whether the
!        columns they correspond to are indexed.
!
!        CTNULL is an array of logical flags that indicate whether the
!        columns they correspond to may contain null values.
!
!
      integer(ip)           ctpool ( 2,       lbpool : ctsize )
      character(cnamsz)     ctnams (                   ctsize )
      integer(ip)           ctclas (                   ctsize )
      integer(ip)           cttyps (                   ctsize )
      integer(ip)           ctlens (                   ctsize )
      logical               ctfixd (                   ctsize )
      integer(ip)           ctsizs (                   ctsize )
      logical               ctindx (                   ctsize )
      logical               ctnull (                   ctsize )


!
!
!     Other local variables
!
      character(typlen)     chtype ( ntypes )
      character(cnamsz)     cnams  ( mxclld )
      character(cnamsz)     cnmset ( lbcell : mxclld )
      character(cnamsz)     colnam
      character(tnamsz)     frmals ( maxtab )
      character(tnamsz)     frmtab ( maxtab )
      character(cnamsz)     lcname
      character(tnamsz)     ltname
      character(short)      problm
      character(cnamsz)     rcname
      character(tnamsz)     rtname
      character(short)      state
      character(tnamsz)     tabnam
      character(tnamsz)     tabvec ( lbcell : maxtab )

      real(wp)              dvals  ( maxcon )

      integer(ip)           begidx
      integer(ip)           cbegs  ( maxcon )
      integer(ip)           cdscrs ( cdscsz,  mxclld )
      integer(ip)           cends  ( maxcon )
      integer(ip)           cjbeg
      integer(ip)           cjend
      integer(ip)           cjrows
      integer(ip)           cjsize
      integer(ip)           cnstyp ( maxcon )
      integer(ip)           col
      integer(ip)           colptr
      integer(ip)           conj
      integer(ip)           cvlen
      integer(ip)           ctnew
      integer(ip)           delseg
      integer(ip)           dtnew
      integer(ip)           dtype  ( maxcon )
      integer(ip)           endidx
      integer(ip)           i
      integer(ip)           ivals  ( maxcon )
      integer(ip)           j
      integer(ip)           jbase1
      integer(ip)           jbase2
      integer(ip)           jsize
      integer(ip)           k
      integer(ip)           key
      integer(ip)           keydsc ( cdscsz )
      integer(ip)           l
      integer(ip)           lcidx  ( maxcon )
      integer(ip)           lelts  ( maxcon )
      integer(ip)           ldscrs ( cdscsz, maxcon )
      integer(ip)           ltbidx ( maxcon )
      integer(ip)           lxbeg
      integer(ip)           lxend
      integer(ip)           nact
      integer(ip)           ncols
      integer(ip)           nconj
      integer(ip)           new
      integer(ip)           next
      integer(ip)           nmatch
      integer(ip)           norder
      integer(ip)           npcol
      integer(ip)           nseg
      integer(ip)           nsel
      integer(ip)           nsv
      integer(ip)           ntab
      integer(ip)           ocols  ( maxord )
      integer(ip)           oelts  ( maxord )
      integer(ip)           ops    ( maxcon )
      integer(ip)           ordbas
      integer(ip)           otabs  ( maxord )
      integer(ip)           ptroff
      integer(ip)           r
      integer(ip)           rbas   ( mxjoin )
      integer(ip)           rcidx  ( maxcon )
      integer(ip)           rdscrs ( cdscsz, maxcon )
      integer(ip)           relts  ( maxcon )
      integer(ip)           resbas
      integer(ip)           rowidx
      integer(ip)           rowvec ( maxtab )
      integer(ip)           rsize  ( mxjrs  )
      integer(ip)           rtbidx ( maxcon )
      integer(ip)           rtotal
      integer(ip)           rwvbas
      integer(ip)           selcol ( maxsel )
      integer(ip)           selctp ( maxsel )
      integer(ip)           seltab ( maxsel )
      integer(ip)           seg
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           segvec ( maxtab )
      integer(ip)           sense  ( maxord )
      integer(ip)           sgvbas
      integer(ip)           sizes  ( maxcon )
      integer(ip)           stnew
      integer(ip)           t
      integer(ip)           tab
      integer(ip)           tabidx
      integer(ip)           tbcurr
      integer(ip)           top
      integer(ip)           tptvec ( lbcell : maxtab )
      integer(ip)           ubase  ( mxjrs )
      integer(ip)           unrows
      integer(ip)           usize

      logical               activc ( maxcon )
      logical               activv ( maxcon )
      logical               attmch
      logical               cmtch
      logical               dosort
      logical               first
      logical               fnd
      logical               indexd
      logical               keyfnd
      logical               nulsok
      logical               presnt
      logical               sorted
      logical               vmtch

!
!
!     Saved variables
!
      save

!
!
!     Initial values
!
      data                  lelts  /  maxcon * 1 /
      data                  oelts  /  maxord * 1 /
      data                  relts  /  maxcon * 1 /

      data                  chtype / 'CHR', 'DP', 'INT', 'TIME' /

      data                  fthead /  0       /
      data                  tbhead /  0       /

      data                  first  / .true.   /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Never come here.
!
      call sigerr ( 'SPICE(BOGUSENTRY)' )


      return







!$Procedure EKLEF ( EK, load event file )

      entry eklef ( fname, handle )

! $ Abstract
!
!     Load an EK file, making it accessible to the EK readers.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     SEARCH
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
!     FNAME      I   Name of EK file to load.
!     HANDLE     O   File handle of loaded EK file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a binary EK file to be loaded.
!
! $ Detailed_Output
!
!     HANDLE   is the handle of the EK file. The file is
!              accessible by the EK reader routines once it
!              has been loaded.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the EK file indicated by FNAME contains a column whose
!         name matches that of a column in an already loaded EK, but
!         whose declared attributes don't match those of the loaded
!         column of the same name, the error SPICE(BADATTRIBUTES) is
!         signaled. HANDLE is is undefined in this case.
!
!     2)  Loading an EK file that is already loaded does not cause side
!         effects. The handle already associated with the file will be
!         returned.
!
!     3)  If a file open error occurs, the error is signaled by a
!         routine in the call tree of this routine. HANDLE is undefined
!         in this case.
!
!     4)  If loading the input file would cause the maximum number of
!         loaded EK files to be exceeded, the error
!         SPICE(EKFILETABLEFULL) is signaled. HANDLE is undefined in
!         this case. This routine will attempt to unload the file
!         from the DAS system.
!
!     5)  If loading the input file would cause the maximum number of
!         loaded DAS files to be exceeded, an error is signaled by a
!         routine in the call tree of this routine. HANDLE is undefined
!         in this case. This routine will attempt to unload the file
!         from the DAS system.
!
!     6)  If loading the input file would cause the maximum number of
!         segments allowed in loaded EK files to be exceeded, the error
!         SPICE(EKSEGMENTTABLEFULL) is signaled. HANDLE is undefined in
!         this case. This routine will attempt to unload the file from
!         the DAS system.
!
!     7)  If loading the input file would cause the maximum number of
!         columns allowed in loaded EK files to be exceeded, the error
!         SPICE(EKCOLDESCTABLEFULL) is signaled. HANDLE is undefined in
!         this case. This routine will attempt to unload the file from
!         the DAS system.
!
!     8)  If loading the input file would cause the maximum allowed
!         number of columns having distinct attributes in loaded EK
!         files to be exceeded, the error SPICE(EKCOLATTRTABLEFULL) is
!         signaled. HANDLE is undefined in this case. This routine will
!         attempt to unload the file from the DAS system.
!
!     9)  If loading the input file would cause the maximum number of
!         instrument codes allowed in loaded EK files to be exceeded,
!         the error SPICE(EKIDTABLEFULL) is signaled. HANDLE is
!         undefined in this case. This routine will attempt to unload
!         the file from the DAS system.
!
!     10) If the input file does not contain at least one segment, the
!         error SPICE(EKNOSEGMENTS) is signaled.
!
! $ Files
!
!     See description of FNAME in $Detailed_Input.
!
! $ Particulars
!
!     This routine makes EK files known to the EK system. It is
!     necessary to load EK files using this routine in order to
!     query the files using the EK readers.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load two EK files and perform a query on them. During query
!        execution, all files will be searched.
!
!        Use the EK kernel below to load the Cassini Science Plan
!        SPICE E-Kernel File based upon the integrated science
!        plan #78.
!
!           S78_CIMSSSUPa.bep
!
!        Use the EK kernel below to load the data based upon the
!        integrated science plan #79.
!
!           S79_CIMSSSUPa.bep
!
!
!        Example code begins here.
!
!
!              PROGRAM EKLEF_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               EKNMLN
!              PARAMETER           ( EKNMLN = 17 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(EKNMLN)    EKNAMS ( 2 )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               N
!              INTEGER               NMROWS
!
!              LOGICAL               ERROR
!
!        C
!        C     Set up the array holding the EK file names.
!        C
!              DATA                  EKNAMS / 'S78_CIMSSSUPa.bep',
!             .                               'S79_CIMSSSUPa.bep'  /
!
!        C
!        C     Load the EK files. This call could be replaced by a call
!        C     to FURNSH (in this case, a meta-kernel listing the EKs
!        C     to be loaded could also be used).
!        C
!              DO I = 1, 2
!
!                 CALL EKLEF ( EKNAMS(I), HANDLE )
!                 WRITE(*,'(2A)') 'Loading EK: ', EKNAMS(I)
!
!              END DO
!
!        C
!        C     The EK files contain a table 'CASSINI_SP_OBSERVATION',
!        C     that contains columns named:
!        C
!        C        NOTES, OBSERVATION_ID, OBSERVATION_TITLE,
!        C        OBS_DESCRIPTION, SCIENCE_OBJECTIVE, SEQUENCE,
!        C        SUBSYSTEM
!        C
!        C     Define a set of constraints to perform a query on all
!        C     loaded EK files (the SELECT clause).
!        C
!              QUERY = 'Select SUBSYSTEM, SCIENCE_OBJECTIVE, '
!             .   //   'OBSERVATION_ID from CASSINI_SP_OBSERVATION '
!             .   //   'order by SUBSYSTEM'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              WRITE(*,*)
!              IF ( ERROR ) THEN
!
!                 WRITE(*,'(2A)') 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!        C
!        C        If no error, NMROWS contains the number of rows
!        C        matching the constraints specified in the query
!        C        string.
!        C
!                 WRITE(*,'(A,I3)') 'Number of matching rows: ', NMROWS
!
!              END IF
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Loading EK: S78_CIMSSSUPa.bep
!        Loading EK: S79_CIMSSSUPa.bep
!
!        Number of matching rows:   9
!
!
!     2) Repeat the previous exercise, using the same input kernels,
!        but this time unloading the previous file before each new
!        file is loaded. Unloading files prevents them from being
!        searched during query execution.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKLEF_EX2
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               EKNMLN
!              PARAMETER           ( EKNMLN = 17 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(EKNMLN)    EKNAMS ( 2 )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               N
!              INTEGER               NMROWS
!
!              LOGICAL               ERROR
!
!        C
!        C     Set up the array holding the EK file names.
!        C
!              DATA                  EKNAMS / 'S78_CIMSSSUPa.bep',
!             .                               'S79_CIMSSSUPa.bep'  /
!
!        C
!        C     The EK files contain a table 'CASSINI_SP_OBSERVATION',
!        C     that contains columns named:
!        C
!        C        NOTES, OBSERVATION_ID, OBSERVATION_TITLE,
!        C        OBS_DESCRIPTION, SCIENCE_OBJECTIVE, SEQUENCE,
!        C        SUBSYSTEM
!        C
!        C     Define a set of constraints to perform a query on all
!        C     loaded EK files (the SELECT clause).
!        C
!              QUERY = 'Select SUBSYSTEM, SCIENCE_OBJECTIVE, '
!             .   //   'OBSERVATION_ID from CASSINI_SP_OBSERVATION '
!             .   //   'order by SUBSYSTEM'
!
!        C
!        C     Load the EK files. This call could be replaced by a call
!        C     to FURNSH.
!        C
!              DO I = 1, 2
!
!                 CALL EKLEF ( EKNAMS(I), HANDLE )
!                 WRITE(*,'(2A)') 'Loading EK: ', EKNAMS(I)
!
!        C
!        C        Query the EK system for data rows matching the
!        C        SELECT constraints.
!        C
!                 CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C        Check whether an error occurred while processing the
!        C        SELECT clause. If so, output the error message.
!        C
!                 IF ( ERROR ) THEN
!
!                    WRITE(*,'(2A)') 'SELECT clause error: ', ERRMSG
!
!                 ELSE
!
!        C
!        C           If no error, NMROWS contains the number of rows
!        C           matching the constraints specified in the query
!        C           string.
!        C
!                    WRITE(*,'(A,I3)') 'Number of matching rows: ',
!             .                        NMROWS
!
!                 END IF
!
!        C
!        C        Unload the current file. Unloading files prevents
!        C        them from being searched during query execution.
!        C
!                 CALL EKUEF ( HANDLE )
!                 WRITE(*,'(2A)') 'Unloading EK: ', EKNAMS(I)
!                 WRITE(*,*)
!
!              END DO
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Loading EK: S78_CIMSSSUPa.bep
!        Number of matching rows:   4
!        Unloading EK: S78_CIMSSSUPa.bep
!
!        Loading EK: S79_CIMSSSUPa.bep
!        Number of matching rows:   5
!        Unloading EK: S79_CIMSSSUPa.bep
!
!
! $ Restrictions
!
!     1)  EK files containing columns having the same name but
!         inconsistent declarations are not diagnosed. Such kernels
!         are invalid in any case.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code examples based on existing fragments.
!
!-    SPICELIB Version 2.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     load EK file
!     load E-Kernel
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Here's a brief overview of what follows:
!
!        -- We do some once-per-program run initializations.
!
!        -- We do some simple error checks.  We need to make sure
!           that DAS can load the file, and that the EK architecture is
!           the right kind.
!
!        -- We need to make sure that there's enough space in our
!           data structures to hold the information about the new
!           EK.  Some of these checks are simple; we do these first.
!           However, checking that we have enough room in the column
!           table is best done by simply loading the column data into
!           the table.  If we run out of room, we abort the load.
!
!        -- We also need to make sure that the column attributes for
!           any two columns with the same name in the same table are
!           identical.  This is easy to do if the attributes for every
!           column we've encountered have been loaded into the column
!           table.
!
!        -- We save the table name and column names and attributes for
!           each new table we encounter.  For each table, we maintain a
!           list of handles of files that contain segments in that
!           table.
!
!        -- We make a segment table entry for each segment we find.
!
!        -- We save the column descriptor for each column we find,
!           associating it with the segment table entry for the segment
!           containing the column.  The column descriptor entries are
!           linked together in the same order that the corresponding
!           column names appear in the parent table's column name list;
!           this order is not necessarily the order that the columns
!           have within the segment.
!
!        -- We maintain a list of handles of loaded EKs.
!
!        If we run out of room in the column table, we clean up our
!        mess.  This means removing the current file's contributions
!        to the column table, segment table, file table, and if
!        necessary, the table list.
!
!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if

!
!     Open the EK file for read access.  Bail out now if this doesn't
!     work.  This retreat will protect the various tables from
!     corruption.
!
      call ekopr ( fname, handle )

      if ( failed() ) return

!
!     Check to see whether the named EK has already been loaded.
!     If so, we've added another link to the EK, which must be
!     removed.
!
      i = fthead

      do while ( i > 0 )

         if ( handle == fthan(i) ) then
!
!           The last call we made to EKOPR added another link to
!           the EK file.  Remove this link.
!
            call dascls ( handle )

            return

         end if

         i = lnknxt ( i, ftpool )

      end do

!
!     Nothing doing unless the architecture is correct.  This file
!     should be a paged DAS EK.
!
      call zzekpgch ( handle, 'READ' )

!
!     Before getting too involved with this new EK file, let's check it
!     out.  We must have enough room to accommodate it in the file
!     table, segment table, table list, and column table.
!
!     Make sure there's enough room in the file table.
!

      if (  lnknfn(ftpool) == 0 ) then
!
!        Sorry, there are no free file table entries left.
!
!        We close the EK AFTER setting the long error message.
!
         call setmsg ( 'The EK file # could not be loaded; the '      // &
                       'maximum number of loaded EKs has already '    // &
                       'been reached.'                                 )
         call errhan ( '#', handle                                     )
         call ekcls  ( handle                                          )
         call sigerr ( 'SPICE(EKFILETABLEFULL)'                        )
         return

      end if

!
!     Find out how many segments are in the new kernel, and make
!     sure there's enough room in the segment table.
!
      nseg = eknseg ( handle )

      if ( nseg > lnknfn(stpool) ) then
!
!        There are too many segments for the amount of space we've got
!        left.
!
!        We close the EK AFTER setting the long error message.
!
         call setmsg ( 'The EK file # could not be loaded; the '      // &
                       'maximum number of loaded segments has '       // &
                       'already been reached.'                         )
         call errhan ( '#', handle                                     )
         call ekcls  ( handle                                          )
         call sigerr ( 'SPICE(EKSEGTABLEFULL)'                         )
         return

      else if ( nseg < 1 ) then

         call setmsg ( 'The EK file # contains no segments.'           )
         call errhan ( '#',  handle                                    )
         call ekcls  ( handle                                          )
         call sigerr ( 'SPICE(EKNOSEGMENTS)'                           )
         return

      end if


!
!     At this point, the file has insinuated itself into our confidence,
!     justified or not.  We'll attempt to load the segment and column
!     tables, and we'll update the table list if new tables are
!     introduced in this file.
!
      seg    =   1
      state  =  'LOAD_FILE_TABLE'

      do while ( state /= 'DONE' )


         if ( state == 'LOAD_FILE_TABLE' ) then
!
!           Allocate a file table entry and link the new entry in before
!           the current head of the list.  Update the list head pointer.
!           Record the file handle in the new file table entry.
!
            call lnkan  ( ftpool, new            )
            call lnkilb ( new,    fthead, ftpool )

            fthead         =   new
            fthan ( new )  =   handle

            state          =  'SUMMARIZE_SEGMENT'



         else if ( state == 'SUMMARIZE_SEGMENT' ) then
!
!           Get the summary information for this segment.
!
            call zzeksinf (  handle,  seg,     tabnam, &
                             segdsc,  cnams,   cdscrs  )

            ncols  =  segdsc(ncidx)

!
!           Before going further, check the segment for duplicate
!           column names.  Bail out if we find any.
!
            call ssizec ( mxclld,        cnmset    )
            call movec  ( cnams,  ncols, cnmset(1) )
            call validc ( mxclld, ncols, cnmset    )

            if ( cardc(cnmset) < ncols ) then

               state   =  'ABORT'
               problm  =  'DUPLICATE_COLUMN_NAMES'

            else

               state   =  'FIND_TABLE'

            end if



         else if ( state == 'FIND_TABLE' ) then
!
!           Traverse the table list, checking for a match.
!
            tbcurr  =  tbhead
            presnt  =  .false.

            do while (  ( tbcurr > 0 ) .and. ( .not. presnt ) )

               if ( tabnam == tbnams(tbcurr) ) then
                  presnt =  .true.
               else
                  tbcurr      =  lnknxt ( tbcurr, tbpool )
               end if

            end do

!
!           If TABNAM is the name of a table we know about, go on to
!           fill out the segment list entry for the current segment.
!           If we didn't find TABNAM, we have a new table.  Make a table
!           list entry for it.
!
            if ( presnt ) then
!
!              Before going further, make sure the number of columns
!              in the segment matches the number of columns in the
!              parent table.
!
               if ( ncols /= tbncol(tbcurr) ) then

                  npcol  =  tbncol(tbcurr)
                  state  = 'ABORT'
                  problm = 'COLUMN_NUMBER_MISMATCH'

               else
!
!                 Add the current file to the list of files containing
!                 the current table.
!
                  tbfils ( 1, tbcurr )  =  handle
                  tbflsz (    tbcurr )  =  tbflsz(tbcurr) + 1

                  state = 'MAKE_SEGMENT_TABLE_ENTRY'

               end if

            else
!
!              This segment belongs to a new table.
!
               state = 'MAKE_TABLE_LIST_ENTRY'

            end if



         else if ( state == 'MAKE_TABLE_LIST_ENTRY' ) then
!
!           Allocate a table list entry, if we can.
!

            if ( lnknfn(tbpool) == 0 ) then
!
!              Oops, we're out of room.
!
               state   =  'ABORT'
               problm  =  'TABLE_LIST_FULL'

            else
!
!              We have an entry; link it to the tail of the table list.
!              For consistency with the case in which the table entry
!              already exists, we'll call the table list node TBCURR.
!
!              If this is the first table in the table list, set the
!              table head pointer.
!
               call lnkan  ( tbpool, tbcurr  )

               if ( tbhead <= 0 ) then
                  tbhead  =  tbcurr
               else
                  call lnkilb ( tbhead, tbcurr, tbpool )
               end if

!
!              Fill in the table name.
!
               tbnams ( tbcurr )   =  tabnam
!
!              Since this table is new, the file list for this table
!              contains only the handle of the current EK.
!
               tbfils ( 1, tbcurr ) = handle
               tbflsz (    tbcurr ) = 1

!
!              Initialize the column count, column table pointer, and
!              segment list pointer for this table.
!
               tbncol(tbcurr)   =  ncols
               tbctpt(tbcurr)   =  0
               tbstpt(tbcurr)   =  0

!
!              Go on to add a segment table entry for the current
!              segment.
!
               state  =  'MAKE_SEGMENT_TABLE_ENTRY'

            end if


         else if ( state == 'MAKE_SEGMENT_TABLE_ENTRY' ) then
!
!           Add the data for the current segment to the segment
!           table.
!
!           Allocate a segment table entry.  We've already verified
!           that there's enough room.
!
            call lnkan ( stpool, stnew )

!
!           Link this segment table entry to the tail of the segment
!           list for the parent table, or, if the tail is NIL, just set
!           the segment list pointer to the current segment node.
!
            if ( tbstpt(tbcurr) <= 0 ) then

               tbstpt(tbcurr)  =  stnew

            else

               call lnkilb ( tbstpt(tbcurr), stnew, stpool )

            end if

!
!           At this point, we can fill in all elements of the segment
!           table entry except for the pointers into the column table
!           and the column base addresses.
!
            sthan (stnew)   =  handle
            stsidx(stnew)   =  seg
            stnrow(stnew)   =  segdsc(nridx)
            stncol(stnew)   =  segdsc(ncidx)
            stdtpt(stnew)   =  0

            call movei ( segdsc, sdscsz, stdscs(1,stnew) )

!
!           The next step is to set up the column attributes and
!           descriptors.
!
            state  =  'MAKE_COLUMN_TABLE_ENTRIES'



         else if ( state == 'MAKE_COLUMN_TABLE_ENTRIES' ) then


            if ( presnt ) then
!
!              If the current table was present before loading the
!              current segment, we must make sure that the attributes
!              of the columns in this segment match those of the table
!              to which the segment belongs.
!
!              We must load the column descriptors for this segment
!              in the *same order* as those for every other segment
!              in the table.  This order matches that of the columns
!              in the column attribute list for the table.
!
!              For each column in the column list of the current table,
!              check the list of column names for the current segment,
!              looking for a match.
!
               j   =   tbctpt ( tbcurr )

               do while (       ( j     >  0       ) &
                          .and. ( state /= 'ABORT'  )   )


                  k  =  isrchc ( ctnams(j), ncols, cnams )


                  if ( k > 0 ) then
!
!                    We have a name match.  At this point, we must
!                    check that the column's other attributes---data
!                    type, size, and whether the column is
!                    indexed---match as well.  It's an error if they
!                    don't.
!
                     indexd = cdscrs(ixtidx,k) /= ifalse
                     nulsok = cdscrs(nflidx,k) /= ifalse
                     attmch = &
                                ( cdscrs(clsidx, k) ==  ctclas(j) ) &
                          .and. ( cdscrs(typidx, k) ==  cttyps(j) ) &
                          .and. ( cdscrs(lenidx, k) ==  ctlens(j) ) &
                          .and. ( cdscrs(sizidx, k) ==  ctsizs(j) ) &
                          .and. ( indexd            .eqv. ctindx(j) ) &
                          .and. ( nulsok            .eqv. ctnull(j) )


                     if ( attmch ) then
!
!                       Great, the attributes match.  Actually, the
!                       addition of the current segment can *change*
!                       one attribute of the current table:  the
!                       maximum non-blank width associated with the
!                       current column, if the column has character
!                       type.  We'll make this change after we're
!                       sure we won't have to undo it.
!
!                       Store the column descriptor for this column
!                       in the descriptor table.  We'll need to
!                       allocate a descriptor table entry first.
!
                        if ( lnknfn( dtpool ) == 0 ) then
!
!                          No free nodes left in the descriptor table.
!
                           state  = 'ABORT'
                           problm = 'DESCRIPTOR_TABLE_FULL'

                        else
!
!                          A free node is available.  Link it in
!                          at the tail of the descriptor list for
!                          the current segment.
!
                           call lnkan  ( dtpool, dtnew )

                           if ( stdtpt(stnew) <= 0 ) then

                              stdtpt(stnew) = dtnew

                           else

                              call lnkilb (stdtpt(stnew), dtnew, dtpool)

                           end if

!
!                          Fill in the descriptor.
!
                           call movei (  cdscrs(1,k), &
                                         cdscsz, &
                                         dtdscs(1,dtnew)  )

                        end if
!
!                       We filled in a descriptor table entry, or
!                       else we ran out of room.
!

                     else
!
!                       Seriously bad news.  Someone's tried to
!                       load an EK containing a column with
!                       attributes that conflict with those of a
!                       loaded column of the same name in the
!                       current table.
!
                        colnam  =   ctnams(j)
                        state   =  'ABORT'
                        problm  =  'MISMATCHED_COLUMN_ATTRIBUTES'

                     end if


                  else
!
!                    No name match; the current column from the current
!                    table is not present in the segment we're looking
!                    at.
!
                     colnam  =   ctnams(j)
                     state   =  'ABORT'
                     problm  =  'MISSING_COLUMN'

                  end if
!
!                 The current column matched one in the column list
!                 for the current table, or else we have a problem.
!
!                 Advance to the next column in the table's column list.
!
                  if ( state /= 'ABORT' ) then
                     j  =  lnknxt ( j, ctpool )
                  end if

               end do
!
!              We've made descriptor table entries for each column in
!              the current segment, or else we have an error.
!


            else
!
!              We need to set up the column attribute entries for
!              the new table introduced by loading this segment.  We
!              also need to set up descriptor table entries for the
!              segment.  We *don't* have to check the consistency of
!              the attributes of the columns.
!
               k  =  1

               do while (  (k <= ncols) .and. (state /= 'ABORT')  )
!
!                 Allocate a new entry in the column attribute table and
!                 link it to the tail of the column list for the
!                 current table.  If the column list is empty, update
!                 the list head.
!
                  if (  lnknfn(ctpool) == 0 ) then
!
!                    There's no more space to store attribute
!                    descriptors.
!
                     state  =  'ABORT'
                     problm =  'ATTRIBUTE_TABLE_FULL'

                  else

                     call lnkan  ( ctpool, ctnew )

                     if  ( tbctpt(tbcurr) <= 0 ) then

                        tbctpt(tbcurr)  =  ctnew

                     else

                        call lnkilb ( tbctpt(tbcurr), ctnew, ctpool )

                     end if

!
!                    Fill in the new column attribute entry with the
!                    attributes for this column.
!
                     ctnams(ctnew)  =  cnams (       k)
                     ctclas(ctnew)  =  cdscrs(clsidx,k)
                     cttyps(ctnew)  =  cdscrs(typidx,k)
                     ctlens(ctnew)  =  cdscrs(lenidx,k)
                     ctsizs(ctnew)  =  cdscrs(sizidx,k)
                     ctindx(ctnew)  =  cdscrs(ixtidx,k) /= ifalse
                     ctfixd(ctnew)  =  cdscrs(sizidx,k) /= ifalse
                     ctnull(ctnew)  =  cdscrs(nflidx,k) /= ifalse

!
!                    Store the column descriptor for this column
!                    in the descriptor table.  We'll need to
!                    allocate a descriptor table entry first.
!
                     if ( lnknfn( dtpool ) == 0 ) then
!
!                       No free nodes left in the descriptor table.
!
                        state  = 'ABORT'
                        problm = 'DESCRIPTOR_TABLE_FULL'

                     else
!
!                       A free node is available.  Link it in at the
!                       tail of the descriptor list for the current
!                       segment.
!
                        call lnkan  ( dtpool, dtnew )

                        if ( stdtpt(stnew) <= 0 ) then

                           stdtpt(stnew) = dtnew

                        else

                           call lnkilb ( stdtpt(stnew), dtnew, dtpool )

                        end if

!
!                       Fill in the descriptor.
!
                        call movei (  cdscrs(1,k), &
                                      cdscsz, &
                                      dtdscs(1,dtnew)  )

                     end if

                  end if
!
!                 We created attribute and descriptor entries for the
!                 current column, or we encountered an error.
!
                  if ( state /= 'ABORT' ) then
!
!                    Consider the next column.
!
                     k  =  k + 1

                  end if


               end do
!
!              We created attribute and descriptor entries for every
!              column in the current segment, or we encountered an
!              error.
!

            end if
!
!           We've processed the current segment in the new file, or
!           else we have an error condition.
!

            if ( state /= 'ABORT' ) then
!
!              We're ready to look at the next segment in the new file.
!
               state  =  'NEXT_SEGMENT'

            end if



         else if ( state == 'NEXT_SEGMENT' ) then


            if ( seg < nseg ) then

               seg    =   seg + 1
               state  =   'SUMMARIZE_SEGMENT'

            else
!
!              We're done with all of the segments.
!
               state  =  'DONE'

            end if


         else if ( state == 'ABORT' ) then
!
!           We must clean up all the data structure additions we made to
!           accommodate the new file.
!
!           Basically, we unload the new file.  We defer the call to
!           EKCLS until after we've reported the error.
!
!           The file table is first.  The file is at the head of the
!           list.  If the file has a successor, that file is now at the
!           head of the list.
!

            fthead  =  lnknxt ( new, ftpool )


            if ( fthead < 0 ) then
!
!              There are no files left.  Clean up the whole shebang.
!
               call lnkini ( ftsize, ftpool )
               call lnkini ( stsize, stpool )
               call lnkini ( dtsize, dtpool )
               call lnkini ( ctsize, ctpool )
               call lnkini ( mxtbld, tbpool )

               fthead  =  0
               tbhead  =  0


            else
!
!              If we arrived here, the file we're unloading is not the
!              only loaded file.
!
!              Free the file table entry for the file.  The entry can be
!              regarded as a sublist that starts and ends with the Ith
!              node, so we can call the `free sublist' routine to get
!              rid of it.
!
               call lnkfsl ( new, new, ftpool )

!
!              It's time to clean up the table list, segment table,
!              column attribute table, and column descriptor table.  The
!              plan is to traverse the table list, and for each member
!              of the list, traverse the corresponding segment list,
!              removing from the list information about segments and
!              columns in the file we're unloading.  If the segment list
!              for any table becomes empty, we remove the entry for that
!              table from the table list.
!
               tbcurr = tbhead

               do while ( tbcurr > 0 )
!
!                 See whether the current table is in the file we're
!                 unloading.
!
                  i  =  1

                  do while (       ( i <=  tbflsz(tbcurr) ) &
                             .and. (   .not. fnd            )  )

                     if ( tbfils(i,tbcurr) == handle ) then
!
!                       This table is affected by unloading the file.
!
                        fnd  =  .true.

                     else
!
!                       Look at the next file handle.
!
                        i    =   i + 1

                     end if

                  end do


                  if ( fnd ) then
!
!                    Update the information for the current table to
!                    reflect the unloading of the specified EK.
!
!                    Unloading the specified EK removes one handle from
!                    the list of file handles associated with this
!                    table.  Compress this handle out of the list.
!
                     do j  =  i,   tbflsz(tbcurr) - 1

                        tbfils( j, tbcurr )  =  tbfils( j+1, tbcurr )

                     end do

                     tbflsz(tbcurr)  =  tbflsz(tbcurr) - 1

!
!                    Traverse the segment list for this table, looking
!                    for segments in the specified EK.
!
                     delseg  =  tbstpt ( tbcurr )

                     do while ( delseg > 0 )

                        if ( sthan(delseg) == handle ) then
!
!                          This segment is aboard the sinking ship.  Put
!                          it out of its misery.
!
!                          First, euthanize its column descriptors.
!                          These descriptors are linked together, so we
!                          can free all of them in one shot.
!
                           j  =  stdtpt ( delseg )

                           if ( j > 0 ) then
                              k  =  lnktl  ( j,    dtpool )
                              call lnkfsl  ( j, k, dtpool )
                           end if

!
!                          Now we can delete the segment table entry
!                          itself.  This deletion may necessitate
!                          updating the segment list pointer in the
!                          parent table's table list entry.
!
                           if ( delseg == tbstpt(tbcurr) ) then

                              tbstpt(tbcurr) =  lnknxt( delseg, stpool )

                           end if


                           next = lnknxt ( delseg,         stpool )
                           call lnkfsl   ( delseg, delseg, stpool )

!
!                          The segment we just freed may have been the
!                          last one belonging to this table.  We deal
!                          with this possibility later, below the end of
!                          the current loop.
!
                           delseg  = next

                        else

                           delseg  = lnknxt ( delseg, stpool )

                        end if


                     end do

!
!                    We've examined all of the segments in the current
!                    table.
!
!                    If the segment list for the current table became
!                    empty as a result of our having plundered the
!                    segment table, delete the entry for this table from
!                    the table list. We do *not* need to concern
!                    ourselves with the possibility that this deletion
!                    will empty the table list, since we know we're
!                    not unloading the last loaded file.  However, we
!                    may need to update the head-of-list pointer for the
!                    table list.
!
                     if ( tbstpt(tbcurr) <= 0 ) then
!
!                       There are no loaded segments left for this
!                       table.
!
!                       Delete the list of column attribute entries for
!                       the columns in this table, then delete the
!                       table's entry from the table list.
!
!                       The column attribute entries are linked, so we
!                       can free them in one shot.  Don't crash if the
!                       column attribute list is empty.
!
                        j  =  tbctpt ( tbcurr )

                        if ( j > 0 ) then

                           k  =  lnktl  ( j,    ctpool )
                           call lnkfsl  ( j, k, ctpool )

                        end if


                        if ( tbcurr == tbhead ) then
!
!                          The entry for this table is at the head of
!                          the table list.  Update the head of the list.
!
                           tbhead  =  lnknxt ( tbcurr, tbpool )
                           next    =  tbhead

                        else

                           next    =  lnknxt ( tbcurr, tbpool )

                        end if

!
!                       Make the entry for this table go away.
!
                        call lnkfsl ( tbcurr, tbcurr, tbpool )

!
!                       Look at the next table.
!
                        tbcurr  =  next


                     else
!
!                       We're done with the current table.  Look at the
!                       next one.
!
                        tbcurr = lnknxt ( tbcurr, tbpool )

                     end if
!
!                    We've cleaned up the table entry for the current
!                    table, if it was necessary to do so.
!

                  else
!
!                    The current table is not affected by unloading this
!                    file.  Examine the next table.
!
                     tbcurr = lnknxt ( tbcurr, tbpool )

                  end if

!
!                 We've processed the current table.
!
               end do


            end if

!
!           We've cleaned up after the aborted partial load.
!
!           Now that the mess has been arranged, tell the user what the
!           problem was.
!
            if ( problm == 'TABLE_LIST_FULL' ) then

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'maximum number of distinct tables has ' // &
                             'already been reached.'                   )
               call errhan ( '#', handle                               )
               call sigerr ( 'SPICE(EKTABLELISTFULL)'                  )


            else if ( problm == 'DUPLICATE_COLUMN_NAMES' ) then

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'segment # contains duplicate column '   // &
                             'names in table #.'                       )
               call errhan ( '#',      handle                          )
               call errint ( '#',      seg                             )
               call errch  ( '#',      tabnam                          )
               call sigerr ( 'SPICE(EKCOLNUMMISMATCH)'                 )


            else if ( problm == 'COLUMN_NUMBER_MISMATCH' ) then

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'number of columns (#) in segment # '    // &
                             'does not match the number of columns '  // &
                             '(#) in the parent table #.'              )
               call errhan ( '#',      handle                          )
               call errint ( '#',      ncols                           )
               call errint ( '#',      seg                             )
               call errint ( '#',      npcol                           )
               call errch  ( '#',      tabnam                          )
               call sigerr ( 'SPICE(EKCOLNUMMISMATCH)'                 )


            else if ( problm == 'MISMATCHED_COLUMN_ATTRIBUTES' ) then

               call setmsg ( 'EK file # contains a column '           // &
                             'whose attributes conflict with a '      // &
                             'loaded column.  The offending '         // &
                             'column name is #; the column is '       // &
                             'in segment #* of the file.'              )
               call errhan ( '#',  handle                              )
               call errch  ( '#',  colnam                              )
               call errint ( '*',  seg                                 )
               call sigerr ( 'SPICE(BADATTRIBUTES)'                    )


            else if ( problm == 'DESCRIPTOR_TABLE_FULL' ) then

               call setmsg ( 'The EK file # could not be loaded; the' // &
                              'maximum allowed number of loaded '     // &
                              'columns already been reached.'          )
               call errhan ( '#',      handle                          )
               call sigerr ( 'SPICE(COLDESCTABLEFULL)'                 )


            else if ( problm == 'ATTRIBUTE_TABLE_FULL' ) then

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'maximum number of columns having'       // &
                             'distinct attributes has already been '  // &
                             'reached.'                                )
               call errhan ( '#',      handle                          )
               call sigerr ( 'SPICE(EKCOLATTRTABLEFULL)'               )


            else if ( problm == 'MISSING_COLUMN' ) then

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'column # in already loaded table # is ' // &
                             'not present in segment # in the EK file.')
               call errhan ( '#',      handle                          )
               call errch  ( '#',      colnam                          )
               call errch  ( '#',      tabnam                          )
               call errint ( '#',      seg                             )
               call sigerr ( 'SPICE(EKMISSINGCOLUMN)'                  )


            else

               call setmsg ( 'The EK file # could not be loaded; the '// &
                             'problem "#" occurred while attempting ' // &
                             'to load the file.  By way, there is a ' // &
                             'bug in EKLEF if you see this message.'   )
               call errhan ( '#',      handle                          )
               call errch  ( '#',      problm                          )
               call sigerr ( 'SPICE(BUG)'                              )

            end if


            call ekcls  (  handle )

            return

         end if


      end do
!
!     At this point, we've made the file table, table list, segment
!     table, column descriptor table, and column attribute table updates
!     necessary to reflect the presence of the new file.
!

      return








!$Procedure EKUEF  ( EK, unload event file )

      entry ekuef ( handle )

! $ Abstract
!
!     Unload an EK file, making its contents inaccessible to the
!     EK reader routines, and clearing space in order to allow other
!     EK files to be loaded.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
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
!     HANDLE     I   Handle of EK file.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle returned by EKLEF.
!
! $ Detailed_Output
!
!     None.
!
!     See $Particulars for a description of the effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Unloading a file that is not loaded has no effect.
!
! $ Files
!
!     This routine unloads a binary EK file from the EK query system.
!
! $ Particulars
!
!     This routine removes information about an EK file from the
!     EK system, freeing space to increase the number of other EK
!     files that can be loaded. The file is also unloaded from
!     the DAS system and closed.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Load two EK files and perform a query on them. During query
!        execution, all files will be searched. Unload the previous
!        file before each new file is loaded. Unloading files prevents
!        them from being searched during query execution.
!
!        Use the EK kernel below to load the Cassini Science Plan
!        SPICE E-Kernel File based upon the integrated science
!        plan #78.
!
!           S78_CIMSSSUPa.bep
!
!        Use the EK kernel below to load the data based upon the
!        integrated science plan #79.
!
!           S79_CIMSSSUPa.bep
!
!
!        Example code begins here.
!
!
!              PROGRAM EKUEF_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               EKNMLN
!              PARAMETER           ( EKNMLN = 17 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(EKNMLN)    EKNAMS ( 2 )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               N
!              INTEGER               NMROWS
!
!              LOGICAL               ERROR
!
!        C
!        C     Set up the array holding the EK file names.
!        C
!              DATA                  EKNAMS / 'S78_CIMSSSUPa.bep',
!             .                               'S79_CIMSSSUPa.bep'  /
!
!        C
!        C     The EK files contain a table 'CASSINI_SP_OBSERVATION',
!        C     that contains columns named:
!        C
!        C        NOTES, OBSERVATION_ID, OBSERVATION_TITLE,
!        C        OBS_DESCRIPTION, SCIENCE_OBJECTIVE, SEQUENCE,
!        C        SUBSYSTEM
!        C
!        C     Define a set of constraints to perform a query on all
!        C     loaded EK files (the SELECT clause).
!        C
!              QUERY = 'Select SUBSYSTEM, SCIENCE_OBJECTIVE, '
!             .   //   'OBSERVATION_ID from CASSINI_SP_OBSERVATION '
!             .   //   'order by SUBSYSTEM'
!
!        C
!        C     Load the EK files. This call could be replaced by a call
!        C     to FURNSH.
!        C
!              DO I = 1, 2
!
!                 CALL EKLEF ( EKNAMS(I), HANDLE )
!                 WRITE(*,'(2A)') 'Loading EK: ', EKNAMS(I)
!
!        C
!        C        Query the EK system for data rows matching the
!        C        SELECT constraints.
!        C
!                 CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C        Check whether an error occurred while processing the
!        C        SELECT clause. If so, output the error message.
!        C
!                 IF ( ERROR ) THEN
!
!                    WRITE(*,'(2A)') 'SELECT clause error: ', ERRMSG
!
!                 ELSE
!
!        C
!        C           If no error, NMROWS contains the number of rows
!        C           matching the constraints specified in the query
!        C           string.
!        C
!                    WRITE(*,'(A,I3)') 'Number of matching rows: ',
!             .                        NMROWS
!
!                 END IF
!
!        C
!        C        Unload the current file. Unloading files prevents
!        C        them from being searched during query execution.
!        C
!                 CALL EKUEF ( HANDLE )
!                 WRITE(*,'(2A)') 'Unloading EK: ', EKNAMS(I)
!                 WRITE(*,*)
!
!              END DO
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Loading EK: S78_CIMSSSUPa.bep
!        Number of matching rows:   4
!        Unloading EK: S78_CIMSSSUPa.bep
!
!        Loading EK: S79_CIMSSSUPa.bep
!        Number of matching rows:   5
!        Unloading EK: S79_CIMSSSUPa.bep
!
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     unload EK file
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if

!
!     Check to see whether the named EK has been loaded.  Do nothing
!     if not.
!
      i    =   fthead
      fnd  =  .false.

      do while (  ( i > 0 ) .and. ( .not. fnd )  )

         if ( handle == fthan(i) ) then
            fnd  =  .true.
         else
            i    =  lnknxt ( i, ftpool )
         end if

      end do

      if ( .not. fnd ) return

!
!     If we got to here, HANDLE points to a loaded EK file.  It's
!     time to wipe from the EK tables all trivial fond records
!     pertaining to the file in question.
!
!     The file table is first.
!
      if ( i == fthead ) then
!
!        The file is at the head of the list.  If the file has a
!        successor, that file is now at the head of the list.
!
         fthead  =  lnknxt ( i, ftpool )

         if ( fthead < 0 ) then
!
!           There are no files left.  Clean up the whole shebang.
!
            call lnkini ( ftsize, ftpool )
            call lnkini ( stsize, stpool )
            call lnkini ( dtsize, dtpool )
            call lnkini ( ctsize, ctpool )
            call lnkini ( mxtbld, tbpool )

            fthead  =  0
            tbhead  =  0

!
!           Close the EK file, to keep the DAS system's bookkeeping
!           up to date.
!
            call ekcls  ( handle )

            return

         end if

      end if

!
!     If we arrived here, the file we're unloading is not the only
!     loaded file.
!
!     Free the file table entry for the file.  The entry can be
!     regarded as a sublist that starts and ends with the Ith node,
!     so we can call the `free sublist' routine to get rid of it.
!
      call lnkfsl ( i, i, ftpool )

!
!     It's time to clean up the table list, segment table, column
!     attribute table, and column descriptor table.  The plan is
!     to traverse the table list, and for each member of the list,
!     traverse the corresponding segment list, removing from the list
!     information about segments and columns in the file we're
!     unloading.  If the segment list for any table becomes empty, we
!     remove the entry for that table from the table list.
!
      tbcurr = tbhead

      do while ( tbcurr > 0 )
!
!        See whether the current table is in the file we're unloading.
!
            i  =  1

            do while (  ( i <= tbflsz(tbcurr) ) .and. ( .not. fnd )  )

            if ( tbfils(i,tbcurr) == handle ) then
!
!              This table is affected by unloading the file.
!
               fnd  =  .true.

            else
!
!              Look at the next file handle.
!
               i    =   i + 1

            end if

         end do


         if ( fnd ) then
!
!           Update the information for the current table to reflect
!           the unloading of the specified EK.
!
!           Unloading the specified EK removes one handle from the
!           list of file handles associated with this table.  Compress
!           this handle out of the list.
!
            do j  =  i,   tbflsz(tbcurr) - 1

               tbfils( j, tbcurr )  =  tbfils( j+1, tbcurr )

            end do

            tbflsz(tbcurr)  =  tbflsz(tbcurr) - 1

!
!           Traverse the segment list for this table, looking
!           for segments in the specified EK.
!
            seg  =  tbstpt ( tbcurr )

            do while ( seg > 0 )

               if ( sthan(seg) == handle ) then
!
!                 This segment is aboard the sinking ship.  Put it
!                 out of its misery.
!
!                 First, euthanize the segment's column descriptors.
!                 These descriptors are linked together, so we can free
!                 all of them in one shot.  Don't crash if the column
!                 descriptor list is empty.
!
                  j  =  stdtpt ( seg )

                  if ( j > 0 ) then
                     k  =  lnktl  ( j,    dtpool )
                     call lnkfsl  ( j, k, dtpool )
                  end if

!
!                 Now we can delete the segment table entry itself.
!                 This deletion may necessitate updating the segment
!                 list pointer in the parent table's table list entry.
!
                  if ( seg == tbstpt(tbcurr) ) then

                     tbstpt(tbcurr)  =  lnknxt( seg, stpool )

                  end if


                  next = lnknxt ( seg,      stpool )
                  call lnkfsl   ( seg, seg, stpool )

                  seg  = next

               else

                  seg  = lnknxt ( seg, stpool )

               end if


            end do

!
!           We've examined all of the segments in the current table.
!
!           If the segment list for the current table became empty
!           as a result of our having plundered the segment table,
!           delete the entry for this table from the table list.  We do
!           *not* need to concern ourselves with the possibility that
!           this deletion will empty the table list, since we know we're
!           not unloading the last loaded file.  However, we may need to
!           update the head-of-list pointer for the table list.
!
            if ( tbstpt(tbcurr) <= 0 ) then
!
!              There are no loaded segments left for this table.
!
!              Delete the list of column attribute entries for the
!              columns in this table, then delete the table's entry from
!              the table list.
!
!              The column attribute entries are linked, so we can free
!              them in one shot.
!
               j  =  tbctpt ( tbcurr )

               if ( j > 0 ) then
                  k  =  lnktl  ( j,    ctpool )
                  call lnkfsl  ( j, k, ctpool )
               end if


               if ( tbcurr == tbhead ) then
!
!                 The entry for this table is at the head of the
!                 table list.  Update the head of the list.
!
                  tbhead  =  lnknxt ( tbcurr, tbpool )
                  next    =  tbhead

               else

                  next    =  lnknxt ( tbcurr, tbpool )

               end if

!
!              Make the entry for this table go away.
!
               call lnkfsl ( tbcurr, tbcurr, tbpool )

!
!              The successor of the current node is the next node to
!              examine.
!
               tbcurr  =  next


            else
!
!              We're done with the current table.  Look at the next one.
!
               tbcurr = lnknxt ( tbcurr, tbpool )

            end if
!
!           We've cleaned up the table entry for the current table,
!           if it was necessary to do so.
!

         else
!
!           The current table is not affected by unloading this file.
!           Examine the next table.
!
            tbcurr = lnknxt ( tbcurr, tbpool )

         end if
!
!        We've processed the current table.
!
      end do

!
!     Don't forget to unload the EK file from the DAS system.
!
      call ekcls  ( handle )

      return







!$Procedure EKNTAB  ( EK, return number of loaded tables )

      entry ekntab ( n )

! $ Abstract
!
!     Return the number of loaded EK tables.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!
! $ Declarations
!
!     INTEGER               N
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          O   Number of loaded tables.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     N        is the number of loaded tables. The count refers
!              to the number of logical tables; if multiple
!              segments contain data for the same table, these
!              segments collectively contribute only one table
!              to the count.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     The returned count is based on the currently loaded EK files.
!
! $ Particulars
!
!     This routine is a utility that provides the caller with the
!     number of loaded tables. Callers of EKTNAM can use this count
!     as the upper bound on set of table indices when looking up table
!     names.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose we have several EK files. Load one at a time and
!        display, right after, the number of loaded EK tables in the
!        system.
!
!        Use the EK kernel below to load the Cassini Science Plan
!        SPICE E-Kernel File based upon integrated science plan. This
!        kernel contains 3 tables.
!
!           S79_CIMSSSUPa.bep
!
!        Use the EK kernel below to load the Cassini Spacecraft
!        Sequence Status SPICE E-Kernel File based upon integrated
!        Predicted Events File. This kernel contains 1 table.
!
!           S79_status_pf.bes
!
!
!        Example code begins here.
!
!
!              PROGRAM EKNTAB_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables
!        C
!              INTEGER               N
!
!        C
!        C     Load the first EK.
!        C
!              CALL FURNSH ( 'S79_CIMSSSUPa.bep' )
!
!        C
!        C     Display the number of EK tables in the system after
!        C     the first EK file is loaded.
!        C
!              CALL EKNTAB ( N )
!              WRITE(*,'(A,I3)') 'EK tables in the system (1 EK):', N
!
!        C
!        C     Load the second EK.
!        C
!              CALL FURNSH ( 'S79_status_pf.bes' )
!
!        C
!        C     Display the number of EK tables in the system after
!        C     the second EK file is loaded.
!        C
!              CALL EKNTAB ( N )
!              WRITE(*,'(A,I3)') 'EK tables in the system (2 EK):', N
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        EK tables in the system (1 EK):  3
!        EK tables in the system (2 EK):  4
!
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragments.
!
!        Removed the requirement of loading the files via EKELF from the
!        $Files section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return number of loaded tables
!
!-&


      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if
!
!     Return the number of loaded tables.
!
      n  =   mxtbld - lnknfn( tbpool )
      return






!$Procedure EKTNAM  ( EK, return name of loaded table )

      entry ektnam ( n, table )

! $ Abstract
!
!     Return the name of a specified, loaded table.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!
! $ Declarations
!
!     INTEGER               N
!     CHARACTER*(*)         TABLE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   Index of table.
!     TABLE      O   Name of table.
!
! $ Detailed_Input
!
!     N        is the index of the table whose name is desired.
!              The value of N ranges from 1 to the number of
!              loaded tables, which count may be obtained from
!              EKNTAB.
!
! $ Detailed_Output
!
!     TABLE    is the name of the N'th loaded table. If TABLE
!              is too small to accommodate the name, the name will
!              be truncated on the right.
!
! $ Parameters
!
!     TNAMSZ   is the maximum allowed table name length. See the
!              include file ektnamsz.inc for the actual value of
!              this parameter.
!
! $ Exceptions
!
!     1)  If this routine is called when no files are loaded, the
!         error SPICE(NOLOADEDFILES) is signaled.
!
!     2)  If the input N is out of range, the error SPICE(INVALDINDEX)
!         is signaled.
!
! $ Files
!
!     The returned name is based on the currently loaded EK files.
!
! $ Particulars
!
!     This routine is a utility that provides the caller with the
!     name of a specified loaded table. The index of a table with
!     a given name depends on the kernels loaded and possibly on
!     the order in which the files have been loaded.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Dump the names of all the loaded tables.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: ektnam_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                 Contents
!              ---------                 --------
!              S78_CIMSSSUPa.bep         Cassini Science Plan #78
!              S79_CIMSSSUPa.bep         Cassini Science Plan #79
!              S79_status_pf.bes         Cassini Spacecraft Sequence
!                                        Status #79
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'S78_CIMSSSUPa.bep',
!                                  'S79_CIMSSSUPa.bep',
!                                  'S79_status_pf.bes'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM EKTNAM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include EK parameter declarations:
!        C
!        C        EK Table Name Size
!        C
!              INCLUDE 'ektnamsz.inc'
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'ektnam_ex1.tm' )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(TNAMSZ)      TABNAM
!
!              INTEGER                 NTAB
!              INTEGER                 TAB
!
!        C
!        C     Load the EK files. Use a meta-kernel for convenience.
!        C
!              CALL FURNSH ( META )
!
!        C
!        C     Get the number of loaded tables. The count refers to the
!        C     number of logical tables; if multiple EKs contain data
!        C     for the same table, these EKs collectively contribute
!        C     only one table to the count.
!        C
!              CALL EKNTAB ( NTAB )
!
!              WRITE(*,'(A,I3)') 'Number of tables in EK subsystem:',
!             .                   NTAB
!
!              DO TAB = 1, NTAB
!
!        C
!        C        Get the name of the current table, and display it.
!        C
!                 CALL EKTNAM ( TAB, TABNAM )
!                 WRITE(*,'(2A)') '   TABLE = ', TABNAM
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Number of tables in EK subsystem:  4
!           TABLE = CASSINI_SP_REQUEST
!           TABLE = CASSINI_SP_OBSERVATION
!           TABLE = CASSINI_SP_REQ_OBS
!           TABLE = CASSINI_STATUS
!
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Added description of TNAMSZ parameter.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return name of a loaded table
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if


      tbcurr  =  tbhead
      fnd     =  .false.
      i       =  0

      do while (  ( tbcurr > 0 ) .and. ( .not. fnd )  )

         i   =  i + 1

         if ( i == n ) then
            fnd     =  .true.
            table   =   tbnams(tbcurr)
         else
            tbcurr  =   lnknxt ( tbcurr, tbpool )
         end if

      end do

      if ( .not. fnd ) then
         call setmsg ( 'The index # does not correspond to a loaded ' // &
                       'table.'                                        )
         call errint ( '#',  n                                         )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
      end if


      return









!$Procedure EKCCNT ( EK, column count )

      entry ekccnt ( table, ccount )

! $ Abstract
!
!     Return the number of distinct columns in a specified, currently
!     loaded table.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         TABLE
!     INTEGER               CCOUNT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TABLE      I   Name of table.
!     CCOUNT     O   Count of distinct, currently loaded columns.
!
! $ Detailed_Input
!
!     TABLE    is the name of a currently loaded table. Case
!              is not significant in the table name.
!
! $ Detailed_Output
!
!     CCOUNT   is the number of distinct columns in TABLE.
!              Columns that have the same name but belong to
!              different segments that are considered to be
!              portions of the same column, if the segments
!              containing those columns belong to TABLE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified table is not loaded, the error
!         SPICE(TABLENOTLOADED) is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine is a utility intended for use in conjunction with
!     the entry point EKCII. These routines can be used to find the
!     names and attributes of the columns that are currently loaded.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Examine an EK. Dump the names and attributes of the columns in
!        each loaded table. EKCCNT is used to obtain column counts.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKCCNT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include EK parameter declarations:
!        C
!        C        ekattdsc.inc: EK Column Attribute Descriptor
!        C                      Parameters
!        C        ekcnamsz.inc: EK Column Name Size
!        C        ektnamsz.inc: EK Table Name Size
!        C        ektype.inc:   EK Data Types
!        C
!              INCLUDE 'ekattdsc.inc'
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ektnamsz.inc'
!              INCLUDE 'ektype.inc'
!
!        C
!        C     Local parameters.
!        C
!              INTEGER                 FILEN
!              PARAMETER             ( FILEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(CNAMSZ)      COLNAM
!              CHARACTER*(FILEN)       EKFILE
!              CHARACTER*(TNAMSZ)      TABNAM
!
!              INTEGER                 ATTDSC ( ADSCSZ )
!              INTEGER                 I
!              INTEGER                 NCOLS
!              INTEGER                 NTAB
!              INTEGER                 TAB
!
!        C
!        C     Prompt for the EK file name.
!        C
!              CALL PROMPT ( 'Enter name of EK to examine > ', EKFILE )
!
!              CALL FURNSH ( EKFILE )
!
!        C
!        C     Get the number of loaded tables.
!        C
!              CALL EKNTAB ( NTAB )
!
!              WRITE(*,*) 'Number of tables in EK:', NTAB
!
!              DO TAB = 1, NTAB
!
!        C
!        C        Get the name of the current table, and look up
!        C        the column count for this table.
!        C
!                 CALL EKTNAM ( TAB,    TABNAM )
!                 CALL EKCCNT ( TABNAM, NCOLS  )
!
!                 WRITE(*,*) '------------------------------'
!             .           // '------------------------------'
!                 WRITE(*,*) 'TABLE = ', TABNAM
!                 WRITE(*,*) ' '
!
!        C
!        C        For each column in the current table, look up the
!        C        column's attributes.  The attribute block
!        C        index parameters are defined in the include file
!        C        ekattdsc.inc.
!        C
!                 DO I = 1, NCOLS
!
!                    CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
!
!                    WRITE (*,*) 'COLUMN = ', COLNAM
!
!        C
!        C           Write out the current column's data type.
!        C
!                    IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN
!
!                       WRITE (*,*) '   TYPE   =  CHR'
!
!                       IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN
!                          WRITE (*,*) '   STRING LENGTH = VARIABLE.'
!                       ELSE
!                          WRITE (*,'(A,I2)') '    STRING LENGTH = ',
!             .                         ATTDSC(ATTLEN)
!                       END IF
!
!                    ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN
!                       WRITE (*,*) '   TYPE   =  DP'
!
!                    ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN
!                       WRITE (*,*) '   TYPE   =  INT'
!
!                    ELSE
!                       WRITE (*,*) '   TYPE   =  TIME'
!                    END IF
!
!        C
!        C           Write out the current column's entry size.
!        C
!                    WRITE (*,'(A,I2)') '    SIZE   = ', ATTDSC(ATTSIZ)
!
!        C
!        C           Indicate whether the current column is indexed.
!        C
!                    IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN
!                       WRITE (*,*) '   NOT INDEXED'
!                    ELSE
!                       WRITE (*,*) '   INDEXED'
!                    END IF
!
!        C
!        C           Indicate whether the current column allows
!        C           null values.
!        C
!                    IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN
!                       WRITE (*,*) '   NULL VALUES NOT ALLOWED'
!                    ELSE
!                       WRITE (*,*) '   NULL VALUES ALLOWED'
!                    END IF
!
!                 END DO
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the EK named S79_CIMSSSUPa.bep to load the
!        Cassini Science Plan SPICE E-Kernel File based upon the
!        integrated science plan, the output was:
!
!
!        Enter name of EK to examine > S79_CIMSSSUPa.bep
!         Number of tables in EK:           3
!         ------------------------------------------------------------
!         TABLE = CASSINI_SP_REQUEST
!
!         COLUMN = SUBSYSTEM
!            TYPE   =  CHR
!            STRING LENGTH = 32
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = REQUEST_ID
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = REQUEST_TITLE
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = BEGIN_TIME
!            TYPE   =  TIME
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = END_TIME
!            TYPE   =  TIME
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = SEQUENCE
!            TYPE   =  CHR
!            STRING LENGTH = 32
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = POINTING_AGREEMENT
!            TYPE   =  CHR
!            STRING LENGTH = 80
!            SIZE   = -1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = PRIMARY_POINTING
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = SECONDARY_POINTING
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = REQ_DESCRIPTION
!            TYPE   =  CHR
!            STRING LENGTH = 80
!            SIZE   = -1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         ------------------------------------------------------------
!         TABLE = CASSINI_SP_OBSERVATION
!
!         COLUMN = SUBSYSTEM
!            TYPE   =  CHR
!            STRING LENGTH = 32
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = OBSERVATION_ID
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = OBSERVATION_TITLE
!            TYPE   =  CHR
!            STRING LENGTH = VARIABLE.
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = SEQUENCE
!            TYPE   =  CHR
!            STRING LENGTH = 32
!            SIZE   =  1
!            INDEXED
!            NULL VALUES NOT ALLOWED
!         COLUMN = SCIENCE_OBJECTIVE
!            TYPE   =  CHR
!            STRING LENGTH = 80
!            SIZE   = -1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = OBS_DESCRIPTION
!            TYPE   =  CHR
!            STRING LENGTH = 80
!            SIZE   = -1
!
!        [...]
!
!
!        Warning: incomplete output. Only 100 out of 129 lines have been
!        provided.
!
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing code fragment.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Misspelling of "conjunction" was fixed.
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return the number of loaded EK columns
!     return the count of loaded EK columns
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if


!
!     Find the table.  If there's no match, the number of loaded columns
!     is zero.
!
      tbcurr   =  tbhead
      fnd      =  .false.

      do while (  ( tbcurr > 0 ) .and. ( .not. fnd )  )

         if (  eqstr( table, tbnams(tbcurr) )  ) then
            fnd     =  .true.
         else
            tbcurr  =  lnknxt ( tbcurr, tbpool )
         end if

      end do


      if ( .not. fnd ) then

         ccount = 0
         call setmsg ( 'The table # is not currently loaded.' )
         call errch  ( '#',  table                            )
         call sigerr ( 'SPICE(TABLENOTLOADED)'                )
         return

      else
!
!        Count the columns in the attribute table for the current table.
!
         ccount  =  0
         col     =  tbctpt ( tbcurr )

         do while ( col > 0 )
            ccount  =  ccount + 1
            col     =  lnknxt ( col, ctpool )
         end do

      end if

      return







!$Procedure EKCII  ( EK, column info by index )

      entry ekcii ( table, cindex,  column,  attdsc )

! $ Abstract
!
!     Return attribute information about a column belonging to a loaded
!     EK table, specifying the column by table and index.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         TABLE
!     INTEGER               CINDEX
!     CHARACTER*(*)         COLUMN
!     INTEGER               ATTDSC ( ADSCSZ )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TABLE      I   Name of table containing column.
!     CINDEX     I   Index of column whose attributes are to be found.
!     COLUMN     O   Name of column.
!     ATTDSC     O   Column attribute descriptor.
!
! $ Detailed_Input
!
!     TABLE    is the name of a loaded EK table. Case is not
!              significant.
!
!     CINDEX   is the index, within TABLE's column attribute
!              table, of the column whose attributes are to be
!              found. The indices of the column table entries
!              range from 1 to CCOUNT, where CCOUNT is the value
!              returned by the entry point EKCCNT.
!
! $ Detailed_Output
!
!     COLUMN   is the name of the specified column.
!
!     ATTDSC   is a column attribute descriptor. ATTDSC is an
!              integer array containing descriptive information
!              that applies uniformly to all loaded columns
!              having the name COLUMN. The following parameter
!              values occur in ATTDSC:
!
!                 IFALSE:  -1
!                 ITRUE:    1
!                 CHR:      1
!                 DP:       2
!                 INT:      3
!                 TIME:     4
!
!              The meanings of the elements of ATTDSC are given
!              below. The indices of the elements are
!              parameterized; the parameter values are defined
!              in the include file ekattdsc.inc.
!
!                 ATTDSC(ATTCLS):   Column class code
!
!                 ATTDSC(ATTTYP):   Data type code---CHR, DP, INT,
!                                   or TIME
!
!                 ATTDSC(ATTLEN):   String length; applies to CHR
!                                   type. Value is IFALSE for
!                                   variable-length strings.
!
!                 ATTDSC(ATTSIZ):   Column entry size; value is
!                                   IFALSE for variable-size
!                                   columns. Here `size' refers
!                                   to the number of array
!                                   elements in a column entry.
!
!                 ATTDSC(ATTIDX):   Index flag; value is ITRUE if
!                                   column is indexed, IFALSE
!                                   otherwise.
!
!                 ATTDSC(ATTNFL):   Null flag; value is ITRUE if
!                                   column may contain null
!                                   values, IFALSE otherwise.
!
! $ Parameters
!
!     ADSCSZ   is the size of column attribute descriptor.
!              (Defined in ekattdsc.inc.)
!
! $ Exceptions
!
!     1)  If the specified table is not loaded, the error
!         SPICE(TABLENOTLOADED) is signaled.
!
!     2)  If the input argument CINDEX is out of range, the error
!         SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine is a utility that allows a calling routine to
!     determine the attributes of the currently loaded columns.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Dump the names and attributes of the columns in each loaded
!        table.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKCII_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include EK parameter declarations:
!        C
!        C        ekattdsc.inc: EK Column Attribute Descriptor
!        C                      Parameters
!        C        ekcnamsz.inc: EK Column Name Size
!        C        ektnamsz.inc: EK Table Name Size
!        C        ektype.inc:   EK Data Types
!        C
!              INCLUDE 'ekattdsc.inc'
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ektnamsz.inc'
!              INCLUDE 'ektype.inc'
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 FILEN
!              PARAMETER             ( FILEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(CNAMSZ)      COLNAM
!              CHARACTER*(FILEN)       EKFILE
!              CHARACTER*(TNAMSZ)      TABNAM
!
!              INTEGER                 ATTDSC ( ADSCSZ )
!              INTEGER                 I
!              INTEGER                 NCOLS
!              INTEGER                 NTAB
!              INTEGER                 TAB
!
!        C
!        C     Prompt for the EK file name.
!        C
!              CALL PROMPT ( 'Enter name of EK to examine > ', EKFILE )
!
!              CALL FURNSH ( EKFILE )
!
!        C
!        C     Get the number of loaded tables.
!        C
!              CALL EKNTAB ( NTAB )
!
!              WRITE(*,*) 'Number of tables in EK:', NTAB
!
!              DO TAB = 1, NTAB
!
!        C
!        C        Get the name of the current table, and look up
!        C        the column count for this table.
!        C
!                 CALL EKTNAM ( TAB,    TABNAM )
!                 CALL EKCCNT ( TABNAM, NCOLS  )
!
!                 WRITE(*,*) '------------------------------'
!             .           // '------------------------------'
!                 WRITE(*,*) 'TABLE = ', TABNAM
!                 WRITE(*,*) ' '
!
!        C
!        C        For each column in the current table, look up the
!        C        column's attributes.  The attribute block
!        C        index parameters are defined in the include file
!        C        ekattdsc.inc.
!        C
!                 DO I = 1, NCOLS
!
!                    CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
!
!                    WRITE (*,*) 'COLUMN = ', COLNAM
!
!        C
!        C           Write out the current column's data type.
!        C
!                    IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN
!
!                       WRITE (*,*) '   TYPE   =  CHR'
!
!                       IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN
!                          WRITE (*,*) '   STRING LENGTH = VARIABLE.'
!                       ELSE
!                          WRITE (*,'(A,I2)') '    STRING LENGTH = ',
!             .                         ATTDSC(ATTLEN)
!                       END IF
!
!                    ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN
!                       WRITE (*,*) '   TYPE   =  DP'
!
!                    ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN
!                       WRITE (*,*) '   TYPE   =  INT'
!
!                    ELSE
!                       WRITE (*,*) '   TYPE   =  TIME'
!                    END IF
!
!        C
!        C           Write out the current column's entry size.
!        C
!                    WRITE (*,'(A,I2)') '    SIZE   = ', ATTDSC(ATTSIZ)
!
!        C
!        C           Indicate whether the current column is indexed.
!        C
!                    IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN
!                       WRITE (*,*) '   NOT INDEXED'
!                    ELSE
!                       WRITE (*,*) '   INDEXED'
!                    END IF
!
!        C
!        C           Indicate whether the current column allows
!        C           null values.
!        C
!                    IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN
!                       WRITE (*,*) '   NULL VALUES NOT ALLOWED'
!                    ELSE
!                       WRITE (*,*) '   NULL VALUES ALLOWED'
!                    END IF
!
!                 END DO
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the EK file named vo_sedr.bdb to load the
!        Viking Orbiter Image SEDR Data, the output was:
!
!
!        Enter name of EK to examine > vo_sedr.bdb
!         Number of tables in EK:           1
!         ------------------------------------------------------------
!         TABLE = VIKING_SEDR_DATA
!
!         COLUMN = IMAGE_ID
!            TYPE   =  CHR
!            STRING LENGTH =  6
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = IMAGE_NUMBER
!            TYPE   =  INT
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = SPACECRAFT_ID
!            TYPE   =  CHR
!            STRING LENGTH =  3
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = IMAGE_TIME
!            TYPE   =  TIME
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = INSTRUMENT_ID
!            TYPE   =  CHR
!            STRING LENGTH =  4
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = GAIN_MODE_ID
!            TYPE   =  CHR
!            STRING LENGTH =  4
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = FLOOD_MODE_ID
!            TYPE   =  CHR
!            STRING LENGTH =  3
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = OFFSET_MODE_ID
!            TYPE   =  CHR
!            STRING LENGTH =  3
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = FILTER_ID
!            TYPE   =  INT
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = EXPOSURE_DURATION
!            TYPE   =  DP
!            SIZE   =  1
!            INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = PLATFORM_IN_MOTION
!            TYPE   =  CHR
!            STRING LENGTH =  3
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = PLATFORM_CONE
!            TYPE   =  DP
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = PLATFORM_CLOCK
!            TYPE   =  DP
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!         COLUMN = PLATFORM_TWIST
!            TYPE   =  DP
!            SIZE   =  1
!            NOT INDEXED
!            NULL VALUES ALLOWED
!
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited header to comply with NAIF standard.
!        Added complete code example from existing code fragment.
!
!-    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS)
!
!        Added description of ADSCSZ to the $Parameters section of the
!        header.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return information on loaded EK column specified by index
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if


!
!     Find the table.  If there's no match, the number of loaded columns
!     is zero.
!
      tbcurr   =  tbhead
      fnd      =  .false.

      do while (  ( tbcurr > 0 ) .and. ( .not. fnd )  )

         if (  eqstr( table, tbnams(tbcurr) )  ) then
            fnd     =  .true.
         else
            tbcurr  =  lnknxt ( tbcurr, tbpool )
         end if

      end do


      if ( .not. fnd ) then

         call setmsg ( 'The table # is not currently loaded.' )
         call errch  ( '#',  table                            )
         call sigerr ( 'SPICE(TABLENOTLOADED)'                )
         return

      end if


!
!     Locate the named column in the column attribute table.
!
      i    =  0
      col  =  tbctpt ( tbcurr )

      do while (  ( col > 0 )  .and. ( i < cindex )  )

         i  =  i + 1

         if ( i == cindex ) then
!
!           We've found the column.  Set the output arguments using
!           its attributes.
!
            column    = ctnams(col)

            attdsc(1) = ctclas(col)
            attdsc(2) = cttyps(col)
            attdsc(3) = ctlens(col)
            attdsc(4) = ctsizs(col)

            if ( ctindx(col) ) then
               attdsc(5) = itrue
            else
               attdsc(5) = ifalse
            end if

            if ( ctnull(col) ) then
               attdsc(6) = itrue
            else
               attdsc(6) = ifalse
            end if

            return
         else
            col = lnknxt ( col, ctpool )
         end if

      end do

!
!     We end up here if we ran out of columns before finding the
!     CINDEXth one, or if CINDEX was non-positive.
!
      call setmsg ( 'Column indices for table # range from # to #; ' // &
                    'requested index was #.'                           )
      call errch  ( '#',  tabnam                                       )
      call errint ( '#',  max( 1, i )                                  )
      call errint ( '#',  i                                            )
      call errint ( '#',  cindex                                       )
      call sigerr ( 'SPICE(INVALIDINDEX)'                              )

      return












!$Procedure EKSRCH ( EK, search for events )

      entry eksrch ( eqryi, eqryc, eqryd, nmrows, semerr, errmsg )

! $ Abstract
!
!     Search for EK events matching a specified set of constraints.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     EVENT
!     FILES
!     SEARCH
!
! $ Declarations
!
!     INTEGER               EQRYI  ( LBCELL : * )
!     CHARACTER*(*)         EQRYC
!     DOUBLE PRECISION      EQRYD  ( * )
!     INTEGER               NMROWS
!     LOGICAL               SEMERR
!     CHARACTER*(*)         ERRMSG
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of encoded query.
!     EQRYC      I   Character component of encoded query.
!     EQRYD      I   D.p. component of encoded query.
!     NMROWS     O   Number of rows matching query constraints.
!     SEMERR     O   Flag indicating whether semantic error occurred.
!     ERRMSG     O   Message describing semantic error, if any.
!
! $ Detailed_Input
!
!     EQRYI,
!     EQRYC,
!     EQRYD    are, respectively, the integer, character, and
!              double precision portions of an encoded query.
!              The query must have been parsed and must have
!              its table and column names resolved. Time values
!              must have been resolved. The query is expected
!              to be semantically correct.
!
! $ Detailed_Output
!
!     NMROWS   is the number of rows matching the input query
!              constraints.
!
!     SEMERR   is a logical flag indicating whether a semantic
!              error was detected while attempting to respond to
!              the input query.
!
!     ERRMSG   is a descriptive error message that is set if a
!              semantic error is detected. Otherwise, ERRMSG
!              is returned blank.
!
! $ Parameters
!
!     LBCELL   is the SPICE cell lower bound.
!
! $ Exceptions
!
!     1)  If this routine is called when no files are loaded, the
!         error SPICE(NOLOADEDFILES) is signaled.
!
!     2)  If the structure of the input query is invalid, this routine
!         may fail in mysterious ways.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     NAIF Toolkit-based applications will rarely need to call this
!     routine directly; the high-level routine EKFIND should normally
!     be used to query the EK system.
!
!     Because the structure of encoded queries is not part of the
!     SPICELIB user interface, we strongly recommend that users'
!     applications not call this routine directly.
!
! $ Examples
!
!     See the header of the umbrella subroutine EKQMGR for a
!     comprehensive example of the use of EKQMGR's entry points.
!
! $ Restrictions
!
!     1)  This routine should normally not be called directly from
!         users' applications.
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS)
!
!        Added description of LBCELL to the $Parameters section of the
!        header.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB)
!
!        ZZEKJSQZ call was added after the ZZEKJOIN call. This change
!        reduces the scratch area usage for intermediate results of
!        joins. It also prevents ZZEKJOIN from being handed a join
!        row set containing a segment vector having no corresponding
!        row vectors.
!
!        Removed a comment in the join loop indicating that non-join
!        constraints involving comparisons of column entries in the
!        table were being activated. This comment was incorrect; the
!        constraints in question were applied earlier.
!
!-    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB)
!
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     search for events in loaded EK files
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     There nothing to search if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if

!
!     No error to begin with.
!
      semerr  =  .false.
      errmsg  =   ' '
      nmrows  =   0


      if ( first ) then
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         call lnkini ( ftsize, ftpool )
         call lnkini ( stsize, stpool )
         call lnkini ( dtsize, dtpool )
         call lnkini ( ctsize, ctpool )
         call lnkini ( mxtbld, tbpool )

         fthead = 0
         tbhead = 0

         first  = .false.

      end if

!
!     Read some of our favorite things from the query.  We need:
!
!        - the table count
!        - the SELECT clause column count
!        - the order-by column count
!        - the table and alias list
!
      call zzekreqi ( eqryi, 'NUM_TABLES',        ntab   )
      call zzekreqi ( eqryi, 'NUM_SELECT_COLS',   nsel   )
      call zzekreqi ( eqryi, 'NUM_ORDERBY_COLS',  norder )

      do i = 1, ntab
         call zzekqtab ( eqryi, eqryc, i, frmtab(i), frmals(i) )
      end do

!
!     Initialize the table vectors.  Also initialize a vector of column
!     list pointers.
!
      call ssizec ( maxtab, tabvec )
      call ssizei ( maxtab, tptvec )

!
!     Fill in the FROM table vector and corresponding column pointer
!     vector.  It's an error if a table referenced in the FROM clause
!     can't be found.
!
      do i = 1, ntab
!
!        Find the table list entry for this table name.
!
         tbcurr  =  tbhead
         fnd     = .false.

         do while (  ( tbcurr > 0 ) .and. ( .not. fnd ) )

            if ( tbnams(tbcurr) == frmtab(i) ) then
!
!              We've found the table list entry for the Ith table.
!
               call appndc ( frmtab(i), tabvec )
               call appndi ( tbcurr,    tptvec )
               fnd     = .true.
            else
               tbcurr  =  lnknxt ( tbcurr, tbpool )
            end if

         end do

         if ( .not. fnd ) then
            call setmsg ( 'The table # is not currently loaded.' )
            call errch  ( '#',   frmtab(i)                       )
            call sigerr ( 'SPICE(INVALIDTABLENAME)'              )
            return
         end if

      end do

!
!     Since this is a new search, re-initialize the stack in the EK
!     scratch area.  Also initialize our total segment list count.
!
      call zzekstop ( top )
      call zzeksdec ( top )

!
!     Initialize the size of the join row set union for the current
!     query.  At this point, no matching rows have been found.
!
      usize  =  0
      unrows =  0

!
!     Get the number of conjunctions and the sizes of the conjunctions.
!
      call zzekreqi ( eqryi, 'NUM_CONJUNCTIONS',  nconj  )
      call cleari   ( maxcon, sizes )

      do i = 1, nconj
         call zzekqcnj ( eqryi, i, sizes(i) )
      end do

!
!     For each conjunction of constraints, we'll build a join row
!     set representing the row vectors matching those constraints.
!     The final result will be a join row set union representing the
!     row vectors satisfying at least one conjunction.
!
!     We want to build a join row set even if there are *no*
!     constraints.  Therefore, we always make at least one pass
!     through the loop below.
!
      cjend = 0

      do conj  =  1,  max( 1, nconj )
!
!        Our objective is to build a join row set representing the table
!        defined by the FROM columns and the input constraints.  To do
!        this, we'll first build a trivial join row set for each table;
!        this join row set represents the rows that satisfy constraints
!        on columns in that table.  Having done this, we'll produce a
!        final (for this conjunction) join row set that represents the
!        constrained join of the FROM tables.  The base address of this
!        join row set will be stored in the array UBASE.
!
!        We'll start out by recording the FROM table indices and column
!        list indices of columns listed in the constraints.
!
         if ( nconj == 0 ) then
            cjsize  =  0
         else
            cjsize  =  sizes(conj)
         end if

         cjbeg  =  cjend + 1
         cjend  =  cjend + cjsize

         do i  =  1, cjsize

            call zzekqcon ( eqryi,     eqryc,      eqryd,     cjbeg+i-1, &
                            cnstyp(i), &
                            ltname,    ltbidx(i),  lcname,    lcidx(i), &
                            ops(i), &
                            rtname,    rtbidx(i),  rcname,    rcidx(i), &
                            dtype(i), &
                            cbegs(i),  cends(i),   dvals(i),  ivals(i) )
         end do


         do t = 1, ntab
!
!           We will build a trivial (one-table) join row set for the
!           current table.
!
!           Initialize the join row set.  Retain the base address.  We
!           can fill in the table count right away; the count is 1.
!
            call zzekstop ( rbas(t) )

            do i = 1, jscidx
               call zzekspsh ( 1, 0 )
            end do

            call zzeksupd ( rbas(t)+jtcidx, rbas(t)+jtcidx, 1 )

!
!           Count the loaded segments for the current table.  We'll
!           leave enough room in the join row set for each segment.
!
            tab = tptvec(t)
            i   = tbstpt(tab)
            nsv = 0

            do while ( i > 0 )

               call zzekspsh ( 1, 0 )

               nsv  =  nsv + 1
               i    =  lnknxt ( i, stpool )

            end do

!
!           Save room for the row vector base addresses and counts.
!
            do i = 1, 2*nsv
               call zzekspsh ( 1, 0 )
            end do

!
!           At this point, we can set the segment vector count in the
!           join row set.
!
            call zzeksupd ( rbas(t)+jscidx, rbas(t)+jscidx, nsv )

!
!           Find the matching rows in the segments belonging to the
!           current table.
!
            seg     =  tbstpt(tab)
            nseg    =  0
            rtotal  =  0

            do while ( seg > 0 )

               nseg = nseg + 1
!
!              The segment vector for this segment is trivial:  it's
!              just the segment's index in the segment table.
!
               sgvbas  =  rbas(t) + jsvbas + ( nseg - 1 )

               call zzeksupd ( sgvbas+1, sgvbas+1, seg )

!
!              Label as `inactive' any constraints that don't apply to
!              this table.  Join constraints are inactive at this stage
!              of the game.  Label all other constraints `active'.
!              We'll keep track of column and value constraints
!              separately.
!
               do i = 1, cjsize
!
!                 Each constraint is active to start with.
!
                  activc(i) = cnstyp(i) == eqcol
                  activv(i) = cnstyp(i) == eqval

!
!                 The parent table of the LHS column must be the Tth
!                 table, or this constraint does not apply.
!
!                 We'll also exclude join constraints.  Note that
!                 constraints comparing values from two columns need not
!                 be join constraints:  it's possible that the column on
!                 the right belongs to the same FROM table as the
!                 column on the left.
!
                  if ( ltbidx(i) /= t ) then

                     activc(i) = .false.
                     activv(i) = .false.


                  else if ( cnstyp(i) == eqcol ) then

                     if ( ltbidx(i) /= rtbidx(i) ) then
!
!                       This is a join constraint; disable it.
!
                        activc(i) = .false.

                     end if

                  end if


               end do



!
!              At this point, we'll have to search the segment for
!              matching rows.  Pick a key column for the segment.  To
!              do this, we'll need to pack an array with column
!              descriptors for each active constraint.  The
!              descriptor for the column on the left side of the Ith
!              constraint will be placed in elements LDSCRS(*,I), if
!              the Ith constraint is active.
!
               call cleari ( cdscsz*maxcon, ldscrs )

               do i = 1, cjsize

                  if ( activv(i) ) then
!
!                     Look up the column descriptor for this
!                     constraint.
!
                     j  =  stdtpt(seg)

                     do k = 2, lcidx(i)
                        j  = lnknxt(j,dtpool)
                     end do

                     call movei ( dtdscs(1,j), cdscsz, ldscrs(1,i) )

                  end if

               end do


               call zzekkey ( sthan (seg),     stdscs(1,seg), &
                              stnrow(seg),     cjsize, &
                              lcidx,           ldscrs, &
                              ops,             dtype, &
                              eqryc,           cbegs, &
                              cends,           dvals, &
                              ivals,           activv, &
                              key,             keydsc, &
                              begidx,          endidx, &
                              keyfnd                        )

!
!              ZZEKKEY has updated ACTIVV to reflect the application
!              of constraints that were used to determine BEGIDX and
!              ENDIDX.
!
               if ( keyfnd ) then

                  indexd  =  .true.

               else
!
!                 A key column could not be determined from the
!                 active constraints.  We'll use the first column of
!                 the segment as the key column.
!
                  indexd  =  .false.
                  begidx  =   1
                  endidx  =   stnrow(seg)

               end if

!
!              Whether or not we have any matching rows, we'll need
!              to record how many we have.  Save the offset from the
!              join row set base for the pointer to the row vectors.
!              The row vector count follows this pointer.
!
               ptroff = jsvbas  +  nsv  +  (nseg - 1) * 2   +   1


               if ( endidx >= begidx ) then
!
!                 Initialize the count of matching rows for this
!                 segment.  The current stack top is the base address
!                 for the row vectors; save the offset of this
!                 address from the join row set's base.
!                 Also compute the base address of the segment vector
!                 for the current segment.
!
                  nmatch = 0
                  call zzekstop ( rwvbas )

                  call zzeksupd ( rbas(t)+ptroff, &
                                  rbas(t)+ptroff,   rwvbas-rbas(t) )

!
!                 Count the active constraints.  While we're at it,
!                 fill in the descriptor lists LDSCRS and RDSCRS
!                 with, respectively, the descriptors for the columns
!                 on the left hand sides and right hand sides of
!                 these constraints.
!
                  call cleari ( cdscsz*maxcon, ldscrs )
                  call cleari ( cdscsz*maxcon, rdscrs )
                  nact = 0

                  do i = 1, cjsize

                     if ( activc(i) .or. activv(i) ) then

                        nact = nact + 1
!
!                       Look up the column descriptor for this
!                       constraint.

                        j  =  stdtpt(seg)

                        do k = 2, lcidx(i)
                           j  = lnknxt(j,dtpool)
                        end do

                        call movei(dtdscs(1,j), cdscsz, ldscrs(1,i) )


                        j  =  stdtpt(seg)

                        do k = 2, rcidx(i)
                           j  = lnknxt(j,dtpool)
                        end do

                        call movei(dtdscs(1,j), cdscsz, rdscrs(1,i) )

                     end if

                  end do


                  if ( nact > 0 ) then
!
!                    There are still active constraints left, so
!                    proceed linearly through the remaining rows,
!                    testing each one against these constraints. Add
!                    matching rows to the current join row set.
!
                     do r = begidx, endidx


                        if ( indexd ) then

                           call zzekixlk ( sthan(seg), &
                                           keydsc, &
                                           r, &
                                           rowidx )
                        else
!
!                          Look up the record pointer for row R.
!
                           call zzekrplk ( sthan(seg), &
                                           stdscs(1,seg), &
                                           r, &
                                           rowidx  )
                        end if

!
!                       Test the row against both value and column
!                       constraints.  For now, we supply an array
!                       of default column entry element indices.
!
                        vmtch = zzekrmch ( cjsize,     activv, &
                                           sthan(seg), stdscs(1,seg), &
                                           ldscrs,     rowidx, &
                                           lelts,      ops, &
                                           dtype,      eqryc, &
                                           cbegs,      cends, &
                                           dvals,      ivals     )

                        cmtch = .true.

!
!                       Note that ZZEKVMCH expects a set of inputs
!                       that are not really parallel to those
!                       expected by ZZEKRMCH.  We feed the
!                       column comparison constraints to ZZEKVMCH
!                       one at a time.
!
                        do j = 1, cjsize

                           cmtch =       cmtch &
                                   .and. zzekvmch ( 1, &
                                                    activc(j), &
                                                    sthan(seg), &
                                                    stdscs(1,seg), &
                                                    ldscrs(1,j), &
                                                    rowidx, &
                                                    1, &
                                                    ops(j), &
                                                    sthan(seg), &
                                                    stdscs(1,seg), &
                                                    rdscrs(1,j), &
                                                    rowidx, &
                                                    1            )
                        end do


                        if ( cmtch .and. vmtch ) then
!
!                          Push the `augmented row vector' for the
!                          current row onto the stack.  In this case,
!                          of course, the augmented row vector is
!                          trivial:  it consists of the row number,
!                          followed by the base address of the parent
!                          segment vector.
!
                           nmatch = nmatch + 1

                           call zzekspsh ( 1, rowidx         )
                           call zzekspsh ( 1, sgvbas-rbas(t) )

                        end if

                     end do


                  else
!
!                    All the rows indicated by BEGIDX and ENDIDX
!                    match the constraints.  This code section should
!                    be upgraded to transfer the row numbers in
!                    chunks.
!
                     nmatch  =  endidx - begidx + 1


                     do r = begidx, endidx

                        if ( indexd ) then
!
!                          Look up the record pointer for row R
!                          from the column index.
!
                           call zzekixlk ( sthan(seg), &
                                           keydsc, &
                                           r, &
                                           rowidx )
                        else
!
!                          Look up the record pointer for row R.
!
                           call zzekrplk ( sthan(seg), &
                                           stdscs(1,seg), &
                                           r, &
                                           rowidx  )
                        end if


                        call zzekspsh ( 1, rowidx         )
                        call zzekspsh ( 1, sgvbas-rbas(t) )

                     end do

                  end if

!
!                 Fill in the row count for this segment in the join row
!                 set.
!
                  call zzeksupd ( rbas(t)+ptroff+1, &
                                  rbas(t)+ptroff+1, nmatch )

               end if

!
!              Take a look at the next segment.  Update the total count
!              of matching rows for this table.
!
               seg    = lnknxt ( seg, stpool )
               rtotal = rtotal + nmatch

            end do

!
!           Fill in the size and count information for the join row set.
!
            call zzekstop ( top )
            rsize(t)  =  top - rbas(t)

            call zzeksupd ( rbas(t)+jszidx, rbas(t)+jszidx, rsize(t) )
            call zzeksupd ( rbas(t)+jrcidx, rbas(t)+jrcidx, rtotal   )

!
!           Compress out any empty segment vectors from the join row
!           set.
!
            call zzekjsqz ( rbas(t) )

!
!           At this point, we've filled in the entire join row set for
!           table T.
!
         end do

!
!        Join the trivial join row sets, producing a final join row set
!        for the current conjunction.  Retain the base address of this
!        join row set, if it is non-empty.  Update the size of the join
!        row set union.
!
!
         resbas = rbas(1)

         do t = 2, ntab
!
!           Arm the join constraints!  Turn on the constraints that
!           have the Tth table on the one side, and tables
!           1, 2, ... , T on the other.
!
            do i = 1, cjsize

               activc(i)  =  .false.

               if ( cnstyp(i) == eqcol ) then

                  l  =  ltbidx(i)
                  r  =  rtbidx(i)

                  if (      ( l >= 1           ) &
                      .and. ( l <= t           ) &
                      .and. ( r >= 1           ) &
                      .and. ( r <= t           ) &
                      .and. ( l /= r           ) &
                      .and. (      ( r == t ) &
                              .or. ( l == t )  )  )  then

                     activc(i)  =  .true.

                  end if

               end if

            end do

!
!           The base address of the first join row set is the base
!           address of the result of the previous join.  The first time
!           through, the base of the join row set for table 1 is used.
!
            if ( t == 2 ) then
               jbase1  =  rbas( 1 )
            else
               jbase1  =  resbas
            end if

            jbase2  = rbas( t )

            call zzekjoin (  jbase1,   jbase2,   cjsize,  activc, &
                             ltbidx,   lcidx,    lelts,   ops, &
                             rtbidx,   rcidx,    relts,   sthan, &
                             stdscs,   stdtpt,   dtpool,  dtdscs, &
                             resbas,   jsize                       )

            call zzekjsqz ( resbas )

         end do

!
!        At this point, we've found the matching rows for the current
!        query conjunction.  Update the size of the join row set union
!        corresponding to the current query.  Save the base address of
!        the final join row set.  Update the total number of matching
!        rows in the join row set union.
!
         usize         =  usize  + 1
         ubase(usize)  =  resbas

         call zzeksrd ( resbas+jrcidx, resbas+jrcidx, cjrows )

         unrows        =  unrows + cjrows

!
!        Remove redundant row vectors from the join row set union.
!        These row vectors may arise in the execution of queries whose
!        WHERE clauses contain multiple conjunctions.
!
         call zzekweed ( usize, ubase, unrows )

!
!        Initialize the addressing function for the current join row
!        set union.
!
         if ( usize > 0 ) then
            call zzekvset ( usize, ubase )
         end if

      end do

!
!     At this point, we've formed the join row set union that
!     represents the set of row vectors matching the entire query.
!
      nmrows  =  unrows

!
!     Get the tables and columns of from the SELECT clause.  For
!     each qualifying table, we need the index in the FROM clause
!     of that table.  For each column, we need the column table
!     index.
!
      do i = 1, nsel

         call zzekqsel ( eqryi,  eqryc,  i,      lxbeg,  lxend, &
                         tabnam, tabidx, colnam, k             )

!
!        Locate the column's attribute information.  Retain the column's
!        index within the parent table's column list.
!
         tab     =  tptvec( tabidx )
         j       =  tbctpt(tab)
         col     =  0
         fnd     =  .false.

         do while (  ( j > 0 ) .and. ( .not. fnd )  )

            col = col + 1

            if ( ctnams(j) == colnam ) then
               fnd  =  .true.
            else
               j    =  lnknxt( j, ctpool )
            end if

         end do

         if ( .not. fnd ) then
            call setmsg ( '# is not name of a column in FROM table #.' )
            call errch  ( '#', colnam                                  )
            call errint ( '#', tabidx                                  )
            call sigerr ( 'SPICE(BUG)'                                 )
            return
         end if

         selctp(i)  =  j
         selcol(i)  =  col
         seltab(i)  =  tabidx

      end do

!
!     Enable sorting of the matching row vectors, if necessary.  The
!     first fetch request will invoke the sort.
!
      dosort  =  ( norder > 0 ) .and. ( nmrows > 0 )
      sorted  =  .false.

      if ( dosort ) then

         do i = 1, norder

            call zzekqord (  eqryi,     eqryc,   i,         tabnam, &
                             otabs(i),  colnam,  ocols(i),  sense(i)  )
         end do

      end if


      return







!$Procedure EKNELT  ( EK, get number of elements in column entry )

      entry eknelt ( selidx, row, nelt )

! $ Abstract
!
!     Return the number of elements in a specified column entry in
!     the current row.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations
!
!     INTEGER               SELIDX
!     INTEGER               ROW
!     INTEGER               NELT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SELIDX     I   Index of parent column in SELECT clause.
!     ROW        I   Row containing element.
!     NELT       O   Number of elements in entry in current row.
!
! $ Detailed_Input
!
!     SELIDX   is the SELECT clause index of the column to
!              fetch from.
!
!     ROW      is the index of the row containing the element.
!              This number refers to a member of the set of rows
!              matching a query. ROW must be in the range
!
!                 1 : NMROWS
!
!              where NMROWS is the matching row count returned
!              by EKSRCH.
!
! $ Detailed_Output
!
!     NELT     is the number of elements in the column entry
!              belonging to the specified column in the current
!              row.
!
!              Null entries in variable-size columns are
!              considered to have size 1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called when no E-kernels have been loaded,
!         the error SPICE(NOLOADEDFILES) is signaled.
!
!     2)  If SELIDX is outside of the range established by the
!         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
!         is signaled.
!
!     3)  If ROW is outside of the range established by the
!         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
!         is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine is meant to be used in conjunction with the EKQMGR
!     fetch entry points EKGC, EKGD, and EKGI. This routine
!     allows the caller of those routines to determine appropriate
!     loop bounds to use to fetch each column entry in the current row.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to fetch integer, double
!        precision and character string values from a column when such
!        column corresponds to either a variable-size array or to a
!        static-size array.
!
!        Create an EK that contains a table TAB that has the following
!        columns:
!
!            Column name   Data Type   Size
!            -----------   ---------   ----
!            IARRAY        INT         3
!            DARRAY        DP          VARIABLE
!            CARRAY        CHR         VARIABLE
!
!        Issue the following query
!
!            QUERY = 'SELECT IARRAY, DARRAY, CARRAY FROM TAB'
!
!        to fetch and dump column values from the rows that satisfy the
!        query.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKNELT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C     and EK Query Limit Parameters (MAXQRY)
!        C
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME  = 'eknelt_ex1.bdb' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE   = 'TAB' )
!
!              INTEGER               CHRSLN
!              PARAMETER           ( CHRSLN = 5   )
!
!              INTEGER               COL1SZ
!              PARAMETER           ( COL1SZ = 3   )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               MXC2SZ
!              PARAMETER           ( MXC2SZ = 4   )
!
!              INTEGER               MXC3SZ
!              PARAMETER           ( MXC3SZ = 7   )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 3   )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 4   )
!
!              INTEGER               STRSIZ
!              PARAMETER           ( STRSIZ  = 30 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
!              CHARACTER*(CHRSLN)    COL3   ( MXC3SZ )
!              CHARACTER*(STRSIZ)    CVALS  ( MXC3SZ )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(MAXQRY)    QUERY
!
!              DOUBLE PRECISION      COL2   ( MXC2SZ )
!              DOUBLE PRECISION      DVALS  ( MXC2SZ )
!
!              INTEGER               COL1   ( COL1SZ )
!              INTEGER               ELTIDX
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               IVALS ( COL1SZ )
!              INTEGER               J
!              INTEGER               NELT
!              INTEGER               NMROWS
!              INTEGER               NRESVC
!              INTEGER               RECNO
!              INTEGER               ROW
!              INTEGER               SEGNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 13-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the column names and declarations
!        C     for the TAB segment.  We'll index all of
!        C     the columns.
!        C
!              CNAMES(1) = 'IARRAY'
!              CDECLS(1) = 'DATATYPE = INTEGER, SIZE = 3'
!
!              CNAMES(2) = 'DARRAY'
!              CDECLS(2) = 'DATATYPE = DOUBLE PRECISION, ' //
!             .            'SIZE = VARIABLE'
!
!              CNAMES(3) = 'CARRAY'
!              CDECLS(3) = 'DATATYPE = CHARACTER*(5), ' //
!             .            'SIZE = VARIABLE'
!
!        C
!        C     Start the segment.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!        C
!        C     At the records to the table.
!        C
!              DO I = 1, NROWS
!
!        C
!        C        Append a new record to the EK.
!        C
!                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!        C
!        C        Add 3 items to IARRAY
!        C
!                 DO J = 1, COL1SZ
!                    COL1(J) =  I + J*100.D0
!                 END DO
!
!                 CALL EKACEI ( HANDLE,    SEGNO,  RECNO,
!             .                 CNAMES(1), COL1SZ, COL1,  .FALSE. )
!
!        C
!        C        Add I items to DARRAY
!        C
!                 DO J = 1, I
!                    COL2(J) = J + I*200.D0
!                 END DO
!
!                 CALL EKACED ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(2), I,     COL2,  .FALSE. )
!
!        C
!        C        Add 3+I items to CARRAY
!        C
!                 DO J = 1, 3+I
!                    CALL REPMI ( 'ST#', '#', J + I*100, COL3(J) )
!                 END DO
!
!                 CALL EKACEC ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(3), I+3,   COL3,  .FALSE. )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Open the created file. Perform the query and show the
!        C     results.
!        C
!              CALL FURNSH ( EKNAME )
!
!              QUERY = 'SELECT IARRAY, DARRAY, CARRAY FROM TAB'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!                 DO ROW = 1, NMROWS
!
!                    WRITE(*,*) ' '
!                    WRITE(*,'(A,I3)') 'ROW  = ', ROW
!
!        C
!        C           Fetch values from column IARRAY in the current
!        C           row.  Since IARRAY was the first column selected,
!        C           the selection index SELIDX is set to 1.
!        C
!                    SELIDX = 1
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  COL1SZ )
!             .                 .AND. (        .NOT. ISNULL )  )
!        C
!        C              If the column entry is null, we'll be kicked
!        C              out of this loop after the first iteration.
!        C
!                       CALL EKGI ( SELIDX,         ROW,     ELTIDX,
!             .                     IVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,'(A)') '   COLUMN = IARRAY: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,3I6)') '   COLUMN = IARRAY:',
!             .                            ( IVALS(I), I = 1, COL1SZ )
!
!                    END IF
!
!        C
!        C           Fetch values from column DARRAY in the current
!        C           row.  Since DARRAY contains variable-size array
!        C           elements, we call EKNELT to determine how many
!        C           elements to fetch.
!        C
!                    SELIDX = 2
!                    CALL EKNELT ( SELIDX, ROW, NELT )
!
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  NELT   )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGD ( SELIDX,         ROW,     ELTIDX,
!             .                     DVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,'(A)') '   COLUMN = DARRAY: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,4F6.1)') '   COLUMN = DARRAY:',
!             .                              ( DVALS(I), I = 1, NELT )
!
!                    END IF
!
!        C
!        C           Fetch values from column CARRAY in the current
!        C           row.
!        C
!                    SELIDX = 3
!                    CALL EKNELT ( SELIDX, ROW, NELT )
!
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  NELT   )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGC ( SELIDX,         ROW,     ELTIDX,
!             .                     CVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,'(A)') '   COLUMN = CARRAY: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(8A)') '   COLUMN = CARRAY: ',
!             .                     ( CVALS(I)(:RTRIM(CVALS(I))) //' ',
!             .                                         I = 1, NELT   )
!
!                    END IF
!
!                 END DO
!
!        C
!        C     We either parsed the SELECT clause or had an error.
!        C
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ROW  =   1
!           COLUMN = IARRAY:   101   201   301
!           COLUMN = DARRAY: 201.0
!           COLUMN = CARRAY: ST101 ST102 ST103 ST104
!
!        ROW  =   2
!           COLUMN = IARRAY:   102   202   302
!           COLUMN = DARRAY: 401.0 402.0
!           COLUMN = CARRAY: ST201 ST202 ST203 ST204 ST205
!
!        ROW  =   3
!           COLUMN = IARRAY:   103   203   303
!           COLUMN = DARRAY: 601.0 602.0 603.0
!           COLUMN = CARRAY: ST301 ST302 ST303 ST304 ST305 ST306
!
!        ROW  =   4
!           COLUMN = IARRAY:   104   204   304
!           COLUMN = DARRAY: 801.0 802.0 803.0 804.0
!           COLUMN = CARRAY: ST401 ST402 ST403 ST404 ST405 ST406 ST407
!
!
!        Note that after run completion, a new EK file exists in the
!        output directory.
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example from existing fragment.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.2.0, 12-FEB-1999 (NJB)
!
!        Bug fix: There was a error handling branch that called CHKOUT
!        where CHKIN should have been called. This has been fixed.
!
!-    SPICELIB Version 1.1.0, 09-JUL-1996 (NJB)
!
!        Bug fix: EKNELT now initiates a sort operation if sorted
!        outputs are required and EKNELT is called after query
!        resolution but before the fetch routines. Also, addressing
!        for sorted query results has been fixed.
!
!        Misspelling of "issued" was fixed. Previous version line was
!        changed from "Beta" to "SPICELIB."
!
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return the number of elements in a column entry
!
!-&


!
!     Use discovery check-in for speed.
!
      if ( return () ) return

!
!     The request doesn't make sense if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if

!
!     The row number must be valid, or we can't proceed.
!
      if (  ( row < 1 ) .or. ( row > unrows )  ) then

         call setmsg ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         call errint ( '#',  unrows                                    )
         call errint ( '#',  row                                       )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
         return

      end if

!
!     Make sure the SELECT clause column index is valid.
!
      if (  ( selidx < 1 ) .or. ( selidx > nsel )  ) then
         call setmsg ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         call errint ( '#', selidx                                )
         call errint ( '#', ntab                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                      )
         return
      end if

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      if ( dosort ) then

         call zzekjsrt ( usize,   ubase,   norder,  otabs, &
                         ocols,   oelts,   sense,   sthan, &
                         stdscs,  stdtpt,  dtpool,  dtdscs,  ordbas  )

         dosort  =  .false.
         sorted  =  .true.
      end if

!
!     Look up the segment vector and row vector for the current row.
!
      if ( sorted ) then
         call zzeksrd  ( ordbas+row,  ordbas+row,  i      )
         call zzekvcal ( i,           rwvbas,      sgvbas )
      else
         call zzekvcal ( row,         rwvbas,      sgvbas )
      end if

      call zzeksrd  ( rwvbas+1, rwvbas+ntab, rowvec )
      call zzeksrd  ( sgvbas+1, sgvbas+ntab, segvec )

      tabidx  =  seltab(selidx)
      rowidx  =  rowvec(tabidx)
      seg     =  segvec(tabidx)
      col     =  selcol(selidx)

      colptr = stdtpt(seg)

      do i = 2, col
         colptr = lnknxt( colptr, dtpool )
      end do

      nelt  =  zzekesiz (  sthan (   seg   ), &
                           stdscs(1, seg   ), &
                           dtdscs(1, colptr), &
                           rowidx             )


      return





!$Procedure EKGC  ( EK, get event data, character )

      entry ekgc ( selidx, row, elment, cdata, null, found )

! $ Abstract
!
!     Return an element of an entry in a column of character
!     type in a specified row.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     ASSIGNMENT
!     EK
!
! $ Declarations
!
!     INTEGER               SELIDX
!     INTEGER               ROW
!     INTEGER               ELMENT
!     CHARACTER*(*)         CDATA
!     LOGICAL               NULL
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SELIDX     I   Index of parent column in SELECT clause.
!     ROW        I   Row to fetch from.
!     ELMENT     I   Index of element, within column entry, to fetch.
!     CDATA      O   Character string element of column entry.
!     NULL       O   Flag indicating whether column entry was null.
!     FOUND      O   Flag indicating whether column was present in row.
!
! $ Detailed_Input
!
!     SELIDX   is the SELECT clause index of the column to
!              fetch from.
!
!     ROW      is the output row containing the entry to fetch
!              from.
!
!     ELMENT   is the index of the element of the column entry
!              to fetch. The normal range of ELMENT is from 1 to
!              the size of the column's entry, but ELMENT is
!              allowed to exceed the number of elements in the
!              column entry; if it does, FOUND is returned .FALSE.
!              This allows the caller to read data from the column
!              entry in a loop without checking the number of
!              available elements first.
!
!              Null values in variable-sized columns are
!              considered to have size 1.
!
! $ Detailed_Output
!
!     CDATA    is the requested element of the specified column
!              entry. If the entry is null, CDATA is undefined.
!
!              If CDATA is too short to accommodate the requested
!              column entry element, the element is truncated on
!              the right to fit CDATA. If CDATA is longer than
!              the element, CDATA is returned blank-padded on
!              the right.
!
!     NULL     is a logical flag indicating whether the entry
!              belonging to the specified column in the specified
!              row is null.
!
!     FOUND    is a logical flag indicating whether the specified
!              element was found. If the element does not exist,
!              FOUND is returned .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input argument ELMENT is less than 1, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!         However, ELMENT is allowed to be greater than the number of
!         elements in the specified column entry; this allows the caller
!         to read data from the column entry in a loop without checking
!         the number of available elements first. If ELMENT is greater
!         than the number of available elements, FOUND is returned
!         .FALSE.
!
!     2)  If SELIDX is outside of the range established by the
!         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
!         is signaled and FOUND is returned .FALSE.
!
!     3)  If the input argument ROW is less than 1 or greater than the
!         number of rows matching the query, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!
!     4)  If the specified column does not have character type, the
!         error SPICE(INVALIDTYPE) is signaled.
!
!     5)  If this routine is called when no E-kernels have been loaded,
!         the error SPICE(NOLOADEDFILES) is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine allows retrieval of data from character columns.
!
!     This routine returns one element at a time in order to save the
!     caller from imposing a limit on the size of the column entries
!     that can be handled.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Perform a query on an EK file that contains a database with
!        the Supplementary Engineering Data Records of the Viking
!        Project in order to retrieve the IMAGE_ID values (character
!        strings) that correspond to the images with IMAGE_NUMBER
!        smaller than a given value, ordered by IMAGE_NUMBER.
!
!
!        Use the EK kernel below to load the information from the
!        original Supplementary Engineering Data Record (SEDR) data
!        set generated by the Viking Project.
!
!           vo_sedr.bdb
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY, and the maximum length of literal string
!        C     values, MAXSTR, from eklimit.inc.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(MAXSTR)    CDATA
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               ELTIDX
!              INTEGER               NMROWS
!              INTEGER               ROWNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open an EK file.
!        C
!              CALL FURNSH ( EKNAME )
!
!        C
!        C     The table 'VIKING_SEDR_DATA' has a column 'IMAGE_ID'
!        C     of scalar strings.
!        C
!        C     Define a set of constraints to perform a query on
!        C     all loaded EK files (the SELECT clause). In this
!        C     case select the column 'IMAGE_ID' from table
!        C     'VIKING_SEDR_DATA' sorted by 'IMAGE_NUMBER'.
!        C
!              QUERY = 'Select IMAGE_ID from VIKING_SEDR_DATA '
!             .   //   'where IMAGE_NUMBER < 25860000 '
!             .   //   'order by IMAGE_NUMBER'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!        C
!        C        Fetch the character data. We know the query returned
!        C        one column and the column contains only scalar data,
!        C        so the index of all elements is 1.
!        C
!                 SELIDX = 1
!                 ELTIDX = 1
!
!        C
!        C        Loop over each row found matching the query.
!        C
!                 DO ROWNO = 1, NMROWS
!
!        C
!        C           Use EKGC to retrieve the string from
!        C
!                    CALL EKGC ( SELIDX,  ROWNO,  ELTIDX,
!             .                  CDATA,   ISNULL, FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE (*,'(A,I3,A)') 'Row ', ROWNO,
!             .                  ': Character data: <Null>'
!
!                    ELSE
!
!                       WRITE (*,'(A,I3,2A)') 'Row ', ROWNO,
!             .                  ': Character data: ', CDATA
!                    END IF
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Row   1: Character data: 168C09
!        Row   2: Character data: 168C10
!        Row   3: Character data: 168C11
!        Row   4: Character data: 168C12
!        Row   5: Character data: 169C01
!        Row   6: Character data: 169C02
!        Row   7: Character data: 169C03
!        Row   8: Character data: 169C04
!        Row   9: Character data: 169C05
!        Row  10: Character data: 169C09
!        Row  11: Character data: 169C11
!        Row  12: Character data: 169C19
!        Row  13: Character data: 169C23
!        Row  14: Character data: 169C25
!        Row  15: Character data: 169C26
!        Row  16: Character data: 169C30
!        Row  17: Character data: 169C32
!        Row  18: Character data: 169C33
!        Row  19: Character data: 169C37
!        Row  20: Character data: 169C39
!        Row  21: Character data: 169C40
!        Row  22: Character data: 169C44
!        Row  23: Character data: 169C46
!        Row  24: Character data: 169C47
!        Row  25: Character data: 169C51
!        Row  26: Character data: 169C53
!
!
!     2) This example demonstrates how to fetch string values from a
!        column in three different cases: single values, variable-size
!        arrays and static-size arrays.
!
!        Create an EK that contains a table TAB that has the following
!        columns:
!
!           Column name   Data Type   Size
!           -----------   ---------   ----
!           CHR_COL_1     CHR         1
!           CHR_COL_2     CHR         VARIABLE
!           CHR_COL_3     CHR         3
!
!        Issue the following query
!
!            QUERY = 'SELECT CHR_COL_1, CHR_COL_2, CHR_COL_3 FROM TAB'
!
!        to fetch and dump column values from the rows that satisfy the
!        query.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGC_EX2
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C     and EK Query Limit Parameters (MAXQRY)
!        C
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME  = 'ekgc_ex2.bdb' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE   = 'TAB' )
!
!              INTEGER               CHRSLN
!              PARAMETER           ( CHRSLN = 3   )
!
!              INTEGER               COL3SZ
!              PARAMETER           ( COL3SZ = 3   )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               MXC2SZ
!              PARAMETER           ( MXC2SZ = 8   )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 3   )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 4   )
!
!              INTEGER               STRSIZ
!              PARAMETER           ( STRSIZ  = 30 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
!              CHARACTER*(STRSIZ)    COL1
!              CHARACTER*(CHRSLN)    COL2   ( MXC2SZ )
!              CHARACTER*(CHRSLN)    COL3   ( COL3SZ )
!              CHARACTER*(STRSIZ)    CVALS  ( MXC2SZ )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               ELTIDX
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               NELT
!              INTEGER               NMROWS
!              INTEGER               NRESVC
!              INTEGER               RECNO
!              INTEGER               ROW
!              INTEGER               SEGNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 13-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the column names and declarations
!        C     for the TAB segment.  We'll index all of
!        C     the columns.
!        C
!              CNAMES(1) = 'CHR_COL_1'
!              CDECLS(1) = 'DATATYPE = CHARACTER*(*), ' //
!             .            'INDEXED  = TRUE'
!
!              CNAMES(2) = 'CHR_COL_2'
!              CDECLS(2) = 'DATATYPE = CHARACTER*(3), ' //
!             .            'SIZE = VARIABLE'
!
!              CNAMES(3) = 'CHR_COL_3'
!              CDECLS(3) = 'DATATYPE = CHARACTER*(3), ' //
!             .            'SIZE = 3'
!
!        C
!        C     Start the segment.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!        C
!        C     At the records to the table.
!        C
!              DO I = 1, NROWS
!
!        C
!        C        Append a new record to the EK.
!        C
!                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!        C
!        C        Add CHR_COL_1
!        C
!                 CALL REPMI ( 'Column #2 has $ elements.',
!             .                '$', I*2, COL1              )
!
!                 CALL EKACEC ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(1), 1,     COL1, .FALSE. )
!
!        C
!        C        Add I*2 items to CHR_COL_2
!        C
!                 DO J = 1, I*2
!                    CALL INTSTR( J + I*100, COL2(J) )
!                 END DO
!
!                 CALL EKACEC ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(2), I*2,   COL2, .FALSE. )
!
!        C
!        C        Add 3 items to CHR_COL_3
!        C
!                 DO J = 1, 3
!                    CALL INTSTR( I + J*100, COL3(J) )
!                 END DO
!
!                 CALL EKACEC ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(3), 3,     COL3, .FALSE. )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Open the created file. Perform the query and show the
!        C     results.
!        C
!              CALL FURNSH ( EKNAME )
!
!              QUERY = 'SELECT CHR_COL_1, CHR_COL_2, CHR_COL_3 FROM TAB'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!                 DO ROW = 1, NMROWS
!
!                    WRITE(*,*) ' '
!                    WRITE(*,'(A,I3)') 'ROW  = ', ROW
!
!        C
!        C           Fetch values from column CHR_COL_1.  Since
!        C           CHR_COL_1 was the first column selected, the
!        C           selection index SELIDX is set to 1.
!        C
!                    SELIDX = 1
!                    ELTIDX = 1
!                    CALL EKGC ( SELIDX,    ROW,     ELTIDX,
!             .                  CVALS(1),  ISNULL,  FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = CHR_COL_1: <Null>'
!
!                    ELSE
!
!                       WRITE(*,*) '  COLUMN = CHR_COL_1: ', CVALS(1)
!
!                    END IF
!
!        C
!        C           Fetch values from column CHR_COL_2 in the current
!        C           row.  Since CHR_COL_2 contains variable-size array
!        C           elements, we call EKNELT to determine how many
!        C           elements to fetch.
!        C
!                    SELIDX = 2
!                    CALL EKNELT ( SELIDX, ROW, NELT )
!
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  NELT   )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGC ( SELIDX,         ROW,     ELTIDX,
!             .                     CVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!        C
!        C           If the column entry is null, we'll be kicked
!        C           out of this loop after the first iteration.
!        C
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = CHR_COL_2: <Null>'
!
!                    ELSE
!
!                        WRITE(*,*) '  COLUMN = CHR_COL_2: ',
!             .                     ( CVALS(I)(:RTRIM(CVALS(I))) //' ',
!             .                                           I = 1, NELT )
!
!                    END IF
!
!        C
!        C           Fetch values from column CHR_COL_3 in the current
!        C           row.  We need not call EKNELT since we know how
!        C           many elements are in each column entry.
!        C
!                    SELIDX = 3
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  COL3SZ )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGC ( SELIDX,         ROW,     ELTIDX,
!             .                     CVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!                       WRITE(*,*) '  COLUMN = CHR_COL_3: <Null>'
!                    ELSE
!                        WRITE(*,*) '  COLUMN = CHR_COL_3: ',
!             .                     ( CVALS(I)(:RTRIM(CVALS(I))) //' ',
!             .                                         I = 1, COL3SZ )
!                    END IF
!
!                 END DO
!
!        C
!        C     We either parsed the SELECT clause or had an error.
!        C
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ROW  =   1
!           COLUMN = CHR_COL_1: Column #2 has 2 elements.
!           COLUMN = CHR_COL_2: 101 102
!           COLUMN = CHR_COL_3: 101 201 301
!
!        ROW  =   2
!           COLUMN = CHR_COL_1: Column #2 has 4 elements.
!           COLUMN = CHR_COL_2: 201 202 203 204
!           COLUMN = CHR_COL_3: 102 202 302
!
!        ROW  =   3
!           COLUMN = CHR_COL_1: Column #2 has 6 elements.
!           COLUMN = CHR_COL_2: 301 302 303 304 305 306
!           COLUMN = CHR_COL_3: 103 203 303
!
!        ROW  =   4
!           COLUMN = CHR_COL_1: Column #2 has 8 elements.
!           COLUMN = CHR_COL_2: 401 402 403 404 405 406 407 408
!           COLUMN = CHR_COL_3: 104 204 304
!
!
!        Note that after run completion, a new EK file exists in the
!        output directory.
!
!
!     3) See $Examples in EKQMGR.
!
!        In this example, the names and data types of the columns from
!        which to fetch data are not known in advance.
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code examples from existing fragments.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Redundant CHKIN call removed from SELIDX error check.
!        Misspelling of "issued" was fixed. Previous version line
!        was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch element from character column entry
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if

!
!     The row number must be valid, or we can't proceed.
!
      if (  ( row < 1 ) .or. ( row > unrows )  ) then

         call setmsg ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         call errint ( '#',  unrows                                    )
         call errint ( '#',  row                                       )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
         return

      end if

!
!     The element index must be positive.
!
      if (  elment < 1 ) then
         call setmsg ( 'ELMENT must be positive but was #.' )
         call errint ( '#',  elment                         )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return
      end if

!
!     Make sure the SELECT clause column index is valid.
!
      if (  ( selidx < 1 ) .or. ( selidx > nsel )  ) then
         call setmsg ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         call errint ( '#', selidx                                )
         call errint ( '#', ntab                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                      )
         return
      end if

!
!     COL is the column's index within the parent
!     table's column list.
!
      tabidx  =  seltab( selidx )
      col     =  selcol( selidx )
      colptr  =  selctp( selidx )
      tab     =  tptvec( tabidx )

!
!     Make sure the column has character type.
!
      if (  cttyps(colptr) /= chr ) then

         call setmsg ( 'Column # has data type #.'      )
         call errch  ( '#', ctnams( colptr )            )
         call errch  ( '#', chtype( cttyps(colptr) )    )
         call sigerr ( 'SPICE(INVALIDTYPE)'             )
         return

      end if

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      if ( dosort ) then

         call zzekjsrt ( usize,   ubase,   norder,  otabs, &
                         ocols,   oelts,   sense,   sthan, &
                         stdscs,  stdtpt,  dtpool,  dtdscs,  ordbas  )

         dosort  =  .false.
         sorted  =  .true.
      end if

!
!     Look up the segment vector and row vector for the current row.
!
      if ( sorted ) then
         call zzeksrd  ( ordbas+row,  ordbas+row,  i      )
         call zzekvcal ( i,           rwvbas,      sgvbas )
      else
         call zzekvcal ( row,         rwvbas,      sgvbas )
      end if

      call zzeksrd  ( rwvbas+1, rwvbas+ntab, rowvec )
      call zzeksrd  ( sgvbas+1, sgvbas+ntab, segvec )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      rowidx  =  rowvec(tabidx)
      seg     =  segvec(tabidx)

      j       =  stdtpt(seg)

      do i = 2, col
         j = lnknxt( j, dtpool )
      end do

!
!     Look up the element.
!
      call zzekrsc ( sthan(seg), &
                     stdscs(1,seg), &
                     dtdscs(1,j), &
                     rowidx, &
                     elment , &
                     cvlen, &
                     cdata, &
                     null, &
                     found         )



      return






!$Procedure EKGD  ( EK, get event data, double precision )

      entry ekgd ( selidx, row, elment, ddata, null, found )

! $ Abstract
!
!     Return an element of an entry in a column of double precision
!     or `time' type in a specified row.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     ASSIGNMENT
!     EK
!
! $ Declarations
!
!     INTEGER               SELIDX
!     INTEGER               ROW
!     INTEGER               ELMENT
!     DOUBLE PRECISION      DDATA
!     LOGICAL               NULL
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SELIDX     I   Index of parent column in SELECT clause.
!     ROW        I   Row to fetch from.
!     ELMENT     I   Index of element, within column entry, to fetch.
!     DDATA      O   D.p. element of column entry.
!     NULL       O   Flag indicating whether column entry was null.
!     FOUND      O   Flag indicating whether column was present in row.
!
! $ Detailed_Input
!
!     SELIDX   is the SELECT clause index of the column to
!              fetch from.
!
!     ROW      is the output row containing the entry to fetch
!              from.
!
!     ELMENT   is the index of the element of the column entry
!              to fetch. The normal range of ELMENT is from 1 to
!              the size of the column's entry, but ELMENT is
!              allowed to exceed the number of elements in the
!              column entry; if it does, FOUND is returned .FALSE.
!              This allows the caller to read data from the column
!              entry in a loop without checking the number of
!              available elements first.
!
!              Null values in variable-sized columns are
!              considered to have size 1.
!
! $ Detailed_Output
!
!     DDATA    is the requested element of the specified column
!              entry. If the entry is null, DDATA is undefined.
!
!     NULL     is a logical flag indicating whether the entry
!              belonging to the specified column in the specified
!              row is null.
!
!     FOUND    is a logical flag indicating whether the specified
!              element was found. If the element does not exist,
!              FOUND is returned .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input argument ELMENT is less than 1, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!         However, ELMENT is allowed to be greater than the number of
!         elements in the specified column entry; this allows the caller
!         to read data from the column entry in a loop without checking
!         the number of available elements first. If ELMENT is greater
!         than the number of available elements, FOUND is returned
!         .FALSE.
!
!     2)  If SELIDX is outside of the range established by the
!         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
!         is signaled and FOUND is returned .FALSE.
!
!     3)  If the input argument ROW is less than 1 or greater than the
!         number of rows matching the query, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!
!     4)  If the specified column does not have DP or TIME type, the
!         error SPICE(INVALIDTYPE) is signaled.
!
!     5)  If this routine is called when no E-kernels have been loaded,
!         the error SPICE(NOLOADEDFILES) is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine allows retrieval of data from double precision or
!     `time' columns.
!
!     This routine returns one element at a time in order to save the
!     caller from imposing a limit on the size of the column entries
!     that can be handled.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Perform a query on an EK file that contains a database with
!        the Supplementary Engineering Data Records of the Viking
!        Project in order to retrieve the PLATFORM_CLOCK values (double
!        precision) that correspond to the images with IMAGE_NUMBER
!        smaller than a given value, ordered by IMAGE_NUMBER.
!
!
!        Use the EK kernel below to load the information from the
!        original Supplementary Engineering Data Record (SEDR) data
!        set generated by the Viking Project.
!
!           vo_sedr.bdb
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGD_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY, and the maximum length of literal string
!        C     values, MAXSTR, from eklimit.inc.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              DOUBLE PRECISION      DDATA
!
!              INTEGER               ELTIDX
!              INTEGER               NMROWS
!              INTEGER               ROWNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open an EK file.
!        C
!              CALL FURNSH ( EKNAME )
!
!        C
!        C     The table 'VIKING_SEDR_DATA' has a column
!        C     'PLATFORM_CLOCK' of double precision values.
!        C
!        C     Define a set of constraints to perform a query on
!        C     all loaded EK files (the SELECT clause). In this
!        C     case select the column 'PLATFORM_CLOCK' from table
!        C     'VIKING_SEDR_DATA' sorted by 'IMAGE_NUMBER'.
!        C
!              QUERY = 'Select PLATFORM_CLOCK from VIKING_SEDR_DATA '
!             .   //   'where IMAGE_NUMBER < 25860000 '
!             .   //   'order by IMAGE_NUMBER'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!        C
!        C        Fetch the character data. We know the query returned
!        C        one column and the column contains only scalar data,
!        C        so the index of all elements is 1.
!        C
!                 SELIDX = 1
!                 ELTIDX = 1
!
!        C
!        C        Loop over each row found matching the query.
!        C
!                 DO ROWNO = 1, NMROWS
!
!
!
!        C
!        C           Use EKGD to retrieve the string from
!        C
!                    CALL EKGD ( SELIDX, ROWNO,  ELTIDX,
!             .                  DDATA,  ISNULL, FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE (*,'(A,I3,A)') 'Row ', ROWNO,
!             .                  ': Double precision data: <Null>'
!
!                    ELSE
!
!                       WRITE (*,'(A,I3,A,F10.6)') 'Row ', ROWNO,
!             .                  ': Double precision data: ', DDATA
!
!                    END IF
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Row   1: Double precision data: 119.880000
!        Row   2: Double precision data: 119.270000
!        Row   3: Double precision data: 119.880000
!        Row   4: Double precision data: 119.270000
!        Row   5: Double precision data: 119.880000
!        Row   6: Double precision data: 119.270000
!        Row   7: Double precision data: 120.140000
!        Row   8: Double precision data: 119.520000
!        Row   9: Double precision data: 120.140000
!        Row  10: Double precision data: 120.140000
!        Row  11: Double precision data: 120.140000
!        Row  12: Double precision data: 221.920000
!        Row  13: Double precision data: 221.920000
!        Row  14: Double precision data: 221.920000
!        Row  15: Double precision data: 120.140000
!        Row  16: Double precision data: 120.140000
!        Row  17: Double precision data: 120.140000
!        Row  18: Double precision data: 120.220000
!        Row  19: Double precision data: 120.220000
!        Row  20: Double precision data: 120.220000
!        Row  21: Double precision data: 120.370000
!        Row  22: Double precision data: 120.370000
!        Row  23: Double precision data: 120.370000
!        Row  24: Double precision data: 120.290000
!        Row  25: Double precision data: 120.290000
!        Row  26: Double precision data: 120.290000
!
!
!     2) Perform a query on an EK file that contains a database with
!        the Supplementary Engineering Data Records of the Viking
!        Project in order to retrieve the IMAGE_TIME values (double
!        precision time) that correspond to the images with
!        IMAGE_NUMBER smaller than a given value, ordered by
!        IMAGE_NUMBER.
!
!
!        Use the EK kernel below to load the information from the
!        original Supplementary Engineering Data Record (SEDR) data
!        set generated by the Viking Project.
!
!           vo_sedr.bdb
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGD_EX2
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY, and the maximum length of literal string
!        C     values, MAXSTR, from eklimit.inc.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
!
!              CHARACTER*(*)         LSKNAM
!              PARAMETER           ( LSKNAM = 'naif0012.tls' )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 24   )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!              CHARACTER*(TIMLEN)    UTCSTR
!
!              DOUBLE PRECISION      DDATA
!
!              INTEGER               ELTIDX
!              INTEGER               NMROWS
!              INTEGER               ROWNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Load leapseconds file for time conversion.
!        C
!              CALL FURNSH ( LSKNAM )
!
!        C
!        C     Open an EK file.
!        C
!              CALL FURNSH ( EKNAME )
!
!        C
!        C     The table 'VIKING_SEDR_DATA' has a column
!        C     'IMAGE_TIME' of time values (stored as double
!        C     precision items).
!        C
!        C     Define a set of constraints to perform a query on
!        C     all loaded EK files (the SELECT clause). In this
!        C     case select the column 'IMAGE_TIME' from table
!        C     'VIKING_SEDR_DATA' sorted by 'IMAGE_NUMBER'.
!        C
!              QUERY = 'Select IMAGE_TIME from VIKING_SEDR_DATA '
!             .   //   'where IMAGE_NUMBER < 25860000 '
!             .   //   'order by IMAGE_NUMBER'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!        C
!        C        Fetch the character data. We know the query returned
!        C        one column and the column contains only scalar data,
!        C        so the index of all elements is 1.
!        C
!                 SELIDX = 1
!                 ELTIDX = 1
!
!        C
!        C        Loop over each row found matching the query.
!        C
!                 DO ROWNO = 1, NMROWS
!
!        C
!        C           Use EKGD to retrieve the string from
!        C
!                    CALL EKGD ( SELIDX, ROWNO,  ELTIDX,
!             .                  DDATA,  ISNULL, FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE (*,'(A,I3,A)') 'Row ', ROWNO,
!             .                              ': Time data: <Null>'
!
!                    ELSE
!
!                       CALL ET2UTC ( DDATA, 'C', 3, UTCSTR )
!
!                       WRITE (*,'(A,I3,2A)') 'Row ', ROWNO,
!             .                               ': Time data:  ', UTCSTR
!
!                    END IF
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Row   1: Time data:  1976 JUN 16 16:50:55.925
!        Row   2: Time data:  1976 JUN 16 16:51:00.269
!        Row   3: Time data:  1976 JUN 16 20:56:53.051
!        Row   4: Time data:  1976 JUN 16 20:56:57.395
!        Row   5: Time data:  1976 JUN 17 01:02:50.177
!        Row   6: Time data:  1976 JUN 17 01:02:54.521
!        Row   7: Time data:  1976 JUN 17 05:08:56.263
!        Row   8: Time data:  1976 JUN 17 05:09:00.607
!        Row   9: Time data:  1976 JUN 17 06:30:28.424
!        Row  10: Time data:  1976 JUN 17 06:30:46.174
!        Row  11: Time data:  1976 JUN 17 06:30:55.168
!        Row  12: Time data:  1976 JUN 17 11:17:47.471
!        Row  13: Time data:  1976 JUN 17 11:18:05.221
!        Row  14: Time data:  1976 JUN 17 11:18:14.215
!        Row  15: Time data:  1976 JUN 17 13:20:23.634
!        Row  16: Time data:  1976 JUN 17 13:20:41.384
!        Row  17: Time data:  1976 JUN 17 13:20:50.378
!        Row  18: Time data:  1976 JUN 17 15:23:17.717
!        Row  19: Time data:  1976 JUN 17 15:23:35.467
!        Row  20: Time data:  1976 JUN 17 15:23:44.461
!        Row  21: Time data:  1976 JUN 17 17:26:20.760
!        Row  22: Time data:  1976 JUN 17 17:26:38.510
!        Row  23: Time data:  1976 JUN 17 17:26:47.504
!        Row  24: Time data:  1976 JUN 17 19:29:23.803
!        Row  25: Time data:  1976 JUN 17 19:29:41.553
!        Row  26: Time data:  1976 JUN 17 19:29:50.547
!
!
!     3) This example demonstrates how to fetch double precision values
!        from a column in three different cases: single values,
!        variable-size arrays and static-size arrays.
!
!        Create an EK that contains a table TAB that has the following
!        columns:
!
!           Column name   Data Type   Size
!           -----------   ---------   ----
!           DP_COL_1      DP          1
!           DP_COL_2      DP          VARIABLE
!           DP_COL_3      DP          3
!
!        Issue the following query
!
!            QUERY = 'SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB'
!
!        to fetch and dump column values from the rows that satisfy the
!        query.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGD_EX3
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C     and EK Query Limit Parameters (MAXQRY)
!        C
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME  = 'ekgd_ex3.bdb' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE   = 'TAB' )
!
!              INTEGER               COL3SZ
!              PARAMETER           ( COL3SZ = 3   )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               MXC2SZ
!              PARAMETER           ( MXC2SZ = 4   )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 3   )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 4   )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(MAXQRY)    QUERY
!
!              DOUBLE PRECISION      COL1
!              DOUBLE PRECISION      COL2   ( MXC2SZ )
!              DOUBLE PRECISION      COL3   ( COL3SZ )
!              DOUBLE PRECISION      DVALS  ( MXC2SZ )
!
!              INTEGER               ELTIDX
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               NELT
!              INTEGER               NMROWS
!              INTEGER               NRESVC
!              INTEGER               RECNO
!              INTEGER               ROW
!              INTEGER               SEGNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 13-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the column names and declarations
!        C     for the TAB segment.  We'll index all of
!        C     the columns.
!        C
!              CNAMES(1) = 'DP_COL_1'
!              CDECLS(1) = 'DATATYPE = DOUBLE PRECISION, ' //
!             .            'INDEXED  = TRUE'
!
!              CNAMES(2) = 'DP_COL_2'
!              CDECLS(2) = 'DATATYPE = DOUBLE PRECISION, ' //
!             .            'SIZE = VARIABLE'
!
!              CNAMES(3) = 'DP_COL_3'
!              CDECLS(3) = 'DATATYPE = DOUBLE PRECISION, ' //
!             .            'SIZE = 3'
!
!        C
!        C     Start the segment.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!        C
!        C     At the records to the table.
!        C
!              DO I = 1, NROWS
!
!        C
!        C        Append a new record to the EK.
!        C
!                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!        C
!        C        Add DP_COL_1
!        C
!                 COL1 = I * 100.D0
!
!                 CALL EKACED ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(1), 1,     COL1,  .FALSE. )
!
!        C
!        C        Add I items to DP_COL_2
!        C
!                 DO J = 1, I
!                    COL2(J) = J + I*200.D0
!                 END DO
!
!                 CALL EKACED ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(2), I,     COL2,  .FALSE. )
!
!        C
!        C        Add 3 items to DP_COL_3
!        C
!                 DO J = 1, 3
!                    COL3(J) =  I + J*100.D0
!                 END DO
!
!                 CALL EKACED ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(3), 3,     COL3, .FALSE. )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Open the created file. Perform the query and show the
!        C     results.
!        C
!              CALL FURNSH ( EKNAME )
!
!              QUERY = 'SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!                 DO ROW = 1, NMROWS
!
!                    WRITE(*,*) ' '
!                    WRITE(*,'(A,I3)') 'ROW  = ', ROW
!
!        C
!        C           Fetch values from column DP_COL_1.  Since
!        C           DP_COL_1 was the first column selected, the
!        C           selection index SELIDX is set to 1.
!        C
!                    SELIDX = 1
!                    ELTIDX = 1
!                    CALL EKGD ( SELIDX,    ROW,     ELTIDX,
!             .                  DVALS(1),  ISNULL,  FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = DP_COL_1: <Null>'
!
!                    ELSE
!
!                       WRITE(*,*) '  COLUMN = DP_COL_1:', DVALS(1)
!
!                    END IF
!
!        C
!        C           Fetch values from column DP_COL_2 in the current
!        C           row.  Since DP_COL_2 contains variable-size array
!        C           elements, we call EKNELT to determine how many
!        C           elements to fetch.
!        C
!                    SELIDX = 2
!                    CALL EKNELT ( SELIDX, ROW, NELT )
!
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  NELT   )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGD ( SELIDX,         ROW,     ELTIDX,
!             .                     DVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!        C
!        C           If the column entry is null, we'll be kicked
!        C           out of this loop after the first iteration.
!        C
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = DP_COL_2: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,4F6.1)') '   COLUMN = DP_COL_2:',
!             .                              ( DVALS(I), I = 1, NELT )
!
!                    END IF
!
!        C
!        C           Fetch values from column DP_COL_3 in the current
!        C           row.  We need not call EKNELT since we know how
!        C           many elements are in each column entry.
!        C
!                    SELIDX = 3
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  COL3SZ )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGD ( SELIDX,         ROW,     ELTIDX,
!             .                     DVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = DP_COL_3: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,3F6.1)') '   COLUMN = DP_COL_3:',
!             .                              ( DVALS(I), I = 1, COL3SZ )
!
!                    END IF
!
!                 END DO
!
!        C
!        C     We either parsed the SELECT clause or had an error.
!        C
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ROW  =   1
!           COLUMN = DP_COL_1:   100.00000000000000
!           COLUMN = DP_COL_2: 201.0
!           COLUMN = DP_COL_3: 101.0 201.0 301.0
!
!        ROW  =   2
!           COLUMN = DP_COL_1:   200.00000000000000
!           COLUMN = DP_COL_2: 401.0 402.0
!           COLUMN = DP_COL_3: 102.0 202.0 302.0
!
!        ROW  =   3
!           COLUMN = DP_COL_1:   300.00000000000000
!           COLUMN = DP_COL_2: 601.0 602.0 603.0
!           COLUMN = DP_COL_3: 103.0 203.0 303.0
!
!        ROW  =   4
!           COLUMN = DP_COL_1:   400.00000000000000
!           COLUMN = DP_COL_2: 801.0 802.0 803.0 804.0
!           COLUMN = DP_COL_3: 104.0 204.0 304.0
!
!
!        Note that after run completion, a new EK file exists in the
!        output directory.
!
!     4) See $Examples in EKQMGR.
!
!        In this example, the names and data types of the columns from
!        which to fetch data are not known in advance.
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code examples from existing fragments.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Redundant CHKIN call removed from SELIDX error check.
!        Misspelling of "issued" was fixed. Previous version line
!        was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch element from double precision column entry
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if

!
!     The row number must be valid, or we can't proceed.
!
      if (  ( row < 1 ) .or. ( row > unrows )  ) then

         call setmsg ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         call errint ( '#',  unrows                                    )
         call errint ( '#',  row                                       )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
         return

      end if

!
!     The element index must be positive.
!
      if (  elment < 1 ) then
         call setmsg ( 'ELMENT must be positive but was #.' )
         call errint ( '#',  elment                         )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return
      end if

!
!     Make sure the SELECT clause column index is valid.
!
      if (  ( selidx < 1 ) .or. ( selidx > nsel )  ) then
         call setmsg ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         call errint ( '#', selidx                                )
         call errint ( '#', ntab                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                      )
         return
      end if

!
!     COL is the column's index within the parent
!     table's column list.
!
      tabidx  =  seltab( selidx )
      col     =  selcol( selidx )
      colptr  =  selctp( selidx )
      tab     =  tptvec( tabidx )

!
!     Make sure the column has double precision or `time' type.
!
      if (       ( cttyps(colptr) /= dp   ) &
           .and. ( cttyps(colptr) /= time )  ) then

         call setmsg ( 'Column # has data type #.'    )
         call errch  ( '#', ctnams( colptr )          )
         call errch  ( '#', chtype( cttyps(colptr) )  )
         call sigerr ( 'SPICE(INVALIDTYPE)'           )
         return

      end if

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      if ( dosort ) then

         call zzekjsrt ( usize,   ubase,   norder,  otabs, &
                         ocols,   oelts,   sense,   sthan, &
                         stdscs,  stdtpt,  dtpool,  dtdscs,  ordbas  )

         dosort  =  .false.
         sorted  =  .true.
      end if

!
!     Look up the segment vector and row vector for the current row.
!
      if ( sorted ) then
         call zzeksrd  ( ordbas+row,  ordbas+row,  i      )
         call zzekvcal ( i,           rwvbas,      sgvbas )
      else
         call zzekvcal ( row,         rwvbas,      sgvbas )
      end if

      call zzeksrd  ( rwvbas+1, rwvbas+ntab, rowvec )
      call zzeksrd  ( sgvbas+1, sgvbas+ntab, segvec )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      rowidx  =  rowvec(tabidx)
      seg     =  segvec(tabidx)

      j       =  stdtpt(seg)

      do i = 2, col
         j = lnknxt( j, dtpool )
      end do

!
!     Look up the element.
!
      call zzekrsd ( sthan (   seg), &
                     stdscs(1, seg), &
                     dtdscs(1, j  ), &
                     rowidx, &
                     elment , &
                     ddata, &
                     null, &
                     found          )


      return







!$Procedure EKGI  ( EK, get event data, integer )

      entry ekgi ( selidx, row, elment, idata, null, found )

! $ Abstract
!
!     Return an element of an entry in a column of integer
!     type in a specified row.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     ASSIGNMENT
!     EK
!
! $ Declarations
!
!     INTEGER               SELIDX
!     INTEGER               ROW
!     INTEGER               ELMENT
!     INTEGER               IDATA
!     LOGICAL               NULL
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SELIDX     I   Index of parent column in SELECT clause.
!     ROW        I   Row to fetch from.
!     ELMENT     I   Index of element, within column entry, to fetch.
!     IDATA      O   Integer element of column entry.
!     NULL       O   Flag indicating whether column entry was null.
!     FOUND      O   Flag indicating whether column was present in row.
!
! $ Detailed_Input
!
!     SELIDX   is the SELECT clause index of the column to
!              fetch from.
!
!     ROW      is the output row containing the entry to fetch
!              from.
!
!     ELMENT   is the index of the element of the column entry
!              to fetch. The normal range of ELMENT is from 1 to
!              the size of the column's entry, but ELMENT is
!              allowed to exceed the number of elements in the
!              column entry; if it does, FOUND is returned .FALSE.
!              This allows the caller to read data from the column
!              entry in a loop without checking the number of
!              available elements first.
!
!              Null values in variable-sized columns are
!              considered to have size 1.
!
! $ Detailed_Output
!
!     IDATA    is the requested element of the specified column
!              entry. If the entry is null, IDATA is undefined.
!
!     NULL     is a logical flag indicating whether the entry
!              belonging to the specified column in the specified
!              row is null.
!
!     FOUND    is a logical flag indicating whether the specified
!              element was found. If the element does not exist,
!              FOUND is returned .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input argument ELMENT is less than 1, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!         However, ELMENT is allowed to be greater than the number of
!         elements in the specified column entry; this allows the caller
!         to read data from the column entry in a loop without checking
!         the number of available elements first. If ELMENT is greater
!         than the number of available elements, FOUND is returned
!         .FALSE.
!
!     2)  If SELIDX is outside of the range established by the
!         last query passed to EKSRCH, the error SPICE(INVALIDINDEX)
!         is signaled and FOUND is returned .FALSE.
!
!     3)  If the input argument ROW is less than 1 or greater than the
!         number of rows matching the query, the error
!         SPICE(INVALIDINDEX) is signaled and FOUND is returned .FALSE.
!
!     4)  If the specified column does not have integer type, the
!         error SPICE(INVALIDTYPE) is signaled.
!
!     5)  If this routine is called when no E-kernels have been loaded,
!         the error SPICE(NOLOADEDFILES) is signaled.
!
! $ Files
!
!     See the header of EKQMGR for a description of files used
!     by this routine.
!
! $ Particulars
!
!     This routine allows retrieval of data from integer columns.
!
!     This routine returns one element at a time in order to save the
!     caller from imposing a limit on the size of the column entries
!     that can be handled.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Perform a query on an EK file that contains a database with
!        the Supplementary Engineering Data Records of the Viking
!        Project in order to retrieve the IMAGE_NUMBER values (integer
!        numbers) that correspond to the images with IMAGE_NUMBER
!        smaller than a given value, ordered by IMAGE_NUMBER.
!
!
!        Use the EK kernel below to load the information from the
!        original Supplementary Engineering Data Record (SEDR) data
!        set generated by the Viking Project.
!
!           vo_sedr.bdb
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGI_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Maximum length of an input query,
!        C     MAXQRY, and the maximum length of literal string
!        C     values, MAXSTR, from eklimit.inc.
!        C
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME = 'vo_sedr.bdb' )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               ELTIDX
!              INTEGER               IDATA
!              INTEGER               NMROWS
!              INTEGER               ROWNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open an EK file.
!        C
!              CALL FURNSH ( EKNAME )
!
!        C
!        C     The table 'VIKING_SEDR_DATA' has a column
!        C     'IMAGE_NUMBER' of double precision values.
!        C
!        C     Define a set of constraints to perform a query on
!        C     all loaded EK files (the SELECT clause). In this
!        C     case select the column 'IMAGE_NUMBER' from table
!        C     'VIKING_SEDR_DATA' sorted by 'IMAGE_NUMBER'.
!        C
!              QUERY = 'Select IMAGE_NUMBER from VIKING_SEDR_DATA '
!             .   //   'where IMAGE_NUMBER < 25860000 '
!             .   //   'order by IMAGE_NUMBER'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!        C
!        C        Fetch the character data. We know the query returned
!        C        one column and the column contains only scalar data,
!        C        so the index of all elements is 1.
!        C
!                 SELIDX = 1
!                 ELTIDX = 1
!
!        C
!        C        Loop over each row found matching the query.
!        C
!                 DO ROWNO = 1, NMROWS
!
!        C
!        C           Use EKGD to retrieve the string from
!        C
!                    CALL EKGI ( SELIDX, ROWNO,  ELTIDX,
!             .                  IDATA,  ISNULL, FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE (*,'(A,I3,A)') 'Row ', ROWNO,
!             .                  ': Integer data: <Null>'
!
!                    ELSE
!
!                       WRITE (*,'(A,I3,A,I10)') 'Row ', ROWNO,
!             .                  ': Integer data: ', IDATA
!
!                    END IF
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Row   1: Integer data:   25837050
!        Row   2: Integer data:   25837051
!        Row   3: Integer data:   25840344
!        Row   4: Integer data:   25840345
!        Row   5: Integer data:   25843638
!        Row   6: Integer data:   25843639
!        Row   7: Integer data:   25846934
!        Row   8: Integer data:   25846935
!        Row   9: Integer data:   25848026
!        Row  10: Integer data:   25848030
!        Row  11: Integer data:   25848032
!        Row  12: Integer data:   25851874
!        Row  13: Integer data:   25851878
!        Row  14: Integer data:   25851880
!        Row  15: Integer data:   25853516
!        Row  16: Integer data:   25853520
!        Row  17: Integer data:   25853522
!        Row  18: Integer data:   25855162
!        Row  19: Integer data:   25855166
!        Row  20: Integer data:   25855168
!        Row  21: Integer data:   25856810
!        Row  22: Integer data:   25856814
!        Row  23: Integer data:   25856816
!        Row  24: Integer data:   25858458
!        Row  25: Integer data:   25858462
!        Row  26: Integer data:   25858464
!
!
!     2) This example demonstrates how to fetch integer values
!        from a column in three different cases: single values,
!        variable-size arrays and static-size arrays.
!
!        Create an EK that contains a table TAB that has the following
!        columns:
!
!           Column name   Data Type   Size
!           -----------   ---------   ----
!           INT_COL_1     INT         1
!           INT_COL_2     INT         VARIABLE
!           INT_COL_3     INT         3
!
!
!        Issue the following query
!
!            QUERY = 'SELECT INT_COL_1, INT_COL2, INT_COL3 FROM TAB'
!
!        to fetch and dump column values from the rows that satisfy the
!        query.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKGI_EX2
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C     and EK Query Limit Parameters (MAXQRY)
!        C
!              INCLUDE 'ekcnamsz.inc'
!              INCLUDE 'ekqlimit.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME  = 'ekgi_ex2.bdb' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE   = 'TAB' )
!
!              INTEGER               COL3SZ
!              PARAMETER           ( COL3SZ = 3   )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN = 1840 )
!
!              INTEGER               MXC2SZ
!              PARAMETER           ( MXC2SZ = 4   )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 3   )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 4   )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
!              CHARACTER*(ERRLEN)    ERRMSG
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(MAXQRY)    QUERY
!
!              INTEGER               COL1
!              INTEGER               COL2   ( MXC2SZ )
!              INTEGER               COL3   ( COL3SZ )
!              INTEGER               ELTIDX
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               IVALS  ( MXC2SZ )
!              INTEGER               J
!              INTEGER               NELT
!              INTEGER               NMROWS
!              INTEGER               NRESVC
!              INTEGER               RECNO
!              INTEGER               ROW
!              INTEGER               SEGNO
!              INTEGER               SELIDX
!
!              LOGICAL               ERROR
!              LOGICAL               FOUND
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 13-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the column names and declarations
!        C     for the TAB segment.  We'll index all of
!        C     the columns.
!        C
!              CNAMES(1) = 'INT_COL_1'
!              CDECLS(1) = 'DATATYPE = INTEGER, INDEXED  = TRUE'
!
!              CNAMES(2) = 'INT_COL_2'
!              CDECLS(2) = 'DATATYPE = INTEGER, SIZE = VARIABLE'
!
!              CNAMES(3) = 'INT_COL_3'
!              CDECLS(3) = 'DATATYPE = INTEGER, SIZE = 3'
!
!        C
!        C     Start the segment.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!        C
!        C     At the records to the table.
!        C
!              DO I = 1, NROWS
!
!        C
!        C        Append a new record to the EK.
!        C
!                 CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!        C
!        C        Add INT_COL_1
!        C
!                 COL1 = I * 100
!
!                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(1), 1,     COL1,  .FALSE. )
!
!        C
!        C        Add I items to INT_COL_2
!        C
!                 DO J = 1, I
!                    COL2(J) = J + I*200
!                 END DO
!
!                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(2), I,     COL2,  .FALSE. )
!
!        C
!        C        Add 3 items to INT_COL_3
!        C
!                 DO J = 1, 3
!                    COL3(J) =  I + J*100.D0
!                 END DO
!
!                 CALL EKACEI ( HANDLE,    SEGNO, RECNO,
!             .                 CNAMES(3), 3,     COL3, .FALSE. )
!
!              END DO
!
!        C
!        C     Close the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Open the created file. Perform the query and show the
!        C     results.
!        C
!              CALL FURNSH ( EKNAME )
!
!              QUERY = 'SELECT INT_COL_1, INT_COL_2, INT_COL_3 '
!             .   //   'FROM TAB'
!
!        C
!        C     Query the EK system for data rows matching the
!        C     SELECT constraints.
!        C
!              CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG )
!
!        C
!        C     Check whether an error occurred while processing the
!        C     SELECT clause. If so, output the error message.
!        C
!              IF ( ERROR ) THEN
!
!                 WRITE(*,*) 'SELECT clause error: ', ERRMSG
!
!              ELSE
!
!                 DO ROW = 1, NMROWS
!
!                    WRITE(*,*) ' '
!                    WRITE(*,'(A,I3)') 'ROW  = ', ROW
!
!        C
!        C           Fetch values from column INT_COL_1.  Since
!        C           INT_COL_1 was the first column selected, the
!        C           selection index SELIDX is set to 1.
!        C
!                    SELIDX = 1
!                    ELTIDX = 1
!                    CALL EKGI ( SELIDX,    ROW,     ELTIDX,
!             .                  IVALS(1),  ISNULL,  FOUND   )
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = INT_COL_1: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,I6)') '   COLUMN = INT_COL_1:',
!             .                           IVALS(1)
!
!                    END IF
!
!        C
!        C           Fetch values from column INT_COL_2 in the current
!        C           row.  Since INT_COL_2 contains variable-size array
!        C           elements, we call EKNELT to determine how many
!        C           elements to fetch.
!        C
!                    SELIDX = 2
!                    CALL EKNELT ( SELIDX, ROW, NELT )
!
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  NELT   )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGI ( SELIDX,         ROW,     ELTIDX,
!             .                     IVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!        C
!        C           If the column entry is null, we'll be kicked
!        C           out of this loop after the first iteration.
!        C
!                    END DO
!
!                    IF ( ISNULL ) THEN
!                       WRITE(*,*) '  COLUMN = INT_COL_2: <Null>'
!                    ELSE
!
!                       WRITE(*,'(A,4I6)') '   COLUMN = INT_COL_2:',
!             .                            ( IVALS(I), I = 1, NELT )
!
!                    END IF
!
!        C
!        C           Fetch values from column INT_COL_3 in the current
!        C           row.  We need not call EKNELT since we know how
!        C           many elements are in each column entry.
!        C
!                    SELIDX = 3
!                    ELTIDX = 1
!                    ISNULL = .FALSE.
!
!                    DO WHILE (       ( ELTIDX .LE.  COL3SZ )
!             .                 .AND. (        .NOT. ISNULL )  )
!
!                       CALL EKGI ( SELIDX,         ROW,     ELTIDX,
!             .                     IVALS(ELTIDX),  ISNULL,  FOUND   )
!
!                       ELTIDX = ELTIDX + 1
!
!                    END DO
!
!                    IF ( ISNULL ) THEN
!
!                       WRITE(*,*) '  COLUMN = INT_COL_3: <Null>'
!
!                    ELSE
!
!                       WRITE(*,'(A,3I6)') '   COLUMN = INT_COL_3:',
!             .                            ( IVALS(I), I = 1, COL3SZ )
!
!                    END IF
!
!                 END DO
!
!        C
!        C     We either parsed the SELECT clause or had an error.
!        C
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ROW  =   1
!           COLUMN = INT_COL_1:   100
!           COLUMN = INT_COL_2:   201
!           COLUMN = INT_COL_3:   101   201   301
!
!        ROW  =   2
!           COLUMN = INT_COL_1:   200
!           COLUMN = INT_COL_2:   401   402
!           COLUMN = INT_COL_3:   102   202   302
!
!        ROW  =   3
!           COLUMN = INT_COL_1:   300
!           COLUMN = INT_COL_2:   601   602   603
!           COLUMN = INT_COL_3:   103   203   303
!
!        ROW  =   4
!           COLUMN = INT_COL_1:   400
!           COLUMN = INT_COL_2:   801   802   803   804
!           COLUMN = INT_COL_3:   104   204   304
!
!
!        Note that after run completion, a new EK file exists in the
!        output directory.
!
!
!     3) See $Examples in EKQMGR.
!
!        In this example, the names and data types of the columns from
!        which to fetch data are not known in advance.
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
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code examples from existing fragments.
!
!-    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW)
!
!        Edited version 1.1.0 entry to not include
!        the token used to mark the $Procedure section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB)
!
!        Bug fix: When an already loaded kernel is opened with EKOPR,
!        it now has its link count reset to 1 via a call to EKCLS.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Redundant CHKIN call removed from SELIDX error check.
!        Misspelling of "issued" was fixed. Previous version line
!        was changed from "Beta" to "SPICELIB." Header $Procedure
!        line was corrected to indicate integer data type.
!
!-    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     fetch element from integer column entry
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      if ( fthead <= 0 ) then
         call setmsg ( 'No E-kernels are currently loaded.' )
         call sigerr ( 'SPICE(NOLOADEDFILES)'               )
         return
      end if

!
!     The row number must be valid, or we can't proceed.
!
      if (  ( row < 1 ) .or. ( row > unrows )  ) then

         call setmsg ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         call errint ( '#',  unrows                                    )
         call errint ( '#',  row                                       )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
         return

      end if

!
!     The element index must be positive.
!
      if (  elment < 1 ) then
         call setmsg ( 'ELMENT must be positive but was #.' )
         call errint ( '#',  elment                         )
         call sigerr ( 'SPICE(INVALIDINDEX)'                )
         return
      end if

!
!     Make sure the SELECT clause column index is valid.
!
      if (  ( selidx < 1 ) .or. ( selidx > nsel )  ) then
         call setmsg ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         call errint ( '#', selidx                                )
         call errint ( '#', ntab                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                      )
         return
      end if

!
!     COL is the column's index within the parent
!     table's column list.
!
      tabidx  =  seltab( selidx )
      col     =  selcol( selidx )
      colptr  =  selctp( selidx )
      tab     =  tptvec( tabidx )

!
!     Make sure the column has integer type.
!
      if ( cttyps(colptr) /= int ) then

         call setmsg ( 'Column # has data type #.'    )
         call errch  ( '#', ctnams( colptr )          )
         call errch  ( '#', chtype( cttyps(colptr) )  )
         call sigerr ( 'SPICE(INVALIDTYPE)'           )
         return

      end if

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      if ( dosort ) then

         call zzekjsrt ( usize,   ubase,   norder,  otabs, &
                         ocols,   oelts,   sense,   sthan, &
                         stdscs,  stdtpt,  dtpool,  dtdscs,  ordbas  )

         dosort  =  .false.
         sorted  =  .true.
      end if

!
!     Look up the segment vector and row vector for the current row.
!
      if ( sorted ) then
         call zzeksrd  ( ordbas+row,  ordbas+row,  i      )
         call zzekvcal ( i,           rwvbas,      sgvbas )
      else
         call zzekvcal ( row,         rwvbas,      sgvbas )
      end if

      call zzeksrd  ( rwvbas+1, rwvbas+ntab, rowvec )
      call zzeksrd  ( sgvbas+1, sgvbas+ntab, segvec )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      rowidx  =  rowvec(tabidx)
      seg     =  segvec(tabidx)

      j       =  stdtpt(seg)

      do i = 2, col
         j = lnknxt( j, dtpool )
      end do

!
!     Look up the element.
!
      call zzekrsi ( sthan (   seg), &
                     stdscs(1, seg), &
                     dtdscs(1, j  ), &
                     rowidx, &
                     elment , &
                     idata, &
                     null, &
                     found           )


      return
      end
