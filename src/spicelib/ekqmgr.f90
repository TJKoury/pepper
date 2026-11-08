!$Procedure EKQMGR  ( EK, query manager )

      SUBROUTINE EKQMGR (  CINDEX,  ELMENT,  EQRYC,   EQRYD,   EQRYI, &
                           FNAME,   ROW,     SELIDX,  COLUMN,  HANDLE, &
                           N,       TABLE,   ATTDSC,  CCOUNT,  FOUND, &
                           NELT,    NMROWS,  SEMERR,  ERRMSG,  CDATA, &
                           DDATA,   IDATA,   NULL                     )

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

      IMPLICIT NONE

      INCLUDE  'ekattdsc.inc'
      INCLUDE  'ekbool.inc'
      INCLUDE  'ekcnamsz.inc'
      INCLUDE  'ekcoldsc.inc'
      INCLUDE  'ekjrs.inc'
      INCLUDE  'ekopcd.inc'
      INCLUDE  'ekqlimit.inc'
      INCLUDE  'ekquery.inc'
      INCLUDE  'eksegdsc.inc'
      INCLUDE  'ektnamsz.inc'
      INCLUDE  'ektype.inc'

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE =   20 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE =  200 )

      INTEGER               MXTBLD
      PARAMETER           ( MXTBLD =  100 )

      INTEGER               MXCLLD
      PARAMETER           ( MXCLLD =  500 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )


      INTEGER               CINDEX
      INTEGER               ELMENT
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         FNAME
      INTEGER               ROW
      INTEGER               SELIDX
      CHARACTER*(*)         COLUMN
      INTEGER               HANDLE
      INTEGER               N
      CHARACTER*(*)         TABLE
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CCOUNT
      LOGICAL               FOUND
      INTEGER               NELT
      INTEGER               NMROWS
      LOGICAL               SEMERR
      CHARACTER*(*)         ERRMSG
      CHARACTER*(*)         CDATA
      DOUBLE PRECISION      DDATA
      INTEGER               IDATA
      LOGICAL               NULL

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
      INTEGER               CARDC
      INTEGER               ISRCHC

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               EKNSEG
      INTEGER               ZZEKESIZ

      LOGICAL               ZZEKRMCH
      LOGICAL               ZZEKVMCH

!
!     Linked list functions:
!
!        Find next node
!        Find tail of list
!        Return number of free nodes
!
!
      INTEGER               LNKNXT
      INTEGER               LNKTL
      INTEGER               LNKNFN


!
!     Local parameters
!

!
!     Maximum number of constraints allowed in a single query:
!
      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = STSIZE )

      INTEGER               CTSIZE
      PARAMETER           ( CTSIZE = MXCLLD )

      INTEGER               DTSIZE
      PARAMETER           ( DTSIZE = MAXSEG * MXCLLD / 10 )


!
!     Miscellaneous parameters
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               NIL
      PARAMETER           ( NIL    =  0 )

!
!     Number of data types
!
      INTEGER               NTYPES
      PARAMETER           ( NTYPES = 4 )

!
!     Length of strings used for data type names.
!
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

!
!     Length of status strings.
!
      INTEGER               SHORT
      PARAMETER           ( SHORT = 80 )


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
      INTEGER               FTPOOL ( 2,  LBPOOL : FTSIZE )
      INTEGER               FTHAN  (              FTSIZE )
      INTEGER               FTHEAD


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
      INTEGER               TBPOOL ( 2,  LBPOOL : MXTBLD )
      INTEGER               TBSTPT (              MXTBLD )
      INTEGER               TBNCOL (              MXTBLD )
      CHARACTER*(TNAMSZ)    TBNAMS (              MXTBLD )
      INTEGER               TBCTPT (              MXTBLD )
      INTEGER               TBFILS ( FTSIZE,      MXTBLD )
      INTEGER               TBFLSZ (              MXTBLD )
      INTEGER               TBHEAD


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
      INTEGER               STPOOL ( 2, LBPOOL : MAXSEG )
      INTEGER               STHAN  (             MAXSEG )
      INTEGER               STSIDX (             MAXSEG )
      INTEGER               STDSCS ( SDSCSZ,     MAXSEG )
      INTEGER               STNROW (             MAXSEG )
      INTEGER               STNCOL (             MAXSEG )
      INTEGER               STDTPT (             MAXSEG )

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
      INTEGER               DTPOOL ( 2,       LBPOOL:DTSIZE )
      INTEGER               DTDSCS ( CDSCSZ,  DTSIZE        )

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
      INTEGER               CTPOOL ( 2,       LBPOOL : CTSIZE )
      CHARACTER*(CNAMSZ)    CTNAMS (                   CTSIZE )
      INTEGER               CTCLAS (                   CTSIZE )
      INTEGER               CTTYPS (                   CTSIZE )
      INTEGER               CTLENS (                   CTSIZE )
      LOGICAL               CTFIXD (                   CTSIZE )
      INTEGER               CTSIZS (                   CTSIZE )
      LOGICAL               CTINDX (                   CTSIZE )
      LOGICAL               CTNULL (                   CTSIZE )


!
!
!     Other local variables
!
      CHARACTER*(TYPLEN)    CHTYPE ( NTYPES )
      CHARACTER*(CNAMSZ)    CNAMS  ( MXCLLD )
      CHARACTER*(CNAMSZ)    CNMSET ( LBCELL : MXCLLD )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    FRMALS ( MAXTAB )
      CHARACTER*(TNAMSZ)    FRMTAB ( MAXTAB )
      CHARACTER*(CNAMSZ)    LCNAME
      CHARACTER*(TNAMSZ)    LTNAME
      CHARACTER*(SHORT)     PROBLM
      CHARACTER*(CNAMSZ)    RCNAME
      CHARACTER*(TNAMSZ)    RTNAME
      CHARACTER*(SHORT)     STATE
      CHARACTER*(TNAMSZ)    TABNAM
      CHARACTER*(TNAMSZ)    TABVEC ( LBCELL : MAXTAB )

      DOUBLE PRECISION      DVALS  ( MAXCON )

      INTEGER               BEGIDX
      INTEGER               CBEGS  ( MAXCON )
      INTEGER               CDSCRS ( CDSCSZ,  MXCLLD )
      INTEGER               CENDS  ( MAXCON )
      INTEGER               CJBEG
      INTEGER               CJEND
      INTEGER               CJROWS
      INTEGER               CJSIZE
      INTEGER               CNSTYP ( MAXCON )
      INTEGER               COL
      INTEGER               COLPTR
      INTEGER               CONJ
      INTEGER               CVLEN
      INTEGER               CTNEW
      INTEGER               DELSEG
      INTEGER               DTNEW
      INTEGER               DTYPE  ( MAXCON )
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               IVALS  ( MAXCON )
      INTEGER               J
      INTEGER               JBASE1
      INTEGER               JBASE2
      INTEGER               JSIZE
      INTEGER               K
      INTEGER               KEY
      INTEGER               KEYDSC ( CDSCSZ )
      INTEGER               L
      INTEGER               LCIDX  ( MAXCON )
      INTEGER               LELTS  ( MAXCON )
      INTEGER               LDSCRS ( CDSCSZ, MAXCON )
      INTEGER               LTBIDX ( MAXCON )
      INTEGER               LXBEG
      INTEGER               LXEND
      INTEGER               NACT
      INTEGER               NCOLS
      INTEGER               NCONJ
      INTEGER               NEW
      INTEGER               NEXT
      INTEGER               NMATCH
      INTEGER               NORDER
      INTEGER               NPCOL
      INTEGER               NSEG
      INTEGER               NSEL
      INTEGER               NSV
      INTEGER               NTAB
      INTEGER               OCOLS  ( MAXORD )
      INTEGER               OELTS  ( MAXORD )
      INTEGER               OPS    ( MAXCON )
      INTEGER               ORDBAS
      INTEGER               OTABS  ( MAXORD )
      INTEGER               PTROFF
      INTEGER               R
      INTEGER               RBAS   ( MXJOIN )
      INTEGER               RCIDX  ( MAXCON )
      INTEGER               RDSCRS ( CDSCSZ, MAXCON )
      INTEGER               RELTS  ( MAXCON )
      INTEGER               RESBAS
      INTEGER               ROWIDX
      INTEGER               ROWVEC ( MAXTAB )
      INTEGER               RSIZE  ( MXJRS  )
      INTEGER               RTBIDX ( MAXCON )
      INTEGER               RTOTAL
      INTEGER               RWVBAS
      INTEGER               SELCOL ( MAXSEL )
      INTEGER               SELCTP ( MAXSEL )
      INTEGER               SELTAB ( MAXSEL )
      INTEGER               SEG
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SENSE  ( MAXORD )
      INTEGER               SGVBAS
      INTEGER               SIZES  ( MAXCON )
      INTEGER               STNEW
      INTEGER               T
      INTEGER               TAB
      INTEGER               TABIDX
      INTEGER               TBCURR
      INTEGER               TOP
      INTEGER               TPTVEC ( LBCELL : MAXTAB )
      INTEGER               UBASE  ( MXJRS )
      INTEGER               UNROWS
      INTEGER               USIZE

      LOGICAL               ACTIVC ( MAXCON )
      LOGICAL               ACTIVV ( MAXCON )
      LOGICAL               ATTMCH
      LOGICAL               CMTCH
      LOGICAL               DOSORT
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               INDEXD
      LOGICAL               KEYFND
      LOGICAL               NULSOK
      LOGICAL               PRESNT
      LOGICAL               SORTED
      LOGICAL               VMTCH

!
!
!     Saved variables
!
      SAVE

!
!
!     Initial values
!
      DATA                  LELTS  /  MAXCON * 1 /
      DATA                  OELTS  /  MAXORD * 1 /
      DATA                  RELTS  /  MAXCON * 1 /

      DATA                  CHTYPE / 'CHR', 'DP', 'INT', 'TIME' /

      DATA                  FTHEAD /  0       /
      DATA                  TBHEAD /  0       /

      DATA                  FIRST  / .TRUE.   /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKQMGR' )
      END IF

!
!     Never come here.
!
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )


      CALL CHKOUT ( 'EKQMGR' )
      RETURN







!$Procedure EKLEF ( EK, load event file )

      ENTRY EKLEF ( FNAME, HANDLE )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKLEF' )
      END IF

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
      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF

!
!     Open the EK file for read access.  Bail out now if this doesn't
!     work.  This retreat will protect the various tables from
!     corruption.
!
      CALL EKOPR ( FNAME, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKLEF' )
         RETURN
      END IF

!
!     Check to see whether the named EK has already been loaded.
!     If so, we've added another link to the EK, which must be
!     removed.
!
      I = FTHEAD

      DO WHILE ( I .GT. 0 )

         IF ( HANDLE .EQ. FTHAN(I) ) THEN
!
!           The last call we made to EKOPR added another link to
!           the EK file.  Remove this link.
!
            CALL DASCLS ( HANDLE )

            CALL CHKOUT ( 'EKLEF' )
            RETURN

         END IF

         I = LNKNXT ( I, FTPOOL )

      END DO

!
!     Nothing doing unless the architecture is correct.  This file
!     should be a paged DAS EK.
!
      CALL ZZEKPGCH ( HANDLE, 'READ' )

!
!     Before getting too involved with this new EK file, let's check it
!     out.  We must have enough room to accommodate it in the file
!     table, segment table, table list, and column table.
!
!     Make sure there's enough room in the file table.
!

      IF (  LNKNFN(FTPOOL) .EQ. 0 ) THEN
!
!        Sorry, there are no free file table entries left.
!
!        We close the EK AFTER setting the long error message.
!
         CALL SETMSG ( 'The EK file # could not be loaded; the '      // &
                       'maximum number of loaded EKs has already '    // &
                       'been reached.'                                 )
         CALL ERRHAN ( '#', HANDLE                                     )
         CALL EKCLS  ( HANDLE                                          )
         CALL SIGERR ( 'SPICE(EKFILETABLEFULL)'                        )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN

      END IF

!
!     Find out how many segments are in the new kernel, and make
!     sure there's enough room in the segment table.
!
      NSEG = EKNSEG ( HANDLE )

      IF ( NSEG .GT. LNKNFN(STPOOL) ) THEN
!
!        There are too many segments for the amount of space we've got
!        left.
!
!        We close the EK AFTER setting the long error message.
!
         CALL SETMSG ( 'The EK file # could not be loaded; the '      // &
                       'maximum number of loaded segments has '       // &
                       'already been reached.'                         )
         CALL ERRHAN ( '#', HANDLE                                     )
         CALL EKCLS  ( HANDLE                                          )
         CALL SIGERR ( 'SPICE(EKSEGTABLEFULL)'                         )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN

      ELSE IF ( NSEG .LT. 1 ) THEN

         CALL SETMSG ( 'The EK file # contains no segments.'           )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL EKCLS  ( HANDLE                                          )
         CALL SIGERR ( 'SPICE(EKNOSEGMENTS)'                           )
         CALL CHKOUT ( 'EKLEF'                                         )
         RETURN

      END IF


!
!     At this point, the file has insinuated itself into our confidence,
!     justified or not.  We'll attempt to load the segment and column
!     tables, and we'll update the table list if new tables are
!     introduced in this file.
!
      SEG    =   1
      STATE  =  'LOAD_FILE_TABLE'

      DO WHILE ( STATE .NE. 'DONE' )


         IF ( STATE .EQ. 'LOAD_FILE_TABLE' ) THEN
!
!           Allocate a file table entry and link the new entry in before
!           the current head of the list.  Update the list head pointer.
!           Record the file handle in the new file table entry.
!
            CALL LNKAN  ( FTPOOL, NEW            )
            CALL LNKILB ( NEW,    FTHEAD, FTPOOL )

            FTHEAD         =   NEW
            FTHAN ( NEW )  =   HANDLE

            STATE          =  'SUMMARIZE_SEGMENT'



         ELSE IF ( STATE .EQ. 'SUMMARIZE_SEGMENT' ) THEN
!
!           Get the summary information for this segment.
!
            CALL ZZEKSINF (  HANDLE,  SEG,     TABNAM, &
                             SEGDSC,  CNAMS,   CDSCRS  )

            NCOLS  =  SEGDSC(NCIDX)

!
!           Before going further, check the segment for duplicate
!           column names.  Bail out if we find any.
!
            CALL SSIZEC ( MXCLLD,        CNMSET    )
            CALL MOVEC  ( CNAMS,  NCOLS, CNMSET(1) )
            CALL VALIDC ( MXCLLD, NCOLS, CNMSET    )

            IF ( CARDC(CNMSET) .LT. NCOLS ) THEN

               STATE   =  'ABORT'
               PROBLM  =  'DUPLICATE_COLUMN_NAMES'

            ELSE

               STATE   =  'FIND_TABLE'

            END IF



         ELSE IF ( STATE .EQ. 'FIND_TABLE' ) THEN
!
!           Traverse the table list, checking for a match.
!
            TBCURR  =  TBHEAD
            PRESNT  =  .FALSE.

            DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. PRESNT ) )

               IF ( TABNAM .EQ. TBNAMS(TBCURR) ) THEN
                  PRESNT =  .TRUE.
               ELSE
                  TBCURR      =  LNKNXT ( TBCURR, TBPOOL )
               END IF

            END DO

!
!           If TABNAM is the name of a table we know about, go on to
!           fill out the segment list entry for the current segment.
!           If we didn't find TABNAM, we have a new table.  Make a table
!           list entry for it.
!
            IF ( PRESNT ) THEN
!
!              Before going further, make sure the number of columns
!              in the segment matches the number of columns in the
!              parent table.
!
               IF ( NCOLS .NE. TBNCOL(TBCURR) ) THEN

                  NPCOL  =  TBNCOL(TBCURR)
                  STATE  = 'ABORT'
                  PROBLM = 'COLUMN_NUMBER_MISMATCH'

               ELSE
!
!                 Add the current file to the list of files containing
!                 the current table.
!
                  TBFILS ( 1, TBCURR )  =  HANDLE
                  TBFLSZ (    TBCURR )  =  TBFLSZ(TBCURR) + 1

                  STATE = 'MAKE_SEGMENT_TABLE_ENTRY'

               END IF

            ELSE
!
!              This segment belongs to a new table.
!
               STATE = 'MAKE_TABLE_LIST_ENTRY'

            END IF



         ELSE IF ( STATE .EQ. 'MAKE_TABLE_LIST_ENTRY' ) THEN
!
!           Allocate a table list entry, if we can.
!

            IF ( LNKNFN(TBPOOL) .EQ. 0 ) THEN
!
!              Oops, we're out of room.
!
               STATE   =  'ABORT'
               PROBLM  =  'TABLE_LIST_FULL'

            ELSE
!
!              We have an entry; link it to the tail of the table list.
!              For consistency with the case in which the table entry
!              already exists, we'll call the table list node TBCURR.
!
!              If this is the first table in the table list, set the
!              table head pointer.
!
               CALL LNKAN  ( TBPOOL, TBCURR  )

               IF ( TBHEAD .LE. 0 ) THEN
                  TBHEAD  =  TBCURR
               ELSE
                  CALL LNKILB ( TBHEAD, TBCURR, TBPOOL )
               END IF

!
!              Fill in the table name.
!
               TBNAMS ( TBCURR )   =  TABNAM
!
!              Since this table is new, the file list for this table
!              contains only the handle of the current EK.
!
               TBFILS ( 1, TBCURR ) = HANDLE
               TBFLSZ (    TBCURR ) = 1

!
!              Initialize the column count, column table pointer, and
!              segment list pointer for this table.
!
               TBNCOL(TBCURR)   =  NCOLS
               TBCTPT(TBCURR)   =  0
               TBSTPT(TBCURR)   =  0

!
!              Go on to add a segment table entry for the current
!              segment.
!
               STATE  =  'MAKE_SEGMENT_TABLE_ENTRY'

            END IF


         ELSE IF ( STATE .EQ. 'MAKE_SEGMENT_TABLE_ENTRY' ) THEN
!
!           Add the data for the current segment to the segment
!           table.
!
!           Allocate a segment table entry.  We've already verified
!           that there's enough room.
!
            CALL LNKAN ( STPOOL, STNEW )

!
!           Link this segment table entry to the tail of the segment
!           list for the parent table, or, if the tail is NIL, just set
!           the segment list pointer to the current segment node.
!
            IF ( TBSTPT(TBCURR) .LE. 0 ) THEN

               TBSTPT(TBCURR)  =  STNEW

            ELSE

               CALL LNKILB ( TBSTPT(TBCURR), STNEW, STPOOL )

            END IF

!
!           At this point, we can fill in all elements of the segment
!           table entry except for the pointers into the column table
!           and the column base addresses.
!
            STHAN (STNEW)   =  HANDLE
            STSIDX(STNEW)   =  SEG
            STNROW(STNEW)   =  SEGDSC(NRIDX)
            STNCOL(STNEW)   =  SEGDSC(NCIDX)
            STDTPT(STNEW)   =  0

            CALL MOVEI ( SEGDSC, SDSCSZ, STDSCS(1,STNEW) )

!
!           The next step is to set up the column attributes and
!           descriptors.
!
            STATE  =  'MAKE_COLUMN_TABLE_ENTRIES'



         ELSE IF ( STATE .EQ. 'MAKE_COLUMN_TABLE_ENTRIES' ) THEN


            IF ( PRESNT ) THEN
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
               J   =   TBCTPT ( TBCURR )

               DO WHILE (       ( J     .GT.  0       ) &
                          .AND. ( STATE .NE. 'ABORT'  )   )


                  K  =  ISRCHC ( CTNAMS(J), NCOLS, CNAMS )


                  IF ( K .GT. 0 ) THEN
!
!                    We have a name match.  At this point, we must
!                    check that the column's other attributes---data
!                    type, size, and whether the column is
!                    indexed---match as well.  It's an error if they
!                    don't.
!
                     INDEXD = CDSCRS(IXTIDX,K) .NE. IFALSE
                     NULSOK = CDSCRS(NFLIDX,K) .NE. IFALSE
                     ATTMCH = &
                                ( CDSCRS(CLSIDX, K) .EQ.  CTCLAS(J) ) &
                          .AND. ( CDSCRS(TYPIDX, K) .EQ.  CTTYPS(J) ) &
                          .AND. ( CDSCRS(LENIDX, K) .EQ.  CTLENS(J) ) &
                          .AND. ( CDSCRS(SIZIDX, K) .EQ.  CTSIZS(J) ) &
                          .AND. ( INDEXD            .EQV. CTINDX(J) ) &
                          .AND. ( NULSOK            .EQV. CTNULL(J) )


                     IF ( ATTMCH ) THEN
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
                        IF ( LNKNFN( DTPOOL ) .EQ. 0 ) THEN
!
!                          No free nodes left in the descriptor table.
!
                           STATE  = 'ABORT'
                           PROBLM = 'DESCRIPTOR_TABLE_FULL'

                        ELSE
!
!                          A free node is available.  Link it in
!                          at the tail of the descriptor list for
!                          the current segment.
!
                           CALL LNKAN  ( DTPOOL, DTNEW )

                           IF ( STDTPT(STNEW) .LE. 0 ) THEN

                              STDTPT(STNEW) = DTNEW

                           ELSE

                              CALL LNKILB (STDTPT(STNEW), DTNEW, DTPOOL)

                           END IF

!
!                          Fill in the descriptor.
!
                           CALL MOVEI (  CDSCRS(1,K), &
                                         CDSCSZ, &
                                         DTDSCS(1,DTNEW)  )

                        END IF
!
!                       We filled in a descriptor table entry, or
!                       else we ran out of room.
!

                     ELSE
!
!                       Seriously bad news.  Someone's tried to
!                       load an EK containing a column with
!                       attributes that conflict with those of a
!                       loaded column of the same name in the
!                       current table.
!
                        COLNAM  =   CTNAMS(J)
                        STATE   =  'ABORT'
                        PROBLM  =  'MISMATCHED_COLUMN_ATTRIBUTES'

                     END IF


                  ELSE
!
!                    No name match; the current column from the current
!                    table is not present in the segment we're looking
!                    at.
!
                     COLNAM  =   CTNAMS(J)
                     STATE   =  'ABORT'
                     PROBLM  =  'MISSING_COLUMN'

                  END IF
!
!                 The current column matched one in the column list
!                 for the current table, or else we have a problem.
!
!                 Advance to the next column in the table's column list.
!
                  IF ( STATE .NE. 'ABORT' ) THEN
                     J  =  LNKNXT ( J, CTPOOL )
                  END IF

               END DO
!
!              We've made descriptor table entries for each column in
!              the current segment, or else we have an error.
!


            ELSE
!
!              We need to set up the column attribute entries for
!              the new table introduced by loading this segment.  We
!              also need to set up descriptor table entries for the
!              segment.  We *don't* have to check the consistency of
!              the attributes of the columns.
!
               K  =  1

               DO WHILE (  (K .LE. NCOLS) .AND. (STATE .NE. 'ABORT')  )
!
!                 Allocate a new entry in the column attribute table and
!                 link it to the tail of the column list for the
!                 current table.  If the column list is empty, update
!                 the list head.
!
                  IF (  LNKNFN(CTPOOL) .EQ. 0 ) THEN
!
!                    There's no more space to store attribute
!                    descriptors.
!
                     STATE  =  'ABORT'
                     PROBLM =  'ATTRIBUTE_TABLE_FULL'

                  ELSE

                     CALL LNKAN  ( CTPOOL, CTNEW )

                     IF  ( TBCTPT(TBCURR) .LE. 0 ) THEN

                        TBCTPT(TBCURR)  =  CTNEW

                     ELSE

                        CALL LNKILB ( TBCTPT(TBCURR), CTNEW, CTPOOL )

                     END IF

!
!                    Fill in the new column attribute entry with the
!                    attributes for this column.
!
                     CTNAMS(CTNEW)  =  CNAMS (       K)
                     CTCLAS(CTNEW)  =  CDSCRS(CLSIDX,K)
                     CTTYPS(CTNEW)  =  CDSCRS(TYPIDX,K)
                     CTLENS(CTNEW)  =  CDSCRS(LENIDX,K)
                     CTSIZS(CTNEW)  =  CDSCRS(SIZIDX,K)
                     CTINDX(CTNEW)  =  CDSCRS(IXTIDX,K) .NE. IFALSE
                     CTFIXD(CTNEW)  =  CDSCRS(SIZIDX,K) .NE. IFALSE
                     CTNULL(CTNEW)  =  CDSCRS(NFLIDX,K) .NE. IFALSE

!
!                    Store the column descriptor for this column
!                    in the descriptor table.  We'll need to
!                    allocate a descriptor table entry first.
!
                     IF ( LNKNFN( DTPOOL ) .EQ. 0 ) THEN
!
!                       No free nodes left in the descriptor table.
!
                        STATE  = 'ABORT'
                        PROBLM = 'DESCRIPTOR_TABLE_FULL'

                     ELSE
!
!                       A free node is available.  Link it in at the
!                       tail of the descriptor list for the current
!                       segment.
!
                        CALL LNKAN  ( DTPOOL, DTNEW )

                        IF ( STDTPT(STNEW) .LE. 0 ) THEN

                           STDTPT(STNEW) = DTNEW

                        ELSE

                           CALL LNKILB ( STDTPT(STNEW), DTNEW, DTPOOL )

                        END IF

!
!                       Fill in the descriptor.
!
                        CALL MOVEI (  CDSCRS(1,K), &
                                      CDSCSZ, &
                                      DTDSCS(1,DTNEW)  )

                     END IF

                  END IF
!
!                 We created attribute and descriptor entries for the
!                 current column, or we encountered an error.
!
                  IF ( STATE .NE. 'ABORT' ) THEN
!
!                    Consider the next column.
!
                     K  =  K + 1

                  END IF


               END DO
!
!              We created attribute and descriptor entries for every
!              column in the current segment, or we encountered an
!              error.
!

            END IF
!
!           We've processed the current segment in the new file, or
!           else we have an error condition.
!

            IF ( STATE .NE. 'ABORT' ) THEN
!
!              We're ready to look at the next segment in the new file.
!
               STATE  =  'NEXT_SEGMENT'

            END IF



         ELSE IF ( STATE .EQ. 'NEXT_SEGMENT' ) THEN


            IF ( SEG .LT. NSEG ) THEN

               SEG    =   SEG + 1
               STATE  =   'SUMMARIZE_SEGMENT'

            ELSE
!
!              We're done with all of the segments.
!
               STATE  =  'DONE'

            END IF


         ELSE IF ( STATE .EQ. 'ABORT' ) THEN
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

            FTHEAD  =  LNKNXT ( NEW, FTPOOL )


            IF ( FTHEAD .LT. 0 ) THEN
!
!              There are no files left.  Clean up the whole shebang.
!
               CALL LNKINI ( FTSIZE, FTPOOL )
               CALL LNKINI ( STSIZE, STPOOL )
               CALL LNKINI ( DTSIZE, DTPOOL )
               CALL LNKINI ( CTSIZE, CTPOOL )
               CALL LNKINI ( MXTBLD, TBPOOL )

               FTHEAD  =  0
               TBHEAD  =  0


            ELSE
!
!              If we arrived here, the file we're unloading is not the
!              only loaded file.
!
!              Free the file table entry for the file.  The entry can be
!              regarded as a sublist that starts and ends with the Ith
!              node, so we can call the `free sublist' routine to get
!              rid of it.
!
               CALL LNKFSL ( NEW, NEW, FTPOOL )

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
               TBCURR = TBHEAD

               DO WHILE ( TBCURR .GT. 0 )
!
!                 See whether the current table is in the file we're
!                 unloading.
!
                  I  =  1

                  DO WHILE (       ( I .LE.  TBFLSZ(TBCURR) ) &
                             .AND. (   .NOT. FND            )  )

                     IF ( TBFILS(I,TBCURR) .EQ. HANDLE ) THEN
!
!                       This table is affected by unloading the file.
!
                        FND  =  .TRUE.

                     ELSE
!
!                       Look at the next file handle.
!
                        I    =   I + 1

                     END IF

                  END DO


                  IF ( FND ) THEN
!
!                    Update the information for the current table to
!                    reflect the unloading of the specified EK.
!
!                    Unloading the specified EK removes one handle from
!                    the list of file handles associated with this
!                    table.  Compress this handle out of the list.
!
                     DO J  =  I,   TBFLSZ(TBCURR) - 1

                        TBFILS( J, TBCURR )  =  TBFILS( J+1, TBCURR )

                     END DO

                     TBFLSZ(TBCURR)  =  TBFLSZ(TBCURR) - 1

!
!                    Traverse the segment list for this table, looking
!                    for segments in the specified EK.
!
                     DELSEG  =  TBSTPT ( TBCURR )

                     DO WHILE ( DELSEG .GT. 0 )

                        IF ( STHAN(DELSEG) .EQ. HANDLE ) THEN
!
!                          This segment is aboard the sinking ship.  Put
!                          it out of its misery.
!
!                          First, euthanize its column descriptors.
!                          These descriptors are linked together, so we
!                          can free all of them in one shot.
!
                           J  =  STDTPT ( DELSEG )

                           IF ( J .GT. 0 ) THEN
                              K  =  LNKTL  ( J,    DTPOOL )
                              CALL LNKFSL  ( J, K, DTPOOL )
                           END IF

!
!                          Now we can delete the segment table entry
!                          itself.  This deletion may necessitate
!                          updating the segment list pointer in the
!                          parent table's table list entry.
!
                           IF ( DELSEG .EQ. TBSTPT(TBCURR) ) THEN

                              TBSTPT(TBCURR) =  LNKNXT( DELSEG, STPOOL )

                           END IF


                           NEXT = LNKNXT ( DELSEG,         STPOOL )
                           CALL LNKFSL   ( DELSEG, DELSEG, STPOOL )

!
!                          The segment we just freed may have been the
!                          last one belonging to this table.  We deal
!                          with this possibility later, below the end of
!                          the current loop.
!
                           DELSEG  = NEXT

                        ELSE

                           DELSEG  = LNKNXT ( DELSEG, STPOOL )

                        END IF


                     END DO

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
                     IF ( TBSTPT(TBCURR) .LE. 0 ) THEN
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
                        J  =  TBCTPT ( TBCURR )

                        IF ( J .GT. 0 ) THEN

                           K  =  LNKTL  ( J,    CTPOOL )
                           CALL LNKFSL  ( J, K, CTPOOL )

                        END IF


                        IF ( TBCURR .EQ. TBHEAD ) THEN
!
!                          The entry for this table is at the head of
!                          the table list.  Update the head of the list.
!
                           TBHEAD  =  LNKNXT ( TBCURR, TBPOOL )
                           NEXT    =  TBHEAD

                        ELSE

                           NEXT    =  LNKNXT ( TBCURR, TBPOOL )

                        END IF

!
!                       Make the entry for this table go away.
!
                        CALL LNKFSL ( TBCURR, TBCURR, TBPOOL )

!
!                       Look at the next table.
!
                        TBCURR  =  NEXT


                     ELSE
!
!                       We're done with the current table.  Look at the
!                       next one.
!
                        TBCURR = LNKNXT ( TBCURR, TBPOOL )

                     END IF
!
!                    We've cleaned up the table entry for the current
!                    table, if it was necessary to do so.
!

                  ELSE
!
!                    The current table is not affected by unloading this
!                    file.  Examine the next table.
!
                     TBCURR = LNKNXT ( TBCURR, TBPOOL )

                  END IF

!
!                 We've processed the current table.
!
               END DO


            END IF

!
!           We've cleaned up after the aborted partial load.
!
!           Now that the mess has been arranged, tell the user what the
!           problem was.
!
            IF ( PROBLM .EQ. 'TABLE_LIST_FULL' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'maximum number of distinct tables has ' // &
                             'already been reached.'                   )
               CALL ERRHAN ( '#', HANDLE                               )
               CALL SIGERR ( 'SPICE(EKTABLELISTFULL)'                  )


            ELSE IF ( PROBLM .EQ. 'DUPLICATE_COLUMN_NAMES' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'segment # contains duplicate column '   // &
                             'names in table #.'                       )
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL ERRINT ( '#',      SEG                             )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL SIGERR ( 'SPICE(EKCOLNUMMISMATCH)'                 )


            ELSE IF ( PROBLM .EQ. 'COLUMN_NUMBER_MISMATCH' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'number of columns (#) in segment # '    // &
                             'does not match the number of columns '  // &
                             '(#) in the parent table #.'              )
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL ERRINT ( '#',      NCOLS                           )
               CALL ERRINT ( '#',      SEG                             )
               CALL ERRINT ( '#',      NPCOL                           )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL SIGERR ( 'SPICE(EKCOLNUMMISMATCH)'                 )


            ELSE IF ( PROBLM .EQ. 'MISMATCHED_COLUMN_ATTRIBUTES' ) THEN

               CALL SETMSG ( 'EK file # contains a column '           // &
                             'whose attributes conflict with a '      // &
                             'loaded column.  The offending '         // &
                             'column name is #; the column is '       // &
                             'in segment #* of the file.'              )
               CALL ERRHAN ( '#',  HANDLE                              )
               CALL ERRCH  ( '#',  COLNAM                              )
               CALL ERRINT ( '*',  SEG                                 )
               CALL SIGERR ( 'SPICE(BADATTRIBUTES)'                    )


            ELSE IF ( PROBLM .EQ. 'DESCRIPTOR_TABLE_FULL' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the' // &
                              'maximum allowed number of loaded '     // &
                              'columns already been reached.'          )
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL SIGERR ( 'SPICE(COLDESCTABLEFULL)'                 )


            ELSE IF ( PROBLM .EQ. 'ATTRIBUTE_TABLE_FULL' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'maximum number of columns having'       // &
                             'distinct attributes has already been '  // &
                             'reached.'                                )
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL SIGERR ( 'SPICE(EKCOLATTRTABLEFULL)'               )


            ELSE IF ( PROBLM .EQ. 'MISSING_COLUMN' ) THEN

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'column # in already loaded table # is ' // &
                             'not present in segment # in the EK file.')
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL ERRCH  ( '#',      COLNAM                          )
               CALL ERRCH  ( '#',      TABNAM                          )
               CALL ERRINT ( '#',      SEG                             )
               CALL SIGERR ( 'SPICE(EKMISSINGCOLUMN)'                  )


            ELSE

               CALL SETMSG ( 'The EK file # could not be loaded; the '// &
                             'problem "#" occurred while attempting ' // &
                             'to load the file.  By way, there is a ' // &
                             'bug in EKLEF if you see this message.'   )
               CALL ERRHAN ( '#',      HANDLE                          )
               CALL ERRCH  ( '#',      PROBLM                          )
               CALL SIGERR ( 'SPICE(BUG)'                              )

            END IF


            CALL EKCLS  (  HANDLE )

            CALL CHKOUT ( 'EKLEF' )
            RETURN

         END IF


      END DO
!
!     At this point, we've made the file table, table list, segment
!     table, column descriptor table, and column attribute table updates
!     necessary to reflect the presence of the new file.
!

      CALL CHKOUT ( 'EKLEF' )
      RETURN








!$Procedure EKUEF  ( EK, unload event file )

      ENTRY EKUEF ( HANDLE )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKUEF' )
      END IF

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF

!
!     Check to see whether the named EK has been loaded.  Do nothing
!     if not.
!
      I    =   FTHEAD
      FND  =  .FALSE.

      DO WHILE (  ( I .GT. 0 ) .AND. ( .NOT. FND )  )

         IF ( HANDLE .EQ. FTHAN(I) ) THEN
            FND  =  .TRUE.
         ELSE
            I    =  LNKNXT ( I, FTPOOL )
         END IF

      END DO

      IF ( .NOT. FND ) THEN
         CALL CHKOUT ( 'EKUEF' )
         RETURN
      END IF

!
!     If we got to here, HANDLE points to a loaded EK file.  It's
!     time to wipe from the EK tables all trivial fond records
!     pertaining to the file in question.
!
!     The file table is first.
!
      IF ( I .EQ. FTHEAD ) THEN
!
!        The file is at the head of the list.  If the file has a
!        successor, that file is now at the head of the list.
!
         FTHEAD  =  LNKNXT ( I, FTPOOL )

         IF ( FTHEAD .LT. 0 ) THEN
!
!           There are no files left.  Clean up the whole shebang.
!
            CALL LNKINI ( FTSIZE, FTPOOL )
            CALL LNKINI ( STSIZE, STPOOL )
            CALL LNKINI ( DTSIZE, DTPOOL )
            CALL LNKINI ( CTSIZE, CTPOOL )
            CALL LNKINI ( MXTBLD, TBPOOL )

            FTHEAD  =  0
            TBHEAD  =  0

!
!           Close the EK file, to keep the DAS system's bookkeeping
!           up to date.
!
            CALL EKCLS  ( HANDLE )

            CALL CHKOUT ( 'EKUEF' )
            RETURN

         END IF

      END IF

!
!     If we arrived here, the file we're unloading is not the only
!     loaded file.
!
!     Free the file table entry for the file.  The entry can be
!     regarded as a sublist that starts and ends with the Ith node,
!     so we can call the `free sublist' routine to get rid of it.
!
      CALL LNKFSL ( I, I, FTPOOL )

!
!     It's time to clean up the table list, segment table, column
!     attribute table, and column descriptor table.  The plan is
!     to traverse the table list, and for each member of the list,
!     traverse the corresponding segment list, removing from the list
!     information about segments and columns in the file we're
!     unloading.  If the segment list for any table becomes empty, we
!     remove the entry for that table from the table list.
!
      TBCURR = TBHEAD

      DO WHILE ( TBCURR .GT. 0 )
!
!        See whether the current table is in the file we're unloading.
!
            I  =  1

            DO WHILE (  ( I .LE. TBFLSZ(TBCURR) ) .AND. ( .NOT. FND )  )

            IF ( TBFILS(I,TBCURR) .EQ. HANDLE ) THEN
!
!              This table is affected by unloading the file.
!
               FND  =  .TRUE.

            ELSE
!
!              Look at the next file handle.
!
               I    =   I + 1

            END IF

         END DO


         IF ( FND ) THEN
!
!           Update the information for the current table to reflect
!           the unloading of the specified EK.
!
!           Unloading the specified EK removes one handle from the
!           list of file handles associated with this table.  Compress
!           this handle out of the list.
!
            DO J  =  I,   TBFLSZ(TBCURR) - 1

               TBFILS( J, TBCURR )  =  TBFILS( J+1, TBCURR )

            END DO

            TBFLSZ(TBCURR)  =  TBFLSZ(TBCURR) - 1

!
!           Traverse the segment list for this table, looking
!           for segments in the specified EK.
!
            SEG  =  TBSTPT ( TBCURR )

            DO WHILE ( SEG .GT. 0 )

               IF ( STHAN(SEG) .EQ. HANDLE ) THEN
!
!                 This segment is aboard the sinking ship.  Put it
!                 out of its misery.
!
!                 First, euthanize the segment's column descriptors.
!                 These descriptors are linked together, so we can free
!                 all of them in one shot.  Don't crash if the column
!                 descriptor list is empty.
!
                  J  =  STDTPT ( SEG )

                  IF ( J .GT. 0 ) THEN
                     K  =  LNKTL  ( J,    DTPOOL )
                     CALL LNKFSL  ( J, K, DTPOOL )
                  END IF

!
!                 Now we can delete the segment table entry itself.
!                 This deletion may necessitate updating the segment
!                 list pointer in the parent table's table list entry.
!
                  IF ( SEG .EQ. TBSTPT(TBCURR) ) THEN

                     TBSTPT(TBCURR)  =  LNKNXT( SEG, STPOOL )

                  END IF


                  NEXT = LNKNXT ( SEG,      STPOOL )
                  CALL LNKFSL   ( SEG, SEG, STPOOL )

                  SEG  = NEXT

               ELSE

                  SEG  = LNKNXT ( SEG, STPOOL )

               END IF


            END DO

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
            IF ( TBSTPT(TBCURR) .LE. 0 ) THEN
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
               J  =  TBCTPT ( TBCURR )

               IF ( J .GT. 0 ) THEN
                  K  =  LNKTL  ( J,    CTPOOL )
                  CALL LNKFSL  ( J, K, CTPOOL )
               END IF


               IF ( TBCURR .EQ. TBHEAD ) THEN
!
!                 The entry for this table is at the head of the
!                 table list.  Update the head of the list.
!
                  TBHEAD  =  LNKNXT ( TBCURR, TBPOOL )
                  NEXT    =  TBHEAD

               ELSE

                  NEXT    =  LNKNXT ( TBCURR, TBPOOL )

               END IF

!
!              Make the entry for this table go away.
!
               CALL LNKFSL ( TBCURR, TBCURR, TBPOOL )

!
!              The successor of the current node is the next node to
!              examine.
!
               TBCURR  =  NEXT


            ELSE
!
!              We're done with the current table.  Look at the next one.
!
               TBCURR = LNKNXT ( TBCURR, TBPOOL )

            END IF
!
!           We've cleaned up the table entry for the current table,
!           if it was necessary to do so.
!

         ELSE
!
!           The current table is not affected by unloading this file.
!           Examine the next table.
!
            TBCURR = LNKNXT ( TBCURR, TBPOOL )

         END IF
!
!        We've processed the current table.
!
      END DO

!
!     Don't forget to unload the EK file from the DAS system.
!
      CALL EKCLS  ( HANDLE )

      CALL CHKOUT ( 'EKUEF' )
      RETURN







!$Procedure EKNTAB  ( EK, return number of loaded tables )

      ENTRY EKNTAB ( N )

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


      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF
!
!     Return the number of loaded tables.
!
      N  =   MXTBLD - LNKNFN( TBPOOL )
      RETURN






!$Procedure EKTNAM  ( EK, return name of loaded table )

      ENTRY EKTNAM ( N, TABLE )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKTNAM' )
      END IF

      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKTNAM'                             )
         RETURN
      END IF


      TBCURR  =  TBHEAD
      FND     =  .FALSE.
      I       =  0

      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )

         I   =  I + 1

         IF ( I .EQ. N ) THEN
            FND     =  .TRUE.
            TABLE   =   TBNAMS(TBCURR)
         ELSE
            TBCURR  =   LNKNXT ( TBCURR, TBPOOL )
         END IF

      END DO

      IF ( .NOT. FND ) THEN
         CALL SETMSG ( 'The index # does not correspond to a loaded ' // &
                       'table.'                                        )
         CALL ERRINT ( '#',  N                                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
      END IF


      CALL CHKOUT ( 'EKTNAM' )
      RETURN









!$Procedure EKCCNT ( EK, column count )

      ENTRY EKCCNT ( TABLE, CCOUNT )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKCCNT' )
      END IF

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF


!
!     Find the table.  If there's no match, the number of loaded columns
!     is zero.
!
      TBCURR   =  TBHEAD
      FND      =  .FALSE.

      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )

         IF (  EQSTR( TABLE, TBNAMS(TBCURR) )  ) THEN
            FND     =  .TRUE.
         ELSE
            TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
         END IF

      END DO


      IF ( .NOT. FND ) THEN

         CCOUNT = 0
         CALL SETMSG ( 'The table # is not currently loaded.' )
         CALL ERRCH  ( '#',  TABLE                            )
         CALL SIGERR ( 'SPICE(TABLENOTLOADED)'                )
         CALL CHKOUT ( 'EKCCNT'                               )
         RETURN

      ELSE
!
!        Count the columns in the attribute table for the current table.
!
         CCOUNT  =  0
         COL     =  TBCTPT ( TBCURR )

         DO WHILE ( COL .GT. 0 )
            CCOUNT  =  CCOUNT + 1
            COL     =  LNKNXT ( COL, CTPOOL )
         END DO

      END IF

      CALL CHKOUT ( 'EKCCNT' )
      RETURN







!$Procedure EKCII  ( EK, column info by index )

      ENTRY EKCII ( TABLE, CINDEX,  COLUMN,  ATTDSC )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKCII' )
      END IF

!
!     On the first pass through this routine, initialize the tables,
!     if it hasn't been done yet.
!
      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF


!
!     Find the table.  If there's no match, the number of loaded columns
!     is zero.
!
      TBCURR   =  TBHEAD
      FND      =  .FALSE.

      DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND )  )

         IF (  EQSTR( TABLE, TBNAMS(TBCURR) )  ) THEN
            FND     =  .TRUE.
         ELSE
            TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
         END IF

      END DO


      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The table # is not currently loaded.' )
         CALL ERRCH  ( '#',  TABLE                            )
         CALL SIGERR ( 'SPICE(TABLENOTLOADED)'                )
         CALL CHKOUT ( 'EKCII'                                )
         RETURN

      END IF


!
!     Locate the named column in the column attribute table.
!
      I    =  0
      COL  =  TBCTPT ( TBCURR )

      DO WHILE (  ( COL .GT. 0 )  .AND. ( I .LT. CINDEX )  )

         I  =  I + 1

         IF ( I .EQ. CINDEX ) THEN
!
!           We've found the column.  Set the output arguments using
!           its attributes.
!
            COLUMN    = CTNAMS(COL)

            ATTDSC(1) = CTCLAS(COL)
            ATTDSC(2) = CTTYPS(COL)
            ATTDSC(3) = CTLENS(COL)
            ATTDSC(4) = CTSIZS(COL)

            IF ( CTINDX(COL) ) THEN
               ATTDSC(5) = ITRUE
            ELSE
               ATTDSC(5) = IFALSE
            END IF

            IF ( CTNULL(COL) ) THEN
               ATTDSC(6) = ITRUE
            ELSE
               ATTDSC(6) = IFALSE
            END IF

            CALL CHKOUT ( 'EKCII' )
            RETURN
         ELSE
            COL = LNKNXT ( COL, CTPOOL )
         END IF

      END DO

!
!     We end up here if we ran out of columns before finding the
!     CINDEXth one, or if CINDEX was non-positive.
!
      CALL SETMSG ( 'Column indices for table # range from # to #; ' // &
                    'requested index was #.'                           )
      CALL ERRCH  ( '#',  TABNAM                                       )
      CALL ERRINT ( '#',  MAX( 1, I )                                  )
      CALL ERRINT ( '#',  I                                            )
      CALL ERRINT ( '#',  CINDEX                                       )
      CALL SIGERR ( 'SPICE(INVALIDINDEX)'                              )

      CALL CHKOUT ( 'EKCII' )
      RETURN












!$Procedure EKSRCH ( EK, search for events )

      ENTRY EKSRCH ( EQRYI, EQRYC, EQRYD, NMROWS, SEMERR, ERRMSG )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKSRCH' )
      END IF

!
!     There nothing to search if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKSRCH'                             )
         RETURN
      END IF

!
!     No error to begin with.
!
      SEMERR  =  .FALSE.
      ERRMSG  =   ' '
      NMROWS  =   0


      IF ( FIRST ) THEN
!
!        Initialize the file table pool, segment table pool, column
!        descriptor pool, column table pool, and table list pool.
!
         CALL LNKINI ( FTSIZE, FTPOOL )
         CALL LNKINI ( STSIZE, STPOOL )
         CALL LNKINI ( DTSIZE, DTPOOL )
         CALL LNKINI ( CTSIZE, CTPOOL )
         CALL LNKINI ( MXTBLD, TBPOOL )

         FTHEAD = 0
         TBHEAD = 0

         FIRST  = .FALSE.

      END IF

!
!     Read some of our favorite things from the query.  We need:
!
!        - the table count
!        - the SELECT clause column count
!        - the order-by column count
!        - the table and alias list
!
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB   )
      CALL ZZEKREQI ( EQRYI, 'NUM_SELECT_COLS',   NSEL   )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS',  NORDER )

      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, FRMTAB(I), FRMALS(I) )
      END DO

!
!     Initialize the table vectors.  Also initialize a vector of column
!     list pointers.
!
      CALL SSIZEC ( MAXTAB, TABVEC )
      CALL SSIZEI ( MAXTAB, TPTVEC )

!
!     Fill in the FROM table vector and corresponding column pointer
!     vector.  It's an error if a table referenced in the FROM clause
!     can't be found.
!
      DO I = 1, NTAB
!
!        Find the table list entry for this table name.
!
         TBCURR  =  TBHEAD
         FND     = .FALSE.

         DO WHILE (  ( TBCURR .GT. 0 ) .AND. ( .NOT. FND ) )

            IF ( TBNAMS(TBCURR) .EQ. FRMTAB(I) ) THEN
!
!              We've found the table list entry for the Ith table.
!
               CALL APPNDC ( FRMTAB(I), TABVEC )
               CALL APPNDI ( TBCURR,    TPTVEC )
               FND     = .TRUE.
            ELSE
               TBCURR  =  LNKNXT ( TBCURR, TBPOOL )
            END IF

         END DO

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( 'The table # is not currently loaded.' )
            CALL ERRCH  ( '#',   FRMTAB(I)                       )
            CALL SIGERR ( 'SPICE(INVALIDTABLENAME)'              )
            CALL CHKOUT ( 'EKSRCH'                               )
            RETURN
         END IF

      END DO

!
!     Since this is a new search, re-initialize the stack in the EK
!     scratch area.  Also initialize our total segment list count.
!
      CALL ZZEKSTOP ( TOP )
      CALL ZZEKSDEC ( TOP )

!
!     Initialize the size of the join row set union for the current
!     query.  At this point, no matching rows have been found.
!
      USIZE  =  0
      UNROWS =  0

!
!     Get the number of conjunctions and the sizes of the conjunctions.
!
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',  NCONJ  )
      CALL CLEARI   ( MAXCON, SIZES )

      DO I = 1, NCONJ
         CALL ZZEKQCNJ ( EQRYI, I, SIZES(I) )
      END DO

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
      CJEND = 0

      DO CONJ  =  1,  MAX( 1, NCONJ )
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
         IF ( NCONJ .EQ. 0 ) THEN
            CJSIZE  =  0
         ELSE
            CJSIZE  =  SIZES(CONJ)
         END IF

         CJBEG  =  CJEND + 1
         CJEND  =  CJEND + CJSIZE

         DO I  =  1, CJSIZE

            CALL ZZEKQCON ( EQRYI,     EQRYC,      EQRYD,     CJBEG+I-1, &
                            CNSTYP(I), &
                            LTNAME,    LTBIDX(I),  LCNAME,    LCIDX(I), &
                            OPS(I), &
                            RTNAME,    RTBIDX(I),  RCNAME,    RCIDX(I), &
                            DTYPE(I), &
                            CBEGS(I),  CENDS(I),   DVALS(I),  IVALS(I) )
         END DO


         DO T = 1, NTAB
!
!           We will build a trivial (one-table) join row set for the
!           current table.
!
!           Initialize the join row set.  Retain the base address.  We
!           can fill in the table count right away; the count is 1.
!
            CALL ZZEKSTOP ( RBAS(T) )

            DO I = 1, JSCIDX
               CALL ZZEKSPSH ( 1, 0 )
            END DO

            CALL ZZEKSUPD ( RBAS(T)+JTCIDX, RBAS(T)+JTCIDX, 1 )

!
!           Count the loaded segments for the current table.  We'll
!           leave enough room in the join row set for each segment.
!
            TAB = TPTVEC(T)
            I   = TBSTPT(TAB)
            NSV = 0

            DO WHILE ( I .GT. 0 )

               CALL ZZEKSPSH ( 1, 0 )

               NSV  =  NSV + 1
               I    =  LNKNXT ( I, STPOOL )

            END DO

!
!           Save room for the row vector base addresses and counts.
!
            DO I = 1, 2*NSV
               CALL ZZEKSPSH ( 1, 0 )
            END DO

!
!           At this point, we can set the segment vector count in the
!           join row set.
!
            CALL ZZEKSUPD ( RBAS(T)+JSCIDX, RBAS(T)+JSCIDX, NSV )

!
!           Find the matching rows in the segments belonging to the
!           current table.
!
            SEG     =  TBSTPT(TAB)
            NSEG    =  0
            RTOTAL  =  0

            DO WHILE ( SEG .GT. 0 )

               NSEG = NSEG + 1
!
!              The segment vector for this segment is trivial:  it's
!              just the segment's index in the segment table.
!
               SGVBAS  =  RBAS(T) + JSVBAS + ( NSEG - 1 )

               CALL ZZEKSUPD ( SGVBAS+1, SGVBAS+1, SEG )

!
!              Label as `inactive' any constraints that don't apply to
!              this table.  Join constraints are inactive at this stage
!              of the game.  Label all other constraints `active'.
!              We'll keep track of column and value constraints
!              separately.
!
               DO I = 1, CJSIZE
!
!                 Each constraint is active to start with.
!
                  ACTIVC(I) = CNSTYP(I) .EQ. EQCOL
                  ACTIVV(I) = CNSTYP(I) .EQ. EQVAL

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
                  IF ( LTBIDX(I) .NE. T ) THEN

                     ACTIVC(I) = .FALSE.
                     ACTIVV(I) = .FALSE.


                  ELSE IF ( CNSTYP(I) .EQ. EQCOL ) THEN

                     IF ( LTBIDX(I) .NE. RTBIDX(I) ) THEN
!
!                       This is a join constraint; disable it.
!
                        ACTIVC(I) = .FALSE.

                     END IF

                  END IF


               END DO



!
!              At this point, we'll have to search the segment for
!              matching rows.  Pick a key column for the segment.  To
!              do this, we'll need to pack an array with column
!              descriptors for each active constraint.  The
!              descriptor for the column on the left side of the Ith
!              constraint will be placed in elements LDSCRS(*,I), if
!              the Ith constraint is active.
!
               CALL CLEARI ( CDSCSZ*MAXCON, LDSCRS )

               DO I = 1, CJSIZE

                  IF ( ACTIVV(I) ) THEN
!
!                     Look up the column descriptor for this
!                     constraint.
!
                     J  =  STDTPT(SEG)

                     DO K = 2, LCIDX(I)
                        J  = LNKNXT(J,DTPOOL)
                     END DO

                     CALL MOVEI ( DTDSCS(1,J), CDSCSZ, LDSCRS(1,I) )

                  END IF

               END DO


               CALL ZZEKKEY ( STHAN (SEG),     STDSCS(1,SEG), &
                              STNROW(SEG),     CJSIZE, &
                              LCIDX,           LDSCRS, &
                              OPS,             DTYPE, &
                              EQRYC,           CBEGS, &
                              CENDS,           DVALS, &
                              IVALS,           ACTIVV, &
                              KEY,             KEYDSC, &
                              BEGIDX,          ENDIDX, &
                              KEYFND                        )

!
!              ZZEKKEY has updated ACTIVV to reflect the application
!              of constraints that were used to determine BEGIDX and
!              ENDIDX.
!
               IF ( KEYFND ) THEN

                  INDEXD  =  .TRUE.

               ELSE
!
!                 A key column could not be determined from the
!                 active constraints.  We'll use the first column of
!                 the segment as the key column.
!
                  INDEXD  =  .FALSE.
                  BEGIDX  =   1
                  ENDIDX  =   STNROW(SEG)

               END IF

!
!              Whether or not we have any matching rows, we'll need
!              to record how many we have.  Save the offset from the
!              join row set base for the pointer to the row vectors.
!              The row vector count follows this pointer.
!
               PTROFF = JSVBAS  +  NSV  +  (NSEG - 1) * 2   +   1


               IF ( ENDIDX .GE. BEGIDX ) THEN
!
!                 Initialize the count of matching rows for this
!                 segment.  The current stack top is the base address
!                 for the row vectors; save the offset of this
!                 address from the join row set's base.
!                 Also compute the base address of the segment vector
!                 for the current segment.
!
                  NMATCH = 0
                  CALL ZZEKSTOP ( RWVBAS )

                  CALL ZZEKSUPD ( RBAS(T)+PTROFF, &
                                  RBAS(T)+PTROFF,   RWVBAS-RBAS(T) )

!
!                 Count the active constraints.  While we're at it,
!                 fill in the descriptor lists LDSCRS and RDSCRS
!                 with, respectively, the descriptors for the columns
!                 on the left hand sides and right hand sides of
!                 these constraints.
!
                  CALL CLEARI ( CDSCSZ*MAXCON, LDSCRS )
                  CALL CLEARI ( CDSCSZ*MAXCON, RDSCRS )
                  NACT = 0

                  DO I = 1, CJSIZE

                     IF ( ACTIVC(I) .OR. ACTIVV(I) ) THEN

                        NACT = NACT + 1
!
!                       Look up the column descriptor for this
!                       constraint.

                        J  =  STDTPT(SEG)

                        DO K = 2, LCIDX(I)
                           J  = LNKNXT(J,DTPOOL)
                        END DO

                        CALL MOVEI(DTDSCS(1,J), CDSCSZ, LDSCRS(1,I) )


                        J  =  STDTPT(SEG)

                        DO K = 2, RCIDX(I)
                           J  = LNKNXT(J,DTPOOL)
                        END DO

                        CALL MOVEI(DTDSCS(1,J), CDSCSZ, RDSCRS(1,I) )

                     END IF

                  END DO


                  IF ( NACT .GT. 0 ) THEN
!
!                    There are still active constraints left, so
!                    proceed linearly through the remaining rows,
!                    testing each one against these constraints. Add
!                    matching rows to the current join row set.
!
                     DO R = BEGIDX, ENDIDX


                        IF ( INDEXD ) THEN

                           CALL ZZEKIXLK ( STHAN(SEG), &
                                           KEYDSC, &
                                           R, &
                                           ROWIDX )
                        ELSE
!
!                          Look up the record pointer for row R.
!
                           CALL ZZEKRPLK ( STHAN(SEG), &
                                           STDSCS(1,SEG), &
                                           R, &
                                           ROWIDX  )
                        END IF

!
!                       Test the row against both value and column
!                       constraints.  For now, we supply an array
!                       of default column entry element indices.
!
                        VMTCH = ZZEKRMCH ( CJSIZE,     ACTIVV, &
                                           STHAN(SEG), STDSCS(1,SEG), &
                                           LDSCRS,     ROWIDX, &
                                           LELTS,      OPS, &
                                           DTYPE,      EQRYC, &
                                           CBEGS,      CENDS, &
                                           DVALS,      IVALS     )

                        CMTCH = .TRUE.

!
!                       Note that ZZEKVMCH expects a set of inputs
!                       that are not really parallel to those
!                       expected by ZZEKRMCH.  We feed the
!                       column comparison constraints to ZZEKVMCH
!                       one at a time.
!
                        DO J = 1, CJSIZE

                           CMTCH =       CMTCH &
                                   .AND. ZZEKVMCH ( 1, &
                                                    ACTIVC(J), &
                                                    STHAN(SEG), &
                                                    STDSCS(1,SEG), &
                                                    LDSCRS(1,J), &
                                                    ROWIDX, &
                                                    1, &
                                                    OPS(J), &
                                                    STHAN(SEG), &
                                                    STDSCS(1,SEG), &
                                                    RDSCRS(1,J), &
                                                    ROWIDX, &
                                                    1            )
                        END DO


                        IF ( CMTCH .AND. VMTCH ) THEN
!
!                          Push the `augmented row vector' for the
!                          current row onto the stack.  In this case,
!                          of course, the augmented row vector is
!                          trivial:  it consists of the row number,
!                          followed by the base address of the parent
!                          segment vector.
!
                           NMATCH = NMATCH + 1

                           CALL ZZEKSPSH ( 1, ROWIDX         )
                           CALL ZZEKSPSH ( 1, SGVBAS-RBAS(T) )

                        END IF

                     END DO


                  ELSE
!
!                    All the rows indicated by BEGIDX and ENDIDX
!                    match the constraints.  This code section should
!                    be upgraded to transfer the row numbers in
!                    chunks.
!
                     NMATCH  =  ENDIDX - BEGIDX + 1


                     DO R = BEGIDX, ENDIDX

                        IF ( INDEXD ) THEN
!
!                          Look up the record pointer for row R
!                          from the column index.
!
                           CALL ZZEKIXLK ( STHAN(SEG), &
                                           KEYDSC, &
                                           R, &
                                           ROWIDX )
                        ELSE
!
!                          Look up the record pointer for row R.
!
                           CALL ZZEKRPLK ( STHAN(SEG), &
                                           STDSCS(1,SEG), &
                                           R, &
                                           ROWIDX  )
                        END IF


                        CALL ZZEKSPSH ( 1, ROWIDX         )
                        CALL ZZEKSPSH ( 1, SGVBAS-RBAS(T) )

                     END DO

                  END IF

!
!                 Fill in the row count for this segment in the join row
!                 set.
!
                  CALL ZZEKSUPD ( RBAS(T)+PTROFF+1, &
                                  RBAS(T)+PTROFF+1, NMATCH )

               END IF

!
!              Take a look at the next segment.  Update the total count
!              of matching rows for this table.
!
               SEG    = LNKNXT ( SEG, STPOOL )
               RTOTAL = RTOTAL + NMATCH

            END DO

!
!           Fill in the size and count information for the join row set.
!
            CALL ZZEKSTOP ( TOP )
            RSIZE(T)  =  TOP - RBAS(T)

            CALL ZZEKSUPD ( RBAS(T)+JSZIDX, RBAS(T)+JSZIDX, RSIZE(T) )
            CALL ZZEKSUPD ( RBAS(T)+JRCIDX, RBAS(T)+JRCIDX, RTOTAL   )

!
!           Compress out any empty segment vectors from the join row
!           set.
!
            CALL ZZEKJSQZ ( RBAS(T) )

!
!           At this point, we've filled in the entire join row set for
!           table T.
!
         END DO

!
!        Join the trivial join row sets, producing a final join row set
!        for the current conjunction.  Retain the base address of this
!        join row set, if it is non-empty.  Update the size of the join
!        row set union.
!
!
         RESBAS = RBAS(1)

         DO T = 2, NTAB
!
!           Arm the join constraints!  Turn on the constraints that
!           have the Tth table on the one side, and tables
!           1, 2, ... , T on the other.
!
            DO I = 1, CJSIZE

               ACTIVC(I)  =  .FALSE.

               IF ( CNSTYP(I) .EQ. EQCOL ) THEN

                  L  =  LTBIDX(I)
                  R  =  RTBIDX(I)

                  IF (      ( L .GE. 1           ) &
                      .AND. ( L .LE. T           ) &
                      .AND. ( R .GE. 1           ) &
                      .AND. ( R .LE. T           ) &
                      .AND. ( L .NE. R           ) &
                      .AND. (      ( R .EQ. T ) &
                              .OR. ( L .EQ. T )  )  )  THEN

                     ACTIVC(I)  =  .TRUE.

                  END IF

               END IF

            END DO

!
!           The base address of the first join row set is the base
!           address of the result of the previous join.  The first time
!           through, the base of the join row set for table 1 is used.
!
            IF ( T .EQ. 2 ) THEN
               JBASE1  =  RBAS( 1 )
            ELSE
               JBASE1  =  RESBAS
            END IF

            JBASE2  = RBAS( T )

            CALL ZZEKJOIN (  JBASE1,   JBASE2,   CJSIZE,  ACTIVC, &
                             LTBIDX,   LCIDX,    LELTS,   OPS, &
                             RTBIDX,   RCIDX,    RELTS,   STHAN, &
                             STDSCS,   STDTPT,   DTPOOL,  DTDSCS, &
                             RESBAS,   JSIZE                       )

            CALL ZZEKJSQZ ( RESBAS )

         END DO

!
!        At this point, we've found the matching rows for the current
!        query conjunction.  Update the size of the join row set union
!        corresponding to the current query.  Save the base address of
!        the final join row set.  Update the total number of matching
!        rows in the join row set union.
!
         USIZE         =  USIZE  + 1
         UBASE(USIZE)  =  RESBAS

         CALL ZZEKSRD ( RESBAS+JRCIDX, RESBAS+JRCIDX, CJROWS )

         UNROWS        =  UNROWS + CJROWS

!
!        Remove redundant row vectors from the join row set union.
!        These row vectors may arise in the execution of queries whose
!        WHERE clauses contain multiple conjunctions.
!
         CALL ZZEKWEED ( USIZE, UBASE, UNROWS )

!
!        Initialize the addressing function for the current join row
!        set union.
!
         IF ( USIZE .GT. 0 ) THEN
            CALL ZZEKVSET ( USIZE, UBASE )
         END IF

      END DO

!
!     At this point, we've formed the join row set union that
!     represents the set of row vectors matching the entire query.
!
      NMROWS  =  UNROWS

!
!     Get the tables and columns of from the SELECT clause.  For
!     each qualifying table, we need the index in the FROM clause
!     of that table.  For each column, we need the column table
!     index.
!
      DO I = 1, NSEL

         CALL ZZEKQSEL ( EQRYI,  EQRYC,  I,      LXBEG,  LXEND, &
                         TABNAM, TABIDX, COLNAM, K             )

!
!        Locate the column's attribute information.  Retain the column's
!        index within the parent table's column list.
!
         TAB     =  TPTVEC( TABIDX )
         J       =  TBCTPT(TAB)
         COL     =  0
         FND     =  .FALSE.

         DO WHILE (  ( J .GT. 0 ) .AND. ( .NOT. FND )  )

            COL = COL + 1

            IF ( CTNAMS(J) .EQ. COLNAM ) THEN
               FND  =  .TRUE.
            ELSE
               J    =  LNKNXT( J, CTPOOL )
            END IF

         END DO

         IF ( .NOT. FND ) THEN
            CALL SETMSG ( '# is not name of a column in FROM table #.' )
            CALL ERRCH  ( '#', COLNAM                                  )
            CALL ERRINT ( '#', TABIDX                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'EKSRCH'                                     )
            RETURN
         END IF

         SELCTP(I)  =  J
         SELCOL(I)  =  COL
         SELTAB(I)  =  TABIDX

      END DO

!
!     Enable sorting of the matching row vectors, if necessary.  The
!     first fetch request will invoke the sort.
!
      DOSORT  =  ( NORDER .GT. 0 ) .AND. ( NMROWS .GT. 0 )
      SORTED  =  .FALSE.

      IF ( DOSORT ) THEN

         DO I = 1, NORDER

            CALL ZZEKQORD (  EQRYI,     EQRYC,   I,         TABNAM, &
                             OTABS(I),  COLNAM,  OCOLS(I),  SENSE(I)  )
         END DO

      END IF


      CALL CHKOUT ( 'EKSRCH' )
      RETURN







!$Procedure EKNELT  ( EK, get number of elements in column entry )

      ENTRY EKNELT ( SELIDX, ROW, NELT )

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
      IF ( RETURN () ) THEN
         RETURN
      END IF

!
!     The request doesn't make sense if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL CHKIN  ( 'EKNELT'                             )
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKNELT'                             )
         RETURN
      END IF

!
!     The row number must be valid, or we can't proceed.
!
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN

         CALL CHKIN  ( 'EKNELT'                                        )
         CALL SETMSG ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKNELT'                                        )
         RETURN

      END IF

!
!     Make sure the SELECT clause column index is valid.
!
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL CHKIN  ( 'EKNELT'                                   )
         CALL SETMSG ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKNELT'                                   )
         RETURN
      END IF

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      IF ( DOSORT ) THEN

         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS, &
                         OCOLS,   OELTS,   SENSE,   STHAN, &
                         STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )

         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF

!
!     Look up the segment vector and row vector for the current row.
!
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF

      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )

      TABIDX  =  SELTAB(SELIDX)
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)
      COL     =  SELCOL(SELIDX)

      COLPTR = STDTPT(SEG)

      DO I = 2, COL
         COLPTR = LNKNXT( COLPTR, DTPOOL )
      END DO

      NELT  =  ZZEKESIZ (  STHAN (   SEG   ), &
                           STDSCS(1, SEG   ), &
                           DTDSCS(1, COLPTR), &
                           ROWIDX             )


      RETURN





!$Procedure EKGC  ( EK, get event data, character )

      ENTRY EKGC ( SELIDX, ROW, ELMENT, CDATA, NULL, FOUND )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGC' )
      END IF

!
!     Nothing found yet.
!
      FOUND = .FALSE.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGC'                               )
         RETURN
      END IF

!
!     The row number must be valid, or we can't proceed.
!
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN

         CALL SETMSG ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGC'                                          )
         RETURN

      END IF

!
!     The element index must be positive.
!
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGC'                               )
         RETURN
      END IF

!
!     Make sure the SELECT clause column index is valid.
!
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGC'                                     )
         RETURN
      END IF

!
!     COL is the column's index within the parent
!     table's column list.
!
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )

!
!     Make sure the column has character type.
!
      IF (  CTTYPS(COLPTR) .NE. CHR ) THEN

         CALL SETMSG ( 'Column # has data type #.'      )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )            )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )    )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'             )
         CALL CHKOUT ( 'EKGC'                           )
         RETURN

      END IF

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      IF ( DOSORT ) THEN

         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS, &
                         OCOLS,   OELTS,   SENSE,   STHAN, &
                         STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )

         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF

!
!     Look up the segment vector and row vector for the current row.
!
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF

      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)

      J       =  STDTPT(SEG)

      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO

!
!     Look up the element.
!
      CALL ZZEKRSC ( STHAN(SEG), &
                     STDSCS(1,SEG), &
                     DTDSCS(1,J), &
                     ROWIDX, &
                     ELMENT , &
                     CVLEN, &
                     CDATA, &
                     NULL, &
                     FOUND         )



      CALL CHKOUT ( 'EKGC' )
      RETURN






!$Procedure EKGD  ( EK, get event data, double precision )

      ENTRY EKGD ( SELIDX, ROW, ELMENT, DDATA, NULL, FOUND )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGD' )
      END IF

!
!     Nothing found yet.
!
      FOUND = .FALSE.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGD'                               )
         RETURN
      END IF

!
!     The row number must be valid, or we can't proceed.
!
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN

         CALL SETMSG ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGD'                                          )
         RETURN

      END IF

!
!     The element index must be positive.
!
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGD'                               )
         RETURN
      END IF

!
!     Make sure the SELECT clause column index is valid.
!
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGD'                                     )
         RETURN
      END IF

!
!     COL is the column's index within the parent
!     table's column list.
!
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )

!
!     Make sure the column has double precision or `time' type.
!
      IF (       ( CTTYPS(COLPTR) .NE. DP   ) &
           .AND. ( CTTYPS(COLPTR) .NE. TIME )  ) THEN

         CALL SETMSG ( 'Column # has data type #.'    )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )          )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'           )
         CALL CHKOUT ( 'EKGD'                         )
         RETURN

      END IF

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      IF ( DOSORT ) THEN

         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS, &
                         OCOLS,   OELTS,   SENSE,   STHAN, &
                         STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )

         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF

!
!     Look up the segment vector and row vector for the current row.
!
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF

      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)

      J       =  STDTPT(SEG)

      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO

!
!     Look up the element.
!
      CALL ZZEKRSD ( STHAN (   SEG), &
                     STDSCS(1, SEG), &
                     DTDSCS(1, J  ), &
                     ROWIDX, &
                     ELMENT , &
                     DDATA, &
                     NULL, &
                     FOUND          )


      CALL CHKOUT ( 'EKGD' )
      RETURN







!$Procedure EKGI  ( EK, get event data, integer )

      ENTRY EKGI ( SELIDX, ROW, ELMENT, IDATA, NULL, FOUND )

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
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKGI' )
      END IF

!
!     Nothing found yet.
!
      FOUND = .FALSE.

!
!     There nothing to fetch if no files are loaded.  A sure
!     symptom of this problem is that the file list is empty.
!
      IF ( FTHEAD .LE. 0 ) THEN
         CALL SETMSG ( 'No E-kernels are currently loaded.' )
         CALL SIGERR ( 'SPICE(NOLOADEDFILES)'               )
         CALL CHKOUT ( 'EKGI'                               )
         RETURN
      END IF

!
!     The row number must be valid, or we can't proceed.
!
      IF (  ( ROW .LT. 1 ) .OR. ( ROW .GT. UNROWS )  ) THEN

         CALL SETMSG ( 'Row indices for query result range from 1 to '// &
                       '#; requested row index was #.'                 )
         CALL ERRINT ( '#',  UNROWS                                    )
         CALL ERRINT ( '#',  ROW                                       )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'EKGI'                                          )
         RETURN

      END IF

!
!     The element index must be positive.
!
      IF (  ELMENT .LT. 1 ) THEN
         CALL SETMSG ( 'ELMENT must be positive but was #.' )
         CALL ERRINT ( '#',  ELMENT                         )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                )
         CALL CHKOUT ( 'EKGI'                               )
         RETURN
      END IF

!
!     Make sure the SELECT clause column index is valid.
!
      IF (  ( SELIDX .LT. 1 ) .OR. ( SELIDX .GT. NSEL )  ) THEN
         CALL SETMSG ( 'The SELECT column index # is out of the ' // &
                       'valid range 1:#'                          )
         CALL ERRINT ( '#', SELIDX                                )
         CALL ERRINT ( '#', NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                      )
         CALL CHKOUT ( 'EKGI'                                     )
         RETURN
      END IF

!
!     COL is the column's index within the parent
!     table's column list.
!
      TABIDX  =  SELTAB( SELIDX )
      COL     =  SELCOL( SELIDX )
      COLPTR  =  SELCTP( SELIDX )
      TAB     =  TPTVEC( TABIDX )

!
!     Make sure the column has integer type.
!
      IF ( CTTYPS(COLPTR) .NE. INT ) THEN

         CALL SETMSG ( 'Column # has data type #.'    )
         CALL ERRCH  ( '#', CTNAMS( COLPTR )          )
         CALL ERRCH  ( '#', CHTYPE( CTTYPS(COLPTR) )  )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'           )
         CALL CHKOUT ( 'EKGI'                         )
         RETURN

      END IF

!
!     If it hasn't been done yet, and if it needs to be done, sort the
!     matching row vectors.
!
      IF ( DOSORT ) THEN

         CALL ZZEKJSRT ( USIZE,   UBASE,   NORDER,  OTABS, &
                         OCOLS,   OELTS,   SENSE,   STHAN, &
                         STDSCS,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS  )

         DOSORT  =  .FALSE.
         SORTED  =  .TRUE.
      END IF

!
!     Look up the segment vector and row vector for the current row.
!
      IF ( SORTED ) THEN
         CALL ZZEKSRD  ( ORDBAS+ROW,  ORDBAS+ROW,  I      )
         CALL ZZEKVCAL ( I,           RWVBAS,      SGVBAS )
      ELSE
         CALL ZZEKVCAL ( ROW,         RWVBAS,      SGVBAS )
      END IF

      CALL ZZEKSRD  ( RWVBAS+1, RWVBAS+NTAB, ROWVEC )
      CALL ZZEKSRD  ( SGVBAS+1, SGVBAS+NTAB, SEGVEC )

!
!     Identify the segment containing the column entry of interest.
!     Obtain the column descriptor for the column.
!
      ROWIDX  =  ROWVEC(TABIDX)
      SEG     =  SEGVEC(TABIDX)

      J       =  STDTPT(SEG)

      DO I = 2, COL
         J = LNKNXT( J, DTPOOL )
      END DO

!
!     Look up the element.
!
      CALL ZZEKRSI ( STHAN (   SEG), &
                     STDSCS(1, SEG), &
                     DTDSCS(1, J  ), &
                     ROWIDX, &
                     ELMENT , &
                     IDATA, &
                     NULL, &
                     FOUND           )


      CALL CHKOUT ( 'EKGI' )
      RETURN
      END
