!$Procedure EKPSEL ( EK, parse SELECT clause )

      subroutine ekpsel (  query,   n,     xbegs,  xends,  xtypes, &
                           xclass,  tabs,  cols,   error,  errmsg  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Parse the SELECT clause of an EK query, returning full particulars
!     concerning each selected item.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none

      include 'ekattdsc.inc'
      include 'ekcnamsz.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektnamsz.inc'
      include 'ektype.inc'

      character(*)          query
      integer(ip)           n
      integer(ip)           xbegs  ( * )
      integer(ip)           xends  ( * )
      character(*)          xtypes ( * )
      character(*)          xclass ( * )
      character(*)          tabs   ( * )
      character(*)          cols   ( * )
      logical               error
      character(*)          errmsg

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     QUERY      I   EK query.
!     N          O   Number of items in SELECT clause of QUERY.
!     XBEGS      O   Begin positions of expressions in SELECT clause.
!     XENDS      O   End positions of expressions in SELECT clause.
!     XTYPES     O   Data types of expressions.
!     XCLASS     O   Classes of expressions.
!     TABS       O   Names of tables qualifying SELECT columns.
!     COLS       O   Names of columns in SELECT clause of QUERY.
!     ERROR      O   Error flag.
!     ERRMSG     O   Parse error message.
!
! $ Detailed_Input
!
!     QUERY    is a character string containing an EK query.
!              EK queries have the general form
!
!                 SELECT <select expr>, <select expr>, ...
!                 FROM <table spec>, <table spec>, ...
!                 [WHERE <constraint list>]
!                 [ORDER BY <order-by column list>]
!
!              Here the symbol <select expr> indicates any
!              expression representing an entity that can be
!              selected. Commonly, the selected items are
!              columns, with or without qualifying table names,
!              having the form
!
!                 <column name>
!                 <table name>.<column name>
!                 <table alias>.<column name>
!
!              but more general expressions may also be selected.
!              Examples are functions, such as
!
!                 COUNT(*)
!                 COUNT( <table name>.<column name> )
!                 MAX  ( <table name>.<column name> )
!
!              or expressions involving constants, such as
!
!                 2 * <column name>
!
! $ Detailed_Output
!
!     N        is the number of items specified in the
!              SELECT clause of the input query.
!
!     XBEGS,
!     XENDS    are, respectively, arrays of begin and end
!              positions of expressions designating items in the
!              SELECT clause of the input query. The Ith
!              expression is located in the substring
!
!                 QUERY ( XBEGS(I) : XENDS(I) )
!
!     XTYPES   is an array of short strings indicating the data
!              types of the expressions in the SELECT clause.
!              Values and meanings of XTYPES are:
!
!                 'CHR'        Character type
!                 'DP'         Double precision type
!                 'INT'        Integer type
!                 'TIME'       Time type
!
!              The Ith element of XTYPES refers to the Ith
!              selected item.
!
!              The data type of an expression indicates which
!              fetch routine to use to obtain values of the
!              selected expression. The mapping of data types
!              to fetch routines is shown below:
!
!                 'CHR'        EKGC
!                 'DP'         EKGD
!                 'INT'        EKGI
!                 'TIME'       EKGD
!
!              Note that time values are stored as d.p. numbers.
!
!     XCLASS   is an array of short strings giving the classes
!              of the expressions occurring in the SELECT clause
!              of the input query. Values and meanings of
!              XCLASS are:
!
!                 'COL'        Selected item was a column. The
!                              column may qualified.
!
!                 'FUNC'       Selected item was a simple
!                              function invocation of the form
!
!                                 F ( <column> )
!
!                              or else was
!
!                                 COUNT(*)
!
!                 'EXPR'       Selected item was a more general
!                              expression than those shown above.
!
!              The Ith element of XCLASS refers to the Ith
!              selected item.
!
!              When a selected item is a column, the values of
!              the arguments TABS and COLS (discussed below) are
!              defined.
!
!     TABS     is an array of names of tables corresponding to
!              the columns in the SELECT clause. The Ith element
!              of TABS corresponds to the table containing the
!              Ith SELECT column. Table names returned in TABS
!              are the actual names of tables in loaded EK, not
!              aliases supplied in the input query. Table names
!              are supplied even if the corresponding column was
!              unqualified in the input query, as long as the
!              column name was unambiguous.
!
!              The contents of TABS(I) are defined if and only if
!              the returned value of XCLASS(I) is 'COL'.
!
!     COLS     is an array containing the columns of the SELECT
!              clause. The contents of COLS(I) are defined if and
!              only if the returned value of XCLASS(I) is 'COL'.
!
!     ERROR    is a logical flag indicating whether the input
!              QUERY parsed correctly. The other outputs of this
!              routine, except for ERRMSG, are undefined if a
!              parse error occurred. ERROR is returned .TRUE. if
!              a parse error occurred, .FALSE. otherwise.
!
!     ERRMSG   is a character string describing the cause of a
!              parse error, if such an error occurred. Otherwise,
!              ERRMSG is returned blank.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  Parse failures do not cause this routine to signal errors;
!         instead, the ERROR and ERRMSG outputs indicate invalid
!         QUERY.
!
!     2)  Queries cannot be parsed correctly unless at least one EK
!         is loaded.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows callers of the EK fetch routines to determine
!     at run time the attributes of the columns from which data is to be
!     fetched.
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
!              PROGRAM EKPSEL_EX1
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
!     1)  Currently, column names are the only supported expressions.
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
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragment.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     parse select clause of EK query
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: lbcell = -5

      integer(ip), parameter :: ntypes = 4

      integer(ip), parameter :: short = 4

!
!     Local variables
!
      character(tnamsz)     aka
      character(short)      chrtyp ( ntypes )
      character(cnamsz)     column
      character(maxcln)     eqryc
      character(tnamsz)     qtab

      real(wp)              eqryd  ( maxqnm )

      integer(ip)           attdsc ( adscsz )
      integer(ip)           colidx
      integer(ip)           eqryi  ( lbcell : eqimin )
      integer(ip)           errptr
      integer(ip)           i
      integer(ip)           tabidx

!
!     Saved values
!
      save                  chrtyp

!
!     Initial values
!
      data                  chrtyp / 'CHR', 'DP', 'INT', 'TIME' /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialize the encoded query each time, for safety.
!
      call zzekqini ( eqimin, maxqnm, eqryi, eqryc, eqryd )

!
!     Encode the input query.
!
      call zzekencd (  query, &
                       eqryi,  eqryc,   eqryd, &
                       error,  errmsg,  errptr  )

      if ( error ) return

!
!     Look up the number of SELECT columns.  For each column, look up
!     the parent table, the alias, and the column's name.
!
      call zzekreqi (  eqryi,  'NUM_SELECT_COLS',  n   )

      do i = 1, n

         call zzekqsel ( eqryi,  eqryc,   i,        xbegs(i),  xends(i), &
                         qtab,   tabidx,  cols(i),  colidx             )

!
!        Make the table index to the table name.
!
         call zzekqtab ( eqryi, eqryc, tabidx, tabs(i), aka )

!
!        Currently, every expression is a column.
!
         xclass(i)  =  'COL'

!
!        Look up the data type of the column.
!
         call ekcii ( tabs(i), colidx, column, attdsc )

         xtypes(i)  =  chrtyp (  attdsc(atttyp)  )

      end do

      return
      end
