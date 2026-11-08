!$Procedure EKACLC ( EK, add character column to segment )

      subroutine ekaclc (  handle,  segno,   column,  cvals, &
                           entszs,  nlflgs,  rcptrs,  wkindx  )

! $ Abstract
!
!     Add an entire character column to an EK segment.
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

      implicit none

      include  'ekcoldsc.inc'
      include  'eksegdsc.inc'
      include  'ektype.inc'

      integer               handle
      integer               segno
      character*(*)         column
      character*(*)         cvals  ( * )
      integer               entszs ( * )
      logical               nlflgs ( * )
      integer               rcptrs ( * )
      integer               wkindx ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     SEGNO      I   Number of segment to add column to.
!     COLUMN     I   Column name.
!     CVALS      I   Character values to add to column.
!     ENTSZS     I   Array of sizes of column entries.
!     NLFLGS     I   Array of null flags for column entries.
!     RCPTRS     I   Record pointers for segment.
!     WKINDX    I-O  Work space for column index.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an EK file that is open for writing.
!              A "begin segment for fast write" operation must
!              have already been performed for the designated
!              segment.
!
!     SEGNO    is the number of the segment to which
!              data is to be added.
!
!     COLUMN   is the name of the column to be added. All of
!              the data for the named column will be added in
!              one shot.
!
!     CVALS    is an array containing the entire set of column
!              entries for the specified column. The entries
!              are listed in row-order:  the column entry for the
!              first row of the segment is first, followed by the
!              column entry for the second row, and so on. The
!              number of column entries must match the declared
!              number of rows in the segment. For columns having
!              fixed-size entries, a null entry must be allocated
!              the same amount of space occupied by a non-null
!              entry in the array CVALS. For columns having
!              variable-size entries, null entries do not require
!              any space in the CVALS array, but in any case must
!              have their allocated space described correctly by
!              the corresponding element of the ENTSZS array
!              (described below).
!
!     ENTSZS   is an array containing sizes of column entries.
!              The Ith element of ENTSZS gives the size of the
!              Ith column entry. ENTSZS is used only for columns
!              having variable-size entries. For such columns,
!              the dimension of ENTSZS must be at least NROWS.
!              The size of null entries should be set to zero.
!
!              For columns having fixed-size entries, the
!              dimension of this array may be any positive value.
!
!     NLFLGS   is an array of logical flags indicating whether
!              the corresponding entries are null. If the Ith
!              element of NLFLGS is .FALSE., the Ith column entry
!              defined by CVALS and ENTSZS is added to the
!              current segment in the specified kernel file.
!
!              If the Ith element of NLFGLS is .TRUE., the
!              contents of the Ith column entry are undefined.
!
!              NLFLGS is used only for columns that allow null
!              values; it's ignored for other columns.
!
!     RCPTRS   is an array of record pointers for the input
!              segment. This array is obtained as an output
!              from EKIFLD, the routine called to initiate a
!              fast write.
!
!     WKINDX   is a work space array used for building a column
!              index. If the column is indexed, the dimension of
!              WKINDX must be at NROWS, where NROWS is the number
!              of rows in the column. If the column is not
!              indexed, this work space is not used, so the
!              dimension may be any positive value.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If COLUMN is not the name of a declared column, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If COLUMN specifies a column of whose data type is not
!         integer, the error SPICE(WRONGDATATYPE) is signaled.
!
!     4)  If the specified column already contains ANY entries, an error
!         is signaled by a routine in the call tree of this routine.
!
!     5)  If an I/O error occurs while reading or writing the indicated
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See the EK Required Reading ek.req for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects: it modifies the named
!     EK file by adding data to the specified column. This routine
!     writes the entire contents of the specified column in one shot.
!     This routine creates columns much more efficiently than can be
!     done by sequential calls to EKACEC, but has the drawback that
!     the caller must use more memory for the routine's inputs. This
!     routine cannot be used to add data to a partially completed
!     column.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose we want to create an Sequence Component E-kernel
!        named 'ekaclc_ex1.bes' which contains records of orders for
!        data products. The E-kernel has a table called DATAORDERS that
!        consists of the set of columns listed below:
!
!           DATAORDERS
!
!              Column Name     Data Type
!              -----------     ---------
!              ORDER_ID        INTEGER
!              CUSTOMER_ID     INTEGER
!              LAST_NAME       CHARACTER*(*)
!              FIRST_NAME      CHARACTER*(*)
!              ORDER_DATE      TIME
!              COST            DOUBLE PRECISION
!
!        The order database also has a table of items that have been
!        ordered. The columns of this table are shown below:
!
!           DATAITEMS
!
!              Column Name     Data Type
!              -----------     ---------
!              ITEM_ID         INTEGER
!              ORDER_ID        INTEGER
!              ITEM_NAME       CHARACTER*(*)
!              DESCRIPTION     CHARACTER*(*)
!              PRICE           DOUBLE PRECISION
!
!
!        The file "ekaclc_ex1.bdb" will contain two segments, the first
!        containing the DATAORDERS table and the second containing the
!        DATAITEMS table.
!
!        This example demonstrates how to open a new EK file and create
!        the first of the segments described above.
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
!              PROGRAM EKACLC_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include the EK Column Name Size (CNAMSZ)
!        C
!              INCLUDE 'ekcnamsz.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         LSK
!              PARAMETER           ( LSK    = 'naif0012.tls' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE  = 'DATAORDERS'   )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               FNMLEN
!              PARAMETER           ( FNMLEN = 50  )
!
!              INTEGER               LNMLEN
!              PARAMETER           ( LNMLEN = 50  )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 6   )
!
!              INTEGER               NROWS
!              PARAMETER           ( NROWS  = 9   )
!
!              INTEGER               UTCLEN
!              PARAMETER           ( UTCLEN = 30  )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
!              CHARACTER*(FNMLEN)    FNAMES ( NROWS )
!              CHARACTER*(LNMLEN)    LNAMES ( NROWS )
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(UTCLEN)    ODATE
!
!              DOUBLE PRECISION      COSTS  ( NROWS )
!              DOUBLE PRECISION      ETS    ( NROWS )
!
!              INTEGER               CSTIDS ( NROWS )
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               NRESVC
!              INTEGER               ORDIDS ( NROWS )
!              INTEGER               RCPTRS ( NROWS )
!              INTEGER               SEGNO
!              INTEGER               SIZES  ( NROWS )
!              INTEGER               WKINDX ( NROWS )
!
!              LOGICAL               NLFLGS ( NROWS )
!
!        C
!        C     Load a leapseconds kernel for UTC/ET conversion.
!        C
!              CALL FURNSH ( 'naif0012.tls' )
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 20-SEP-1995'
!
!              CALL EKOPN ( 'ekaclc_ex1.bes', IFNAME, NRESVC, HANDLE )
!
!        C
!        C     Set up the table and column names and declarations
!        C     for the DATAORDERS segment.  We'll index all of
!        C     the columns.  All columns are scalar, so we omit
!        C     the size declaration.  Only the COST column may take
!        C     null values.
!        C
!              CNAMES(1) =  'ORDER_ID'
!              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
!
!              CNAMES(2) =  'CUSTOMER_ID'
!              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
!
!              CNAMES(3) =  'LAST_NAME'
!              CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
!             .             'INDEXED  = TRUE'
!
!              CNAMES(4) =  'FIRST_NAME'
!              CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
!             .             'INDEXED  = TRUE'
!
!              CNAMES(5) =  'ORDER_DATE'
!              CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE'
!
!              CNAMES(6) =  'COST'
!              CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' //
!             .             'INDEXED  = TRUE,'             //
!             .             'NULLS_OK = TRUE'
!
!
!        C
!        C     Start the segment.  We presume the number of  rows
!        C     of data is known in advance.
!        C
!              CALL EKIFLD ( HANDLE,  TABLE,  NCOLS, NROWS,
!             .              CNAMES,  CDECLS, SEGNO, RCPTRS )
!
!
!        C
!        C     At this point, arrays containing data for the
!        C     segment's columns may be filled in.  The names
!        C     of the data arrays are shown below.
!        C
!        C        Column           Data array
!        C
!        C        'ORDER_ID'       ORDIDS
!        C        'CUSTOMER_ID'    CSTIDS
!        C        'LAST_NAME'      LNAMES
!        C        'FIRST_NAME'     FNAMES
!        C        'ORDER_DATE'     ETS
!        C        'COST'           COSTS
!        C
!              DO I = 1, NROWS
!
!                 ORDIDS(I) = I
!                 CSTIDS(I) = I * 100
!                 COSTS(I)  = I * 100.D0
!
!                 CALL REPMI ( 'Order # Customer first name',
!             .                '#', I, FNAMES(I)             )
!                 CALL REPMI ( 'Order # Customer last name',
!             .                '#', I, LNAMES(I)             )
!                 CALL REPMI ( '1998 Mar #', '#', I, ODATE   )
!
!                 CALL UTC2ET ( ODATE,  ETS(I) )
!
!                 NLFLGS(I) = .FALSE.
!
!              END DO
!
!              NLFLGS(2) = .TRUE.
!
!        C
!        C     The SIZES array shown below is ignored for scalar
!        C     and fixed-size array columns, so we need not
!        C     initialize it.  For variable-size arrays, the
!        C     Ith element of the SIZES array must contain the size
!        C     of the Ith column entry in the column being written.
!        C     Normally, the SIZES array would be reset for each
!        C     variable-size column.
!        C
!        C     The NLFLGS array indicates which entries are null.
!        C     It is ignored for columns that don't allow null
!        C     values.  In this case, only the COST column allows
!        C     nulls.
!        C
!        C     Add the columns of data to the segment.  All of the
!        C     data for each column is written in one shot.
!        C
!              CALL EKACLI ( HANDLE, SEGNO,  'ORDER_ID',
!             .              ORDIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!              CALL EKACLI ( HANDLE, SEGNO,  'CUSTOMER_ID',
!             .              CSTIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!              CALL EKACLC ( HANDLE, SEGNO,  'LAST_NAME',
!             .              LNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!              CALL EKACLC ( HANDLE, SEGNO,  'FIRST_NAME',
!             .              FNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!              CALL EKACLD ( HANDLE, SEGNO,  'ORDER_DATE',
!             .              ETS,    SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!              CALL EKACLD ( HANDLE, SEGNO,  'COST',
!             .              COSTS,  SIZES,  NLFLGS,  RCPTRS, WKINDX )
!
!        C
!        C     Complete the segment.  The RCPTRS array is that
!        C     returned by EKIFLD.
!        C
!              CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
!
!        C
!        C     At this point, the second segment could be
!        C     created by an analogous process.  In fact, the
!        C     second segment could be created at any time; it is
!        C     not necessary to populate the first segment with
!        C     data before starting the second segment.
!        C
!        C     The file must be closed by a call to EKCLS.
!        C
!              CALL EKCLS ( HANDLE )
!
!              END
!
!
!        When this program is executed, no output is presented on
!        screen. After run completion, a new EK file exists in the
!        output directory.
!
! $ Restrictions
!
!     1)  Only one segment can be created at a time using the fast
!         write routines.
!
!     2)  No other EK operation may interrupt a fast write. For
!         example, it is not valid to issue a query while a fast write
!         is in progress.
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) (BVS)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. and
!        created complete code example from existing fragment.
!
!-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
!
!        Documentation change: instances of the phrase "fast load"
!        were replaced with "fast write."
!
!     Beta Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     write entire character column to EK segment
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local variables
!
      integer               class
      integer               coldsc ( cdscsz )
      integer               dtype
      integer               segdsc ( sdscsz )

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'EKACLC' )
      end if

!
!     Find the descriptors for the specified segment and column.
!
      call zzeksdsc ( handle,  segno,   segdsc          )
      call zzekcdsc ( handle,  segdsc,  column,  coldsc )

      if ( failed() ) then
         call chkout ( 'EKACLC' )
         return
      end if

!
!     This column had better be of character type.
!
      class  =  coldsc ( clsidx )
      dtype  =  coldsc ( typidx )

      if ( dtype .ne. chr ) then

         call setmsg ( 'Column # is of type #; EKACLC only works '  // &
                       'with character columns.'                     )
         call errch  ( '#',  column                                  )
         call errint ( '#',  dtype                                   )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                        )
         call chkout ( 'EKACLC'                                      )
         return

      end if

!
!     Hand off the task to the routine of the appropriate class.
!
      if ( class .eq. 3 ) then
!
!        Class 3 columns contain character scalars.
!
         call zzekac03 (  handle,  segdsc,  coldsc,  cvals, &
                                   nlflgs,  rcptrs,  wkindx )

      else if ( class .eq. 6 ) then
!
!        Class 6 columns contain character arrays.
!
         call zzekac06 ( handle, segdsc, coldsc, cvals, entszs, nlflgs )


      else if ( class .eq. 9 ) then
!
!        Class 9 columns contain fixed-count, fixed-length character
!        scalars.
!
         call zzekac09 ( handle, segdsc, coldsc, cvals, nlflgs, wkindx )


      else
!
!        This is an unsupported column class.
!
         call setmsg ( 'Unsupported column class code # found in '   // &
                       'descriptor for column #.'                      )
         call errint ( '#', class                                      )
         call errch  ( '#', column                                     )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                           )
         call chkout ( 'EKACLC'                                        )
         return

      end if

      call chkout ( 'EKACLC' )
      return
      end
