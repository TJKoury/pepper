!$Procedure EKFFLD ( EK, finish fast write )

      subroutine ekffld ( handle, segno, rcptrs )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Complete a fast write operation on a new E-kernel segment.
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

      include 'eksegdsc.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           rcptrs ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RCPTRS     I   Record pointers.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an EK file that is open for writing.
!              A "begin segment for fast write" operation must
!              have already been performed for the designated
!              segment.
!
!     SEGNO    is the number of the segment to complete.
!
!     RCPTRS   is an array of record pointers for the input
!              segment. This array is obtained as an output
!              from EKIFLD, the routine called to initiate a
!              fast write.
!
! $ Detailed_Output
!
!     None.
!
!     See the $Particulars section for a description of the
!     effects of this routine.
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
!     2)  If an attempt is made to finish a segment other than the one
!         last initialized by EKIFLD, an error is signaled by a routine
!         in the call tree of this routine.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine completes an EK segment after the data has been
!     written via the fast column writer routines. The segment must
!     have been created by a call to EKIFLD. The fast column
!     writer routines are:
!
!        EKACLC {EK, add column, character}
!        EKACLD {EK, add column, double precision}
!        EKACLI {EK, add column, integer}
!
!     The segment is not guaranteed to be readable until all columns
!     have been added. After the columns have been added, the segment
!     may be extended by inserting more records and filling in those
!     records using the EKACEx routines.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose we want to create an Sequence Component E-kernel
!        named 'ekffld_ex1.bes' which contains records of orders for
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
!        The file "ekffld_ex1.bdb" will contain two segments, the first
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
!              PROGRAM EKFFLD_EX1
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
!              CHARACTER*(32)        CNAMES ( NCOLS )
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
!              CALL EKOPN ( 'ekffld_ex1.bes', IFNAME, NRESVC, HANDLE )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 24-NOV-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. and
!        created complete code example from existing fragment.
!
!-    SPICELIB Version 1.1.2, 09-JAN-2002 (NJB)
!
!        Documentation change: instances of the phrase "fast load"
!        were replaced with "fast write."
!
!-    SPICELIB Version 1.1.1, 18-JUN-1999 (WLT)
!
!        Corrected CHKOUT value to be same as CHKIN.
!
!-    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB)
!
!        Made miscellaneous header corrections.
!
!-    SPICELIB Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     finish fast write of an EK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           stype

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Read in the segment descriptor, and get the segment's type.
!
      call zzeksdsc (  handle,  segno,  segdsc )

      stype  =  segdsc(ektidx)

!
!     Complete the fast write preparations appropriate to the segment's
!     type.
!
      if ( stype == 1 ) then

         call zzekff01 ( handle, segno, rcptrs )


      else if ( stype == 2 ) then
!
!        Currently, no actions are taken to complete a type 2 segment.
!

      else

         call setmsg ( 'Segment type # is not currently supported.' )
         call errint ( '#', stype                                   )
         call sigerr ( 'SPICE(BUG)'                                 )
         return

      end if

      return
      end
