!$Procedure EKAPPR ( EK, append record onto segment )

      subroutine ekappr ( handle, segno, recno )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Append a new, empty record at the end of a specified E-kernel
!     segment.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations

      implicit none

      include 'ekdatpag.inc'
      include 'ekpage.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segno
      integer(ip)           recno

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGNO      I   Segment number.
!     RECNO      O   Number of appended record.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle of an EK open for write access.
!
!     SEGNO    is the number of the segment to which the record
!              is to be added.
!
! $ Detailed_Output
!
!     RECNO    is the number of the record appended by this
!              routine. RECNO is used to identify the record
!              when writing column entries to it.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine. The file will not be modified.
!
!     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
!         is signaled. The file will not be modified.
!
!     3)  If an I/O error occurs while reading or writing the indicated
!         file, the error is signaled by a routine in the call tree of
!         this routine. The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading ek.req for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects: It appends a new, empty
!     record structure to an EK segment. The ordinal position of the
!     new record is one greater than the previous number of records in
!     in the segment.
!
!     After a new record has been appended to a segment by this routine,
!     the record must be populated with data using the EKACEx
!     routines.  EKs are valid only when all of their column entries
!     are initialized.
!
!     To insert a record into a segment at a specified ordinal position,
!     use the routine EKAPPR.
!
!     This routine cannot be used with the "fast write" suite of
!     routines. See the EK Required Reading ek.req for a discussion of
!     the fast writers.
!
!     When a record is inserted into an EK file that is not shadowed,
!     the status of the record starts out set to OLD. The status
!     does not change when data is added to the record.
!
!     If the target EK is shadowed, the new record will be given the
!     status NEW. Updating column values in the record does not change
!     its status. When changes are committed, the status is set to OLD.
!     If a rollback is performed before changes are committed, the
!     record is deleted. Closing the target file without committing
!     changes implies a rollback.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Append a record to a specified segment.
!
!        Suppose we have an E-kernel which contains records of orders
!        for data products. The E-kernel has a table called DATAORDERS
!        that consists of the set of columns listed below:
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
!        We'll suppose that the EK file contains two segments, the
!        first containing the DATAORDERS table and the second
!        containing the DATAITEMS table.
!
!        This examples creates such EK, with no records in either
!        table, and after re-opening the file, inserts a new record
!        into the DATAITEMS table.
!
!        Example code begins here.
!
!              PROGRAM EKAPPR_EX1
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
!              CHARACTER*(*)         EKNAME
!              PARAMETER           ( EKNAME  = 'ekappr_ex1.bdb' )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
!
!              INTEGER               DESCLN
!              PARAMETER           ( DESCLN = 80  )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 40  )
!
!              INTEGER               NCOLS
!              PARAMETER           ( NCOLS  = 6   )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(DECLEN)    CDECLS ( NCOLS )
!              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
!              CHARACTER*(DESCLN)    DESCRP
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    ITEMNM
!
!              DOUBLE PRECISION      PRICE
!
!              INTEGER               ESIZE
!              INTEGER               HANDLE
!              INTEGER               ITEMID
!              INTEGER               NRESVC
!              INTEGER               ORDID
!              INTEGER               RECNO
!              INTEGER               SEGNO
!
!              LOGICAL               ISNULL
!
!        C
!        C     Open a new EK file.  For simplicity, we will not
!        C     reserve any space for the comment area, so the
!        C     number of reserved comment characters is zero.
!        C     The variable IFNAME is the internal file name.
!        C
!              NRESVC  =  0
!              IFNAME  =  'Test EK/Created 01-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
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
!              CDECLS(3) =  'DATATYPE = CHARACTER*(*), ' //
!             .             'INDEXED  = TRUE'
!
!              CNAMES(4) =  'FIRST_NAME'
!              CDECLS(4) =  'DATATYPE = CHARACTER*(*), ' //
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
!        C
!        C     Start the first segment. Since we have no data for this
!        C     segment, start the segment by just defining the new
!        C     segment's schema.
!        C
!              CALL EKBSEG ( HANDLE, 'DATAORDERS', NCOLS,
!             .              CNAMES, CDECLS,       SEGNO )
!
!        C
!        C     At this point, the second segment could be
!        C     created by an analogous process.  In fact, the
!        C     second segment could be created at any time; it is
!        C     not necessary to populate the first segment with
!        C     data before starting the second segment.
!        C
!        C     Set up the table and column names and declarations
!        C     for the DATAITEMS segment.  We'll index all of
!        C     the columns.  All columns are scalar, so we omit
!        C     the size declaration.
!        C
!              CNAMES(1) =  'ITEM_ID'
!              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
!
!              CNAMES(2) =  'ORDER_ID'
!              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
!
!              CNAMES(3) =  'ITEM_NAME'
!              CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
!             .             'INDEXED  = TRUE'
!
!              CNAMES(4) =  'DESCRIPTION'
!              CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
!             .             'INDEXED  = TRUE'
!
!              CNAMES(5) =  'PRICE'
!              CDECLS(5) =  'DATATYPE = DOUBLE PRECISION,' //
!             .             'INDEXED  = TRUE'
!
!        C
!        C     Start the new segment. Since we have no data for this
!        C     segment, start the segment by just defining the new
!        C     segment's schema.
!        C
!              CALL EKBSEG ( HANDLE, 'DATAITEMS', 5,
!             .              CNAMES, CDECLS,      SEGNO )
!
!        C
!        C     End the file by a call to EKCLS.
!        C
!              CALL EKCLS ( HANDLE )
!
!        C
!        C     Now, we want to insert a new record into the DATAITEMS
!        C     table.
!        C
!        C     Open the database for write access.  This call is
!        C     made when the file already exists.
!        C
!              CALL EKOPW ( EKNAME, HANDLE )
!
!        C
!        C     Append a new, empty record to the DATAITEMS
!        C     table. Recall that the DATAITEMS table
!        C     is in segment number 2.  The call will return
!        C     the number of the new, empty record.
!        C
!              SEGNO = 2
!              CALL EKAPPR ( HANDLE, SEGNO, RECNO )
!
!        C
!        C     At this point, the new record is empty.  A valid EK
!        C     cannot contain empty records.  We fill in the data
!        C     here.  Data items are filled in one column at a time.
!        C     The order in which the columns are filled in is not
!        C     important.  We use the EKACEx (add column entry)
!        C     routines to fill in column entries.  We'll assume
!        C     that no entries are null.  All entries are scalar,
!        C     so the entry size is 1.
!        C
!              ISNULL =  .FALSE.
!              ESIZE  =  1
!
!        C
!        C     The following variables will contain the data for
!        C     the new record.
!        C
!              ORDID  =   10011
!              ITEMID =   531
!              ITEMNM =  'Sample item'
!              DESCRP =  'This sample item is used only in tests.'
!              PRICE  =   1345.678D0
!
!        C
!        C     Note that the names of the routines called
!        C     correspond to the data types of the columns:  the
!        C     last letter of the routine name is C, I, or D,
!        C     depending on the data type.
!        C
!              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID',
!             .              ESIZE,  ORDID,  ISNULL               )
!
!              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ITEM_ID',
!             .              ESIZE,  ITEMID, ISNULL               )
!
!              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'ITEM_NAME',
!             .              ESIZE,  ITEMNM, ISNULL               )
!
!              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'DESCRIPTION',
!             .              ESIZE,  DESCRP, ISNULL               )
!
!              CALL EKACED ( HANDLE, SEGNO,  RECNO, 'PRICE',
!             .              ESIZE,  PRICE,  ISNULL               )
!
!        C
!        C     Close the file to make the update permanent.
!        C
!              CALL EKCLS ( HANDLE )
!
!              END
!
!        When this program is executed, no output is presented on
!        screen. After run completion, a new EK file exists in the
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
!-    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
!
!        Documentation change: instances of the phrase "fast load"
!        were replaced with "fast write."
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&

! $ Index_Entries
!
!     append record to EK segment
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
      integer(ip)           mbase
      integer(ip)           mp
      integer(ip)           nrec
      integer(ip)           segdsc ( sdscsz )

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Look up the integer metadata page and page base for the segment.
!     Given the base address, we can read the pertinent metadata in
!     one shot.
!
      call zzekmloc ( handle,  segno,  mp,  mbase  )

      if ( failed() ) return

      call dasrdi ( handle,  mbase+1,  mbase+sdscsz,  segdsc )

!
!     Obtain the number of records already present.
!
      nrec   =  segdsc ( nridx )

!
!     Insert the new record at the end of the segment.
!
      recno  =  nrec + 1

      call ekinsr ( handle, segno, recno )

      return
      end
