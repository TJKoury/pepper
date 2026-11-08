!$Procedure EKBSEG ( EK, start new segment )

      subroutine ekbseg ( handle, tabnam, ncols, cnames, decls, segno )

! $ Abstract
!
!     Start a new segment in an E-kernel.
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

      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekcnamsz.inc'
      include 'ekdatpag.inc'
      include 'ekfilpar.inc'
      include 'ekglimit.inc'
      include 'ekpage.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektnamsz.inc'
      include 'ektype.inc'

      integer               handle
      character*(*)         tabnam
      integer               ncols
      character*(*)         cnames ( * )
      character*(*)         decls  ( * )
      integer               segno

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TABNAM     I   Table name.
!     NCOLS      I   Number of columns in the segment.
!     CNAMES     I   Names of columns.
!     DECLS      I   Declarations of columns.
!     SEGNO      O   Segment number.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an EK file that is open for writing.
!
!     TABNAM   is the name of the EK table to which the current
!              segment belongs. All segments in the EK file
!              designated by HANDLE must have identical column
!              attributes. TABNAM must not exceed 32 characters
!              in length. Case is not significant. Table names
!              must start with a letter and contain only
!              characters from the set {A-Z,a-z,0-9,$,_}.
!
!     NCOLS    is the number of columns in a new segment.
!
!     CNAMES,
!     DECLS    are, respectively, and array of column names and
!              their corresponding declarations: the Ith element
!              of CNAMES and the Ith element of DECLS apply to
!              the Ith column in the segment.
!
!              Column names must not exceed CNAMSZ (32) characters
!              in length. Case is not significant. Column names
!              must start with a letter and contain only
!              characters from the set {A-Z,a-z,0-9,$,_}.
!
!              The declarations are strings that contain
!              `keyword=value' assignments that define the
!              attributes of the columns to which they apply. The
!              column attributes that are defined by a column
!              declaration are:
!
!                 DATATYPE
!                 SIZE
!                 <is the column indexed?>
!                 <does the column allow null values?>
!
!              The form of a declaration is
!
!                 'DATATYPE  = <type>,
!                  SIZE      = <size>,
!                  INDEXED   = <boolean>,
!                  NULLS_OK  = <boolean>'
!
!              For example, an indexed, scalar, integer column
!              that allows null values would have the declaration
!
!                 'DATATYPE  = INTEGER,
!                  SIZE      = 1,
!                  INDEXED   = TRUE,
!                  NULLS_OK  = TRUE'
!
!              Commas are required to separate the assignments
!              within declarations; white space is optional;
!              case is not significant.
!
!              The order in which the attribute keywords are
!              listed in declaration is not significant.
!
!              Every column in a segment must be declared.
!
!              Each column entry is effectively an array, each
!              element of which has the declared data type. The
!              SIZE keyword indicates how many elements are in
!              each entry of the column in whose declaration the
!              keyword appears. Note that only scalar-valued
!              columns (those for which SIZE = 1) may be
!              referenced in query constraints. A size
!              assignment has the syntax
!
!                 'SIZE = <integer>'
!
!              or
!
!                 'SIZE = VARIABLE'
!
!              The size value defaults to 1 if omitted.
!
!              The DATATYPE keyword defines the data type of
!              column entries. The DATATYPE assignment syntax
!              has any of the forms
!
!                 'DATATYPE = CHARACTER*(<length>)'
!                 'DATATYPE = CHARACTER*(*)'
!                 'DATATYPE = DOUBLE PRECISION'
!                 'DATATYPE = INTEGER'
!                 'DATATYPE = TIME'
!
!              As the datatype declaration syntax suggests,
!              character strings may have fixed or variable
!              length. Variable-length strings are allowed only
!              in columns of size 1.
!
!              Optionally, scalar-valued columns may be indexed.
!              To create an index for a column, use the assignment
!
!                 'INDEXED = TRUE'
!
!              By default, columns are not indexed.
!
!              Optionally, any column can allow null values. To
!              indicate that a column may allow null values, use
!              the assignment
!
!                 'NULLS_OK = TRUE'
!
!              in the column declaration. By default, null
!              values are not allowed in column entries.
!
! $ Detailed_Output
!
!     SEGNO    is the number of the segment created by this
!              routine. Segment numbers are used as unique
!              identifiers by other EK access routines.
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
!     2)  If TABNAM is more than TNAMSZ characters long, an error
!         is signaled by a routine in the call tree of this routine.
!
!     3)  If TABNAM contains any nonprintable characters, an error
!         is signaled by a routine in the call tree of this routine.
!
!     4)  If NCOLS is non-positive or greater than the maximum allowed
!         number MXCLSG, the error SPICE(INVALIDCOUNT) is signaled.
!
!     5)  If any column name exceeds CNAMSZ characters in length, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     6)  If any column name contains non-printable characters, an error
!         is signaled by a routine in the call tree of this routine.
!
!     7)  If a declaration cannot be understood by this routine, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     8)  If an non-positive string length or element size is specified,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If an I/O error occurs while reading or writing the indicated
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
!     This routine operates by side effects: it prepares an EK for
!     the addition of a new segment. It is not necessary to take
!     any special action to `complete' a segment; segments are readable
!     after the completion of any record insertion, deletion, write,
!     or update operation.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose we want to create an E-kernel which contains a table
!        of items that have been ordered. The columns of this table
!        are shown below:
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
!        This EK file will have one segment containing the DATAITEMS
!        table.
!
!        This examples demonstrates how to open a new EK file; create
!        the segment described above and how to insert a new record
!        into it.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKBSEG_EX1
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
!              PARAMETER           ( EKNAME  = 'ekbseg_ex1.bdb' )
!
!              CHARACTER*(*)         TABLE
!              PARAMETER           ( TABLE   = 'DATAITEMS'      )
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
!              PARAMETER           ( NCOLS  = 5   )
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
!              IFNAME  =  'Test EK;Created 21-JUN-2019'
!
!              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
!
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
!
!        C
!        C     Start the segment. Since we have no data for this
!        C     segment, start the segment by just defining the new
!        C     segment's schema.
!        C
!              CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
!             .              CNAMES, CDECLS, SEGNO )
!
!        C
!        C     Append a new, empty record to the DATAITEMS
!        C     table. Recall that the DATAITEMS table
!        C     is in segment number 1.  The call will return
!        C     the number of the new, empty record.
!        C
!              SEGNO = 1
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
!              ISNULL   =  .FALSE.
!              ESIZE    =  1
!
!        C
!        C     The following variables will contain the data for
!        C     the new record.
!        C
!              ORDID    =   10011
!              ITEMID   =   531
!              ITEMNM   =  'Sample item'
!              DESCRP   =  'This sample item is used only in tests.'
!              PRICE    =   1345.678D0
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
!-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICI NONE statement.
!
!        Edited the header to comply with NAIF standard and
!        created complete code example from existing fragment.
!
!-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
!
!        Erroneous error message for invalid column names was fixed.
!        Previous version line was changed from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     start new E-kernel segment
!     start new EK segment
!
!-&


!
!     SPICELIB functions
!
      integer               lastnb

      logical               failed
      logical               return

!
!     Non-SPICELIB functions
!
      integer               zzekstyp

!
!     Local parameters
!
      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               mxspec
      parameter           ( mxspec = 512 )

      integer               namlim
      parameter           ( namlim = 32 )

!
!     Local variables
!
      integer               cdscrs ( cdscsz, mxclsg )
      integer               i
      integer               idend
      integer               idspec ( lbcell : mxspec )
      integer               nchars
      integer               stype

      logical               first

!
!     Saved variables
!
      save                  first
      save                  idspec

!
!     Initial values
!
      data                  first / .true. /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'EKBSEG' )
      end if

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) then
         call chkout ( 'EKBSEG' )
         return
      end if

!
!     Get the default identifier specification the first time through.
!
      if ( first ) then

         call ssizei ( mxspec, idspec )
         call lxdfid (         idspec )
         first = .false.

      end if

!
!     The table name must not be too long, and all of its characters
!     must be printable (it's ok for it to unprintable).
!
      call chckid ( 'EK table name', namlim, tabnam )

      if ( failed() ) then
         call chkout ( 'EKBSEG' )
         return
      end if

!
!     Make sure the table name satisfies all of our restrictions on
!     allowed characters.
!
      call lxidnt ( idspec, tabnam, 1, idend, nchars )

      if ( ( nchars .eq. 0 ) .or. ( nchars .lt. lastnb(tabnam))  ) then

         call setmsg ( 'Table name <#> violates syntax rules.' )
         call errch  ( '#', tabnam                             )
         call sigerr ( 'SPICE(INVALIDNAME)'                    )
         call chkout ( 'EKBSEG'                                )
         return

      end if

!
!     Check out NCOLS.
!
      if (  ( ncols .lt. 1 ) .or. ( ncols .gt. mxclsg )  ) then

         call setmsg ( 'Number of columns must be in range 1:#, was #.')
         call errint ( '#',  mxclsg                                    )
         call errint ( '#',  ncols                                     )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         call chkout ( 'EKBSEG'                                        )
         return

      end if

!
!     Check the column names for length and printability.
!
      do i = 1, ncols

         call chckid ( 'EK column name', cnamsz, cnames(i) )

         if ( failed() ) then
            call chkout ( 'EKBSEG' )
            return
         end if

!
!        Make sure each column name satisfies all of our restrictions on
!        allowed characters.
!
         call lxidnt ( idspec, cnames(i), 1, idend, nchars )

         if (       ( nchars .eq. 0                    ) &
               .or. ( nchars .lt. lastnb( cnames(i) )  )   ) then

            call setmsg ( 'Column name <#> violates syntax rules.' )
            call errch  ( '#', cnames(i)                           )
            call sigerr ( 'SPICE(INVALIDNAME)'                     )
            call chkout ( 'EKBSEG'                                 )
            return

         end if

      end do

!
!     Parse the column declarations before proceeding.
!
      do i = 1, ncols
!
!        Parse the declaration of the Ith column.  The descriptor is
!        returned with all elements other than pointers initialized.
!
         call zzekpdec ( decls(i), cdscrs(1,i) )

         if ( failed() ) then
            call chkout ( 'EKBSEG' )
            return
         end if

      end do

!
!     Determine the segment type.
!
      stype  =  zzekstyp ( ncols, cdscrs )


!
!     Create the segment metadata according to the segment's type.
!
      if ( stype .eq. 1 ) then

         call zzekbs01 ( handle, tabnam, ncols, cnames, cdscrs, segno )

      else if ( stype .eq. 2 ) then

         call zzekbs02 ( handle, tabnam, ncols, cnames, cdscrs, segno )

      else

         call setmsg ( 'Segment type # is not currently supported.' )
         call errint ( '#', stype                                   )
         call sigerr ( 'SPICE(BUG)'                                 )
         call chkout ( 'EKBSEG'                                     )
         return

      end if


      call chkout ( 'EKBSEG' )
      return
      end
