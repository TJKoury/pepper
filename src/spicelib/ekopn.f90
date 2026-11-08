!$Procedure EKOPN ( EK, open new file )

      subroutine ekopn ( fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Open a new E-kernel file and prepare the file for writing.
!
! $ Required_Reading
!
!     EK
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     EK
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      include 'ektype.inc'
      include 'ekfilpar.inc'

      character(*)          fname
      character(*)          ifname
      integer(ip)           ncomch
      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of EK file.
!     IFNAME     I   Internal file name.
!     NCOMCH     I   The number of characters to reserve for comments.
!     HANDLE     O   Handle attached to new EK file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new E-kernel file to be created.
!
!     IFNAME   is the internal file name of a new E-kernel. The
!              internal file name may be up to 60 characters in
!              length.
!
!     NCOMCH   is the amount of space, measured in characters, to
!              be allocated in the comment area when the new EK
!              file is created. It is not necessary to allocate
!              space in advance in order to add comments, but
!              doing so may greatly increase the efficiency with
!              which comments may be added. Making room for
!              comments after data has already been added to the
!              file involves moving the data, and thus is slower.
!
!              NCOMCH must be greater than or equal to zero.
!
! $ Detailed_Output
!
!     HANDLE   is the EK handle of the file designated by FNAME.
!              This handle is used to identify the file to other
!              EK routines.
!
! $ Parameters
!
!     FTSIZE   is the maximum number of DAS files that a user can
!              have open simultaneously. This includes any files used
!              by the DAS system.
!
!              See the include file das.inc for the actual value of
!              this parameter.
!
! $ Exceptions
!
!     1)  If NCOMCH is less than zero, the error SPICE(INVALIDCOUNT)
!         is signaled. No file will be created.
!
!     2)  If IFNAME is invalid, an error is signaled by a routine in the
!         call tree of this routine.
!
!     3)  If the indicated file cannot be opened, an error is signaled
!         by a routine in the call tree of this routine. The new file
!         will be deleted.
!
!     4)  If an I/O error occurs while reading or writing the indicated
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
!     This routine operates by side effects: it opens and prepares
!     an EK for addition of data.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to open a new EK file, creating
!        the two segments described below, without any records.
!
!        The EK file will contain two segments, the first containing
!        the DATAORDERS table and the second containing the DATAITEMS
!        table.
!
!        The E-kernel DATAORDERS table called consists of the set of
!        columns listed below:
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
!        The columns of the DATAITEMS table are shown below:
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
!        Note that it is not necessary to populate the first segment
!        with data before starting the second segment, or before
!        closing the EK.
!
!
!        Example code begins here.
!
!
!              PROGRAM EKOPN_EX1
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
!              PARAMETER           ( EKNAME  = 'ekopn_ex1.bdb' )
!
!              INTEGER               DECLEN
!              PARAMETER           ( DECLEN = 200 )
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
!              CHARACTER*(NAMLEN)    IFNAME
!
!              INTEGER               HANDLE
!              INTEGER               NRESVC
!              INTEGER               SEGNO
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
!
!        C
!        C     Start the first segment. Since we have no data for this
!        C     segment, start the segment by just defining the new
!        C     segment's schema.
!        C
!              CALL EKBSEG ( HANDLE, 'DATAORDERS', 6,
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
!        C     Close the file by a call to EKCLS.
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
!     1)  No more than FTSIZE DAS files may be opened simultaneously.
!         See the include file das.inc for the value of FTSIZE.
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
!        Edited the header to comply with NAIF standard.
!        Added complete code example, and updated $Restrictions and
!        $Parameters sections.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     open new E-kernel
!     open new EK
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return


!
!     Local parameters
!
      integer(ip)           nwc
      parameter           ( nwc    = 1024 )

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           ncr
      integer(ip)           p

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Check the comment character count.
!
      if ( ncomch < 0 ) then

         call setmsg ( 'The number of reserved comment characters ' // &
                       'must be non-negative but was #.'            )
         call errint ( '#',  ncomch                                 )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     A new DAS file is a must.  The file type is EK.
!     Reserve enough comment records to accommodate the requested
!     number of comment characters.
!
      ncr  =  ( nwc + ncomch - 1 ) / nwc

      call dasonw ( fname, 'EK', ifname, ncr, handle )

      if ( failed() ) return

!
!     Initialize the file for paged access.  The EK architecture
!     code is automatically set by the paging initialization routine.
!
      call zzekpgin ( handle )

      if ( failed() ) return

!
!     Allocate the first integer page for the file's metadata.  We
!     don't need to examine the page number; it's 1.
!
      call zzekpgan ( handle, int, p, base )

!
!     Initialize a new tree.  This tree will point to the file's
!     segments.
!
      call zzektrit ( handle, p )

!
!     Save the segment pointer's root page number.
!
      call dasudi ( handle, base+sgtidx, base+sgtidx, p )

!
!     That's it.  We're ready to add data to the file.
!
      return
      end
