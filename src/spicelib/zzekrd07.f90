!$Procedure   ZZEKRD07 ( EK, read class 7 column entry )

      SUBROUTINE ZZEKRD07 (  HANDLE,  SEGDSC,  COLDSC, &
                             RECPTR,  IVAL,    ISNULL  )

! $ Abstract
!
!     Read a column entry from a specified record in a class 7 column.
!     Class 7 columns contain fixed-count, scalar integer values.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekclas07.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               IVAL
      LOGICAL               ISNULL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     IVAL       O   Integer value in column entry.
!     ISNULL     O   Flag indicating whether column entry is null.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.
!
!     SEGDSC         is the descriptor of the segment from which data is
!                    to be read.
!
!     COLDSC         is the descriptor of the column from which data is
!                    to be read.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to be written.  For class 7 columns, record
!                    pointers are identical to record numbers.
!
! $ Detailed_Output
!
!     IVAL           is the value read from the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the column
!                    entry is null.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If the ordinal position of the column specified by COLDSC
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
!     3)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility for reading data from class 7 columns.
!
! $ Examples
!
!     See EKRCEI.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Bug fix: changed max column index in long error
!        message from NREC to NCOLS.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N

!
!     Local variables
!
      CHARACTER*(1)         CFLAG

      INTEGER               ADDRSS
      INTEGER               COLIDX
      INTEGER               DATBAS
      INTEGER               MDAT   ( MDSZ07 )
      INTEGER               METLOC
      INTEGER               NCOLS
      INTEGER               NFLBAS
      INTEGER               OFFSET
      INTEGER               Q
      INTEGER               R
      INTEGER               RECNO

      LOGICAL               NULLOK

!
!     Use discovery check-in.
!
!
!     Make sure the column exists.
!
      NCOLS   =  SEGDSC ( NCIDX  )
      COLIDX  =  COLDSC ( ORDIDX )
      METLOC  =  COLDSC ( METIDX )
      NULLOK  =  COLDSC ( NFLIDX ) .EQ. ITRUE


      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )

         CALL CHKIN  ( 'ZZEKRD07'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NCOLS                             )
         CALL ERRINT ( '#',  SEGDSC(SNOIDX)                    )
         CALL ERRINT ( '#',  RECNO                             )
         CALL ERRHAN ( '#',  HANDLE                            )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKRD07'                              )
         RETURN

      END IF

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      CALL DASRDI ( HANDLE, METLOC+1, METLOC+MDSZ07, MDAT )

      DATBAS = MDAT ( DBIX07 )
      NFLBAS = MDAT ( NFIX07 )

!
!     If null values are permitted, the first step is to get
!     the null flag for the value of interest.  Compute the
!     address of this flag.
!
!     There are CPSIZE null flags per page, and each page has size
!     PGSIZC.  The null flags start at the beginning of the page.
!
      IF ( NULLOK ) THEN

         Q       =  (RECPTR-1) / CPSIZE
         R       =  RECPTR - Q * CPSIZE
         OFFSET  =  R      + Q * PGSIZC
         ADDRSS  =  NFLBAS + OFFSET

         CALL DASRDC ( HANDLE, ADDRSS, ADDRSS, 1, 1, CFLAG )

         ISNULL  =  CFLAG .EQ. CTRUE

         IF ( ISNULL ) THEN
            RETURN
         END IF

      END IF

!
!     If we're still here, we'll read the data value.
!
      ISNULL  =  .FALSE.

!
!     The address calculation for the value is analogous to that
!     for the null flag.
!
      Q       =  (RECPTR-1)  / IPSIZE
      R       =  RECPTR - Q * IPSIZE
      OFFSET  =  R      + Q * PGSIZI
      ADDRSS  =  DATBAS + OFFSET

      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, IVAL )

      RETURN
      END
