!$Procedure ZZEKSZ05 ( EK, element entry size, class 5 )

      INTEGER FUNCTION ZZEKSZ05 ( HANDLE, SEGDSC, COLDSC, RECPTR )

! $ Abstract
!
!     Return the size of a specified entry in a class 5 column.
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
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               RECPTR
      INTEGER               COLDSC ( CDSCSZ )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!
!     The function returns the number of elements in the specified
!     column entry.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column containing
!                    the entry whose size is requested.  The column
!                    must be class 5.
!
!     RECPTR         is a pointer to the EK record containing the
!                    column entry of interest.
!
! $ Detailed_Output
!
!     The function returns the number of elements in the specified
!     column entry.
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
!     2)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     3)  If the column index contained in the input column descriptor
!         is out of range, the error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This utility supports the commonly performed function of
!     determining the element count of a column entry.
!
! $ Examples
!
!     See ZZEKESIZ.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      DPCNT

      INTEGER               COLIDX
      INTEGER               DATPTR
      INTEGER               NCOLS
      INTEGER               NREC
      INTEGER               PTRLOC

!
!     Use discovery check-in.
!
!     Initialize the function's return value.
!
      ZZEKSZ05  =  0

      NREC    =  SEGDSC ( NRIDX  )
      COLIDX  =  COLDSC ( ORDIDX )

!
!     Make sure the column exists.
!
      NCOLS  =  SEGDSC ( NCIDX )

      IF (  ( COLIDX .LT. 1 ) .OR. ( COLIDX .GT. NCOLS )  ) THEN

         CALL CHKIN  ( 'ZZEKSZ05'                              )
         CALL SETMSG ( 'Column index = #; valid range is 1:#.' )
         CALL ERRINT ( '#',  COLIDX                            )
         CALL ERRINT ( '#',  NREC                              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
         CALL CHKOUT ( 'ZZEKSZ05'                              )
         RETURN

      END IF

!
!     If the column has fixed-size entries, just return the declared
!     size.
!
      IF ( COLDSC(SIZIDX) .NE. IFALSE ) THEN

         ZZEKSZ05  =  COLDSC(SIZIDX)
         RETURN

      END IF

!
!     Compute the data pointer location.  Read the data pointer.
!
      PTRLOC  =  RECPTR + DPTBAS + COLIDX

      CALL DASRDI ( HANDLE, PTRLOC, PTRLOC, DATPTR )


      IF ( DATPTR .LT. 1 ) THEN
!
!        The value is null.  Null entries are always considered to have
!        size 1.
!
         ZZEKSZ05  =  1

      ELSE
!
!        DATPTR points to the element count.
!
         CALL DASRDD ( HANDLE, DATPTR, DATPTR, DPCNT )

         ZZEKSZ05  =  NINT ( DPCNT )

      END IF

      RETURN
      END
