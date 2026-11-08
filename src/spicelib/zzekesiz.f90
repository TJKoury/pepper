!$Procedure ZZEKESIZ ( EK, element entry size )

      INTEGER FUNCTION ZZEKESIZ ( HANDLE, SEGDSC, COLDSC, RECPTR )

! $ Abstract
!
!     Return the size of a specified column entry.
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

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'eksegdsc.inc'

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
!                    the entry whose size is requested.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This utility centralizes the commonly performed function of
!     determining the element count of a column entry.
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
!        long error message.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKRP2N
      INTEGER               ZZEKSZ04
      INTEGER               ZZEKSZ05
      INTEGER               ZZEKSZ06

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN

      INTEGER               CLASS
      INTEGER               RECNO
      INTEGER               SEGNO

!
!     Initialize the function's return value.
!
      ZZEKESIZ  =  0

!
!     Use discovery check-in.
!
!     Delegate the problem to the routine of the appropriate class.
!     The first three classes are scalars.
!
!
      CLASS  =  COLDSC ( CLSIDX )

      IF ( CLASS .EQ. 1 ) THEN

         ZZEKESIZ  =  1


      ELSE IF ( CLASS .EQ. 2 ) THEN

         ZZEKESIZ  =  1


      ELSE IF ( CLASS .EQ. 3 ) THEN

         ZZEKESIZ  =  1


      ELSE IF ( CLASS .EQ. 4 ) THEN

         ZZEKESIZ  =  ZZEKSZ04 ( HANDLE, SEGDSC, COLDSC, RECPTR )


      ELSE IF ( CLASS .EQ. 5 ) THEN

         ZZEKESIZ  =  ZZEKSZ05 ( HANDLE, SEGDSC, COLDSC, RECPTR )


      ELSE IF ( CLASS .EQ. 6 ) THEN

         ZZEKESIZ  =  ZZEKSZ06 ( HANDLE, SEGDSC, COLDSC, RECPTR )


      ELSE IF ( CLASS .EQ. 7 ) THEN

         ZZEKESIZ  =  1


      ELSE IF ( CLASS .EQ. 8 ) THEN

         ZZEKESIZ  =  1


      ELSE IF ( CLASS .EQ. 9 ) THEN

         ZZEKESIZ  =  1


      ELSE

!
!        This is an unsupported column class.
!
         CALL ZZEKCNAM ( HANDLE, COLDSC, COLUMN )

         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         SEGNO  =  SEGDSC ( SNOIDX )

         CALL CHKIN  ( 'ZZEKESIZ'                                      )
         CALL SETMSG ( 'Class # from input column descriptor is not ' // &
                       'a supported integer class.  COLUMN = #; '     // &
                       'RECNO = #; SEGNO = #; EK = #.'                 )
         CALL ERRINT ( '#',  CLASS                                     )
         CALL ERRCH  ( '#',  COLUMN                                    )
         CALL ERRINT ( '#',  RECNO                                     )
         CALL ERRINT ( '#',  SEGNO                                     )
         CALL ERRHAN ( '#',  HANDLE                                    )
         CALL SIGERR ( 'SPICE(NOCLASS)'                                )
         CALL CHKOUT ( 'ZZEKESIZ'                                      )
         RETURN

      END IF


      RETURN
      END
