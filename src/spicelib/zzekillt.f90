!$Procedure      ZZEKILLT ( EK, indirect, last element less than )

      INTEGER FUNCTION ZZEKILLT (  HANDLE,  SEGDSC,  COLDSC,  NROWS, &
                                   DTYPE,   CVAL,    DVAL,    IVAL  )

! $ Abstract
!
!     Find the ordinal position of the row, in an specified EK segment,
!     whose value in a specified column is the last last element less
!     than a specified value, where the order relation is given by an
!     order vector in a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations


      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               NROWS
      INTEGER               DTYPE
      CHARACTER*(*)         CVAL
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   HANDLE of EK file.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Descriptor of column to be searched.
!     NROWS      I   Number of rows in column.
!     DTYPE      I   Data type of input value.
!     CVAL       I   Character string value.
!     DVAL       I   Double precision value.
!     IVAL       I   Integer value.
!
!     The function returns the index of the last order vector element
!     that points to an array element that is less than the input
!     value of the same data type as the column.
!
! $ Detailed_Input
!
!     HANDLE         is the file handle of the EK containing the
!                    segment of interest.
!
!     SEGDSC         is the segment descriptor of the EK
!                    segment of interest.
!
!     COLDSC         is a column descriptor for the column whose
!                    entries are to be compared with an input scalar
!                    value.  The column must be indexed.
!
!     NROWS          is the number of rows in the segment of interest.
!
!     DTYPE          is the data type of the input scalar value.
!
!     CVAL,
!     DVAL,
!     IVAL           are a set of scalar variables of character,
!                    double precision, and integer type.  Whichever
!                    of these has the same data type as the column
!                    indicated by COLDSC is used to compare rows
!                    against.  If COLDSC has data type TIME, DVAL
!                    is used in the comparison.
!
! $ Detailed_Output
!
!     The function returns the index of the last order vector element
!     that points to a column entry that is less than whichever of
!     CVAL, DVAL, or IVAL has the same data type as the input column.
!     If the least element of the column is greater than the input
!     value of the matching type, the function returns the value zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the array size NROWS is non-positive, the error
!         SPICE(INVALIDSIZE) will be signaled.
!
!     2)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs during any access to the file
!         specified by HANDLE, the error will be diagnosed by routines
!         called by this routine.
!
!     4)  If any of SEGDSC, COLDSC, or NROWS are invalid, this routine
!         may fail in unpredictable, but possibly spectacular, ways.
!         Except as described in this header section, no attempt is
!         made to handle these errors.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine supports allow rapid look-up of elements in indexed
!     EK columns.
!
! $ Examples
!
!     See ZZEKKEY.
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
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DNUM

      INTEGER               COLTYP
      INTEGER               INUM
      INTEGER               REC

!
!     Initialize the function's return value.
!
      ZZEKILLT  =  0

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKILLT' )
      END IF

!
!     Validate the number of rows in the column.
!
      IF ( NROWS .LT. 1 ) THEN
!
!        There's nobody home---that is, there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         ZZEKILLT = 0
         CALL SETMSG ( 'Number of rows must be positive; was #.' )
         CALL ERRINT ( '#',  NROWS                               )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                      )
         CALL CHKOUT ( 'ZZEKILLT'                                )
         RETURN
      END IF

!
!     Hand off the problem to the LLT routine of the correct type.
!
      COLTYP  =  COLDSC ( TYPIDX )

      IF ( COLTYP .EQ. CHR ) THEN

         CALL ZZEKLLTC ( HANDLE, SEGDSC, COLDSC, CVAL, ZZEKILLT, REC )


      ELSE IF ( COLTYP .EQ. DP ) THEN

         IF ( DTYPE .EQ. DP ) THEN
            DNUM = DVAL
         ELSE
            DNUM = IVAL
         END IF

         CALL ZZEKLLTD ( HANDLE, SEGDSC, COLDSC, DNUM, ZZEKILLT, REC )


      ELSE IF ( COLTYP .EQ. TIME ) THEN

         CALL ZZEKLLTD ( HANDLE, SEGDSC, COLDSC, DVAL, ZZEKILLT, REC )


      ELSE IF ( COLTYP .EQ. INT ) THEN

         IF ( DTYPE .EQ. DP ) THEN
            INUM = NINT ( DVAL )
         ELSE
            INUM = IVAL
         END IF

         CALL ZZEKLLTI ( HANDLE, SEGDSC, COLDSC, INUM, ZZEKILLT, REC )

      ELSE

         CALL SETMSG ( 'The data type # is not supported.' )
         CALL ERRINT ( '#',  COLTYP                        )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                )
         CALL CHKOUT ( 'ZZEKILLT'                          )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZEKILLT' )
      RETURN
      END
