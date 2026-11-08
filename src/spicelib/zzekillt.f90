!$Procedure      ZZEKILLT ( EK, indirect, last element less than )

      integer(ip) function zzekillt (  handle,  segdsc,  coldsc,  nrows, &
                                   dtype,   cval,    dval,    ival  )
      use pepper_kinds, only: ip, wp

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


      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           nrows
      integer(ip)           dtype
      character(*)          cval
      real(wp)              dval
      integer(ip)           ival

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
      logical               return

!
!     Local variables
!
      real(wp)              dnum

      integer(ip)           coltyp
      integer(ip)           inum
      integer(ip)           rec

!
!     Initialize the function's return value.
!
      zzekillt  =  0

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKILLT' )
      end if

!
!     Validate the number of rows in the column.
!
      if ( nrows < 1 ) then
!
!        There's nobody home---that is, there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         zzekillt = 0
         call setmsg ( 'Number of rows must be positive; was #.' )
         call errint ( '#',  nrows                               )
         call sigerr ( 'SPICE(INVALIDSIZE)'                      )
         call chkout ( 'ZZEKILLT'                                )
         return
      end if

!
!     Hand off the problem to the LLT routine of the correct type.
!
      coltyp  =  coldsc ( typidx )

      if ( coltyp == chr ) then

         call zzeklltc ( handle, segdsc, coldsc, cval, zzekillt, rec )


      else if ( coltyp == dp ) then

         if ( dtype == dp ) then
            dnum = dval
         else
            dnum = ival
         end if

         call zzeklltd ( handle, segdsc, coldsc, dnum, zzekillt, rec )


      else if ( coltyp == time ) then

         call zzeklltd ( handle, segdsc, coldsc, dval, zzekillt, rec )


      else if ( coltyp == int ) then

         if ( dtype == dp ) then
            inum = nint ( dval )
         else
            inum = ival
         end if

         call zzekllti ( handle, segdsc, coldsc, inum, zzekillt, rec )

      else

         call setmsg ( 'The data type # is not supported.' )
         call errint ( '#',  coltyp                        )
         call sigerr ( 'SPICE(INVALIDSIZE)'                )
         call chkout ( 'ZZEKILLT'                          )
         return

      end if

      call chkout ( 'ZZEKILLT' )
      return
      end
