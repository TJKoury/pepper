!$Procedure ZZEKLERC  ( EK, LLE, using record pointers, character  )

      subroutine zzeklerc  (  handle,  segdsc,  coldsc,  ckey, &
                              recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified, indexed character EK column, using dictionary
!     ordering on character data values and record pointers.
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


      include 'ekbool.inc'
      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      character(*)          ckey
      integer(ip)           recptr
      logical               null
      integer(ip)           prvidx
      integer(ip)           prvptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     CKEY       I   Double precision key.
!     RECPTR     I   Record pointer.
!     NULL       I   Null flag.
!     PRVIDX     O   Ordinal position of predecessor of CKEY.
!     PRVPTR     O   Record pointer for predecessor of CKEY.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK that is open for read or
!                    write access.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     CKEY,
!     RECPTR         are, respectively, a character key and a pointer
!                    to the EK record containing that key.
!                    The last column entry less than or equal to
!                    this key is sought.  The order relation used
!                    is dictionary ordering on the pair (CKEY, RECPTR).
!
!     NULL           is a logical flag indicating whether the input
!                    key is null.  When NULL is .TRUE., CKEY is
!                    ignored by this routine.
!
! $ Detailed_Output
!
!     PRVIDX         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to CKEY, where the order relation is
!                    as indicated above.  If the column contains
!                    elements equal to CKEY, PRVIDX is the index of the
!                    last such element.
!
!                    If all elements of the column are greater than
!                    CKEY, PRVIDX is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVIDX.
!
!                    If all elements of the column are greater than
!                    CKEY, PRVPTR is set to zero.
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
!     2)  If the data type of the input column is not character,
!         the error SPICE(INVALIDTYPE) is signaled.
!
!     3)  If the input column is not indexed, the error
!         SPICE(NOTINDEXED) is signaled.
!
!     4)  If the index type of the input column is not recognized,
!         the error SPICE(INVALIDTYPE) is signaled.
!
!     5)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine finds the last column element less than or equal
!     to a specified character key, within a specified segment and
!     column.
!
!     In order to support the capability of creating an index for a
!     column that has already been populated with data, this routine
!     does not require that number of elements referenced by the
!     input column's index match the number of elements in the column;
!     the index is allowed to reference fewer elements.  However,
!     every record referenced by the index must be populated with data.
!
! $ Examples
!
!     See ZZEKIIXC.
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed an unbalanced call to CHKOUT.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      character(cnamsz)     column

      integer(ip)           dtype
      integer(ip)           itype

      logical               indexd

!
!     Use discovery check-in.
!
      if ( failed() ) then
         return
      end if

!
!     If the column's not indexed, we have no business being here.
!
      indexd  =  coldsc(ixtidx) /= ifalse

      if ( .not. indexd ) then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKLERC '                )
         call setmsg ( 'Column # is not indexed.' )
         call errch  ( '#',  column               )
         call sigerr ( 'SPICE(NOTINDEXED)'        )
         call chkout ( 'ZZEKLERC '                )
         return

      end if

!
!     Check the column's data type.
!
      dtype  =  coldsc(typidx)

      if ( dtype /= chr ) then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKLERC '                              )
         call setmsg ( 'Column # should be CHR but has type #.' )
         call errch  ( '#',  column                             )
         call errint ( '#',  dtype                              )
         call sigerr ( 'SPICE(INVALIDTYPE)'                     )
         call chkout ( 'ZZEKLERC '                              )
         return

      end if

!
!     Hand the problem off to the subroutine that understands this
!     column's index type.
!
      itype  =  coldsc(ixtidx)

      if ( itype == 1 ) then

         call zzekerc1 (  handle,  segdsc,  coldsc,  ckey, &
                          recptr,  null,    prvidx,  prvptr  )

      else

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKLERC '                  )
         call setmsg ( 'Column # has index type #.' )
         call errch  ( '#',  column                 )
         call errint ( '#',  itype                  )
         call sigerr ( 'SPICE(INVALIDTYPE)'         )
         call chkout ( 'ZZEKLERC '                  )
         return

      end if

      return
      end
