!$Procedure ZZEKLLEI ( EK, last less than or equal to, integer )

      subroutine zzekllei (  handle,  segdsc,  coldsc, &
                             ikey,    prvloc,  prvptr  )

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified, indexed integer EK column.
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
      include 'ekopcd.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      integer               ikey
      integer               prvloc
      integer               prvptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     IKEY       I   Integer key.
!     PRVLOC     O   Ordinal position of predecessor of IKEY.
!     PRVPTR     O   Pointer to a record containing predecessor of IKEY.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column specified by COLDSC.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     IKEY           is an integer key.  The last column entry
!                    less than or equal to this key is sought.
!
! $ Detailed_Output
!
!     PRVLOC         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to IKEY.  If the column contains elements
!                    equal to IKEY, PRVLOC is the index of the last
!                    such element.
!
!                    If all elements of the column are greater than
!                    IKEY, PRVLOC is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVLOC.
!
!                    If all elements of the column are greater than
!                    IKEY, PRVPTR is set to zero.
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
!         the error will be diagnosed by routines called by this
!         routine.
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
!     to a specified integer key, within a specified segment and
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
!     See ZZEKILLE.
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
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      logical               zzekscmp

!
!     Local variables
!
      character*(cnamsz)    column

      integer               begin
      integer               begptr
      integer               dtype
      integer               end
      integer               endptr
      integer               middle
      integer               midptr
      integer               nrows

      logical               indexd

!
!     Use discovery check-in.
!
!     If the column's not indexed, we have no business being here.
!
      indexd  =  coldsc(ixtidx) .ne. ifalse

      if ( .not. indexd ) then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKLLEI'                 )
         call setmsg ( 'Column # is not indexed.' )
         call errch  ( '#',  column               )
         call sigerr ( 'SPICE(NOTINDEXED)'        )
         call chkout ( 'ZZEKLLEI'                 )
         return

      end if

!
!     Check the column's data type.
!
      dtype  =  coldsc(typidx)

      if ( dtype .ne. int ) then

         call zzekcnam ( handle, coldsc, column )

         call chkin  ( 'ZZEKLLEI'                               )
         call setmsg ( 'Column # should be INT but has type #.' )
         call errch  ( '#',  column                             )
         call errint ( '#',  dtype                              )
         call sigerr ( 'SPICE(INVALIDTYPE)'                     )
         call chkout ( 'ZZEKLLEI'                               )
         return

      end if

!
!     Handle the case of an empty segment gracefully.
!
      nrows  =  segdsc(nridx)

      if ( nrows .eq. 0 ) then

         prvloc = 0
         prvptr = 0

         return

      end if

!
!     The algorithm used here is very like unto that used in LSTLED.
!
      begin   =   1
      end     =   nrows

!
!     Get the record pointers BEGPTR and ENDPTR of the least and
!     greatest elements in the column.
!
      call zzekixlk ( handle, coldsc, begin, begptr )
      call zzekixlk ( handle, coldsc, end,   endptr )


      if  (  zzekscmp ( gt,      handle,  segdsc, &
                        coldsc,  begptr,  1,      int, &
                        ' ',     0.d0,    ikey,  .false. )  ) then
!
!        The smallest entry of the column is greater than
!        the input value, so none of the entries
!        are less than or equal to the input value.
!
         prvloc = 0
         prvptr = 0


      else if (  zzekscmp ( le,      handle,  segdsc, &
                            coldsc,  endptr,  1,      int, &
                            ' ',     0.d0,    ikey,  .false. )  ) then
!
!        The last element of the array is less than or equal to the
!        input value.
!
         prvloc = nrows

         call zzekixlk ( handle, coldsc, prvloc, prvptr )


      else
!
!        The input value lies between some pair of column entries.
!        The value is greater than or equal to the smallest column entry
!        and less than the greatest entry.
!
         do while ( end .gt. begin+1 )
!
!           Find the address of the element whose ordinal position
!           is halfway between BEGIN and END.
!
            middle   =  ( begin  + end )  / 2

            call zzekixlk ( handle, coldsc, middle, midptr )


            if (  zzekscmp ( le,      handle,  segdsc, &
                             coldsc,  midptr,  1,       int, &
                             ' ' ,    0.d0,    ikey,   .false. )  ) then
!
!              The middle value is less than or equal to the input
!              value.
!
               begin = middle
            else
               end   = middle
            end if
!
!           The input value is greater than or equal to the element
!           having ordinal position BEGIN and strictly less than the
!           element having ordinal position END.
!
         end do

         prvloc = begin
         call zzekixlk ( handle, coldsc, prvloc, prvptr )

      end if

      return
      end
