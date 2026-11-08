!$Procedure ZZEKERI1 ( EK, LLE using record pointers, integer, type 1 )

      subroutine zzekeri1 (  handle,  segdsc,  coldsc,  ikey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the last column value less than or equal to a specified key,
!     for a specified integer EK column having a type 1 index, using
!     dictionary ordering on integer data values and record pointers.
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


      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           ikey
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
!     IKEY       I   Integer key.
!     RECPTR     I   Record pointer.
!     NULL       I   Null flag.
!     PRVIDX     O   Ordinal position of predecessor of IKEY.
!     PRVPTR     O   Pointer to record containing predecessor of IKEY.
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
!     IKEY,
!     RECPTR         are, respectively, an integer key and a pointer to
!                    an EK record containing that key. The last column
!                    entry less than or equal to this key is sought.
!                    The order relation used is dictionary ordering on
!                    the pair (IKEY, RECPTR).
!
!     NULL           is a logical flag indicating whether the input
!                    key is null.  When NULL is .TRUE., IKEY is
!                    ignored by this routine.
!
! $ Detailed_Output
!
!     PRVIDX         is the ordinal position, according to the order
!                    relation implied by the column's index, of the
!                    record containing the last element less than or
!                    equal to IKEY, where the order relation is
!                    as indicated above.  If the column contains
!                    elements equal to IKEY, PRVIDX is the index of the
!                    record designated by the input RECPTR.
!
!                    If all elements of the column are greater than
!                    IKEY, PRVIDX is set to zero.
!
!     PRVPTR         is a pointer to the record containing the element
!                    whose ordinal position is PRVIDX.
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
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
!     3)  If the tree is empty, PRVIDX and PRVPTR are set to zero.
!         This case is not considered an error.
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
!     column.  The column must be indexed by a type 1 index.  The order
!     relation used is dictionary ordering on ordered pairs consisting
!     of data values and record pointers:  if the data values in two
!     column entries are equal, the associated record pointers determine
!     the order relation of the column entries.
!
!     Type 1 indexes are implemented as DAS B*-trees.  The data
!     pointers of an index tree contain record pointers.  Therefore, the
!     tree implements an abstract order vector.
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
!     See ZZEKLERI.
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
!        Corrected typo in comments.
!
!-    Beta Version 1.1.0, 07-FEB-1997 (NJB)
!
!        Errors in comparisons of items of equal value were fixed.
!        In such cases, items are compared according to order of
!        their record pointers.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Non-SPICELIB functions
!
      integer(ip)           zzektrsz

      logical               zzekscmp

!
!     Local variables
!
      integer(ip)           begin
      integer(ip)           begptr
      integer(ip)           end
      integer(ip)           endptr
      integer(ip)           middle
      integer(ip)           midptr
      integer(ip)           nrec
      integer(ip)           tree
      integer(ip)           tsize

      logical               leq

!
!     Use discovery check-in.
!
      if ( failed() ) return

!
!     Make sure the number of records in the segment is at least as
!     large as the number of entries in the index:  we must not look
!     up any entries that don't exist!
!
      tree   =  coldsc   ( ixpidx )
      tsize  =  zzektrsz ( handle, tree )
      nrec   =  segdsc   ( nridx  )

      if ( tsize > nrec ) then

         call setmsg ( 'Index size = # but column contains # records.' )
         call errint ( '#',  tsize                                     )
         call errint ( '#',  nrec                                      )
         call sigerr ( 'SPICE(SIZEMISMATCH)'                           )
         return

      end if

!
!     Handle the case of an empty tree gracefully.
!
      if ( tsize == 0 ) then

         prvidx = 0
         prvptr = 0

         return

      end if

!
!     The algorithm used here is very like unto that used in LSTLED.
!
      begin   =   1
      end     =   tsize

!
!     Get the record pointers BEGPTR and ENDPTR of the least and
!     greatest elements in the column.
!
      call zzektrdp ( handle, tree, begin, begptr )
      call zzektrdp ( handle, tree, end,   endptr )

!
!     Compare the input value to the smallest value in the column.
!
      if  (  zzekscmp ( gt,      handle,  segdsc,  coldsc, &
                        begptr,  1,       int,     ' ', &
                        0.0_wp,    ikey,    null            )  ) then
!
!        The smallest entry of the column is greater than the input
!        value, so none of the entries are less than or equal to the
!        input value.
!
         prvidx = 0
         prvptr = 0

         return


      else if  (  zzekscmp ( eq,      handle,  segdsc,  coldsc, &
                             begptr,  1,       int,     ' ', &
                             0.0_wp,    ikey,    null            ) &

                .and.      (  recptr  <  begptr )             ) then
!
!        The smallest entry of the column is greater than the input
!        value, based on a comparison of record pointers, so none of the
!        entries are less than or equal to the input value.
!
         prvidx = 0
         prvptr = 0

         return

      end if

!
!     At this point, we know the input value is greater than or equal
!     to the smallest element of the column.
!
!     Compare the input value to the greatest value in the column.
!
      if (  zzekscmp ( lt,      handle,  segdsc,   coldsc, &
                       endptr,  1,       int,      ' ', &
                       0.0_wp,    ikey,    null             )  ) then
!
!        The last element of the column is less than the
!        input value.
!
         prvidx  =  tsize

         call zzektrdp ( handle, tree, prvidx, prvptr )

         return


      else if (  zzekscmp ( eq,      handle,  segdsc,  coldsc, &
                            endptr,  1,       int,     ' ', &
                            0.0_wp,    ikey,    null             ) &

               .and.      (  endptr  <=  recptr )       ) then
!
!        The last element of the column is less than or equal to the
!        input value, based on a comparison of record pointers.
!
         prvidx  =  tsize
         prvptr  =  endptr

         return

      end if

!
!     The input value lies between some pair of column entries.
!     The value is greater than or equal to the smallest column entry
!     and less than the greatest entry, according to the dictionary
!     ordering we're using.
!
!     Below, we'll use the variable LEQ to indicate whether the "middle"
!     element in our search is less than or equal to the input value.
!
      do while ( end > begin+1 )
!
!        Find the record pointer of the element whose ordinal position
!        is halfway between BEGIN and END.
!
         middle   =  ( begin  + end )  / 2

         call zzektrdp ( handle, tree, middle, midptr )

!
!        Determine the order relation between IKEY and the column
!        entry at record MIDPTR.
!
         if (  zzekscmp ( lt,      handle,  segdsc,  coldsc, &
                          midptr,  1,       int,     ' ' , &
                          0.0_wp,    ikey,    null            )  ) then
!
!           The column element at record MIDPTR is strictly less than
!           IKEY, based on data values.
!
            leq  =  .true.


         else if ( zzekscmp ( eq,      handle,  segdsc,  coldsc, &
                              midptr,  1,       int,     ' ', &
                              0.0_wp,    ikey,    null           ) ) then
!
!           The column entry's value matches IKEY.  We must
!           compare record pointers at this point.
!
            leq  =  midptr <= recptr

         else
!
!           The inequality of data values is strict.
!
            leq  = .false.

         end if


         if ( leq ) then
!
!           The middle value is less than or equal to the input
!           value.
!
            begin = middle
         else
            end   = middle
         end if
!
!        The input value is greater than or equal to the element
!        having ordinal position BEGIN and strictly less than the
!        element having ordinal position END.
!
      end do


      prvidx = begin
      call zzektrdp ( handle, tree, prvidx, prvptr )


      return
      end
