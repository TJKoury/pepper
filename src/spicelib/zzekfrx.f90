!$Procedure     ZZEKFRX ( EK, find record in index )

      subroutine zzekfrx ( handle, segdsc, coldsc, recptr, pos )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the ordinal position of a specified record in a specified,
!     indexed EK column.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations


      include 'ekcoldsc.inc'
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr
      integer(ip)           pos

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Pointer to record to locate.
!     POS        O   Ordinal position of record.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column to be searched.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     RECPTR         is a pointer to the record whose ordinal position
!                    is to be found.
!
! $ Detailed_Output
!
!     POS            is the ordinal position in the specified column
!                    of the input record, where the order relation is
!                    specified by the column's index.
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
!     2)  If an I/O error occurs while reading the indicated
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
!     Various EK write operations require the capability of locating
!     the index key that maps to a given record number.  An example is
!     updating a column's index to reflect deletion of a specified
!     record:  the key that maps to the record must be deleted.
!     Locating this key is the inverse of the problem that the index
!     is meant to solve.
!
! $ Examples
!
!     See ZZEKIXDL.
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
!-    SPICELIB Version 2.1.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error message.
!
!-    SPICELIB Version 2.0.0, 31-MAY-2010 (NJB)
!
!        Bug fix: substring bound out-of-range violation
!        in reference to local variable CVAL has been
!        corrected. This error could occur if the a
!        class 3 column entry had length exceeding MAXSTR.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekrp2n

!
!     Local variables
!
      character(maxstr)     cval

      real(wp)              dval

      integer(ip)           cmplen
      integer(ip)           cvlen
      integer(ip)           dtype
      integer(ip)           ival
      integer(ip)           prvptr
      integer(ip)           recno

      logical               found
      logical               isnull

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Determine the data type of the column, and look up the value
!     associated with RECPTR.
!
      dtype  =  coldsc ( typidx )

      if ( dtype == chr ) then

         call zzekrsc (  handle,  segdsc,  coldsc,  recptr, &
                         1,       cvlen,   cval,    isnull,  found  )


         if (  found  .and. ( .not. isnull )  ) then

            cmplen = min ( cvlen, maxstr )
         else
            cmplen = 0
         end if


      else if (  ( dtype == dp ) .or. ( dtype == time )  ) then

         call zzekrsd (  handle,  segdsc,  coldsc,  recptr, &
                         1,       dval,    isnull,  found  )


      else if ( dtype == int ) then

         call zzekrsi (  handle,  segdsc,  coldsc,  recptr, &
                         1,       ival,    isnull,  found  )

      else

         call setmsg ( 'File = #; COLIDX = #. Unrecognized data ' // &
                       'type code # found in descriptor.'           )
         call errhan ( '#',     handle                              )
         call errint ( '#',     coldsc(ordidx)                      )
         call errint ( '#',     dtype                               )
         call sigerr ( 'SPICE(ITEMNOTFOUND)'                        )
         return

      end if



      if ( .not. found ) then
!
!        We have a most heinous situation.  We should always be able
!        to find the value associated with a record.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'File = #; RECNO = #; COLIDX = #. Column '     // &
                       'entry was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         call errhan ( '#',     handle                                 )
         call errint ( '#',     recno                                  )
         call errint ( '#',     coldsc(ordidx)                         )
         call sigerr ( 'SPICE(ITEMNOTFOUND)'                           )
         return

      end if

!
!     Find the last column entry less than or equal to the one
!     associated with the input record, where the order relation is
!     dictionary ordering on (<column value>, <record number>) pairs.
!     These ordered pairs are distinct, even if the column entries
!     are not.  Therefore, the ordinal position POS will actually be
!     the ordinal position of our record.
!
      if ( dtype == chr ) then

         call zzeklerc ( handle,  segdsc,  coldsc,  cval(:cmplen), &
                         recptr,  isnull,  pos,     prvptr         )


      else if (  ( dtype == dp ) .or. ( dtype == time )  ) then

         call zzeklerd ( handle,  segdsc,  coldsc,  dval, &
                         recptr,  isnull,  pos,     prvptr  )


      else
!
!        The data type is INT.  (We've already checked for invalid
!        types.)
!

         call zzekleri ( handle,  segdsc,  coldsc,  ival, &
                         recptr,  isnull,  pos,     prvptr  )

      end if


      if ( prvptr /= recptr ) then
!
!        Big problem.  This should never happen.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'File = #; RECNO = #; COLIDX = #.  Record ' // &
                       'that was last less than or equal to RECNO '// &
                       'was not equal to RECNO.  This probably '   // &
                       'indicates  a corrupted file or a bug in '  // &
                       'the EK code.'                               )
         call errhan ( '#',     handle                              )
         call errint ( '#',     recno                               )
         call errint ( '#',     coldsc(ordidx)                      )
         call sigerr ( 'SPICE(ITEMNOTFOUND)'                        )
         return

      end if


      return
      end
