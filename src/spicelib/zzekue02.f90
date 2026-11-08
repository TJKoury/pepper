!$Procedure      ZZEKUE02 ( EK, update column entry, class 2 )

      subroutine zzekue02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )

! $ Abstract
!
!     Update a specified class 2 column entry in an EK record.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations


      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekdatpag.inc'
      include 'ekrecptr.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      integer               recptr
      double precision      dval
      logical               isnull

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer.
!     DVAL       I   Double precision value.
!     ISNULL     I   Null flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment containing
!                    the specified column entry.
!
!     COLDSC         is the descriptor of the column containing
!                    the specified column entry.
!
!     RECPTR         is a pointer to the record containing the column
!                    entry to update.
!
!     DVAL           is the double precision value with which to update
!                    the specified column entry.
!
!     ISNULL         is a logical flag indicating whether the value
!                    of the specified column entry is to be set to NULL.
!                    If so, the input DVAL is ignored.
!
! $ Detailed_Output
!
!     None.  See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it updates a column entry
!     in an EK segment.  This routine does not participate in shadowing
!     functions.  If the target EK is shadowed, the caller is
!     responsible for performing necessary backup operations.  If the
!     target EK is not shadowed, the target record's status is not
!     modified.
!
!     If the column containing the entry is indexed, the corresponding
!     index is updated.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKUCED.
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
!-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Deleted unnecessary code and variable declarations.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed redundant calls to CHKIN.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Non-SPICELIB functions
!
      integer               zzekrp2n

!
!     Local variables
!
      integer               datptr
      integer               idxtyp
      integer               nlinks
      integer               p
      integer               pbase
      integer               ptrloc
      integer               recno

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKUE02' )

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) then
         call chkout ( 'ZZEKUE02' )
         return
      end if

!
!     Compute the data pointer location.
!
      ptrloc  =  recptr + dptbas + coldsc(ordidx)

      call dasrdi ( handle, ptrloc, ptrloc, datptr )


      if ( datptr .gt. 0 ) then
!
!        The column entry is non-null.  Determine whether the column is
!        indexed.
!
         idxtyp  =  coldsc(ixtidx)

         if ( idxtyp .eq. 1 ) then
!
!           The column has a type 1 index.  Delete the index entry
!           for this column.  Create an index entry for the new value.
!
            call zzekixdl ( handle,  segdsc,  coldsc,  recptr )

            call zzekiid1 ( handle,  segdsc,  coldsc,  dval, &
                            recptr,  isnull                  )

         else if ( idxtyp .ne. ifalse ) then

            call setmsg ( 'Column having index # in segment # has '   // &
                          'index type #.'                             )
            call errint ( '#',  coldsc(ordidx)                        )
            call errint ( '#',  segdsc(snoidx)                        )
            call errint ( '#',  idxtyp                                )
            call sigerr ( 'SPICE(INVALIDTYPE)'                        )
            call chkout ( 'ZZEKUE02'                                  )
            return

         end if

!
!        If the new value is null, set the data pointer to indicate a
!        null value.  Otherwise, overwrite the old value with the new
!        one.
!
         if ( isnull ) then
!
!           The data location used by the previous value is no longer
!           needed, so we have one less link to this page.
!
            call zzekpgpg ( dp,     datptr, p,      pbase    )
            call zzekglnk ( handle, dp,     p,      nlinks   )
            call zzekslnk ( handle, dp,     p,      nlinks-1 )

            call dasudi   ( handle, ptrloc, ptrloc, null     )

         else
!
!           No link counts change; we just have a new value.
!
            call dasudd ( handle, datptr, datptr, dval )

         end if


      else if ( datptr .eq. null ) then
!
!        If the new entry is null too, there's nothing to do.
!        We don't have to adjust link counts or indexes.
!
!        If the new entry is non-null, we must add a new column entry,
!        since no space was reserved for the old one.  The column
!        index entry must be cleaned up, if the column is indexed.
!
         if ( .not. isnull ) then

            idxtyp  =  coldsc(ixtidx)

            if ( idxtyp .eq. 1 ) then
!
!              The column has a type 1 index.  Delete the index entry
!              for this column.
!
               call zzekixdl ( handle,  segdsc,  coldsc, recptr )


            else if ( idxtyp .ne. ifalse ) then

               call setmsg ( 'Column having index # in segment # has '// &
                             'index type #.'                          )
               call errint ( '#',  coldsc(ordidx)                     )
               call errint ( '#',  segdsc(snoidx)                     )
               call errint ( '#',  idxtyp                             )
               call sigerr ( 'SPICE(INVALIDTYPE)'                     )
               call chkout ( 'ZZEKUE02'                               )
               return

            end if

!
!           We don't need to decrement the link count for this page.
!           Just add the new value to the column.  But first, set the
!           data pointer to indicate an uninitialized value, so the
!           data addition routine doesn't choke.
!
            call dasudi   (  handle,  ptrloc,  ptrloc,  uninit )

            call zzekad02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )

         end if


      else if (      ( datptr .eq. uninit ) &
                .or. ( datptr .eq. noback )  ) then
!
!        There is no current column entry.  Just add a new entry.
!
         call zzekad02 ( handle, segdsc, coldsc, recptr, dval, isnull )


      else
!
!        The data pointer is corrupted.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call errint ( '#',  segdsc(snoidx)                      )
         call errint ( '#',  coldsc(ordidx)                      )
         call errint ( '#',  recno                               )
         call errhan ( '#',  handle                              )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKUE02'                                )
         return

      end if


      call chkout ( 'ZZEKUE02' )
      return
      end
