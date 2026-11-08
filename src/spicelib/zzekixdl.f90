!$Procedure     ZZEKIXDL ( EK, delete record from index )

      subroutine zzekixdl ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update an EK column index to reflect deletion of a record
!     specified by a record pointer.
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
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           coldsc ( cdscsz )
      integer(ip)           recptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record pointer to locate.
!
! $ Detailed_Input
!
!     HANDLE         is a handle of an EK file open for write access.
!
!     SEGDSC         is the segment descriptor of the segment
!                    containing the column to be searched.
!
!     COLDSC         is the column descriptor of the column to be
!                    searched.
!
!     RECPTR         is a pointer to a record whose corresponding
!                    index entry is to be deleted.
!
! $ Detailed_Output
!
!     None.  This routine operates by side effects.  See $Particulars
!     for a discussion of the action of this routine.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine supports EK update or delete operations, both of
!     which involve removing pointers to records from column indexes.
!
! $ Examples
!
!     See ZZEKDE01.
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
      integer(ip)           idx
      integer(ip)           idxtyp
      integer(ip)           recno
      integer(ip)           tree

!
!     Standard SPICE error handling.
!
      if ( return () ) return

      idxtyp  =  coldsc ( ixtidx )

      if ( idxtyp /= ifalse ) then
!
!        This column is indexed.
!
!        Some entry in the index points to RECPTR.  Find the entry
!        and delete it.
!
         call zzekfrx ( handle, segdsc, coldsc, recptr, idx )

         if ( idxtyp == 1 ) then
!
!           For type 1 indexes, the index pointer is the root node of
!           a B*-tree.  Just use the tree deletion routine.
!
            tree  = coldsc ( ixpidx )

            call zzektrdl ( handle, tree, idx )

         else
!
!           Sorry, no other types of indexes are supported.
!
            call setmsg ( 'The index type # is not supported.' )
            call errint ( '#',   idxtyp                        )
            call sigerr ( 'SPICE(INVALIDTYPE)'                 )
            return

         end if

      else
!
!        This routine should not have been called if the column in
!        question is not indexed.
!
         recno  =  zzekrp2n ( handle, segdsc(snoidx), recptr )

         call setmsg ( 'Column was not indexed. File = #; ' // &
                       'RECNO = #; COLIDX = #.'              )
         call errhan ( '#',  handle                          )
         call errint ( '#',  recno                           )
         call errint ( '#',  coldsc(ordidx)                  )
         call sigerr ( 'SPICE(BUG)'                          )
         return

      end if

      return
      end
