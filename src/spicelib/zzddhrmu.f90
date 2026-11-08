!$Procedure ZZDDHRMU ( Private --- DDH Remove Unit )

      subroutine zzddhrmu ( uindex, nft,   utcst, uthan, &
                            utlck,  utlun, nut           )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Remove an entry from the unit table.
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

      implicit none

      include               'zzddhman.inc'

      integer               uindex
      integer               nft

      integer               utcst ( * )
      integer               uthan ( * )
      logical               utlck ( * )
      integer               utlun ( * )
      integer               nut

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UINDEX     I   Row index to remove from the unit table.
!     NFT        I   Number of entries in the file table.
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN     I/O  Unit table.
!     NUT       I/O  Number of entries in the unit table.
!
! $ Detailed_Input
!
!     HANDLE     is the index of the row in the unit table for the
!                unit to remove.
!
!     NFT        is the number of entries in the file table after
!                the file whose unit is about to be disconnected
!                has been removed.
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the cost, handle, locked, and logical unit columns
!                of the unit table respectively.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the cost, handle, locked, and logical unit columns
!                of the unit table respectively.  The contents will
!                change, for specifics see the Particulars section
!                below.
!
!     NUT        is the number of entries in the unit table.  Depending
!                on the state of the file table, this may or may not
!                change.  See the $Particulars section below for
!                details.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(INDEXOUTOFRANGE) is signaled when the input UINDEX is
!        either greater than NUT or less than 1.
!
!     2) If NUT is 0 on input, then this module simply returns.
!
! $ Particulars
!
!     This routine only manipulates the contents of the unit table.
!     It is utilized to delete an entry in the unit table that is
!     the result of a file 'unload' or close operation.
!
!     If the number of files listed in the file table exceeds the
!     number of entries in the unit table, then this module will
!     reserve the logical unit listed in the row to remove, zero
!     out the cost and return.  In this event, NUT will remain
!     unchanged.
!
!     However, if there are less files in the file table than the
!     number of entries in the unit table, then this routine removes
!     the row and compresses the unit table, as one would expect.
!
!     The file attached to UNIT is not closed by this routine, the
!     closure should occur before invoking this module.
!
! $ Examples
!
!     See ZZDDHHLU for sample usage.
!
! $ Restrictions
!
!     1) This routine operates when an error condition introduced
!        by a prior call to SIGERR exists.  It calls no routines
!        that return on entry when proper inputs are provided.
!        Any updates to this routine must preserve this behavior.
!
!     2) The file attached to the unit that is to be removed should
!        already have been removed from the file table.  This is
!        necessary so the value of NFT reflects the number of files
!        available after the removal.
!
!     3) The logical unit in UTLUN(UINDEX) must be closed or buffered
!        externally prior to calling this routine.  Knowledge of its
!        value could be lost otherwise.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-NOV-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      integer               i

!
!     First check to see if NUT is 0.  If so, just return, as there
!     are no rows to remove.
!
      if ( nut .eq. 0 ) then
         return
      end if

!
!     Check to see if we found the UINDEX in the unit table.
!     If not, use discovery check-in, signal an error and return.
!
      if ( ( uindex .gt. nut ) .or. ( uindex .lt. 1 ) ) then

         call chkin  ( 'ZZDDHRMU'                                     )
         call setmsg ( 'Attempt to remove row # from the unit table ' &
         //            'failed because valid row indices range from ' &
         //            '1 to NUT.'                                    )
         call errint ( '#', uindex                                    )
         call errint ( '#', nut                                       )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'                       )
         call chkout ( 'ZZDDHRMU'                                     )
         return

      end if

!
!     We have found the row we need to remove from the table.
!     Check to see whether we are to remove this row or simply
!     mark it as zero cost and reserve the unit.  We know this
!     is the case when NFT is greater than or equal to NUT.
!
      if ( nft .ge. nut ) then

!
!        Zero the cost, clear the handle, and unlock the unit.
!
         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

!
!        Reserve the unit for the handle manager's usage and
!        return.
!
         call reslun ( utlun(uindex) )
         return

      end if

!
!     If we reach here, then we have to remove the row from the
!     unit table and compress.
!
      do i = uindex+1, nut

         utcst(i-1) = utcst(i)
         uthan(i-1) = uthan(i)
         utlck(i-1) = utlck(i)
         utlun(i-1) = utlun(i)

      end do

!
!     Decrement NUT.
!
      nut = nut - 1

      return
      end
