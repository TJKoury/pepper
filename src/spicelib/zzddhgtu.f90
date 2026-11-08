!$Procedure ZZDDHGTU ( Private --- DDH Get Unit )

      subroutine zzddhgtu ( utcst, uthan, utlck, utlun, nut, uindex )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get or prepare an entry in the unit table to receive a new
!     file.
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

      include              'zzddhman.inc'

      integer(ip)           utcst  ( * )
      integer(ip)           uthan  ( * )
      logical               utlck  ( * )
      integer(ip)           utlun  ( * )
      integer(ip)           nut

      integer(ip)           uindex

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,    I/O   Unit table.
!     NUT       I/O   Number of entries in the unit table.
!     UINDEX     O    Row in the unit table that can be replaced.
!
! $ Detailed_Input
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the
!                unit table.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the
!                unit table.  This may change as a new unit is
!                added or old ones are removed.
!
!     NUT        is the number of entries in the unit table.  This may
!                change as new entries are added.
!
!     UINDEX     is the index of the row where the new unit should
!                be attached.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine may disconnect a file from its logical unit, to
!     successfully process the caller's request for a unit.
!
! $ Exceptions
!
!     1) If GETLUN fails to assign a logical unit for any reason to
!        the row of interest, this routine sets the logical unit to -1,
!        since negative logical units in Fortran are not permitted.
!
! $ Particulars
!
!      This routine only manipulates the contents of the unit table.
!      Any "zero" cost rows in the table indicate rows where the
!      listed logical unit has been reserved, but no file is currently
!      attached.
!
!      Callers of this routine should check FAILED since this
!      routine may invoke GETLUN.
!
! $ Examples
!
!     See ZZDDHHLU for sample usage.
!
! $ Restrictions
!
!     1) This routine must not be used to retrieve a unit for a
!        file that is already connected to a unit listed in the
!        unit table.
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
!-    SPICELIB Version 1.0.0, 29-MAY-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      logical               failed
      logical               return

!
!     Local Variables
!
      integer(ip)           i
      integer(ip)           orderv ( utsize )

      logical               done


!
!     Standard SPICE discovery error handling.
!
      if ( return() ) then
         return
      end if

!
!     First check the case when the unit table is completely empty.
!
      if ( nut .eq. 0 ) then

         nut    = 1
         uindex = 1

         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

         call getlun ( utlun(uindex) )

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         if ( failed() ) then
            utlun(uindex) = -1
            return
         end if

!
!        If we end up here, then GETLUN succeeded and we have the new
!        unit.  Now return.
!
         return

      end if

!
!     If we reach here, then the table contains at least one entry.
!     Order the table rows by cost.
!
      call orderi ( utcst, nut, orderv )

!
!     Now check to for '0' cost rows as this indicates rows whose
!     logical units are reserved for this suite of routines usage,
!     but are not currently assigned a file.
!
      if ( utcst(orderv(1)) .le. 0 ) then

         uindex = orderv(1)

!
!        '0' cost rows end up in the unit table as the result of a
!        row deletion, occurring when excess files are present.
!        When this process occurs, the logical unit listed in this
!        row is reserved for this module's usage only with RESLUN.
!        Free it, since we're about to reassign it.
!
         call frelun ( utlun(uindex) )

         return

      end if

!
!     Now if no '0' cost rows exist, check to see if we can
!     expand the table.
!
      if ( nut .lt. utsize ) then

!
!        Now increment NUT and set UINDEX.
!
         nut    = nut + 1
         uindex = nut

!
!        Prepare the default values for the new row.
!
         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

         call getlun ( utlun(uindex) )

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         if ( failed() ) then
            utlun(uindex) = -1
            return
         end if

!
!        If we end up here, then GETLUN worked properly.  Now return.
!
         return

      end if

!
!     If we reach here, then we have no zero-cost rows and a full unit
!     table.  Now it's time to determine which entry in the table to
!     bump.  We do this by stepping through the order vector until
!     we find the first 'non-locked' row.
!
      i    = 0
      done = .false.

      do while ( (.not. done) .and. (i .ne. nut) )

         i    = i + 1
         done = .not. utlck( orderv(i) )

      end do

!
!     Before going any further, signal an error if we discover
!     we have not found a row.
!
      if ( .not. done ) then

         uindex = 0
         call chkin  ( 'ZZDDHGTU' )
         call setmsg ( 'The unit table is full and all entries are ' &
         //            'locked.  This should never happen. Contact ' &
         //            'NAIF.'                                       )
         call sigerr ( 'SPICE(BUG)'                                  )
         call chkout ( 'ZZDDHGTU'                                    )
         return

      end if

!
!     Clear UTCST and UTHAN since we intend to disconnect
!     the unit upon return.
!
      utcst( orderv(i) ) = 0
      uthan( orderv(i) ) = 0

!
!     Set UINDEX and CLSLUN, then return.
!
      uindex = orderv(i)

!
!     At this point we need to close the unit from the row of interest.
!
      close ( unit = utlun(uindex) )

      return
      end
