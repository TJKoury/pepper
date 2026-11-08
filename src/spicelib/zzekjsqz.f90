!$Procedure  ZZEKJSQZ ( Private: EK, join row set squeeze )

      subroutine zzekjsqz ( jrsbas )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compress a join row set by eliminating segment vectors for
!     which there are no corresponding row vectors.
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

      include 'ekjrs.inc'
      include 'ekqlimit.inc'

      integer(ip)           jrsbas

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     JRSBAS     I   Scratch area base address of join row set.
!
! $ Detailed_Input
!
!     JRSBAS         is the base address, in the scratch area, of a
!                    join row set to be compressed.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If JRSBAS is not the base address of a structurally valid
!         join row set, the results of this routine will be
!         unpredictable.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine operates by side effects:  it modifies the join row
!     set designated by the input argument JRSBAS.  Every row vector
!     marked for deletion is removed.  Every empty segment vector is
!     removed, along with the row count and row vector base for that
!     segment vector.  The join row set is compressed to remove all
!     gaps.   All counts are updated to reflect the updated join row
!     set.
!
!     The purpose of the compression performed by this routine is to
!     save work during joins by reducing the size of the cartesian
!     products of sets of segment vectors.  Also, special cases
!     involving null segment vectors can be avoided by this clean-up
!     mechanism.  Finally, it may be possible to save space in the EK
!     scratch area freed by the compression.
!
! $ Examples
!
!     See EKSRCH.
!
! $ Restrictions
!
!     1) Relies on the EK scratch area.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
!
!        Bug fix:  added initialization of variable NRVDEL to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms.
!
!-    SPICELIB Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
!
!        Bug fix:  added initialization of variable NRVDEL to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms. The
!                  statement referencing the uninitialized variable
!                  was:
!
!           IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN
!
!        In the previous version of the code, NRVDEL is uninitialized
!        when NRVDEL is 0.  NRVDEL *is* initialized when RC is
!        non-zero, so the logical value of the IF expression is not
!        affected by the lack of proper initialization.
!
!        However, the Intel Fortran compiler for the Mac flags a runtime
!        error when the above code is exercised.  So NRVDEL is now
!        initialized prior to the above IF statement.
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           cntloc
      integer(ip)           delta
      integer(ip)           i
      integer(ip)           j
      integer(ip)           nsvdel
      integer(ip)           nrvdel
      integer(ip)           newnsv
      integer(ip)           nr
      integer(ip)           nrloc
      integer(ip)           nsv
      integer(ip)           nsvloc
      integer(ip)           ntab
      integer(ip)           ntloc
      integer(ip)           pcpair ( 2 )
      integer(ip)           ptarg
      integer(ip)           ptbase
      integer(ip)           ptrloc
      integer(ip)           rc
      integer(ip)           rbase
      integer(ip)           rowvec ( maxtab+1 )
      integer(ip)           rtarg
      integer(ip)           rvsize
      integer(ip)           segvec ( maxtab )
      integer(ip)           setbas
      integer(ip)           size
      integer(ip)           sizloc
      integer(ip)           svbase
      integer(ip)           svsize
      integer(ip)           vtarg

!
!     Use discovery check-in.
!
!
!     Look up the counts that are of interest:
!
!       -- The table count
!       -- The segment vector count
!       -- The join row set size
!
!     Save the address of each count.
!
      sizloc  =  jrsbas + jszidx
      nsvloc  =  jrsbas + jscidx
      ntloc   =  jrsbas + jtcidx

      call zzeksrd ( sizloc, sizloc, size   )
      call zzeksrd ( ntloc,  ntloc,  ntab   )
      call zzeksrd ( nsvloc, nsvloc, nsv    )

      if ( failed() ) then
         return
      end if

!
!     Set the sizes of segment and row vectors.
!
      svsize  =  ntab
      rvsize  =  ntab + 1

!
!     For each segment vector, obtain the row count.  Clean up after
!     null segment vectors:  compress out the space allocated for their
!     row vector pointers.  Keep track of the number of deletions.
!
      nsvdel  =  0
      nrvdel  =  0
      vtarg   =  jrsbas + jsvbas

      do i = 1, nsv
!
!        The location of the row count is CNTLOC.  The row vector base
!        pointer precedes the row count.
!
         cntloc  =  jrsbas + jsvbas + nsv*svsize + 2*(i-1) + 2
         ptrloc  =  cntloc - 1

         call zzeksrd ( cntloc, cntloc, rc )


         if ( rc > 0 ) then
!
!           The row vector set for this segment vector is non-empty.
!           scan the rows, looking for those marked for deletion, and
!           update the row count to reflect the number of rows that
!           we're going to keep.
!
            call zzeksrd ( ptrloc, ptrloc, setbas )

            nrvdel = 0

            do j = 1, rc

               rbase  =  jrsbas + setbas + (j-1)*rvsize

               call zzeksrd ( rbase+1, rbase+1, rowvec(1) )

               if ( rowvec(1) == 0 ) then
                  nrvdel = nrvdel + 1
               end if

            end do

         end if

!
!        Compute the base address of the current segment vector.
!
         svbase  =  jrsbas + jsvbas + (i-1)*svsize


         if (  ( rc == 0 ) .or. ( nrvdel == rc )  ) then
!
!           We're going to delete the current segment vector.  We'll
!           just skip over it without advancing our target pointers.
!
            nsvdel   =  nsvdel + 1


         else if ( nsvdel > 0 ) then
!
!           We need to shift the current segment vector to its
!           destination.
!
            call zzeksrd  ( svbase+1, svbase+svsize, segvec )
            call zzeksupd ( vtarg+1,  vtarg+svsize,  segvec )

            vtarg = vtarg + svsize

         else
!
!           No segment vectors have been deleted yet.  We still must
!           update the target in case we shift vectors later on in this
!           loop.
!
            vtarg = vtarg + svsize

         end if

      end do

!
!     At this point, we've compressed out the null segment vectors.
!     The next step is to compress out the row vector counts and row
!     vector pointers that corresponded to those segment vectors.  We
!     also want to remove the gap between the segment vectors and the
!     row vector pointer/count pairs.
!
!     We need to do this only if we deleted some segment vectors.
!
      if ( nsvdel > 0 ) then

         newnsv =  nsv    - nsvdel
         ptarg  =  jrsbas + jsvbas + (newnsv)*svsize

         do i = 1, nsv
!
!           The row count is RC.
!
            svsize  =  ntab
            cntloc  =  jrsbas + jsvbas + nsv*svsize + 2*(i-1) + 2

            call zzeksrd ( cntloc, cntloc, rc )

            ptbase  =  cntloc - 2

            if ( rc > 0 ) then
!
!              Shift the current row vector pointer and row vector
!              count.
!
               call zzeksrd  ( ptbase+1, ptbase+2, pcpair )
               call zzeksupd ( ptarg+1,  ptarg+2,  pcpair )

               ptarg =  ptarg + 2

            end if

         end do


      else

         newnsv = nsv

      end if

!
!     Update the segment vector count.
!
      call zzeksupd ( nsvloc, nsvloc, newnsv )

!
!     Remove any gaps that may exist between any of the row vectors,
!     or between the end of the segment vector's row vector counts
!     and base addresses and the first row vector.
!
!     The initial target location is the first element following the
!     last segment vector's row vector count.  RTARG is used as a base
!     address; it precedes this location by 1.
!
!     If we deleted any segment vectors, the segment vector pointers
!     embedded in the row vectors must change.  Make these updates
!     if necessary.
!
!
      rtarg   =  jrsbas + jsvbas + newnsv*(svsize+2)

      do i = 1, newnsv
!
!        Find the row count and row pointer for the current segment
!        vector.
!
         cntloc  =  jrsbas + jsvbas + newnsv*svsize + 2*(i-1) + 2

         call zzeksrd ( cntloc, cntloc, rc )

         ptrloc  =  cntloc - 1

!
!        Get the row vector set base pointer.  After capturing the
!        current value, we'll update this pointer to account for
!        the shifting of row vectors.
!
         call zzeksrd  ( ptrloc, ptrloc, setbas )

         rbase   =  jrsbas + setbas
         delta   =  rtarg  - rbase

         call zzeksupd ( ptrloc, ptrloc, setbas+delta )

!
!        Shift the row vectors for the current segment vector,
!        leaving behind the row vectors marked for deletion.
!
         nrvdel = 0

         do j = 1, rc

            call zzeksrd  ( rbase+1, rbase+rvsize, rowvec )

            if ( rowvec(1) == 0 ) then
!
!              This row vector is to be deleted; don't copy it.
!
               rbase  = rbase  + rvsize
               nrvdel = nrvdel + 1

            else
!
!              The segment vector pointer is base-relative.
!
               rowvec( rvsize )  =  jsvbas + (i-1)*svsize

               call zzeksupd ( rtarg+1, rtarg+rvsize, rowvec )

               rbase  =  rbase + rvsize
               rtarg  =  rtarg + rvsize

            end if

         end do

!
!        Update the row count for the current segment vector, if
!        necessary.  Note that no segment vector will become empty
!        as a result of the row vector deletions we've done; we
!        already eliminated any segment vectors for which that
!        could happen, before we entered this loop.
!
         if ( nrvdel > 0 ) then
            call zzeksupd (  cntloc,  cntloc,  rc - nrvdel  )
         end if

      end do


!
!     Update the total row count and size of the join row set.
!
      nr  =  0

      do i = 1, newnsv

         cntloc  =  jrsbas + jsvbas + newnsv*svsize + (i-1)*2 + 2

         call zzeksrd ( cntloc, cntloc, rc )

         nr      =  nr     + rc

      end do


      nrloc  =  jrsbas   +   jrcidx
      size   =  jsvbas   +   newnsv * ( svsize + 2 )  +  nr * rvsize

      call zzeksupd ( nrloc,  nrloc,  nr   )
      call zzeksupd ( sizloc, sizloc, size )

      return
      end
