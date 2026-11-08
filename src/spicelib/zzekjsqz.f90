!$Procedure  ZZEKJSQZ ( Private: EK, join row set squeeze )

      SUBROUTINE ZZEKJSQZ ( JRSBAS )

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

      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekqlimit.inc'

      INTEGER               JRSBAS

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
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               CNTLOC
      INTEGER               DELTA
      INTEGER               I
      INTEGER               J
      INTEGER               NSVDEL
      INTEGER               NRVDEL
      INTEGER               NEWNSV
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NSV
      INTEGER               NSVLOC
      INTEGER               NTAB
      INTEGER               NTLOC
      INTEGER               PCPAIR ( 2 )
      INTEGER               PTARG
      INTEGER               PTBASE
      INTEGER               PTRLOC
      INTEGER               RC
      INTEGER               RBASE
      INTEGER               ROWVEC ( MAXTAB+1 )
      INTEGER               RTARG
      INTEGER               RVSIZE
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SETBAS
      INTEGER               SIZE
      INTEGER               SIZLOC
      INTEGER               SVBASE
      INTEGER               SVSIZE
      INTEGER               VTARG

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
      SIZLOC  =  JRSBAS + JSZIDX
      NSVLOC  =  JRSBAS + JSCIDX
      NTLOC   =  JRSBAS + JTCIDX

      CALL ZZEKSRD ( SIZLOC, SIZLOC, SIZE   )
      CALL ZZEKSRD ( NTLOC,  NTLOC,  NTAB   )
      CALL ZZEKSRD ( NSVLOC, NSVLOC, NSV    )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Set the sizes of segment and row vectors.
!
      SVSIZE  =  NTAB
      RVSIZE  =  NTAB + 1

!
!     For each segment vector, obtain the row count.  Clean up after
!     null segment vectors:  compress out the space allocated for their
!     row vector pointers.  Keep track of the number of deletions.
!
      NSVDEL  =  0
      NRVDEL  =  0
      VTARG   =  JRSBAS + JSVBAS

      DO I = 1, NSV
!
!        The location of the row count is CNTLOC.  The row vector base
!        pointer precedes the row count.
!
         CNTLOC  =  JRSBAS + JSVBAS + NSV*SVSIZE + 2*(I-1) + 2
         PTRLOC  =  CNTLOC - 1

         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )


         IF ( RC .GT. 0 ) THEN
!
!           The row vector set for this segment vector is non-empty.
!           scan the rows, looking for those marked for deletion, and
!           update the row count to reflect the number of rows that
!           we're going to keep.
!
            CALL ZZEKSRD ( PTRLOC, PTRLOC, SETBAS )

            NRVDEL = 0

            DO J = 1, RC

               RBASE  =  JRSBAS + SETBAS + (J-1)*RVSIZE

               CALL ZZEKSRD ( RBASE+1, RBASE+1, ROWVEC(1) )

               IF ( ROWVEC(1) .EQ. 0 ) THEN
                  NRVDEL = NRVDEL + 1
               END IF

            END DO

         END IF

!
!        Compute the base address of the current segment vector.
!
         SVBASE  =  JRSBAS + JSVBAS + (I-1)*SVSIZE


         IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN
!
!           We're going to delete the current segment vector.  We'll
!           just skip over it without advancing our target pointers.
!
            NSVDEL   =  NSVDEL + 1


         ELSE IF ( NSVDEL .GT. 0 ) THEN
!
!           We need to shift the current segment vector to its
!           destination.
!
            CALL ZZEKSRD  ( SVBASE+1, SVBASE+SVSIZE, SEGVEC )
            CALL ZZEKSUPD ( VTARG+1,  VTARG+SVSIZE,  SEGVEC )

            VTARG = VTARG + SVSIZE

         ELSE
!
!           No segment vectors have been deleted yet.  We still must
!           update the target in case we shift vectors later on in this
!           loop.
!
            VTARG = VTARG + SVSIZE

         END IF

      END DO

!
!     At this point, we've compressed out the null segment vectors.
!     The next step is to compress out the row vector counts and row
!     vector pointers that corresponded to those segment vectors.  We
!     also want to remove the gap between the segment vectors and the
!     row vector pointer/count pairs.
!
!     We need to do this only if we deleted some segment vectors.
!
      IF ( NSVDEL .GT. 0 ) THEN

         NEWNSV =  NSV    - NSVDEL
         PTARG  =  JRSBAS + JSVBAS + (NEWNSV)*SVSIZE

         DO I = 1, NSV
!
!           The row count is RC.
!
            SVSIZE  =  NTAB
            CNTLOC  =  JRSBAS + JSVBAS + NSV*SVSIZE + 2*(I-1) + 2

            CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )

            PTBASE  =  CNTLOC - 2

            IF ( RC .GT. 0 ) THEN
!
!              Shift the current row vector pointer and row vector
!              count.
!
               CALL ZZEKSRD  ( PTBASE+1, PTBASE+2, PCPAIR )
               CALL ZZEKSUPD ( PTARG+1,  PTARG+2,  PCPAIR )

               PTARG =  PTARG + 2

            END IF

         END DO


      ELSE

         NEWNSV = NSV

      END IF

!
!     Update the segment vector count.
!
      CALL ZZEKSUPD ( NSVLOC, NSVLOC, NEWNSV )

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
      RTARG   =  JRSBAS + JSVBAS + NEWNSV*(SVSIZE+2)

      DO I = 1, NEWNSV
!
!        Find the row count and row pointer for the current segment
!        vector.
!
         CNTLOC  =  JRSBAS + JSVBAS + NEWNSV*SVSIZE + 2*(I-1) + 2

         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )

         PTRLOC  =  CNTLOC - 1

!
!        Get the row vector set base pointer.  After capturing the
!        current value, we'll update this pointer to account for
!        the shifting of row vectors.
!
         CALL ZZEKSRD  ( PTRLOC, PTRLOC, SETBAS )

         RBASE   =  JRSBAS + SETBAS
         DELTA   =  RTARG  - RBASE

         CALL ZZEKSUPD ( PTRLOC, PTRLOC, SETBAS+DELTA )

!
!        Shift the row vectors for the current segment vector,
!        leaving behind the row vectors marked for deletion.
!
         NRVDEL = 0

         DO J = 1, RC

            CALL ZZEKSRD  ( RBASE+1, RBASE+RVSIZE, ROWVEC )

            IF ( ROWVEC(1) .EQ. 0 ) THEN
!
!              This row vector is to be deleted; don't copy it.
!
               RBASE  = RBASE  + RVSIZE
               NRVDEL = NRVDEL + 1

            ELSE
!
!              The segment vector pointer is base-relative.
!
               ROWVEC( RVSIZE )  =  JSVBAS + (I-1)*SVSIZE

               CALL ZZEKSUPD ( RTARG+1, RTARG+RVSIZE, ROWVEC )

               RBASE  =  RBASE + RVSIZE
               RTARG  =  RTARG + RVSIZE

            END IF

         END DO

!
!        Update the row count for the current segment vector, if
!        necessary.  Note that no segment vector will become empty
!        as a result of the row vector deletions we've done; we
!        already eliminated any segment vectors for which that
!        could happen, before we entered this loop.
!
         IF ( NRVDEL .GT. 0 ) THEN
            CALL ZZEKSUPD (  CNTLOC,  CNTLOC,  RC - NRVDEL  )
         END IF

      END DO


!
!     Update the total row count and size of the join row set.
!
      NR  =  0

      DO I = 1, NEWNSV

         CNTLOC  =  JRSBAS + JSVBAS + NEWNSV*SVSIZE + (I-1)*2 + 2

         CALL ZZEKSRD ( CNTLOC, CNTLOC, RC )

         NR      =  NR     + RC

      END DO


      NRLOC  =  JRSBAS   +   JRCIDX
      SIZE   =  JSVBAS   +   NEWNSV * ( SVSIZE + 2 )  +  NR * RVSIZE

      CALL ZZEKSUPD ( NRLOC,  NRLOC,  NR   )
      CALL ZZEKSUPD ( SIZLOC, SIZLOC, SIZE )

      RETURN
      END
