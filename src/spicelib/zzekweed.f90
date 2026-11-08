!$Procedure  ZZEKWEED ( Private: EK, weed out redundant row vectors )

      subroutine zzekweed ( njrs, bases, nrows )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Weed out redundant, fully qualified row vectors from a join row
!     set union.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekjrs.inc'
      include 'ekqlimit.inc'

      integer(ip)           njrs
      integer(ip)           bases ( * )
      integer(ip)           nrows

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NJRS      I-O  Number of join row sets in union.
!     BASES     I-O  Scratch area base addresses of join row sets.
!     NROWS      O   Total number of row vectors in join row set union.
!
! $ Detailed_Input
!
!     NJRS           is the number of join row sets in a join row set
!                    union to be weeded.
!
!     BASES          is an array of base addresses, in the scratch area,
!                    of a collection of join row sets from which
!                    redundant row vectors are to be weeded out.  A row
!                    vector is is redundant if and only if it is
!                    identical to another row vector, and the qualifying
!                    segment vectors of the two row vectors are
!                    identical as well.
!
! $ Detailed_Output
!
!     NJRS           is the number of join row sets after redundant
!                    rows have been removed.  If any join row sets
!                    become empty as a result of this weeding-out,
!                    the count of join row sets is reduced accordingly.
!
!     BASES          is the set of bases of join rows in the join row
!                    set union after weeding has been completed.
!                    Bases of empty join row sets are compressed out;
!                    the valid elements of the array are the first
!                    NJRS elements of BASES, where NJRS has been
!                    updated by this routine.
!
!     NROWS          is the total number of rows in the join row set
!                    union after the weeding process is finished.
!
!     See $Particulars for a more detailed description of the effect of
!     this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If JRSBAS is not the base address of a structurally valid
!         join row set union, the results of this routine will be
!         unpredictable.
!
!     2)  If NJRS is non-positive, or if NJRS exceeds the maximum
!         allowed number of constraint relations MAXCON, the error
!         SPICE(INVALIDCOUNT) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine operates by side effects:  it modifies the join row
!     set designated by the input argument JRSBAS.  Every redundant
!     row vector is removed, and join row sets from which row vectors
!     are removed are compressed.  Empty join row sets are removed
!     from the union, as reflected by updates to NJRS and BASES.
!
!     The principal purpose of this routine is to support execution of
!     queries involving OR clauses; such queries may cause row vectors
!     satisfying both disjuncts to be included multiple times in the
!     set of matching row vectors.
!
!     The layout of a join row set in the EK scratch area is shown
!     in the join row set parameter include file.
!
! $ Examples
!
!     See EKSRCH.
!
! $ Restrictions
!
!     1) Loading or unloading EK files between name resolution of the
!        the input query and passing the query to this routine will
!        invalidate the checking done by this routine, and may cause
!        the routine to fail.
!
!     2) Assumes redundant row vectors never occur in any join row set.
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
!-    Beta Version 1.1.1,  03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.1.0,  8-JAN-1996 (WLT)
!
!        Replaced a call to REPMI with ERRINT in the first
!        error check.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               sameai

!
!     Local variables
!
      integer(ip)           i
      integer(ip)           base
      integer(ip)           cand
      integer(ip)           candsv ( maxtab )
      integer(ip)           crv
      integer(ip)           crwbas
      integer(ip)           crwvec ( maxtab + 1 )
      integer(ip)           csgbas
      integer(ip)           csv
      integer(ip)           j
      integer(ip)           loc
      integer(ip)           ncndrv
      integer(ip)           ncndsv
      integer(ip)           ndel
      integer(ip)           nprdrv
      integer(ip)           nprdsv
      integer(ip)           nr
      integer(ip)           nrloc
      integer(ip)           nsvloc
      integer(ip)           ntab
      integer(ip)           pred
      integer(ip)           predsv ( maxtab )
      integer(ip)           prv
      integer(ip)           prwbas
      integer(ip)           prwvec ( maxtab + 1 )
      integer(ip)           psgbas
      integer(ip)           psv
      integer(ip)           rvsize
      integer(ip)           svsize

      logical               hit

!
!     Use discovery check-in.
!
      if ( ( njrs < 1 ) .or. ( njrs > mxjrs )  ) then

         call setmsg ( 'The number of join row sets in the union is #' )
         call errint ( '#', njrs                                       )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         return

      end if

!
!     Make sure that the addressing routines are properly initialized.
!
      call zzekvset ( njrs, bases )

!
!     Get the segment vector and row vector sizes.  The sizes that
!     apply to the first join row set will suffice throughout.
!
      loc     =  bases(1) + jtcidx

      call zzeksrd ( loc, loc, ntab )

      svsize  =  ntab
      rvsize  =  ntab + 1

!
!     Mark redundant rows vectors for deletion.  One saving grace is
!     that redundant rows can never occur in the same join row set, as
!     long as that join row set represents a set of rows satisfying
!     a conjunction of constraints.
!
      do cand = 2, njrs
!
!        We'll compare row vectors in the CAND join row set to row
!        vectors in the preceding join row sets.  Only row vectors
!        corresponding to matching segment vectors need be compared.
!        Therefore, we'll loop over the segment vectors in the CAND
!        join row set, and for each such segment vector, loop over the
!        segment vectors in the preceding join row sets.  If a match
!        occurs, we'll compare row vectors corresponding to those
!        segment vectors.
!
!        NCNDSV will contain the number of segment vectors in the
!        `candidate' join row set.
!
         nsvloc  =  bases(cand)  +  jscidx

         call zzeksrd  ( nsvloc, nsvloc, ncndsv  )


         do csv = 1, ncndsv
!
!           Look up the candidate segment vector.
!
            csgbas  =  bases(cand)  +  jsvbas  +  (csv-1)*svsize

            call zzeksrd  ( csgbas+1,  csgbas+svsize,  candsv  )

!
!           Get the row vector count and base address of the set of
!           row vectors for the candidate segment vector, in case
!           we need them.  (Referring to the diagram of the join
!           row set structure in the join row set parameter include
!           file may be helpful here.)
!
            base  =      bases(cand) &
                      +  jsvbas &
                      +  ncndsv  * svsize &
                      +  (csv-1) * 2

            call zzeksrd ( base+1,  base+1,  crwbas )
            crwbas = crwbas + bases(cand)

            call zzeksrd ( base+2,  base+2,  ncndrv )


!
!           For the current predecessor join row set, look up the
!           segment vectors in that join row set and compare them to the
!           candidate.
!
            do pred = 1, cand-1
!
!              Get the count of segment vectors in the current
!              predecessor join row set.
!
               nsvloc = bases(pred) + jscidx

               call zzeksrd  ( nsvloc, nsvloc, nprdsv )


               do psv = 1, nprdsv
!
!                 Look up the predecessor segment vector.
!
                  psgbas  =  bases(pred)  +  jsvbas  +  (psv-1)*svsize

                  call zzeksrd  ( csgbas+1,  csgbas+svsize,  predsv  )

!
!                 Compare the segment vectors and hope for the best.
!
                  if (  sameai ( candsv, predsv, svsize )  ) then
!
!                    Unfortunately, the two segment vectors match, so
!                    there's something to do.  We'll have to compare
!                    every row vector corresponding to the candidate
!                    segment vector with every row vector corresponding
!                    to the predecessor.
!
!                    Get the row vector count and base address of the
!                    set of row vectors for the current predecessor
!                    segment vector.  We already have on hand the
!                    corresponding quantities for the candidate
!                    segment vector.
!

                     base  =      bases(pred) &
                               +  jsvbas &
                               +  nprdsv  * svsize &
                               +  (psv-1) * 2

                     call zzeksrd ( base+1,  base+1,  prwbas )
                     prwbas = prwbas + bases(pred)

                     call zzeksrd ( base+2,  base+2,  nprdrv )

!
!                    Compare all row vectors.
!
                     do crv = 1, ncndrv


                        base  =  crwbas + (crv-1)*rvsize
                        call zzeksrd ( base+1, base+rvsize, crwvec )

                        prv   =  1
                        hit   =  .false.


                        do while (         ( prv <=  nprdrv ) &
                                    .and.  (     .not. hit    )   )

                           base  =  prwbas + (prv-1)*rvsize
                           call zzeksrd ( base+1, base+rvsize, prwvec )


                           if ( sameai(crwvec, prwvec, rvsize) ) then
!
!                             The row vectors, together with their
!                             qualifying segment vectors, match.  The
!                             higher-indexed vector is considered
!                             redundant.  To mark this vector for
!                             deletion, we simply zero out the first
!                             element of the row vector.  This makes the
!                             row vector invalid, so it will not match
!                             any valid row vector we see later.
!
                              base  =  crwbas + (crv-1)*rvsize
                              call zzeksupd ( base+1, base+1, 0 )
                              hit   =  .true.
                           else
                              prv   =  prv + 1
                           end if

                        end do

                     end do


                  end if
!
!                 We've finished comparing row vectors for a pair of
!                 segment vectors, if it was necessary to do so.
!

               end do
!
!              We've compared all segment vectors in the current
!              predecessor join row set with the candidate segment
!              vector.
!

            end do
!
!           We've compared all segment vectors in all predecessor join
!           row sets to the current segment vector.
!

         end do
!
!        We've compared the candidate join row set to its predecessors.
!

      end do
!
!     We've compared all of the join row sets.
!
!
!     Now, clean up the join row set union by compressing out deleted
!     rows, segment vectors, and join row sets.
!
      j      =  1
      ndel   =  0

      do i = 1, njrs
!
!        Compress the current join row set.  If it ends up empty,
!        expel it from the union.
!
         call zzekjsqz ( bases(i) )

         nrloc = bases(i) + jrcidx
         call zzeksrd  ( nrloc, nrloc, nr )

         if ( nr == 0 ) then
!
!           This entire join row set can be deleted from the union.
!           Consider the next join row set.
!
            ndel     =  ndel + 1

         else

            bases(j) =  bases(i)
            j        =  j + 1

         end if

      end do


      njrs  = njrs - ndel

!
!     Count the rows remaining after our clean-up operation.
!
      nrows = 0

      do i = 1, njrs

         nrloc = bases(i) + jrcidx

         call zzeksrd ( nrloc, nrloc, nr )

         nrows = nrows + nr

      end do


      return
      end
