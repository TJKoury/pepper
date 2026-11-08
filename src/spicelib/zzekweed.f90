!$Procedure  ZZEKWEED ( Private: EK, weed out redundant row vectors )

      SUBROUTINE ZZEKWEED ( NJRS, BASES, NROWS )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Weed out redundant, fully qualified row vectors from a join row
!     set union.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekqlimit.inc'

      INTEGER               NJRS
      INTEGER               BASES ( * )
      INTEGER               NROWS

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
      LOGICAL               SAMEAI

!
!     Local variables
!
      INTEGER               I
      INTEGER               BASE
      INTEGER               CAND
      INTEGER               CANDSV ( MAXTAB )
      INTEGER               CRV
      INTEGER               CRWBAS
      INTEGER               CRWVEC ( MAXTAB + 1 )
      INTEGER               CSGBAS
      INTEGER               CSV
      INTEGER               J
      INTEGER               LOC
      INTEGER               NCNDRV
      INTEGER               NCNDSV
      INTEGER               NDEL
      INTEGER               NPRDRV
      INTEGER               NPRDSV
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NSVLOC
      INTEGER               NTAB
      INTEGER               PRED
      INTEGER               PREDSV ( MAXTAB )
      INTEGER               PRV
      INTEGER               PRWBAS
      INTEGER               PRWVEC ( MAXTAB + 1 )
      INTEGER               PSGBAS
      INTEGER               PSV
      INTEGER               RVSIZE
      INTEGER               SVSIZE

      LOGICAL               HIT

!
!     Use discovery check-in.
!
      IF ( ( NJRS .LT. 1 ) .OR. ( NJRS .GT. MXJRS )  ) THEN

         CALL CHKIN  ( 'ZZEKWEED'                                      )
         CALL SETMSG ( 'The number of join row sets in the union is #' )
         CALL ERRINT ( '#', NJRS                                       )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKWEED'                                      )
         RETURN

      END IF

!
!     Make sure that the addressing routines are properly initialized.
!
      CALL ZZEKVSET ( NJRS, BASES )

!
!     Get the segment vector and row vector sizes.  The sizes that
!     apply to the first join row set will suffice throughout.
!
      LOC     =  BASES(1) + JTCIDX

      CALL ZZEKSRD ( LOC, LOC, NTAB )

      SVSIZE  =  NTAB
      RVSIZE  =  NTAB + 1

!
!     Mark redundant rows vectors for deletion.  One saving grace is
!     that redundant rows can never occur in the same join row set, as
!     long as that join row set represents a set of rows satisfying
!     a conjunction of constraints.
!
      DO CAND = 2, NJRS
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
         NSVLOC  =  BASES(CAND)  +  JSCIDX

         CALL ZZEKSRD  ( NSVLOC, NSVLOC, NCNDSV  )


         DO CSV = 1, NCNDSV
!
!           Look up the candidate segment vector.
!
            CSGBAS  =  BASES(CAND)  +  JSVBAS  +  (CSV-1)*SVSIZE

            CALL ZZEKSRD  ( CSGBAS+1,  CSGBAS+SVSIZE,  CANDSV  )

!
!           Get the row vector count and base address of the set of
!           row vectors for the candidate segment vector, in case
!           we need them.  (Referring to the diagram of the join
!           row set structure in the join row set parameter include
!           file may be helpful here.)
!
            BASE  =      BASES(CAND) &
                      +  JSVBAS &
                      +  NCNDSV  * SVSIZE &
                      +  (CSV-1) * 2

            CALL ZZEKSRD ( BASE+1,  BASE+1,  CRWBAS )
            CRWBAS = CRWBAS + BASES(CAND)

            CALL ZZEKSRD ( BASE+2,  BASE+2,  NCNDRV )


!
!           For the current predecessor join row set, look up the
!           segment vectors in that join row set and compare them to the
!           candidate.
!
            DO PRED = 1, CAND-1
!
!              Get the count of segment vectors in the current
!              predecessor join row set.
!
               NSVLOC = BASES(PRED) + JSCIDX

               CALL ZZEKSRD  ( NSVLOC, NSVLOC, NPRDSV )


               DO PSV = 1, NPRDSV
!
!                 Look up the predecessor segment vector.
!
                  PSGBAS  =  BASES(PRED)  +  JSVBAS  +  (PSV-1)*SVSIZE

                  CALL ZZEKSRD  ( CSGBAS+1,  CSGBAS+SVSIZE,  PREDSV  )

!
!                 Compare the segment vectors and hope for the best.
!
                  IF (  SAMEAI ( CANDSV, PREDSV, SVSIZE )  ) THEN
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

                     BASE  =      BASES(PRED) &
                               +  JSVBAS &
                               +  NPRDSV  * SVSIZE &
                               +  (PSV-1) * 2

                     CALL ZZEKSRD ( BASE+1,  BASE+1,  PRWBAS )
                     PRWBAS = PRWBAS + BASES(PRED)

                     CALL ZZEKSRD ( BASE+2,  BASE+2,  NPRDRV )

!
!                    Compare all row vectors.
!
                     DO CRV = 1, NCNDRV


                        BASE  =  CRWBAS + (CRV-1)*RVSIZE
                        CALL ZZEKSRD ( BASE+1, BASE+RVSIZE, CRWVEC )

                        PRV   =  1
                        HIT   =  .FALSE.


                        DO WHILE (         ( PRV .LE.  NPRDRV ) &
                                    .AND.  (     .NOT. HIT    )   )

                           BASE  =  PRWBAS + (PRV-1)*RVSIZE
                           CALL ZZEKSRD ( BASE+1, BASE+RVSIZE, PRWVEC )


                           IF ( SAMEAI(CRWVEC, PRWVEC, RVSIZE) ) THEN
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
                              BASE  =  CRWBAS + (CRV-1)*RVSIZE
                              CALL ZZEKSUPD ( BASE+1, BASE+1, 0 )
                              HIT   =  .TRUE.
                           ELSE
                              PRV   =  PRV + 1
                           END IF

                        END DO

                     END DO


                  END IF
!
!                 We've finished comparing row vectors for a pair of
!                 segment vectors, if it was necessary to do so.
!

               END DO
!
!              We've compared all segment vectors in the current
!              predecessor join row set with the candidate segment
!              vector.
!

            END DO
!
!           We've compared all segment vectors in all predecessor join
!           row sets to the current segment vector.
!

         END DO
!
!        We've compared the candidate join row set to its predecessors.
!

      END DO
!
!     We've compared all of the join row sets.
!
!
!     Now, clean up the join row set union by compressing out deleted
!     rows, segment vectors, and join row sets.
!
      J      =  1
      NDEL   =  0

      DO I = 1, NJRS
!
!        Compress the current join row set.  If it ends up empty,
!        expel it from the union.
!
         CALL ZZEKJSQZ ( BASES(I) )

         NRLOC = BASES(I) + JRCIDX
         CALL ZZEKSRD  ( NRLOC, NRLOC, NR )

         IF ( NR .EQ. 0 ) THEN
!
!           This entire join row set can be deleted from the union.
!           Consider the next join row set.
!
            NDEL     =  NDEL + 1

         ELSE

            BASES(J) =  BASES(I)
            J        =  J + 1

         END IF

      END DO


      NJRS  = NJRS - NDEL

!
!     Count the rows remaining after our clean-up operation.
!
      NROWS = 0

      DO I = 1, NJRS

         NRLOC = BASES(I) + JRCIDX

         CALL ZZEKSRD ( NRLOC, NRLOC, NR )

         NROWS = NROWS + NR

      END DO


      RETURN
      END
