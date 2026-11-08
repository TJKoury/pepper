!$Procedure T_ZZCKCV06 (Private --- C-kernel segment coverage, type 06)

      SUBROUTINE T_ZZCKCV06 ( HANDLE, DESCR,  SCLKID, &
                              TOL,    TIMSYS, SCHEDL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is a TSPICE test routine. This routine has both an
!     alternate calling sequence and implementation relative to
!     ZZCKCV06.
!
!     Determine the "window" of coverage of a type 06 C-kernel segment.
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
!     CK
!     DAF
!
! $ Keywords
!
!     CK
!     UTILITY
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'ck06.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               SCLKID
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     DESCR      I   DAF segment descriptor.
!     SCLKID     I   SCLK ID.
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     DESCR      is the DAF descriptor of a type 06 CK segment. DESCR
!                identifies the instrument (frame class ID) for which
!                coverage is to be found. DESCR also provides the
!                segment time coverage bounds DC(1), DC(2).
!
!                Each interpolation interval is replaced with its
!                intersection with the segment coverage interval
!
!                   [ DC(1), DC(2) ]
!
!                before being expanded by TOL. Interpolation intervals
!                that don't intersect the segment coverage interval are
!                discarded, even if after expansion by TOL they would
!                have non-empty intersection with the segment coverage
!                interval.
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. After
!                truncation by the segment coverage interval, and
!                before insertion into the coverage window, each
!                non-empty truncated interpolation interval is expanded
!                by TOL:  the left endpoint of each interval is reduced
!                by TOL and the right endpoint is increased by TOL.
!                Any intervals that overlap as a result of the
!                expansion are merged.
!
!                The coverage window returned when TOL > 0 indicates
!                the coverage provided by the file to the CK readers
!                CKGPAV and CKGP when that value of TOL is passed to
!                them as an input.
!
!
!     TIMSYS     is a string indicating the time system used in the
!                output coverage window.TIMSYS may have the values:
!
!                   'SCLK'    Elements of SCHEDL are expressed in
!                             encoded SCLK ("ticks"), where the clock
!                             is associated with the object designated
!                             by IDCODE.
!
!                   'TDB'     Elements of SCHEDL are expressed as
!                             seconds past J2000 TDB.
!
!                TIMSYS must be consistent with the system used for
!                the contents of SCHEDL on input, if any.
!
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment. The schedule has
!                been adjusted to account for the provided tolerance
!                value. Coverage lying outside the interval
!
!                   DC(1) - TOL : DC(2) + TOL
!
!                is excluded.
!
!                The elements of SCHEDL are given in the time system
!                indicated by TIMSYS.
!
! $ Parameters
!
!     Several parameters associated with the type 06 C-kernel
!     are utilized to compute the packet size of each subtype.
!     See the include file 'ck06.inc' for details.
!
! $ Exceptions
!
!     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
!         the CK type 06 segment is not recognized.
!
!     2)  Routines in the call tree of this routine may signal errors
!         if insufficient room in SCHEDL exists or other error
!         conditions relating to file access arise.
!
!     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!         is signaled.
!
!     5)  If a time conversion error occurs, the error will be
!         diagnosed by a routine in the call tree of this routine.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 06 C-kernel segment.
!
! $ Examples
!
!     See CKCOV.
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT)
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

!
!     Local Variables
!
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      IVLBDS ( 2 )
      DOUBLE PRECISION      LSTEPC
      DOUBLE PRECISION      RATE

      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NINTVL
      INTEGER               NREC
      INTEGER               SUBTYP
      INTEGER               WINSIZ

      LOGICAL               ISTDB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ZZCKCV06' )

!
!     Check tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'T_ZZCKCV06'                                   )
         RETURN

      END IF

!
!     Set a logical flag indicating whether the time system is SCLK.
!
      ISTDB = EQSTR ( TIMSYS, 'TDB' )

!
!     Check time system.
!
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'T_ZZCKCV06'                              )
            RETURN

         END IF

      END IF

!
!     Unpack the descriptor.
!
      CALL DAFUS ( DESCR, ND, NI, DC, IC )

!
!     Fetch the mini-segment count from the segment.
!
      CALL CKNM06 ( HANDLE, DESCR, NINTVL )

!
!     Each mini-segment contributes a coverage interval to the
!     total coverage of the segment. Since mini-segments can
!     contain gaps, we need to examine not only the interpolation
!     interval bounds but the final epochs of the mini-segments.
!
!     Now loop over the mini-segments and find the contribution
!     from each one.
!
      DO I = 1,  NINTVL
!
!        Find the interval bounds for this mini-segment.
!
         CALL CKMP06 ( HANDLE, DESCR, I,      RATE,  SUBTYP, &
                       WINSIZ, NREC,  IVLBDS, LSTEPC        )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_ZZCKCV06' )
            RETURN
         END IF


         BEGIN  = IVLBDS(1)
!
!        The smaller of LSTEPC and IVLBDS(2) is the
!        end of the mini-segment's coverage.
!
         FINISH = MIN ( LSTEPC, IVLBDS(2) )
!
!        Truncate the interval using the segment bounds.
!
         BEGIN   =  MAX ( BEGIN,  DC(1) )
         FINISH  =  MIN ( FINISH, DC(2) )

!
!        Adjust the interval using the tolerance. Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 6 reading algorithm.
!
         IF ( BEGIN .LE. FINISH ) THEN

            IF ( TOL .GT. 0.D0 ) THEN

               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF

         END IF

!
!        Convert the time to TDB if necessary.
!
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, BEGIN,  ET )
            BEGIN  = ET

            CALL SCT2E ( SCLKID, FINISH, ET )
            FINISH = ET

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_ZZCKCV06' )
               RETURN
            END IF

         END IF

!
!        Insert the interval into the window.
!
         IF ( BEGIN .LE. FINISH ) THEN

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_ZZCKCV06' )
               RETURN
            END IF

         END IF

      END DO


      CALL CHKOUT ( 'T_ZZCKCV06' )
      RETURN
      END
