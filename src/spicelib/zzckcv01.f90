!$Procedure ZZCKCV01 ( Private --- C-kernel segment coverage, type 01 )

      SUBROUTINE ZZCKCV01 ( HANDLE,  ARRBEG,  ARREND, &
                            SCLKID,  TOL,     TIMSYS,  SCHEDL  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 01 C-kernel segment.
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

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HANDLE
      INTEGER               ARRBEG
      INTEGER               ARREND
      INTEGER               SCLKID
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
!     SCLKID     I   ID of SCLK associated with segment.
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 01 segment
!
!     ARREND     is the ending address of a type 01 segment.
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. Before
!                each interval is inserted into the coverage window,
!                the intervals are expanded by TOL:  the left endpoint
!                of each interval is reduced by TOL and the right
!                endpoint is increased by TOL.  Any intervals that
!                overlap as a result of the expansion are merged.
!
!                The coverage window returned when TOL > 0 indicates
!                the coverage provided by the file to the CK readers
!                CKGPAV and CKGP when that value of TOL is passed to
!                them as an input.
!
!
!     TIMSYS     is a string indicating the time system used in the
!                output coverage window.  TIMSYS may have the values:
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
!                of coverage for this segment. Since type 01 segments,
!                don't have interpolation intervals, each epoch
!                associated with a pointing instance is treated as a
!                singleton interval.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     1) The error SPICE(BADCK1SEGMENT) is signaled if the derived
!        segment length from ARRBEG and ARREND does not match
!        the possible lengths computed from the segment metadata.
!
!     2) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
!     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!         is signaled.
!
!     5) If a time conversion error occurs, the error will be
!         diagnosed by a routine in the call tree of this routine.
!C
! $ Particulars
!
!     This is a utility routine that determines the intervals of
!     coverage for a type 01 C-kernel segment. Since type 01 segments,
!     don't have interpolation intervals, each epoch associated with a
!     pointing instance is treated as a singleton interval.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SUPPORT Version 1.0.0, 03-JAN-2005 (WLT)(NJB)(BVS)
!
!        Initial version.
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

      INTEGER               QAVSIZ
      PARAMETER           ( QAVSIZ = 7 )

      INTEGER               QSIZ
      PARAMETER           ( QSIZ   = 4 )

!
!     Local Variables
!
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH

      INTEGER               I
      INTEGER               N
      INTEGER               NREC
      INTEGER               OFFSET
      INTEGER               PSIZ
      INTEGER               REMAIN
      INTEGER               TBASE
      INTEGER               NAVSLN
      INTEGER               AVSLN
      INTEGER               SEGLEN

      LOGICAL               ISTDB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCKCV01' )
      END IF

!
!     Check tolerance value.
!
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZCKCV01'                                     )
         RETURN

      END IF

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      ISTDB  = EQSTR ( TIMSYS, 'TDB' )

!
!     Check time system.
!
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'ZZCKCV01'                                )
            RETURN

         END IF

      END IF

!
!     The coverage window is the set of singleton intervals consisting
!     of the epochs of the pointing records. We'll need to find the
!     epochs.
!
!     First, get the number of records in this segment.
!
      CALL DAFGDA ( HANDLE, ARREND, ARREND, BUFFER )

      NREC = INT( BUFFER(1) )

!
!     Determine the size of the pointing packets. This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      NAVSLN = 5 * NREC + ( NREC - 1 ) / 100 + 1

!
!     Assume a record size of 7, i.e. angular rate data.
!
      AVSLN  = 8 * NREC + ( NREC - 1 ) / 100 + 1

!
!     Compute the actual length of the segment.
!
      SEGLEN = ARREND - ARRBEG + 1

      IF ( SEGLEN .EQ. NAVSLN ) THEN
         PSIZ = QSIZ
      ELSE IF ( SEGLEN .EQ. AVSLN ) THEN
         PSIZ = QAVSIZ
      ELSE
         CALL SETMSG ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 1?'                              )
         CALL ERRHAN ( '#', HANDLE                                   )
         CALL ERRINT ( '#', SEGLEN                                   )
         CALL ERRINT ( '#', NAVSLN                                   )
         CALL ERRINT ( '#', AVSLN                                    )
         CALL SIGERR ( 'SPICE(BADCK1SEGMENT)'                        )
         CALL CHKOUT ( 'ZZCKCV01'                                    )
         RETURN
      END IF

!
!     The epochs start right after the pointing data. Let TBASE be the
!     address preceding the first epoch.
!
      TBASE  = ARRBEG  +  NREC * PSIZ  -  1

!
!     Grab the epochs. Make a singleton interval out of each one; add
!     the interval to the coverage window.
!
!     For efficiency, we'll read the epochs into a buffer of length
!     BUFSIZ.
!
      REMAIN = NREC
      OFFSET = 0

      DO WHILE ( REMAIN .GT. 0 )

!
!        Buffer the next set of epochs.
!
         N  =  MIN ( BUFSIZ, REMAIN )

         CALL DAFGDA ( HANDLE, &
                       TBASE + OFFSET + 1, &
                       TBASE + OFFSET + N, &
                       BUFFER              )

!
!        Insert the current batch of N singleton intervals.
!
         DO I = 1, N

            BEGIN   =  BUFFER(I)
            FINISH  =  BUFFER(I)


            IF ( TOL .GT. 0.D0 ) THEN
!
!              Adjust the interval using the tolerance.
!
               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF

!
!           Convert the time to TDB if necessary.
!
            IF ( ISTDB ) THEN

               CALL SCT2E ( SCLKID, BEGIN,  ET )
               BEGIN  = ET

               CALL SCT2E ( SCLKID, FINISH, ET )
               FINISH = ET

            END IF

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

         END DO

         OFFSET = OFFSET + N
         REMAIN = REMAIN - N

      END DO

      CALL CHKOUT ( 'ZZCKCV01' )
      RETURN
      END
