!$Procedure CKR04 ( C-kernel, read pointing record, data type 4 )

      SUBROUTINE CKR04 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, &
                         RECORD, FOUND )

! $ Abstract
!
!     Read a single data record from a type 4 CK segment.
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
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'ckparam.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Pointing request time.
!     TOL        I   Time tolerance.
!     NEEDAV     I   Angular velocity request flag.
!     RECORD     O   Pointing data record.
!     FOUND      O   .TRUE. when a record covering SCLKDP is found.
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle of the CK file containing the
!              segment.
!
!     DESCR    is the descriptor of the segment.
!
!     SCLKDP   is the encoded spacecraft clock time for which
!              pointing is being requested.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls within the bounds of one of the
!              interpolation intervals then the tolerance has no
!              effect because pointing will be returned at the
!              request time.
!
!              However, if the request time is not in one of the
!              intervals, then the tolerance is used to determine
!              if pointing at one of the interval endpoints should
!              be returned.
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
! $ Detailed_Output
!
!     RECORD   is the record that CKE04 will evaluate to determine
!              the pointing and it includes parameters:
!
!              ---------------------------------------------------
!              |    Encoded onboard time which is the closest    |
!              |  to SCLKDP and belongs to one of approximation  |
!              |                   intervals                     |
!              ---------------------------------------------------
!              |       encoded SCLK time of the midpoint of      |
!              |             interpolation interval              |
!              ---------------------------------------------------
!              |          radii of interpolation interval        |
!              |    expressed as double precision SCLK ticks     |
!              ---------------------------------------------------
!              |         Number of coefficients for q0           |
!              ---------------------------------------------------
!              |         Number of coefficients for q1           |
!              ---------------------------------------------------
!              |         Number of coefficients for q2           |
!              ---------------------------------------------------
!              |         Number of coefficients for q3           |
!              ---------------------------------------------------
!              |         Number of coefficients for AV1          |
!              ---------------------------------------------------
!              |         Number of coefficients for AV2          |
!              ---------------------------------------------------
!              |         Number of coefficients for AV3          |
!              ---------------------------------------------------
!              |               q0 Cheby coefficients             |
!              ---------------------------------------------------
!              |               q1 Cheby coefficients             |
!              ---------------------------------------------------
!              |               q2 Cheby coefficients             |
!              ---------------------------------------------------
!              |               q3 Cheby coefficients             |
!              ---------------------------------------------------
!              |         AV1 Cheby coefficients (optional)       |
!              ---------------------------------------------------
!              |         AV2 Cheby coefficients (optional)       |
!              ---------------------------------------------------
!              |         AV3 Cheby coefficients (optional)       |
!              ---------------------------------------------------
!
!     FOUND    is .TRUE. if a record was found to satisfy the pointing
!              request. This occurs when the time for which pointing
!              is requested falls inside one of the interpolation
!              intervals, or when the request time is within the
!              tolerance of an interval endpoint.
!
! $ Parameters
!
!     See 'ckparam.inc'.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to an open DAF file,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the specified descriptor does not belong to a segment data
!         format organized in accordance with generic segment
!         architecture, an error is signaled by a routine in the call
!         tree of this routine.
!
!     3)  If DESCR is not a valid descriptor of a segment in the CK
!         file specified by HANDLE, the results of this routine are
!         unpredictable.
!
!     4)  If the segment is not of data type 4, as specified in the
!         third integer component of the segment descriptor,
!         the error SPICE(WRONGDATATYPE) is signaled.
!
!     5)  If angular velocity data was requested but the segment
!         contains no such data, the error SPICE(NOAVDATA) is
!         signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the CK Required Reading file for a detailed description of
!     the structure of a type 4 pointing segment.
!
!     When the time for which pointing was requested falls within an
!     interpolation interval, then FOUND will be true and RECORD will
!     contain the set of Chebyshev polynomial coefficients for the
!     time interval that brackets the request time. CKE04 will
!     evaluate RECORD to give pointing at the request time.
!
!     However, when the request time is not within any of the
!     interpolation intervals, then FOUND will be true only if the
!     interval endpoint closest to the request time is within the
!     tolerance specified by the user. In this case RECORD will
!     contain the set of Chebyshev polynomial coefficients for the
!     time interval one of the ends of which was within tolerance
!     from the request time, and CKE04 will evaluate RECORD to give
!     pointing at the time associated with that interval end time.
!
! $ Examples
!
!     The CKRnn routines are usually used in tandem with the CKEnn
!     routines, which evaluate the record returned by CKRnn to give
!     the pointing information and output time.
!
!     The following code fragment searches backwards through all of the
!     segments in a file applicable to the Mars Global Surveyor
!     spacecraft bus that are of data type 4, for a particular
!     spacecraft clock time. It then evaluates the pointing for that
!     epoch and prints the result.
!
!     The search performed here does not mimic the behavior of the CK
!     reader APIs CKGP and CKGPAV, which consider data from multiple CK
!     files, when available. See the CK Required reading for details.
!
!     C
!     C     CK parameters include file.
!     C
!           INCLUDE               'ckparam.inc'
!
!     C
!     C     Local variables
!     C
!           CHARACTER*(20)        SCLKCH
!           CHARACTER*(20)        SCTIME
!           CHARACTER*(40)        IDENT
!
!           DOUBLE PRECISION      AV     ( 3 )
!           DOUBLE PRECISION      CLKOUT
!           DOUBLE PRECISION      CMAT   ( 3, 3 )
!           DOUBLE PRECISION      DCD    ( 2 )
!           DOUBLE PRECISION      DESCR  ( 5 )
!           DOUBLE PRECISION      RECORD ( CK4RSZ )
!           DOUBLE PRECISION      SCLKDP
!           DOUBLE PRECISION      TOL
!
!           INTEGER               HANDLE
!           INTEGER               I
!           INTEGER               ICD    ( 6 )
!           INTEGER               INST
!           INTEGER               SC
!
!           LOGICAL               FND
!           LOGICAL               NEEDAV
!           LOGICAL               SFND
!
!     C
!     C     Initial values.
!     C
!           SC     = -94
!           INST   = -94000
!           NEEDAV = .FALSE.
!
!     C
!     C     Load the MGS SCLK kernel and the C-kernel.
!     C
!           CALL FURNSH( 'MGS_SCLK.TSC' )
!           CALL DAFOPR( 'MGS_CK4.BC', HANDLE )
!
!     C
!     C     Get the spacecraft clock time. Then encode it for use
!     C     in the C-kernel.
!     C
!           CALL PROMPT( 'Enter SCLK string: ', SCLKCH )
!           CALL SCENCD( SC, SCLKCH, SCLKDP )
!
!     C
!     C     Use a tolerance of 2 seconds (half of the nominal
!     C     separation between MGS pointing instances ).
!     C
!           CALL SCTIKS ( SC, '0000000002:000', TOL )
!
!     C
!     C     Search backwards from the end of the CK file through all
!     C     of the segments.
!     C
!           CALL DAFBBS( HANDLE )
!           CALL DAFFPA( SFND   )
!
!           FND = .FALSE.
!
!           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
!
!     C
!     C        Get the segment identifier and descriptor.
!     C
!              CALL DAFGN( IDENT )
!              CALL DAFGS( DESCR )
!
!     C
!     C        Unpack the segment descriptor into its integer and
!     C        double precision components.
!     C
!              CALL DAFUS( DESCR, 2, 6, DCD, ICD )
!
!     C
!     C        Determine if this segment should be processed.
!     C
!              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
!          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
!          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
!          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN
!
!     C
!     C           Find CK 4 record covering requested time.
!     C
!                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                  RECORD, FND )
!
!                 IF ( FND ) THEN
!
!     C
!     C              Compute pointing using found CK 4 record.
!     C
!                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT)
!
!                    CALL SCDECD( SC, CLKOUT, SCTIME )
!
!                    WRITE (*,*)
!                    WRITE (*,*) 'Segment identifier: ', IDENT
!                    WRITE (*,*)
!                    WRITE (*,*) 'Pointing returned for time: ',
!          .                      SCTIME
!                    WRITE (*,*)
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*)
!                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
!                    WRITE (*,*)
!
!                 END IF
!
!              END IF
!
!              CALL DAFFPA ( SFND )
!
!           END DO
!
! $ Restrictions
!
!     1)  The file containing the segment should be opened for read
!         or write access either by CKLPF, DAFOPR, or DAFOPW.
!
!     2)  The record returned by this routine is intended to be
!         evaluated by CKE04.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!     Y.K. Zaiko         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 17-AUG-2021 (NJB) (JDR)
!
!        Updated code example to use backwards search. Added
!        note regarding difference between this search and those
!        performed by the CK reader APIs CKGP and CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
!
!        Minor header edits.
!
!-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
!
!-&


! $ Index_Entries
!
!     read record from type_4 CK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local variables
!
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      MIDPT1
      DOUBLE PRECISION      MIDPT2
      DOUBLE PRECISION      RAD1
      DOUBLE PRECISION      RAD2
      DOUBLE PRECISION      LBND1
      DOUBLE PRECISION      LBND2
      DOUBLE PRECISION      RBND1
      DOUBLE PRECISION      DCD    ( 2 )
      DOUBLE PRECISION      CLKOUT

      INTEGER               NUMALL
      INTEGER               NUMCFT ( QAVSIZ )
      INTEGER               ICD    ( 6 )
      INTEGER               ENDS
      INTEGER               INDX
      INTEGER               K
      INTEGER               NREC

      LOGICAL               EXIST

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKR04' )
      END IF

!
!     Set initial value of the found flag to "NOT FOUND".
!
      FOUND = .FALSE.

!
!     We need to unpack and analyze descriptor components. The
!     unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!
      CALL DAFUS  ( DESCR, 2, 6, DCD, ICD )

!
!     Check if the segment is type 4. Signal an error if it's not.
!
      IF ( ICD( 3 ) .NE. CK4DTP ) THEN

         CALL SETMSG ( 'The segment is not a type 4 segment.  '   // &
                       'Type is #'                                 )
         CALL ERRINT ( '#', ICD(3)                                 )
         CALL SIGERR ( 'SPICE(WRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKR04'                                     )
         RETURN

      END IF

      IF ( NEEDAV ) THEN

!
!        Signal an error if angular velocities are required but
!        they are not present in the segment.
!
         IF ( ICD( 4 ) .NE. 1 ) THEN

            CALL SETMSG ( 'Segment does not contain angular '     // &
                          'velocity data.'                        )
            CALL SIGERR ( 'SPICE(NOAVDATA)'                       )
            CALL CHKOUT ( 'CKR04'                                 )
            RETURN

         END IF

      END IF

!
!     Get number of records (packets) in the segment.
!
      CALL CKNR04 ( HANDLE, DESCR, NREC )

!
!     Locate the last time in the set of reference epochs less than or
!     equal to the input SCLKDP.
!
      CALL SGFRVI ( HANDLE, DESCR, SCLKDP, VALUE, INDX, EXIST )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKR04' )
         RETURN
      END IF

      IF ( .NOT. EXIST ) THEN

!
!        We didn't find reference value with means that SCLKDP is
!        less than the left bound of the first interpolation interval.
!        Fetch the first record.
!
         INDX = 1
         CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CKR04' )
            RETURN
         END IF

         MIDPT1 = RECORD( 1 )
         RAD1   = RECORD( 2 )

!
!        Check whether SCLKDP is within TOL of the left bound of the
!        first interval.
!
         LBND1 = MIDPT1 - RAD1 - TOL

         IF ( SCLKDP .GE. LBND1  ) THEN

            FOUND = .TRUE.
            CLKOUT = MIDPT1 - RAD1

         END IF

      ELSE

!
!        We found reference value.
!
         IF ( INDX .GE. NREC ) THEN

!
!           The SCLKDP is greater than the left bound of the last
!           interpolation interval. Fetch the last record.
!
            INDX = NREC

            CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR04' )
               RETURN
            END IF

            MIDPT1 = RECORD( 1 )
            RAD1   = RECORD( 2 )

!
!           Check whether SCLKDP is within TOL of the right bound of
!           the last interval.
!
            RBND1 = MIDPT1 + RAD1 + TOL

            IF ( SCLKDP .LE. RBND1 ) THEN

               FOUND = .TRUE.

!
!              Check whether SCLKDP falls between right bound of the
!              last interval and right bound + TOL.
!
               RBND1 = MIDPT1 + RAD1

               IF ( SCLKDP .GE. RBND1 ) THEN

                  CLKOUT = MIDPT1 + RAD1

               ELSE

!
!                 SCLKDP belongs to the last interval
!
                  CLKOUT = SCLKDP

               END IF

            END IF

         ELSE IF ( ( INDX .GE. 1 ) .AND. ( INDX .LT. NREC ) ) THEN

!
!           The SCLKDP lies between left bound of the first interval
!           and the right bound of the interval before the last
!           interval. Fetch the found record.
!
            CALL SGFPKT ( HANDLE, DESCR, INDX, INDX, RECORD, ENDS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CKR04' )
               RETURN
            END IF

            MIDPT1 = RECORD( 1 )
            RAD1   = RECORD( 2 )

!
!           Check whether SCLKDP belongs to current interval.
!
            RBND1 = MIDPT1 + RAD1

            IF ( SCLKDP .LE. RBND1 ) THEN

               FOUND = .TRUE.
               CLKOUT = SCLKDP

            ELSE

!
!              SCLKDP doesn't belong to current interval. Fetch the
!              next packet.
!
               CALL SGFPKT ( HANDLE, DESCR, INDX + 1, INDX + 1, RECORD, &
                                                               ENDS )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CKR04' )
                  RETURN
               END IF

               MIDPT2 = RECORD( 1 )
               RAD2   = RECORD( 2 )

!
!              Find the closest interval bound for SCLKDP.
!
               RBND1 = MIDPT1 + RAD1
               LBND2 = MIDPT2 - RAD2

               IF ( ( SCLKDP - RBND1 ) .LE. ( LBND2 - SCLKDP ) ) THEN

!
!                 SCLKDP is closer to the right bound of current
!                 interval. Check whether it's within TOL of it.
!
                  RBND1 = MIDPT1 + RAD1 + TOL

                  IF ( SCLKDP .LE. RBND1 ) THEN

                     FOUND = .TRUE.
                     CLKOUT = MIDPT1 + RAD1

!
!                    At this point we need to re-read our current
!                    record because it was overwritten by the next
!                    record. No FAILED() check here -- we already
!                    fetched this packet successfully one call to
!                    SGFPKT ago.
!
                     CALL SGFPKT( HANDLE, DESCR, INDX, INDX, RECORD, &
                                                                ENDS )

                  END IF

               ELSE

!
!                 SCLKDP is closer to the left bound of the next
!                 interval. Check whether it's within TOL of it.
!
                  LBND2 = MIDPT2 - RAD2 - TOL

                  IF ( SCLKDP .GE. LBND2 ) THEN

                     FOUND = .TRUE.
                     INDX = INDX + 1
                     CLKOUT = MIDPT2 - RAD2

                  END IF

               END IF

            END IF

         END IF

      END IF

!
!     If we found the interval on segment the SCLKDP belongs to, then
!
      IF ( FOUND ) THEN

!
!        Decode numbers of polynomial coefficients.
!
         CALL ZZCK4D2I ( RECORD( 3 ), QAVSIZ, CK4PCD, NUMCFT )

!
!        Count total number of coefficients.
!
         NUMALL = 0

         DO K = 1, QAVSIZ
            NUMALL = NUMALL + NUMCFT ( K )
         END DO

!
!        Move coefficients to the right and insert numbers of
!        coefficients into output RECORD.
!
         DO K = NUMALL, 1, -1
            RECORD ( K + CK4SFT ) = RECORD( K + 3 )
         END DO

         DO K = 1, QAVSIZ
            RECORD( K + 3 ) = NUMCFT( K )
         END DO

         RECORD( 3 ) = RECORD( 2 )
         RECORD( 2 ) = RECORD( 1 )

!
!        Insert CLKOUT into output RECORD
!
         RECORD( 1 ) = CLKOUT

      END IF

!
!     All done.
!
      CALL CHKOUT ( 'CKR04' )

      RETURN

      END
