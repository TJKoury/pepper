!$Procedure CKR04 ( C-kernel, read pointing record, data type 4 )

      subroutine ckr04 ( handle, descr, sclkdp, tol, needav, &
                         record, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single data record from a type 4 CK segment.
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

      implicit none

      include               'ckparam.inc'

      integer(ip)           handle
      real(wp)              descr  ( * )
      real(wp)              sclkdp
      real(wp)              tol
      logical               needav
      real(wp)              record ( * )
      logical               found

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
      logical               return
      logical               failed

!
!     Local variables
!
      real(wp)              value
      real(wp)              midpt1
      real(wp)              midpt2
      real(wp)              rad1
      real(wp)              rad2
      real(wp)              lbnd1
      real(wp)              lbnd2
      real(wp)              rbnd1
      real(wp)              dcd    ( 2 )
      real(wp)              clkout

      integer(ip)           numall
      integer(ip)           numcft ( qavsiz )
      integer(ip)           icd    ( 6 )
      integer(ip)           ends
      integer(ip)           indx
      integer(ip)           k
      integer(ip)           nrec

      logical               exist

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CKR04' )
      end if

!
!     Set initial value of the found flag to "NOT FOUND".
!
      found = .false.

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
      call dafus  ( descr, 2, 6, dcd, icd )

!
!     Check if the segment is type 4. Signal an error if it's not.
!
      if ( icd( 3 ) /= ck4dtp ) then

         call setmsg ( 'The segment is not a type 4 segment.  '   // &
                       'Type is #'                                 )
         call errint ( '#', icd(3)                                 )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         call chkout ( 'CKR04'                                     )
         return

      end if

      if ( needav ) then

!
!        Signal an error if angular velocities are required but
!        they are not present in the segment.
!
         if ( icd( 4 ) /= 1 ) then

            call setmsg ( 'Segment does not contain angular '     // &
                          'velocity data.'                        )
            call sigerr ( 'SPICE(NOAVDATA)'                       )
            call chkout ( 'CKR04'                                 )
            return

         end if

      end if

!
!     Get number of records (packets) in the segment.
!
      call cknr04 ( handle, descr, nrec )

!
!     Locate the last time in the set of reference epochs less than or
!     equal to the input SCLKDP.
!
      call sgfrvi ( handle, descr, sclkdp, value, indx, exist )

      if ( failed() ) then
         call chkout ( 'CKR04' )
         return
      end if

      if ( .not. exist ) then

!
!        We didn't find reference value with means that SCLKDP is
!        less than the left bound of the first interpolation interval.
!        Fetch the first record.
!
         indx = 1
         call sgfpkt ( handle, descr, indx, indx, record, ends )

         if ( failed() ) then
            call chkout ( 'CKR04' )
            return
         end if

         midpt1 = record( 1 )
         rad1   = record( 2 )

!
!        Check whether SCLKDP is within TOL of the left bound of the
!        first interval.
!
         lbnd1 = midpt1 - rad1 - tol

         if ( sclkdp >= lbnd1  ) then

            found = .true.
            clkout = midpt1 - rad1

         end if

      else

!
!        We found reference value.
!
         if ( indx >= nrec ) then

!
!           The SCLKDP is greater than the left bound of the last
!           interpolation interval. Fetch the last record.
!
            indx = nrec

            call sgfpkt ( handle, descr, indx, indx, record, ends )

            if ( failed() ) then
               call chkout ( 'CKR04' )
               return
            end if

            midpt1 = record( 1 )
            rad1   = record( 2 )

!
!           Check whether SCLKDP is within TOL of the right bound of
!           the last interval.
!
            rbnd1 = midpt1 + rad1 + tol

            if ( sclkdp <= rbnd1 ) then

               found = .true.

!
!              Check whether SCLKDP falls between right bound of the
!              last interval and right bound + TOL.
!
               rbnd1 = midpt1 + rad1

               if ( sclkdp >= rbnd1 ) then

                  clkout = midpt1 + rad1

               else

!
!                 SCLKDP belongs to the last interval
!
                  clkout = sclkdp

               end if

            end if

         else if ( ( indx >= 1 ) .and. ( indx < nrec ) ) then

!
!           The SCLKDP lies between left bound of the first interval
!           and the right bound of the interval before the last
!           interval. Fetch the found record.
!
            call sgfpkt ( handle, descr, indx, indx, record, ends )

            if ( failed() ) then
               call chkout ( 'CKR04' )
               return
            end if

            midpt1 = record( 1 )
            rad1   = record( 2 )

!
!           Check whether SCLKDP belongs to current interval.
!
            rbnd1 = midpt1 + rad1

            if ( sclkdp <= rbnd1 ) then

               found = .true.
               clkout = sclkdp

            else

!
!              SCLKDP doesn't belong to current interval. Fetch the
!              next packet.
!
               call sgfpkt ( handle, descr, indx + 1, indx + 1, record, &
                                                               ends )

               if ( failed() ) then
                  call chkout ( 'CKR04' )
                  return
               end if

               midpt2 = record( 1 )
               rad2   = record( 2 )

!
!              Find the closest interval bound for SCLKDP.
!
               rbnd1 = midpt1 + rad1
               lbnd2 = midpt2 - rad2

               if ( ( sclkdp - rbnd1 ) <= ( lbnd2 - sclkdp ) ) then

!
!                 SCLKDP is closer to the right bound of current
!                 interval. Check whether it's within TOL of it.
!
                  rbnd1 = midpt1 + rad1 + tol

                  if ( sclkdp <= rbnd1 ) then

                     found = .true.
                     clkout = midpt1 + rad1

!
!                    At this point we need to re-read our current
!                    record because it was overwritten by the next
!                    record. No FAILED() check here -- we already
!                    fetched this packet successfully one call to
!                    SGFPKT ago.
!
                     call sgfpkt( handle, descr, indx, indx, record, &
                                                                ends )

                  end if

               else

!
!                 SCLKDP is closer to the left bound of the next
!                 interval. Check whether it's within TOL of it.
!
                  lbnd2 = midpt2 - rad2 - tol

                  if ( sclkdp >= lbnd2 ) then

                     found = .true.
                     indx = indx + 1
                     clkout = midpt2 - rad2

                  end if

               end if

            end if

         end if

      end if

!
!     If we found the interval on segment the SCLKDP belongs to, then
!
      if ( found ) then

!
!        Decode numbers of polynomial coefficients.
!
         call zzck4d2i ( record( 3 ), qavsiz, ck4pcd, numcft )

!
!        Count total number of coefficients.
!
         numall = 0

         do k = 1, qavsiz
            numall = numall + numcft ( k )
         end do

!
!        Move coefficients to the right and insert numbers of
!        coefficients into output RECORD.
!
         do k = numall, 1, -1
            record ( k + ck4sft ) = record( k + 3 )
         end do

         do k = 1, qavsiz
            record( k + 3 ) = numcft( k )
         end do

         record( 3 ) = record( 2 )
         record( 2 ) = record( 1 )

!
!        Insert CLKOUT into output RECORD
!
         record( 1 ) = clkout

      end if

!
!     All done.
!
      call chkout ( 'CKR04' )

      return

      end
