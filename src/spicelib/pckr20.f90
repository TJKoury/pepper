!$Procedure PCKR20 ( PCK, read record from segment, type 20 )

      SUBROUTINE PCKR20 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single PCK data record from a segment of type 20
!     (Chebyshev, derivative coefficients only).
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
!     PCK
!
! $ Keywords
!
!     ORIENTATION
!     ROTATION
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Evaluation epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a PCK segment of type 20.
!
!     ET       is an epoch for which a data record from a specific
!              segment is required. ET is expressed as seconds past
!              J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which, when
!              evaluated at epoch ET, will give Euler angles and
!              Euler angle rates representing the orientation and
!              angular velocity of the body-fixed reference frame
!              associated with the segment.
!
!              The structure of the record is as follows:
!
!                 +--------------------------------------+
!                 | record size (excluding this element) |
!                 +--------------------------------------+
!                 | Coverage interval midpoint           |
!                 +--------------------------------------+
!                 | Coverage interval radius             |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_1 rate              |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_2 rate              |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_3 rate              |
!                 +--------------------------------------+
!                 | ANGLE_1 at interval midpoint         |
!                 +--------------------------------------+
!                 | ANGLE_2 at interval midpoint         |
!                 +--------------------------------------+
!                 | ANGLE_3 at interval midpoint         |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Angular components have units of radians.
!                 - Rate coefficients have units of radians/s.
!
!              RECORD must be declared by the caller with size large
!              enough to accommodate the largest record that can be
!              returned by this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an issue is detected while looking up PCK data, an error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the PCK Required Reading file for a description of the
!     structure of a data type 20 (Chebyshev polynomials,
!     derivative coefficients only) segment.
!
! $ Examples
!
!     The data returned by the PCKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the PCKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified frame class ID
!     C     and epoch.
!     C
!           CALL PCKSFS ( CLSSID, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           REF    = ICD( 2 )
!           TYPE   = ICD( 3 )
!
!           IF ( TYPE .EQ. 20 ) THEN
!              CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_20 PCK segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DC       ( 2 )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      INTRVL
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      POS      ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RECBEG
      DOUBLE PRECISION      TSCALE

      INTEGER               BEGIN
      INTEGER               END
      INTEGER               I
      INTEGER               IC       ( 6 )
      INTEGER               LOC
      INTEGER               NREC
      INTEGER               NTERMS
      INTEGER               RECADR
      INTEGER               RECNO
      INTEGER               RECSIZ
      INTEGER               SIZE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKR20' )
!
!     Unpack the segment descriptor.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      BEGIN = IC( 4 )
      END   = IC( 5 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      CALL DAFGDA ( HANDLE, END-6, END, RECORD )

      DSCALE = RECORD( 1 )
      TSCALE = RECORD( 2 )
      INITJD = RECORD( 3 )
      INITFR = RECORD( 4 )
      INTLEN = RECORD( 5 )
      RECSIZ = INT( RECORD( 6 ) )
      NREC   = INT( RECORD( 7 ) )

!
!     NTERMS is the number of rate coefficients per component,
!     plus 1 (for the angle component).
!
      NTERMS = RECSIZ / 3

!
!     Convert the initial epoch and interval length to
!     seconds past J2000 TDB.
!
      INIT   = (  ( INITJD - J2000() )  +  INITFR  ) * SPD()
      INTRVL = INTLEN * SPD()

!
!     Locate the record containing the coefficients to use.
!
      RECNO  = INT( (ET - INIT) / INTRVL ) + 1
      RECNO  = MAX( 1,  MIN( RECNO, NREC )  )

!
!     Compute the midpoint and radius of the record at
!     index RECNO. We want to compute the midpoint in such
!     a way that we take advantage of interval lengths that
!     are exactly representable, when we have them.
!
!     RECBEG is the record start time, minus the fractional
!     part of the segment start time, expressed as seconds
!     past J2000. We'll account for the fractional part of the
!     start time below when we compute MID.
!
      RECBEG = (  ( INITJD - J2000() )  +  (RECNO-1)*INTLEN  ) * SPD()


      RADIUS = INTRVL / 2.D0

      MID    = RECBEG + ( INITFR * SPD() ) + RADIUS

!
!     Compute the address of the desired record.
!
      RECADR = ( RECNO - 1 )*RECSIZ + BEGIN

!
!     Along with the record, return the size, midpoint, and
!     radius of the record.
!
      RECORD( 1 ) = RECORD( 6 ) + 2
      RECORD( 2 ) = MID
      RECORD( 3 ) = RADIUS
      CALL DAFGDA ( HANDLE, RECADR, RECADR + RECSIZ - 1, RECORD( 4 ) )

!
!     We're going to re-arrange the record: the angle components
!     will be transferred to the end of the record, and the record
!     contents will be left-shifted to fill in the free elements.
!
      DO I = 1, 3
         POS( I ) = RECORD ( 3  +  I*NTERMS )
      END DO


      SIZE = RECSIZ + 3
!
!     Remove the angle elements from the record.
!
      DO I = 1, 3
!
!        LOC is the index of the element to delete. After the first
!        removal, we must account for the resulting left shift when
!        calculating the indices of subsequent elements to be removed.
!
         LOC = 3 + I*NTERMS - (I-1)

         CALL REMLAD ( 1, LOC, RECORD, SIZE )
!
!        Note that SIZE is an in-out argument; on output it
!        indicates the size of the array after removal of
!        the indicated element(s).
!
      END DO

!
!     Convert the angles to radians.
!
      CALL VSCLIP ( DSCALE, POS )

!
!     Append the angles to the record. Since we inserted three
!     elements at the start of the record and deleted three angle
!     elements, the target index is the same as if we had copied the
!     record directly to the output array.
!
      CALL MOVED ( POS, 3, RECORD(RECSIZ+1) )

!
!     Convert the angular rate Chebyshev coefficients to units of
!     radians/s.
!
      DO I = 4, RECSIZ
         RECORD(I) = RECORD(I) * (DSCALE/TSCALE)
      END DO

      CALL CHKOUT ( 'PCKR20' )
      RETURN
      END
