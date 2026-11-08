!$Procedure ZZCHRLAT ( Chord latitude  )

      SUBROUTINE ZZCHRLAT ( MIDLAT, DLON, EPTLAT )

! $ Abstract
!
!     Given the latitude of a midpoint of chord on a circle of constant
!     latitude, and given the longitude extent of the chord, compute
!     the latitude of the chord's endpoints. The coordinate system is
!     "latitudinal," aka planetocentric.
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
!     COORDINATES
!     LATITUDE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dsktol.inc'

      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      EPTLAT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MIDLAT     I   is the latitude of the midpoint of a chord of a
!                    latitude circle.
!     DLON       I   is the longitude extent of the chord.
!     EPTLAT     O   is the latitude of the endpoints of the chord.
!
! $ Detailed_Input
!
!     MIDLAT     is the latitude of the midpoint of a chord of a circle
!                of constant latitude. Units are radians. The range of
!                MIDLAT is -pi : pi.
!
!     DLON       is the extent in longitude of the chord. DLON is the
!                difference of the longitudes of the endpoints of the
!                chords, expressed as a non-negative value. Units are
!                radians. DLON must be strictly less than pi.
!
! $ Detailed_Output
!
!     EPTLAT     is the latitude of the circle to which the chord
!                is tangent. Units are radians.
!
! $ Parameters
!
!     ANGMRG is a tolerance used to determine whether the input latitude
!     is within range. See 'dsktol.inc' for further information.
!
! $ Exceptions
!
!     1)  If the input longitude extent DLON is negative, or if the
!         extent is greater than or equal to pi radians, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If the input latitude is outside the range
!
!             -pi/2 - ANGMRG  :  pi/2 + ANGMRG
!
!         the error SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     In the remarks below, the coordinate system is presumed to be
!     latitudinal.
!
!     A "chord" of a circle is a line segment having endpoints on the
!     circle.
!
!     This routine supports partitioning of a tessellated spheroid
!     surface into bands of plates covering specified latitude ranges.
!     Note that, for a plate edge that is a chord of a positive
!     latitude circle, the maximum latitude on the chord is attained at
!     the chord's midpoint. So a set of plates lying on or above the
!     plate containing the latitude circle cannot actually cover the
!     latitude range above the circle: rays emanating from the origin
!     can pass above the latitude circle and miss the set of plates. An
!     analogous situation exists for circles of negative latitude.
!
!     In order to generate a set of plates that cover a positive
!     latitude band, the plates can be arranged so that the maximum
!     latitude of the lowest plate edges is less than or equal to the
!     lower latitude bound of the band. An analogous solution can be
!     derived for bands of negative latitude. This routine can be used
!     to solve these problems: it can compute the latitude of a circle
!     ---where the circle is horizontal and centered on the z axis---
!     in which a regular polygon must be inscribed so that the maximum
!     latitude on the polygon is less than a specified value, or such
!     that the minimum latitude on the polygon is greater than a
!     specified value.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1)  Find the latitude circle on which a line segment's endpoints
!         must lie, if the longitude extent of the segment is 60
!         degrees, and if the latitude of the segment's midpoint is 30
!         degrees.
!
!
!     Example code begins here.
!
!
!           PROGRAM EX1
!           IMPLICIT NONE
!
!     C
!     C     SPICELIB functions
!     C
!           DOUBLE PRECISION      DPR
!           DOUBLE PRECISION      RPD
!
!     C
!     C     Local variables
!     C
!           DOUBLE PRECISION      DLON
!           DOUBLE PRECISION      EP     ( 3, 2 )
!           DOUBLE PRECISION      EPTLAT
!           DOUBLE PRECISION      MIDLAT
!           DOUBLE PRECISION      MIDLON
!           DOUBLE PRECISION      MIDRAD
!           DOUBLE PRECISION      MIDPT  ( 3 )
!           DOUBLE PRECISION      OUTLAT
!
!     C
!     C     Let the segment have a longitude extent
!     C     of 60 degrees.
!     C
!           DLON   = 60.D0 * RPD()
!
!     C
!     C     Let the midpoint of the segment have
!     C     a latitude of 30 degrees.
!     C
!           MIDLAT = 30.D0 * RPD()
!
!     C
!     C     Find the latitude of the segment's endpoints.
!     C
!           CALL ZZCHRLAT ( MIDLAT, DLON, EPTLAT )
!
!           WRITE (*,*) 'Endpoint latitude (deg) = ', EPTLAT*DPR()
!
!     C
!     C     Generate the endpoints and the latitude
!     C     of the segment's midpoint.
!     C
!     C     Note that the scale is arbitrary: we can set it
!     C     to 1.0. The longitudes of the endpoints are
!     C     arbitrary as well; only the difference is known.
!     C
!           CALL LATREC ( 1.D0, 0.D0, EPTLAT, EP(1,1) )
!           CALL LATREC ( 1.D0, DLON, EPTLAT, EP(1,2) )
!
!           CALL VLCOM  ( 0.5D0, EP(1,1),
!          .              0.5D0, EP(1,2), MIDPT )
!
!           CALL RECLAT ( MIDPT, MIDRAD, MIDLON, OUTLAT )
!
!           WRITE (*,*) 'Check: difference (deg) = ',
!          .            (MIDLAT - OUTLAT) * DPR()
!
!           END
!
!
!     When this program was executed on a PC/Linux/gfortran 64-bit
!     platform, the output was:
!
!
!        Endpoint latitude (deg) =    26.565051177077986
!        Check: difference (deg) =   6.36110936292703354E-015
!
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
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 20-APR-2016 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     find latitude of chord's endpoints on latitude circle
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      MLAT

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( ( DLON .LT. 0.D0 ) .OR. ( DLON .GE. PI() ) )  THEN

         CALL CHKIN  ( 'ZZCHRLAT'                          )
         CALL SETMSG ( 'The input longitude extent was #; ' &
         //            'this value must be in the range ' &
         //            '[0 : pi ) radians.'                )
         CALL ERRDP  ( '#', DLON                           )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'            )
         CALL CHKOUT ( 'ZZCHRLAT'                          )
         RETURN

      END IF

      IF (  ABS(MIDLAT)  .GT.  ( HALFPI()+ANGMRG )  ) THEN

         CALL CHKIN  ( 'ZZCHRLAT'                          )
         CALL SETMSG ( 'The input latitude was #; this ' &
         //            'value must be in the interval ' &
         //            '-pi/2 : pi/2 (radians).'           )
         CALL ERRDP  ( '#', MIDLAT                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'            )
         CALL CHKOUT ( 'ZZCHRLAT'                          )
         RETURN

      END IF

!
!     The input latitude is, at worst, slightly out of range.
!     Bracket it.
!
      MLAT = BRCKTD( MIDLAT, -HALFPI(), HALFPI() )

!
!     The endpoint latitude EPTLAT is defined by
!
!        EPTLAT = atan ( tan(MLAT) * cos( DLON/2 ) )
!
!     For numerical robustness, we'll re-write this using
!     the two-argument arctangent function and well-behaved
!     trig functions as input arguments:
!
      EPTLAT = ATAN2 ( SIN(MLAT) * COS(DLON/2),  COS(MLAT) )

      RETURN
      END
