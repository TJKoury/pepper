!$Procedure ZZPDCMPL (Planetodetic coordinates, compare latitudes )

      SUBROUTINE ZZPDCMPL ( RE, F, P, LAT, REL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compare the planetodetic latitude of a point in 3-dimensional
!     space against a specified value, without converting the point to
!     planetodetic coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COORDINATES
!     GEOMETRY
!     PLANETODETIC
!     LATITUDE
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      DOUBLE PRECISION      P   ( 3 )
      DOUBLE PRECISION      LAT
      INTEGER               REL

      INTEGER               LT
      PARAMETER           ( LT = -1 )

      INTEGER               EQ
      PARAMETER           ( EQ =  0 )

      INTEGER               GT
      PARAMETER           ( GT =  1 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     RE         I   Equatorial radius.
!     F          I   Flattening coefficient.
!     P          I   Three-dimensional point.
!     LAT        I   Planetodetic latitude.
!     REL        O   Relation code.
!     LT         P   Code indicating latitude of P < LAT.
!     EQ         P   Code indicating latitude of P = LAT.
!     GT         P   Code indicating latitude of P > LAT.
!
! $ Detailed_Input
!
!     RE,
!     F          are, respectively, the equatorial radius
!                and flattening coefficient of a biaxial
!                spheroid.
!
!                The polar radius RP of the spheroid is
!
!                   RP = RE * ( 1 - F )
!
!                RP may be less than, equal to, or greater than RE.
!
!
!     P          is a point (equivalently, a vector) in
!                three-dimensional space. P is expressed in Cartesian
!                coordinates.
!
!                The units of P must be consistent with those of RE.
!
!
!     LAT        is a planetodetic latitude value to be compared
!                against the planetodetic latitude of P. Units
!                are radians.
!
! $ Detailed_Output
!
!     REL        is an integer code that indicates the order
!                relation between the planetodetic latitude of P
!                and LAT. The planetodetic coordinate system is
!                defined by the inputs RE and F.
!
!                The code <rel> indicates that the relation
!
!                   <latitude of P>  <rel>  LAT
!
!                is true. See the Parameters section below for
!                the parameter names.
!
!
! $ Parameters
!
!     LT,
!     EQ,
!     GT         are, respectively, codes indicating the relationship
!                of the planetodetic latitude of the input vector to
!                the input latitude value. Let LP represent the
!                planetodetic latitude of P.
!
!                   Code LT indicates   LP < LAT
!                   Code EQ indicates   LP = LAT
!                   Code GT indicates   LP > LAT
!
! $ Exceptions
!
!     1)  If either the equatorial radius or flattening coefficient
!         is invalid, the error will be signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs a planetodetic latitude comparison more
!     efficiently than can be done using a rectangular-to-planetodetic
!     coordinate conversion.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     There are some cases for which this routine cannot be applied.
!     See the SPICELIB routine ZZPDPLTC and its usage in ZZRYTPDT.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB)
!
!       Original version 22-AUG-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     compare planetodetic latitude of vector against value
!     compare planetodetic latitude of point against value
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      OFFPCL
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      XINCPT
      DOUBLE PRECISION      YINCPT

!
!     Saved variables
!
      SAVE                  APEX

!
!     Initial values
!
      DATA                  APEX / 3  *  0.D0       /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZPDCMPL' )

!
!     Treat points on the Z axis as a special case. The
!     computations performed in the general case may introduce
!     round-off errors that will lead to false results for
!     this case.
!
      IF (  ( P(1) .EQ. 0.D0 ) .AND. ( P(2) .EQ. 0.D0 )  ) THEN

         IF ( P(3) .GT. 0.D0 ) THEN

            IF ( LAT .EQ. HALFPI() ) THEN

               REL = EQ
            ELSE
               REL = GT
            END IF

         ELSE IF ( P(3) .EQ. 0.D0 ) THEN
!
!           We consider the latitude of P to be zero.
!
            IF ( LAT .GT. 0.D0 ) THEN

               REL = LT

            ELSE IF ( LAT .EQ. 0.D0 ) THEN

               REL = EQ
            ELSE
               REL = GT
            END IF

         ELSE
!
!           P(3) < 0.
!
            IF ( LAT .EQ. -HALFPI() ) THEN

               REL = EQ
            ELSE
               REL = LT
            END IF

         END IF

         CALL CHKOUT ( 'ZZPDCMPL' )
         RETURN

      END IF

!
!     Latitude zero is a special case. The planetodetic latitude of the
!     input point has the same sign as the Z component of the point.
!
      RP = RE * ( 1.D0 - F )
!
!     Get the y-intercept of the latitude cone for LAT. Note that a
!     result is defined for LAT = +/- pi/2.
!
      CALL ZZELNAXX ( RE, RP, LAT, XINCPT, YINCPT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZPDCMPL' )
         RETURN
      END IF

!
!     Ideally YINCPT is zero if and only if LAT is zero.
!     We'll group these conditions together.
!
      IF (  ( LAT .EQ. 0.D0 ) .OR. ( YINCPT .EQ. 0.D0 )  ) THEN

         IF ( P(3) .GT. 0.D0 ) THEN

            REL = GT

         ELSE IF ( P(3) .EQ. 0.D0 ) THEN

            REL = EQ
         ELSE
            REL = LT
         END IF

         CALL CHKOUT ( 'ZZPDCMPL' )
         RETURN

      END IF

!
!     This is the normal case.
!
!     Find the offset of the point from the latitude cone's apex.
!     Create a unit-length copy of the offset vector.
!
      APEX(3) = YINCPT

      CALL VSUB ( P, APEX, OFFSET )

!     We'll use the planetocentric [sic] latitude of the offset
!     vector for comparison.
!
      CALL RECLAT ( OFFSET, R, LON, OFFPCL )


      IF ( LAT .GT. 0.D0 ) THEN

         IF ( YINCPT .GT. 0 ) THEN
!
!           This is the prolate case.
!
            IF ( OFFPCL .GT. LAT ) THEN

               REL = GT

            ELSE IF ( OFFPCL .EQ. LAT ) THEN

               REL = EQ
            ELSE
               REL = LT
            END IF

         ELSE
!
!           YINCPT = 0 was handled previously, so YINCPT < 0.
!
!           This is the oblate case.
!
!           In addition to the comparison of angles, we need to know
!           the input point is above the X-Y plane in order for the
!           GT or EQ relations to hold.
!
            IF ( P(3) .GT. 0.D0 ) THEN

               IF ( OFFPCL .GT. LAT ) THEN

                  REL = GT

               ELSE IF ( OFFPCL .EQ. LAT ) THEN

                  REL = EQ
               ELSE
                  REL = LT
               END IF

            ELSE
!
!              The input latitude is positive, while the point
!              is on or below the X-Y plane.
!
               REL = LT

            END IF

         END IF


      ELSE
!
!        LAT < 0, since the case LAT = 0 has already been handled.
!
         IF ( YINCPT .LT. 0.D0 ) THEN
!
!           This is the prolate case.
!
            IF ( OFFPCL .GT. LAT ) THEN

               REL = GT

            ELSE IF ( OFFPCL .EQ. LAT ) THEN

               REL = EQ
            ELSE
               REL = LT
            END IF

         ELSE
!
!           YINCPT > 0, since the case YINCPT = 0 was handled
!           previously.
!
!           This is the oblate case.
!
            IF ( P(3) .LT. 0.D0 ) THEN

               IF ( OFFPCL .GT. LAT ) THEN

                  REL = GT

               ELSE IF ( OFFPCL .EQ. LAT ) THEN

                  REL = EQ
               ELSE
                  REL = LT
               END IF

            ELSE
!
!              The input latitude is negative, while the point
!              is on or above the X-Y plane.
!
               REL = GT

            END IF

         END IF

      END IF

      CALL CHKOUT ( 'ZZPDCMPL' )
      RETURN
      END
