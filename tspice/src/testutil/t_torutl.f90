!$Procedure T_TORUTL ( Torus utility package )

      SUBROUTINE T_TORUTL ( R,     CENTER, NORMAL, T, &
                            CURVE, DERIV,  TWIST     )

! $ Abstract
!
!     Umbrella routine for entry points that provide a central curve
!     for a torus.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSK
!     PLATE
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      R
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      CURVE  ( 3 )
      DOUBLE PRECISION      DERIV  ( 3 )
      DOUBLE PRECISION      TWIST

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     R          I   T_TORSET
!     CENTER     I   T_TORSET
!     NORMAL     I   T_TORSET
!     T          I   T_TORCRV
!     CURVE      O   T_TORCRV
!     DERIV      O   T_TORCRV
!     TWIST      O   T_TORCRV
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!
!     See the entry points for descriptions of errors specific to those
!     routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This package contains two entry points:
!
!        T_TORSET {initialize central curve}
!        T_TORCRV {get curve and derivative}
!
!     These entry points are used by the test utility routine
!
!        T_TORUS
!
!     which is an entry point of
!
!        T_WRDSK2
!
!
! $ Examples
!
!     None.
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
!-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
!
!-&




!
!     SPICELIB functions
!
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      C
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SAVCTR ( 3 )
      DOUBLE PRECISION      SAVR
      DOUBLE PRECISION      SAVX   ( 3 )
      DOUBLE PRECISION      SAVY   ( 3 )
      DOUBLE PRECISION      THETA

!
!     Saved variables
!
      SAVE                  SAVCTR
      SAVE                  SAVR
      SAVE                  SAVX
      SAVE                  SAVY

!
!     Initial values
!
      DATA                  SAVR / -1.D0 /


      CALL CHKIN  ( 'T_TORUTL'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_TORUTL'          )
      RETURN





!$Procedure T_TORCRV ( Torus curve )

      ENTRY T_TORCRV ( T, CURVE, DERIV, TWIST )

! $ Abstract
!
!     Return a point on a parameterized central curve of
!     a torus; return the curve's derivative at this point
!     as well. Return a twist value of zero.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSK
!     PLATE
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      T
!     DOUBLE PRECISION      CURVE ( 3 )
!     DOUBLE PRECISION      DERIV ( 3 )
!     DOUBLE PRECISION      TWIST
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     T          I   The curve parameter. Values are in [0, 1].
!     CURVE      O   The value of the curve at T.
!     DERIV      O   The derivative of the curve with respect to T at T.
!     TWIST      O   The twist value at T; identically zero.
!
! $ Detailed_Input
!
!     T          is the value of the curve parameter. T is in the
!                closed interval [0, 1].
!
! $ Detailed_Output
!
!     CURVE      is the value of the curve at T.
!
!     DERIV      is the value of the derivative of the curve with
!                respect to T, at T.
!
!     TWIST      is the twist at T; this value is always set to zero.
!                See Particulars.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If T is outside the interval [0, 1 ], the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine defines a central curve a torus.
!
!     The argument list of this routine is compatible with the
!     requirements of the test utility routine
!
!        ZZPSTUBE
!
!     The output TWIST is provided because ZZPSTUBE requires it. (Twist
!     is used by ZZPSTUBE to orient cross sections of the tube it
!     creates.)
!
!
!     In a Cartesian coordinate system having the Z axis parallel to
!     NORMAL, and having an origin coincident with CENTER, the torus
!     is the set of points
!
!                                2   2  1/2  2     2           2
!        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
!
!
! $ Examples
!
!     None.
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
!-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
!
!-&

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF (  ( T .LT. 0.D0 ) .OR. ( T .GT. 1.D0 ) ) THEN

         CALL CHKIN ( 'T_TORCRV'                          )
         CALL SETMSG ( 'Parameter T was # but must be in ' &
         //            'the range [0, 1].'                )
         CALL ERRDP  ( '#',  T                            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
         CALL CHKOUT ( 'T_TORCRV'                         )
         RETURN

      END IF

      THETA = T * TWOPI()

      C     = SAVR * COS(THETA)
      S     = SAVR * SIN(THETA)

      CALL VLCOM3 (  C, SAVX, S, SAVY, 1.D0, SAVCTR, CURVE )
      CALL VLCOM  ( -S, SAVX, C, SAVY,               DERIV )

      TWIST = 0.D0

      RETURN







!$Procedure T_TORSET ( Torus set-up )

      ENTRY T_TORSET ( R, CENTER, NORMAL )

! $ Abstract
!
!     Perform set-up functions for the entry point T_TORCRV.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSK
!     PLATE
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      R
!     DOUBLE PRECISION      CENTER ( 3 )
!     DOUBLE PRECISION      NORMAL ( 3 )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     R          I   Radius of central curve.
!     CENTER     I   Center of a torus.
!     NORMAL     I   Normal vector to plane of central curve.
!
! $ Detailed_Input
!
!     R          is the radius of the central curve of a torus.
!                Each cross section is centered on this curve and
!                is normal to the derivative of the curve at the point
!                of intersection.
!
!     CENTER     is the center of the torus. The is the center of the
!                central curve of the torus. The line normal to the
!                plane of the central curve and passing through CENTER
!                is the axis of azimuthal symmetry of the torus.
!
!     NORMAL     is a vector normal to the plane containing the
!                central curve of the torus.
!
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an orthonormal basis having its +Z axis parallel to
!         NORMAL cannot be created, an error will be signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine saves the values of the input arguments R and CENTER
!     so that they may be used by the entry point T_TORCRV. It
!     generates an orthonormal basis having its +Z axis parallel to
!     NORMAL.
!
!     In a Cartesian coordinate system having the Z axis parallel to
!     NORMAL, and having an origin coincident with CENTER, the torus
!     is the set of points
!
!                                2   2  1/2  2     2           2
!        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
!
! $ Examples
!
!     None.
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
!-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
!
!-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TORSET' )


      SAVR = R
      CALL VEQU ( CENTER, SAVCTR )

!
!     Generate basis vectors for the plane containing
!     the central curve of the torus.
!
      CALL FRAME ( NORMAL, SAVX, SAVY )

      CALL CHKOUT ( 'T_TORSET' )
      RETURN
      END
