!$Procedure VPRJPI ( Vector projection onto plane, inverted )

      SUBROUTINE VPRJPI ( VIN, PROJPL, INVPL, VOUT, FOUND )

! $ Abstract
!
!     Find the vector in a specified plane that maps to a specified
!     vector in another plane under orthogonal projection.
!
! $ Required_Reading
!
!     PLANES
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!     PLANE
!     VECTOR
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               UBPL
      PARAMETER           ( UBPL   =    4 )

      DOUBLE PRECISION      VIN    (    3 )
      DOUBLE PRECISION      PROJPL ( UBPL )
      DOUBLE PRECISION      INVPL  ( UBPL )
      DOUBLE PRECISION      VOUT   (    3 )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VIN        I   The projected vector.
!     PROJPL     I   Plane containing VIN.
!     INVPL      I   Plane containing inverse image of VIN.
!     VOUT       O   Inverse projection of VIN.
!     FOUND      O   Flag indicating whether VOUT could be calculated.
!     UBPL       P   SPICE plane upper bound.
!
! $ Detailed_Input
!
!     VIN,
!     PROJPL,
!     INVPL    are, respectively, a 3-vector, a SPICE plane
!              containing the vector, and a SPICE plane
!              containing the inverse image of the vector under
!              orthogonal projection onto PROJPL.
!
! $ Detailed_Output
!
!     VOUT     is the inverse orthogonal projection of VIN. This
!              is the vector lying in the plane INVPL whose
!              orthogonal projection onto the plane PROJPL is
!              VIN. VOUT is valid only when FOUND (defined below)
!              is .TRUE. Otherwise, VOUT is undefined.
!
!     FOUND    indicates whether the inverse orthogonal projection
!              of VIN could be computed. FOUND is .TRUE. if so,
!              .FALSE. otherwise.
!
! $ Parameters
!
!     UBPL     is the upper bound of a SPICE plane array.
!
! $ Exceptions
!
!     1)  If the normal vector of either input plane does not have unit
!         length (allowing for round-off error), the error
!         SPICE(NONUNITNORMAL) is signaled.
!
!     2)  If the geometric planes defined by PROJPL and INVPL are
!         orthogonal, or nearly so, the inverse orthogonal projection
!         of VIN may be undefined or have magnitude too large to
!         represent with double precision numbers. In either such
!         case, FOUND will be set to .FALSE.
!
!     3)  Even when FOUND is .TRUE., VOUT may be a vector of extremely
!         large magnitude, perhaps so large that it is impractical to
!         compute with it. It's up to you to make sure that this
!         situation does not occur in your application of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Projecting a vector orthogonally onto a plane can be thought of
!     as finding the closest vector in the plane to the original vector.
!     This "closest vector" always exists; it may be coincident with the
!     original vector. Inverting an orthogonal projection means finding
!     the vector in a specified plane whose orthogonal projection onto
!     a second specified plane is a specified vector. The vector whose
!     projection is the specified vector is the inverse projection of
!     the specified vector, also called the "inverse image under
!     orthogonal projection" of the specified vector. This routine
!     finds the inverse orthogonal projection of a vector onto a plane.
!
!     Related routines are VPRJP, which projects a vector onto a plane
!     orthogonally, and VPROJ, which projects a vector onto another
!     vector orthogonally.
!
! $ Examples
!
!     1)   Suppose
!
!             VIN    =  ( 0.0, 1.0, 0.0 ),
!
!          and that PROJPL has normal vector
!
!             PROJN  =  ( 0.0, 0.0, 1.0 ).
!
!          Also, let's suppose that INVPL has normal vector and constant
!
!             INVN   =  ( 0.0, 2.0, 2.0 )
!             INVC   =    4.0.
!
!          Then VIN lies on the y-axis in the x-y plane, and we want to
!          find the vector VOUT lying in INVPL such that the orthogonal
!          projection of VOUT the x-y plane is VIN. Let the notation
!          < a, b > indicate the inner product of vectors a and b.
!          Since every point X in INVPL satisfies the equation
!
!             <  X,  (0.0, 2.0, 2.0)  >  =  4.0,
!
!          we can verify by inspection that the vector
!
!             ( 0.0, 1.0, 1.0 )
!
!          is in INVPL and differs from VIN by a multiple of PROJN. So
!
!             ( 0.0, 1.0, 1.0 )
!
!          must be VOUT.
!
!          To find this result using SPICELIB, we can create the
!          SPICE planes PROJPL and INVPL using the code fragment
!
!             CALL NVP2PL  ( PROJN,  VIN,   PROJPL )
!             CALL NVC2PL  ( INVN,   INVC,  INVPL  )
!
!          and then perform the inverse projection using the call
!
!             CALL VPRJPI ( VIN, PROJPL, INVPL, VOUT )
!
!          VPRJPI will return the value
!
!             VOUT = ( 0.0, 1.0, 1.0 )
!
! $ Restrictions
!
!     1)  It is recommended that the input planes be created by one of
!         the SPICELIB routines
!
!            NVC2PL ( Normal vector and constant to plane )
!            NVP2PL ( Normal vector and point to plane    )
!            PSV2PL ( Point and spanning vectors to plane )
!
!         In any case each input plane must have a unit length normal
!         vector and a plane constant consistent with the normal
!         vector.
!
! $ Literature_References
!
!     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
!          7th Edition, Addison Wesley, 1988.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 25-AUG-2021 (NJB) (JDR)
!
!        Added error checks for non-unit plane normal vectors.
!        Changed check-in style to discovery.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added documentation of the parameter UBPL.
!
!-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
!
!        Computation of LIMIT was re-structured to avoid
!        run-time underflow warnings on some platforms.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     vector projection onto plane inverted
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
!
!        Computation of LIMIT was re-structured to avoid
!        run-time underflow warnings on some platforms.
!        In the revised code, BOUND/DPMAX() is never
!        scaled by a number having absolute value < 1.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               APPROX
      LOGICAL               RETURN


!
!     Local parameters
!

!
!     BOUND is used to bound the magnitudes of the numbers that we
!     try to take the reciprocal of, since we can't necessarily invert
!     any non-zero number.  We won't try to invert any numbers with
!     magnitude less than
!
!        BOUND / DPMAX().
!
!     BOUND is chosen somewhat arbitrarily....
!
      DOUBLE PRECISION      BOUND
      PARAMETER           ( BOUND = 10.D0 )

!
!     Tolerance for deviation from unit length of the normal
!     vector of the input plane.
!
      DOUBLE PRECISION      MAGTOL
      PARAMETER           ( MAGTOL = 1.D-14 )

!
!     Local variables
!
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      INVC
      DOUBLE PRECISION      INVN  ( 3 )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      MULT
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      PROJC
      DOUBLE PRECISION      PROJN ( 3 )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

!
!     Unpack the planes.
!
      CALL PL2NVC ( PROJPL, PROJN, PROJC )
      CALL PL2NVC ( INVPL,  INVN,  INVC  )

!
!     Check the normal vectors obtained from the planes.
!
!     Each normal vector returned by PL2NVC should be a unit vector.
!
      IF (  .NOT.  APPROX( VNORM(PROJN), 1.D0, MAGTOL )  ) THEN

         CALL CHKIN  ( 'VPRJPI' )
         CALL SETMSG ( 'Normal vector of plane containing input ' &
         //            'point does not have unit length; the ' &
         //            'difference of the length from 1 is #. ' &
         //            'The input plane is invalid. '             )
         CALL ERRDP  ( '#',  VNORM(PROJN) - 1.D0                  )
         CALL SIGERR ( 'SPICE(NONUNITNORMAL)'                     )
         CALL CHKOUT ( 'VPRJPI' )
         RETURN

      END IF

      IF (  .NOT.  APPROX( VNORM(INVN), 1.D0, MAGTOL )  ) THEN

         CALL CHKIN  ( 'VPRJPI' )
         CALL SETMSG ( 'Normal vector of plane containing output ' &
         //            'point does not have unit length; the ' &
         //            'difference of the length from 1 is #. ' &
         //            'The output plane is invalid. '            )
         CALL ERRDP  ( '#',  VNORM(INVN) - 1.D0                   )
         CALL SIGERR ( 'SPICE(NONUNITNORMAL)'                     )
         CALL CHKOUT ( 'VPRJPI' )
         RETURN

      END IF

!
!     We'll first discuss the computation of VOUT in the nominal case,
!     and then deal with the exceptional cases.
!
!     When PROJPL and INVPL are not orthogonal to each other, the
!     inverse projection of VIN will differ from VIN by a multiple of
!     PROJN, the unit normal vector to PROJPL. We find this multiple
!     by using the fact that the inverse projection VOUT satisfies the
!     plane equation for the inverse projection plane INVPL.
!
!        We have
!
!           VOUT = VIN  +  MULT * PROJN;                           (1)
!
!        since VOUT satisfies
!
!           < VOUT, INVN >  =  INVC
!
!        we must have
!
!           <  VIN  +  MULT * PROJN,  INVN  > = INVC
!
!        which in turn implies
!
!
!                     INVC  -  < VIN, INVN >
!           MULT  =  ------------------------.                     (2)
!                        < PROJN, INVN >
!
!        Having MULT, we can compute VOUT according to equation (1).
!
!     Now, if the denominator in the above expression for MULT is zero
!     or just too small, performing the division would cause a
!     divide-by-zero error or an overflow of MULT.  In either case, we
!     will avoid carrying out the division, and we'll set FOUND to
!     .FALSE.
!
!
!     Compute the numerator and denominator of the right side of (2).
!
      NUMER  =  INVC - VDOT ( VIN,   INVN )
      DENOM  =         VDOT ( PROJN, INVN )

!
!     If the magnitude of the denominator is greater than the absolute
!     value of
!
!                    BOUND
!        LIMIT  =  --------- * NUMER,
!                   DPMAX()
!
!     we can safely divide the numerator by the denominator, and the
!     magnitude of the result will be no greater than
!
!         DPMAX()
!        --------- .
!          BOUND
!
!     Note that we have ruled out the case where NUMER and DENOM are
!     both zero by insisting on strict inequality in the comparison of
!     DENOM and LIMIT.
!
!     We never set LIMIT smaller than BOUND/DPMAX(), since
!     the computation using NUMER causes underflow to be signaled
!     on some systems.
!
      IF ( ABS(NUMER) .LT. 1.D0 ) THEN
         LIMIT  =           BOUND / DPMAX()
      ELSE
         LIMIT  =  ABS (  ( BOUND / DPMAX() ) * NUMER  )
      END IF

      IF (  ABS (DENOM) .GT. LIMIT  ) THEN
!
!        We can find VOUT after all.
!
         MULT = NUMER / DENOM

         CALL VLCOM ( 1.0D0, VIN, MULT, PROJN, VOUT )

         FOUND = .TRUE.

      ELSE
!
!        No dice.
!
         FOUND = .FALSE.

      END IF

      RETURN
      END
