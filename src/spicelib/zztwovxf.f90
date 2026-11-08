!$Procedure ZZTWOVXF ( Two states defining a frame transformation )

      SUBROUTINE ZZTWOVXF ( AXDEF, INDEXA, PLNDEF, INDEXP, XFORM )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the state transformation to a base frame from the
!     right-handed frame defined by two state vectors:  one state
!     vector defining a specified axis and a second state vector
!     defining a specified coordinate plane.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     AXES
!     FRAMES
!     MATRIX
!     TRANSFORMATION
!
! $ Declarations

      DOUBLE PRECISION   AXDEF  ( 6 )
      INTEGER            INDEXA
      DOUBLE PRECISION   PLNDEF ( 6 )
      INTEGER            INDEXP
      DOUBLE PRECISION   XFORM  ( 6, 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     AXDEF      I   State defining a principal axis.
!     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3).
!     PLNDEF     I   State defining (with AXDEF) a principal plane.
!     INDEXP     I   Second axis number (with INDEXA) of principal
!                    plane.
!     XFORM      O   Output state transformation matrix.
!
! $ Detailed_Input
!
!     AXDEF      is a "generalized" state vector defining one of the
!                principal axes of a reference frame. This vector
!                consists of three components of a vector-valued
!                function of one independent variable t followed by
!                the derivatives of the components with respect to that
!                variable:
!
!                   ( a, b, c, da/dt, db/dt, dc/dt )
!
!                This routine treats the input states as unitless, but
!                in most applications the input states represent
!                quantities that have associated units. The first three
!                components must have the same units, and the units of
!                the last three components must be compatible with
!                those of the first three:  if the first three
!                components of AXDEF
!
!                   ( a, b, c )
!
!                have units U and t has units T, then the units of
!                AXDEF normally would be
!
!                   ( U, U, U, U/T, U/T, U/T )
!
!                Note that the direction and angular velocity defined
!                by AXDEF are actually independent of U, so scaling
!                AXDEF doesn't affect the output of this routine.
!
!                AXDEF could represent position and velocity; it could
!                also represent velocity and acceleration.  AXDEF could
!                for example represent the velocity and acceleration of
!                a time-dependent position vector ( x(t), y(t), z(t) ),
!                in which case AXDEF would be defined by
!
!                   a     = dx/dt
!                   b     = dy/dt
!                   c     = dz/dt
!
!                            2      2
!                   da/dt = d x / dt
!
!                            2      2
!                   db/dt = d y / dt
!
!                            2      2
!                   dc/dt = d z / dt
!
!                Below, we'll call the normalized (unit length) version
!                of
!
!                   ( a, b, c )
!
!                the "direction" of AXDEF.
!
!                We call the frame relative to which AXDEF is specified
!                the "base frame."  The input state PLNDEF must be
!                specified relative to the same base frame.
!
!
!     INDEXA     is the index of the reference frame axis that is
!                parallel to the direction of AXDEF.
!
!                   Value of INDEXA             Axis
!
!                         1                      X
!                         2                      Y
!                         3                      Z
!
!
!     PLNDEF     is a state vector defining (with AXDEF) a principal
!                plane of the reference frame.  This vector consists
!                of three components followed by their derivatives with
!                respect to the independent variable t associated with
!                AXDEF, so PLNDEF is
!
!                   ( e, f, g, de/dt, df/dt, dg/dt )
!
!                Below, we'll call the unitized version of
!
!                   ( e, f, g )
!
!                the "direction" of PLNDEF.
!
!                The second axis of the principal plane containing the
!                direction vectors of AXDEF and PLNDEF is perpendicular
!                to the first axis and has positive dot product with
!                the direction vector of PLNDEF.
!
!                The first three components of PLNDEF must have the
!                same units, and the units of the last three components
!                must be compatible with those of the first three:  if
!                the first three components of PLNDEF
!
!                   ( e, f, g )
!
!                have units U2 and t has units T, then the units of
!                PLNDEF normally would be
!
!                   ( U2, U2, U2, U2/T, U2/T, U2/T )
!
!                ***For meaningful results, the angular velocities
!                   defined by AXDEF and PLNDEF must both have units of
!                   1/T.***
!
!                As with AXDEF, scaling PLNDEF doesn't affect the
!                output of this routine.
!
!                AXDEF and PLNDEF must be specified relative to a
!                common reference frame, which we call the "base
!                frame."
!
!
!     INDEXP     is the index of  second axis of the principal frame
!                determined by AXDEF and PLNDEF.  The association of
!                integer values and axes is the same as for INDEXA.
!
! $ Detailed_Output
!
!     XFORM      is the 6x6 matrix that transforms states to the frame
!                relative to which AXDEF and PLNDEF are specified (the
!                "base frame") from the frame whose axes and derivative
!                are determined by AXDEF, PLNDEF, INDEXA and INDEXP.
!
!                The matrix XFORM has the structure shown below:
!
!                    -            -
!                   |       :      |
!                   |   R   :  0   |
!                   | ......:......|
!                   |       :      |
!                   | dR_dt :  R   |
!                   |       :      |
!                    -            -
!
!                where R is a rotation matrix that is a function of
!                the independent variable associated with AXDEF and
!                PLNDEF, and where dR_dt is the derivative of R
!                with respect to that independent variable.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If INDEXA or INDEXP is not in the set {1,2,3} the error
!        SPICE(BADINDEX) will be signaled.
!
!     2) If INDEXA and INDEXP are the same the error
!        SPICE(UNDEFINEDFRAME) will be signaled.
!
!     3) If the cross product of the vectors AXDEF and PLNDEF is zero,
!        the error SPICE(DEPENDENTVECTORS) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exists to support the public routine TWOVXF:
!     TWOVXF does its job by calling this routine, inverting the
!     matrix returned by this routine, and returning the result.
!
!     The SPICELIB frame subsystem typically requires this routine
!     rather than TWOVXF, since the frame subsystem produces
!     transformations from frames defined in frame kernels to their
!     base frames.  Calling this routine rather than TWOVXF allows
!     the frame subsystem to eliminate two unnecessary calls to
!     INVSTM.
!
!     Given two linearly independent state vectors AXDEF and PLNDEF,
!     define vectors DIR1 and DIR2 by
!
!        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  )
!        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) )
!
!     Then there is a unique right-handed reference frame F having:
!
!        DIR1 lying along the INDEXA axis.
!
!        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that
!        the dot product of DIR2 with the positive INDEXP axis is
!        positive.
!
!     This routine determines the 6x6 matrix that transforms states
!     to the base frame used to represent the input vectors from the
!     the frame F determined by AXDEF and PLNDEF.  Thus a state vector
!
!        S       = ( x, y, z, dx/dt, dy/dt, dz/dt )
!         F
!
!     in the reference frame F will be transformed to
!
!        S      = XFORM * S
!         base             F
!
!     in the base frame relative to which AXDEF and PLNDEF are
!     specified.
!
! $ Examples
!
!     See TWOVXF.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.M. Owen       (JPL)
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in DUCRSS and MOVED calls.
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in DUCRSS and MOVED calls.
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local Variables
!
      DOUBLE PRECISION      TMPSTA ( 6 )

      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               J
      INTEGER               SEQNCE ( 5 )

!
!     Saved variables
!
      SAVE                  SEQNCE

!
!     Initial values
!
      DATA                  SEQNCE  / 1, 2, 3, 1, 2 /

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZTWOVXF' )


!
!     Check for obvious bad inputs.
!
      IF (      ( MAX(INDEXP,INDEXA) .GT. 3 ) &
           .OR. ( MIN(INDEXP,INDEXA) .LT. 1 ) ) THEN

         CALL SETMSG ( 'The definition indices must lie in the range '// &
                       'from 1 to 3.  The value of INDEXA was #. '    // &
                       'The value of INDEXP was #. '                  )
         CALL ERRINT ( '#', INDEXA       )
         CALL ERRINT ( '#', INDEXP       )
         CALL SIGERR ( 'SPICE(BADINDEX)' )
         CALL CHKOUT ( 'ZZTWOVXF'        )
         RETURN

      ELSE IF ( INDEXA .EQ. INDEXP ) THEN

         CALL SETMSG ( 'The values of INDEXA and INDEXP were the '    // &
                       'same, namely #.  They are required to be '    // &
                       'different.'                                   )
         CALL ERRINT ( '#', INDEXA             )
         CALL SIGERR ( 'SPICE(UNDEFINEDFRAME)' )
         CALL CHKOUT ( 'ZZTWOVXF'              )
         RETURN

      END IF

!
!     Get indices for right-handed axes:
!
!     First AXDEF ...
!
      I1 = INDEXA
!
!     ... then the other two.
!
      I2 = SEQNCE ( INDEXA + 1 )
      I3 = SEQNCE ( INDEXA + 2 )

!
!     Column I1 of XFORM contains a unit vector parallel to AXDEF and
!     the derivative of the unit vector.
!
      CALL DVHAT ( AXDEF, XFORM(1,I1) )

!
!     Obtain columns I2 and I3 of XFORM using cross products.
!     Which order to use depends on whether INDEXP = I2 (next axis in
!     right-handed order) or INDEXP = I3 (previous axis in right-handed
!     order).
!
!     Select column indices...
!
      IF ( INDEXP .EQ. I2 ) THEN
!
!        We compute the third axis in the sequence, then the second.
!
         CALL DUCRSS ( AXDEF,        PLNDEF,        XFORM(1,I3) )
         CALL DUCRSS ( XFORM(1,I3),  AXDEF,         TMPSTA      )
         CALL MOVED  ( TMPSTA,       6,             XFORM(1,I2) )
      ELSE
         CALL DUCRSS ( PLNDEF,       AXDEF,         XFORM(1,I2) )
         CALL DUCRSS ( AXDEF,        XFORM(1,I2),   TMPSTA      )
         CALL MOVED  ( TMPSTA,       6,             XFORM(1,I3) )
      END IF

!
!     ...and compute the output frame's non-principal unit basis
!     vectors and the derivatives of these vectors.
!

!
!     At this point, we've filled in the left half of XFORM.
!
!     The upper right block is the 3x3 zero matrix.
!     The lower right block matches the upper left block.
!
      CALL CLEARD ( 3,                XFORM(1,4) )
      CALL CLEARD ( 3,                XFORM(1,5) )
      CALL CLEARD ( 3,                XFORM(1,6) )

      DO J = 1, 3

         DO I = 1, 3

            XFORM(3+I,3+J) = XFORM(I,J)

         END DO

      END DO

!
!     Finally, check to see that we actually got something non-zero in
!     the first three components of at least one of the columns
!     XFORM(1,I2) and XFORM(1,I3) (we need only check one of them since
!     they are related by a cross product).
!
      IF (  VZERO ( XFORM(1,I2) )  ) THEN

         CALL SETMSG ( 'The direction vectors associated with ' // &
                       'states AXDEF and PLNDEF are '           // &
                       'linearly dependent.'                    )
         CALL SIGERR ( 'SPICE(DEPENDENTVECTORS)'                )
         CALL CHKOUT ( 'ZZTWOVXF'                               )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZTWOVXF' )
      RETURN
      END
