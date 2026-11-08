!$Procedure  ZZCXBRUT ( Cone-segment intersection by brute force )

      SUBROUTINE ZZCXBRUT ( APEX,   AXIS,   ANGLE, &
                            ENDPT1, ENDPT2, XPT,   ISBRCK )

! $ Abstract
!
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute a bracketed point of intersection of a specified nappe of
!     a cone and a line segment by "brute force"---specifically by
!     bisection.
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
!     CONE
!     GEOMETRY
!     INTERSECTION
!     LINE
!     MATH
!     SEGMENT
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      LOGICAL               ISBRCK

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     APEX       I   Apex of cone.
!     AXIS       I   Axis of cone.
!     ANGLE      I   Angle of cone.
!     ENDPT1,
!     ENDPT2     I   Endpoints of line segment.
!     XPT        O   Intersection point, if it exists.
!     ISBRCK     O   Logical flag indicating whether root is bracketed.
!
! $ Detailed_Input
!
!     APEX       is the apex (tip) of the cone. In this routine's
!                documentation, we'll consider the cone to be a
!                semi-infinite pyramid with circular cross-section. In
!                some contexts, this object is called one "nappe" of
!                the complete cone.
!
!     AXIS       is an axis vector of the cone.
!
!     ANGLE      is the angular separation from AXIS of the rays
!                comprising the cone. Let the notation
!
!                   < A, B >
!
!                denote the dot product of vectors A and B, and let
!
!                   ||A||
!
!                denote the norm of vector A. Then the cone is the set
!                of points
!
!                             X-APEX       AXIS
!                   { X:  < ----------,  -------- >  =  cos(ANGLE) }
!                           ||X-APEX||   ||AXIS||
!
!
!     ENDPT1,
!     ENDPT2     are endpoints of a line segment. These points
!                must be distinct.
!
!                Exactly one of ENDPT1 and ENDPT2 must be inside
!                the cone.
!
! $ Detailed_Output
!
!     NXPTS      is the number of points of intersection of the input
!                line segment and cone.
!
!     XPT        is the unique point of intersection of the segment and
!                cone that lies on the line segment connecting ENDPT1
!                and ENDPT2.
!
!     ISBRCK     is a logical flag that is set to .TRUE. if and only if
!                ENDPT1 and ENDPT2 bracket a root. Equivalently, the
!                endpoints bracket a root when exactly one of ENDPT1
!                and ENDPT2 is inside the cone.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR)
!         will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used by the SPICELIB routine INCNSG to handle
!     cases where solution of a quadratic equation is subject to
!     excessive loss of precision.
!
! $ Examples
!
!     See usage in INCNSG.
!
! $ Restrictions
!
!     This is a private SPICELIB routine.
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
!-    SPICELIB Version 1.0.0 30-SEP-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     brute force intersection of line segment and cone
!
!-&







!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT

      LOGICAL               RETURN
      LOGICAL               STATE1
      LOGICAL               STATE2
      LOGICAL               VZERO

!
!     Local parameters
!
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-15 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 1000 )

!
!     Local variables
!
      DOUBLE PRECISION      COSANG
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      DP1
      DOUBLE PRECISION      DP2
      DOUBLE PRECISION      HIGH
      DOUBLE PRECISION      LOCANG
      DOUBLE PRECISION      LOCAXI ( 3 )
      DOUBLE PRECISION      LOW
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      OFF1   ( 3 )
      DOUBLE PRECISION      OFF2   ( 3 )
      DOUBLE PRECISION      PRVDLT
      DOUBLE PRECISION      SEG    ( 3 )
      DOUBLE PRECISION      UOFF1  ( 3 )
      DOUBLE PRECISION      UOFF2  ( 3 )
      DOUBLE PRECISION      UX     ( 3 )
      DOUBLE PRECISION      X      ( 3 )

      INTEGER               NITR

      LOGICAL               STATE

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Check the axis.
!
      IF ( VZERO(AXIS) ) THEN

         CALL CHKIN  ( 'ZZCXBRUT'                     )
         CALL SETMSG ( 'Cone axis is the zero vector' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'            )
         CALL CHKOUT ( 'ZZCXBRUT'                     )
         RETURN

      END IF

!
!     Make a local version of the cone's axis and angle. The
!     angle will be less than or equal to pi/2 radians.
!
      IF ( ANGLE .GT. HALFPI() ) THEN

         LOCANG = PI() - ANGLE
         CALL VMINUS ( AXIS, LOCAXI )
      ELSE
         LOCANG = ANGLE
         CALL VEQU   ( AXIS, LOCAXI )
      END IF

      CALL VHATIP ( LOCAXI )

      COSANG = COS( LOCANG )

!
!     Calculate the offsets of the endpoints from the apex,
!     and get unit-length versions of these.
!
      CALL VSUB ( ENDPT1, APEX, OFF1 )
      CALL VSUB ( ENDPT2, APEX, OFF2 )

      CALL VHAT ( OFF1, UOFF1 )
      CALL VHAT ( OFF2, UOFF2 )

!
!     Get the dot products of the unit offsets with the axis.
!     These will serve as proxies for latitude.
!
      DP1 = VDOT( UOFF1, LOCAXI )
      DP2 = VDOT( UOFF2, LOCAXI )

!
!     The "state" variables at the endpoints are .TRUE. if
!     the endpoints are on or inside the cone.
!
      STATE1 = DP1 .GE. COSANG
      STATE2 = DP2 .GE. COSANG

!
!     The intersection is supposed to be bracketed. Return
!     if not, indicating the situation via ISBRCK.
!
      ISBRCK = STATE1 .NEQV. STATE2

      IF ( .NOT. ISBRCK) THEN
         RETURN
      END IF

!
!     Prepare for a solution by bisection.
!
      CALL VSUB ( OFF2, OFF1, SEG )

      LOW    = 0.D0
      HIGH   = 1.D0
      DELTA  = ABS( HIGH - LOW )
      PRVDLT = 2.D0
      NITR   = 0

      DO WHILE (       ( DELTA .GT. CNVLIM ) &
                 .AND. ( DELTA .LT. PRVDLT ) &
                 .AND. ( NITR  .LT. MAXITR )  )

         MIDPT = ( LOW + HIGH ) / 2

         CALL VLCOM ( 1.D0, OFF1, MIDPT, SEG, X )

         CALL VHAT ( X, UX )

         DP    = VDOT ( UX, LOCAXI )

         STATE = DP .GE. COSANG

         IF ( STATE .EQV. STATE1 ) THEN
!
!           There has been no state change between OFF1
!           and XPT.
!
            LOW  = MIDPT
         ELSE
            HIGH = MIDPT
         END IF

         PRVDLT = DELTA
         DELTA  = ABS( HIGH - LOW )

         NITR   = NITR + 1

      END DO
!
!     X is an offset from APEX. The solution is an offset from the
!     origin.
!
      CALL VADD ( APEX, X, XPT )

      RETURN
      END



