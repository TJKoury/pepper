!$Procedure ZZELNAXX ( ellipse normal axis intercepts )

      SUBROUTINE ZZELNAXX ( A, B, LAT, XXPT, YXPT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Given semi-axis lengths of an ellipse and a planetodetic
!     latitude, find the X-axis and Y-axis intercepts of the normal
!     line corresponding to the point at that latitude in the right
!     (positive X) half-plane.
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
!     DSK
!     ELLIPSE
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      XXPT
      DOUBLE PRECISION      YXPT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     A          I   Ellipse's semi-axis length in the X direction.
!     B          I   Ellipse's semi-axis length in the Y direction.
!     LAT        I   Planetodetic latitude in radians.
!     XXPT       O   Normal line intercept on the X-axis.
!     YXPT       O   Normal line intercept on the Y-axis.
!
! $ Detailed_Input
!
!     A,
!     B          are, respectively, an ellipse's semi-axis lengths
!                in the X and Y directions. The ellipse lies in
!                two-dimensional Euclidean space.
!
!                Any of the relationships
!
!                   A < B,  A = B,  A > B
!
!                are allowed.
!
!
!     LAT        is a planetodetic latitude corresponding to
!                some point P on the ellipse defined by A and B,
!                where
!
!                   P(1) >= 0
!
!                Units are radians.
!
! $ Detailed_Output
!
!     XXPT       is the X-intercept of a line passing through the
!                ellipse and normal to the ellipse at the point having
!                the given latitude.
!
!                If LAT = 0 radians, XXPT is defined to be
!
!                        2
!                   A - B / A
!
!
!     YXPT       is the Y-intercept of a line passing through the
!                ellipse and normal to the ellipse at the point having
!                the given latitude.
!
!                If LAT = pi/2 radians, YXPT is defined to be
!
!                         2
!                    B - A / B
!
!                If LAT = -pi/2 radians, YXPT is defined to be
!
!                         2
!                   -B + A / B
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the X semi-axis length A is non-positive, the
!         error SPICE(NONPOSITIVEAXIS) will be signaled.
!
!     2)  If the Y semi-axis length B is non-positive, the
!         error SPICE(NONPOSITIVEAXIS) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!
!     The outputs of this routine can be used to determine the
!     apex of a cone of constant planetodetic latitude.
!
!
!     The axis intercepts computed by this routine are defined
!     as described below.
!
!     Let M be the normal vector's slope. Let DELTAX be
!     X - XXPT. Then, given that the normal direction is
!     parallel to
!
!             2     2
!        ( X/A,  Y/B  )
!
!     we have, for non-zero DELTAX,
!
!        Y = M * DELTAX
!
!               2   2
!          = ( A / B )  * ( Y / X ) * ( X - XXPT )
!
!     So
!             2    2
!        X ( B  / A ) = X - XXPT
!
!     and
!                         2   2
!        XXPT = X ( 1 - (B / A ) )
!
!
!     The X intercept is a linear function of the X-coordinate
!     of the point on the ellipsoid. Define the intercept for
!
!        X = 0
!
!     as the limit as X -> 0 of the expression for XXPT above.
!     The expression is continuous and defined at X = 0, so
!     we can just evaluate the expression at X = 0 as for other
!     values of X.
!
!
!     Using the definition of M above, we have for non-zero X:
!
!        Y - YXPT = M * X
!
!     or
!
!        YXPT = Y - M*X
!
!                      2   2
!             = Y - ( A / B ) (Y/X) * X
!
!                           2   2
!             = Y (  1 - ( A / B )  )
!
!     As above, we define YXPT at X = 0 to be the limit as X -> 0
!     of the expression above, which is
!
!                    2   2
!        B (  1 - ( A / B )  )
!
!
! $ Examples
!
!     See usage in ZZRYXPDT.
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
!-    SPICELIB Version 1.0.0, 09-MAR-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     find x and y intercepts of ellipse normal line
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      EPT    ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )


      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF (  ( A .LE. 0.D0 ) .OR. ( B .LE. 0.D0 )  ) THEN

         CALL CHKIN  ( 'ZZELNAXX'                             )
         CALL SETMSG ( 'Semi-axis lengths were A = #; B = #. ' &
         //            'Both must be positive.'               )
         CALL ERRDP  ( '#',  A                                )
         CALL ERRDP  ( '#',  B                                )
         CALL SIGERR ( 'SPICE(NONPOSITIVEAXIS)'               )
         CALL CHKOUT ( 'ZZELNAXX'                             )
         RETURN

      END IF

!
!     Find the point lying on the positive X portion of the ellipsoid
!     and having the input planetodetic latitude.
!
!     To start, create a normal vector pointing in the direction
!     indicated by the latitude. We'll work in three dimensions in
!     order to take advantage of existing code. The third coordinates
!     of all participating vectors will be zero.
!
      NORMAL(1) = COS( LAT )
      NORMAL(2) = SIN( LAT )
      NORMAL(3) = 0.D0

      CALL EDNMPT ( A, B, B, NORMAL, EPT )

!
!     Compute the X-axis and Y-axis intercepts of the line
!     passing through EPT and parallel to a normal vector
!     at EPT. Refer to the Particulars above for details.
!
      XXPT = (  1.D0 - ( (B/A)**2 )  ) * EPT(1)

      YXPT = (  1.D0 - ( (A/B)**2 )  ) * EPT(2)

      RETURN
      END
