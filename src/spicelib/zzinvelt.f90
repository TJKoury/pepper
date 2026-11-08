!$Procedure ZZINVELT ( DSK, in volume element? )

      SUBROUTINE ZZINVELT ( P,      CORSYS, CORPAR, BOUNDS, &
                            MARGIN, EXCLUD, INSIDE         )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of Cartesian coordinates for
!     inclusion in a volume element in a specified coordinate system.
!     The volume element is bounded by surfaces on which one coordinate
!     is constant. The test is performed using margins for the element.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     INTERSECTION
!     SURFACE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dsktol.inc'
      INCLUDE 'dskdsc.inc'


      DOUBLE PRECISION      P      ( 3 )
      INTEGER               CORSYS
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      MARGIN
      INTEGER               EXCLUD
      LOGICAL               INSIDE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     CORSYS     I   Coordinate system code.
!     CORPAR     I   Coordinate system parameters.
!     BOUNDS     I   Coordinate bounds of element.
!     MARGIN     I   Margin used for inclusion testing.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!     CORSYS     is an integer parameter identifying the coordinate
!                system in which the bounds are to be computed. See the
!                include file dskdsc.inc for allowed values of CORSYS.
!
!     CORPAR     is an array of parameters associated with the
!                coordinate system. Currently the only supported system
!                that has associated parameters is the planetodetic
!                system. For planetodetic coordinates,
!
!                   CORPAR(1) is the equatorial radius
!
!                   CORPAR(2) is the flattening coefficient. Let RE and
!                   RP represent, respectively, the equatorial and
!                   polar radii of the reference ellipsoid of the
!                   system. Then
!
!                       CORPAR(2) = ( RE - RP ) / RE
!
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in a supported coordinate system,
!                specifically one of:
!
!                   LATITUDINAL
!                   PLANETODETIC
!                   RECTANGULAR
!
!
!                BOUNDS defines the volume element used in the
!                comparison. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. I is the bound index.
!
!                   I = 1   ->   lower bound
!                   I = 2   ->   upper bound
!
!                See the routines
!
!                   ZZINLAT
!                   ZZINPDT
!                   ZZINREC
!
!                for details on the contents of BOUNDS for the
!                respective coordinate systems supported by those
!                routines.
!
!
!     MARGIN     is a fraction used to expand the volume element for
!                inclusion testing.
!
!                See the routines
!
!                   ZZINLAT
!                   ZZINPDT
!                   ZZINREC
!
!                for details regarding the application of MARGIN.
!
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { 1, 2, 3 }
!
!                then the indicated coordinate is excluded from
!                comparison with the corresponding volume element
!                boundaries.
!
!                If EXCLUD is set to NONE, all coordinates are
!                compared.
!
!                Exclusion of coordinates is used in cases where a
!                point is known to be on a level surface of a given
!                coordinate. For example, if a point is on the sphere
!                of radius equal to the upper radius bound, radius need
!                not be used in the comparison and in fact can't be
!                meaningfully compared, due to round-off errors.
!
!                See the routines
!
!                   ZZINLAT
!                   ZZINPDT
!                   ZZINREC
!
!                for details regarding the application of EXCLUD.
!
! $ Detailed_Output
!
!     INSIDE     is a logical flag that is set to .TRUE. if and only if
!                the input coordinates represent a point inside or on
!                the surface of the volume element, according to the
!                comparisons that are performed.
!
!                The value of INSIDE is not affected by the value of
!                any excluded coordinate.
!
! $ Parameters
!
!     See the include files
!
!        dskdsc.inc
!        dsktol.inc
!
!     and the Parameters sections of the routines
!
!        ZZINLAT
!        ZZINPDT
!        ZZINREC
!
!
! $ Exceptions
!
!     1)  If CORSYS is not recognized, the error SPICE(NOTSUPPORTED)
!         is signaled.
!
!     2)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     3)  If EXCLUD is less than 0 or greater than 3, the error will
!         be signaled by a routine in the call tree of this routine.
!
!     4)  If an error occurs while determining the planetodetic
!         coordinates of the input point, the error will be signaled by
!         a routine in the call tree of this routine.
!
!     5)  If any rectangular coordinate upper bound is less than the
!         corresponding lower bound, the error will be signaled by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See usage in ZZPTPL02.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
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
!-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
!
!     03-JUN-2016 (NJB)
!
!        Original version.
!
!-&

! $ Index_Entries
!
!     test point against volume element using margin
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZINVELT' )


      IF ( MARGIN .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Margin must be non-negative but was #.' )
         CALL ERRDP  ( '#', MARGIN                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
         CALL CHKOUT ( 'ZZINVELT'                               )
         RETURN

      END IF

!
!     Delegate the job to one of the coordinate system-specific
!     routines.
!
      IF ( CORSYS .EQ. LATSYS ) THEN

         CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )


      ELSE IF ( CORSYS .EQ. PDTSYS ) THEN

         CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )


      ELSE IF ( CORSYS .EQ. RECSYS ) THEN

         CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )


      ELSE

         CALL SETMSG ( 'Coordinate system code # was not recognized.' )
         CALL ERRINT ( '#', CORSYS                                    )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZINVELT'                                     )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZINVELT' )
      RETURN
      END

