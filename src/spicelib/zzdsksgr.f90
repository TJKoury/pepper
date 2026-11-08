!$Procedure ZZDSKSGR ( DSK, return segment bounding radius )

      DOUBLE PRECISION FUNCTION ZZDSKSGR ( DSKDSC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return a radius of a bounding sphere for the spatial region
!     described by a DSK segment descriptor. The radius is measured
!     from the origin of the coordinate system associated from the
!     segment.
!
! $ Required_Reading
!
!     DAS
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'

      DOUBLE PRECISION      DSKDSC ( * )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DSKDSC     I   is a DSK descriptor.
!
!     The function returns the maximum radius of the segment.
!
! $ Detailed_Input
!
!     DSKDSC         is a DSK descriptor.
!
! $ Detailed_Output
!
!     The function returns the maximum radius attained on the
!     boundary of the coverage region of the segment associated
!     with the input descriptor. The radius is measured from
!     the origin of the coordinate system in which the coverage
!     bounds are given.
!
!     Units are km.
!
! $ Parameters
!
!     See the INCLUDE file dskdsc.inc for parameter declarations and
!     documentation.
!
! $ Exceptions
!
!     1)  If the coordinate system code in the descriptor is not
!         recognized, the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If an invalid radius is found, the error
!         SPICE(INVALIDVALUE) is signaled.
!
!     3)  If an invalid flattening coefficient is found, the error
!         SPICE(INVALIDVALUE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes an upper bound on the radius of
!     the coverage region of a DSK segment. The radius at
!     a point on the boundary of the coverage region is
!     measured from the origin of the coordinate system.
!
!     This routine can be used to generate a bounding sphere for
!     a surface described by one or more DSK segments. Note that
!     the DSK segments describing the surface of an object don't
!     necessarily have a common central body, so offsets between
!     segments' central bodies must be taken into account when
!     computing a bounding sphere.
!
! $ Examples
!
!
!     1) Get a bounding radius for a DSK segment using the
!        latitudinal coordinate system.
!
!        Example code begins here.
!
!
!                 PROGRAM DSKGSR_EX1
!                 IMPLICIT NONE
!
!                 INCLUDE 'dskdsc.inc'
!
!                 DOUBLE PRECISION      ZZDSKSGR
!                 DOUBLE PRECISION      PI
!
!                 DOUBLE PRECISION      DSKDSC( DSKDSZ )
!
!           C
!           C     Latitudinal coordinates:
!           C
!                 CALL CLEARD( DSKDSZ, DSKDSC )
!
!                 DSKDSC( SYSIDX ) = LATSYS
!
!           C
!           C     Fill in the coordinate bounds in the descriptor.
!           C
!           C     Longitude:
!           C
!                 DSKDSC(MN1IDX) =  - PI()/2
!                 DSKDSC(MX1IDX) =    PI()/2
!           C
!           C     Latitude:
!           C
!                 DSKDSC(MN2IDX) =    0
!                 DSKDSC(MX2IDX) =    PI()/4
!           C
!           C     Radius:
!           C
!                 DSKDSC(MN3IDX) =    1.D3
!                 DSKDSC(MX3IDX) =    1.1D3
!
!                 WRITE (*,*) 'Radius bound for latitudinal section: ',
!                .            ZZDSKSGR( DSKDSC )
!
!
!                 END
!
!
!        When run on a PC/Linux/gfortran platform, the output
!        from this program was:
!
!           Radius bound for latitudinal section:    1100.0000000000000
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
!-    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB)
!
!        Added case for planetodetic coordinates; deleted case for
!        cylindrical coordinates. Recoded algorithm for rectangular
!        coordinates.
!
!        22-JUL-2016 (NJB)
!
!           Renamed to ZZDSKSGR.
!
!        14-MAY-2010 (NJB)
!
!-&

! $ Index_Entries
!
!     Return upper bound on dsk segment coverage radius
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM

!
!     Local variables
!
      DOUBLE PRECISION      BDS    ( 2, 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      MAG    ( 3 )
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP

      INTEGER               B
      INTEGER               CORSYS
      INTEGER               I
      INTEGER               J


!
!     Use discovery check-in.
!

!
!     Set an initial return value.
!
      ZZDSKSGR = -1.D0


!
!     The radius calculation depends on the coordinate system.
!
      CORSYS = NINT( DSKDSC(SYSIDX) )

      IF ( CORSYS .EQ. LATSYS ) THEN
!
!        Fetch the minimum radius from the descriptor.
!
         MINR = DSKDSC(MN3IDX)

         IF ( MINR .LE. 0.D0 ) THEN

            CALL CHKIN  ( 'ZZDSKSGR'                )
            CALL SETMSG ( 'Minimum radius was *.'   )
            CALL ERRDP  ( '*', MINR                 )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'  )
            CALL CHKOUT ( 'ZZDSKSGR'                )
            RETURN

         END IF

!
!        This is as simple as it gets. The radius bounds
!        correspond to the third coordinate in the descriptor.
!
         ZZDSKSGR = DSKDSC( MX3IDX )


      ELSE IF ( CORSYS .EQ. PDTSYS ) THEN
!
!        Fetch the equatorial radius from the descriptor.
!
         RE = DSKDSC(PARIDX)

         IF ( RE .LE. 0.D0 ) THEN

            CALL CHKIN  ( 'ZZDSKSGR'                  )
            CALL SETMSG ( 'Equatorial radius was *.'  )
            CALL ERRDP  ( '*', RE                     )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'    )
            CALL CHKOUT ( 'ZZDSKSGR'                  )
            RETURN

         END IF

!
!        Fetch the flattening coefficient from the descriptor.
!
         F = DSKDSC(PARIDX+1)

         IF ( ( F .GE. 0.D0 ) .AND. ( F .LT. 1.D0 ) ) THEN
!
!           This is the oblate case.
!
!           The maximum radius of an oblate planetodetic boundary
!           occurs on the X-Y plane at the maximum height.
!
            ZZDSKSGR = DSKDSC(MX3IDX) +  RE


         ELSE IF ( F .LT. 0.D0 ) THEN
!
!           This is the prolate case.
!
!           The maximum radius of an prolate planetodetic boundary
!           occurs on the poles at the maximum height.
!
            RE       = DSKDSC(PARIDX)
            RP       = RE  * ( 1.D0 - F )
            ZZDSKSGR = DSKDSC(MX3IDX) +  RP

         ELSE
!
!           We have an invalid flattening coefficient.
!
!           If the flattening coefficient is greater than one, the
!           polar radius computed below is negative. If it's equal to
!           one, the polar radius is zero. Either case is a problem, so
!           signal an error and check out.
!
            CALL CHKIN  ( 'ZZDSKSGR'                       )
            CALL SETMSG ( 'Flattening coefficient was *.'  )
            CALL ERRDP  ( '*', F                           )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'         )
            CALL CHKOUT ( 'ZZDSKSGR'                       )
            RETURN

         END IF


      ELSE IF ( CORSYS .EQ. RECSYS ) THEN
!
!        The bounding cell of the segment has its extreme value at
!        a corner. Just take the maximum of these values.
!
!        First copy the bounds into an appropriately dimensioned
!        array.
!
         CALL MOVED ( DSKDSC(MN1IDX), 6, BDS )


          B = MN1IDX - 1

          DO I = 1, 3

             J      = B + (2*I) - 1

             MAG(I) = MAX(  ABS(DSKDSC(J)), ABS(DSKDSC(J+1))  )

          END DO

          ZZDSKSGR  = VNORM( MAG )


      ELSE
!
!        Never heard of this coordinate system.
!
         CALL CHKIN ( 'ZZDSKSGR'                                       )
         CALL SETMSG( 'The coordinate system code # is not recognized.')
         CALL ERRINT( '#',  CORSYS                                     )
         CALL SIGERR( 'SPICE(NOTSUPPORTED)'                            )
         CALL CHKOUT( 'ZZDSKSGR'                                       )
         RETURN

      END IF

      END
