!$Procedure      F_TERM ( Test terminator routines )

      SUBROUTINE F_TERM ( OK )

! $ Abstract
!
!     Test the SPICELIB terminator routines.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Currently this test suite is limited to routines that
!     deal with ellipsoids.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 2.0.0, 07-MAY-2012 (NJB)
!
!        Added test cases for EDTERM. Updated header
!        to conform to standard format.
!
!-    TSPICE Version 1.0.0, 31-JAN-2007 (NJB)
!
!-&


!
!     Test Utility Functions
!

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'edterm.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'edterm.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = TIGHT )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 10 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXPT
      PARAMETER           ( MAXPT = 1000 )


!
!     Local Variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    SOURCE
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TITLE


      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      D
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GRIDPT ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONDIF
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PRVLON
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      SRADII ( 3 )
      DOUBLE PRECISION      SRCPOS ( 3 )
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      TRMGRD ( 3, MAXPT )
      DOUBLE PRECISION      XDIFF
      DOUBLE PRECISION      XGRID  ( 3, MAXPT )
      DOUBLE PRECISION      XOBSPS ( 3 )
!      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               NPTS

      LOGICAL               UMBRAL

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TERM' )

!******************************************************************
!
!     ZZEDTERM tests
!
!******************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Spherical target, D/R = 2, ' &
      //           'on z-axis, umbral case'               )


      D = 2.D6

      CALL VPACK ( 0.D0, 0.D0, D, SRCPOS )

      A = 1.D4
      B = A
      C = A

      R = D/2  +  A


      NPTS = 360

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      UMBRAL =  .TRUE.
      PRVLON =  0.D0

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

!
!        Check that the grid point is a tangency point of
!        a plane that is also tangent to the light source.
!        Also check that the center of the source is on the
!        correct side of the plane.
!
         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

!
!        Validate spacing of grid points:  longitude deltas
!        should be constant.  To avoid problems with singularities,
!        we work in a frame where SRCPOS is on the +Z axis and
!        the component of the first grid point orthogonal to the
!        +Z axis lies on the +X axis.
!
         IF ( I .EQ. 1 ) THEN

            CALL TWOVEC ( SRCPOS, 3, TRMGRD(1,1), 1, TRANS )

         END IF

         CALL MXV ( TRANS, TRMGRD(1,I), GRIDPT )

         CALL RECLAT ( GRIDPT, RADIUS, LON, LAT )

         IF ( LON .LT. 0.D0 ) THEN
            LON = LON + TWOPI()
         END IF

         IF ( I .EQ. 2 ) THEN
!
!           We don't know the expected sign of the difference, but we
!           know the expected magnitude.
!
            XDIFF  = LON - PRVLON

            IF ( XDIFF .GT. PI() ) THEN
                XDIFF = XDIFF - TWOPI()
            END IF

            CALL CHCKSD ( 'ABS(XDIFF)', ABS(XDIFF), '~', &
                          TWOPI()/NPTS, TOL,        OK   )

         ELSE IF ( I .GT. 2 ) THEN

            LONDIF = LON - PRVLON

            CALL CHCKSD ( 'LONDIF', LONDIF, '~', &
                          XDIFF,    TOL,    OK   )

         END IF

!         WRITE (*,*) RADIUS,LON*DPR(), LAT*DPR()

         PRVLON = LON

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Spherical target, D/R = 2, ' &
      //           'on z-axis, penumbral case'            )


      D = 2.D6

      CALL VPACK ( 0.D0, 0.D0, D, SRCPOS )

      A = 1.D4
      B = A
      C = A

      R = D/2  +  A


      NPTS = 6

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      UMBRAL =  .FALSE.

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

         CALL RECLAT ( TRMGRD(1,I), RADIUS, LON, LAT )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Slightly prolate target, ' &
      //           'D/R = 2, on z-axis'                 )


      D = 2.D6

      CALL VPACK ( 0.D0, 0.D0, D, SRCPOS )

      A = 1.D4
      B = A - 1.D3
      C = A - 1.D3

      R = 1.D6 + A


      NPTS   =  MAXPT
      UMBRAL =  .TRUE.

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Geometry comparable to Sun-Earth, ' &
      //           'umbral case' )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = 6357.D0

      R = 1.D6


      NPTS = 360

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      UMBRAL =  .TRUE.

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Geometry comparable to Sun-Earth, ' &
      //           'penumbral case'                     )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = 6357.D0

      R = 1.D6


      NPTS   = 360
      UMBRAL = .FALSE.

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Source and target have equal size, ' &
      //           'umbral case'                     )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = A

      R = A

      NPTS   = 360
      UMBRAL = .TRUE.

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Source and target have equal size, ' &
      //           'penumbral case'                     )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = A

      R = A

      NPTS   = 360
      UMBRAL = .FALSE.

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Source is smaller than target, ' &
      //           'umbral case' )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = A

      R = A / 2

      NPTS   = 360
      UMBRAL = .TRUE.

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Source is smaller than target, ' &
      //           'penumbral case' )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )

      A = 6378.D0
      B = A
      C = A

      R = A / 2

      NPTS   = 360
      UMBRAL = .TRUE.

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure each point found is a terminator point.
!
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, &
                        A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



!
!
!     Error handling tests follow.
!
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Bad terminator type.'  )

      CALL ZZEDTERM ( 'PNUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Bad grid size'  )

      NPTS = 0
      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Bad ellipsoid axis'  )

      NPTS = 4

      CALL ZZEDTERM ( 'PENUMBRAL', 0.D0, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTERM ( 'PENUMBRAL', A, 0.D0, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, 0.D0, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Bad light source radius'  )

      NPTS = 4

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, 0.D0, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZEDTERM: Light source and target are too close'  )

      NPTS = 4

      CALL VPACK ( 0.D0, 0.D0, 3.D0, SRCPOS )

      CALL ZZEDTERM ( 'UMBRAL', 2.D0, 1.D0, 1.D0, 1.5D0, &
                      SRCPOS,   NPTS, TRMGRD             )
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )



!******************************************************************
!
!     EDTERM tests
!
!******************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: setup: create and load kernels.'  )

!
!     We'll need a generic test PCK and an SPK file.
!
      CALL TSTPCK ( PCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: observer = moon, target = earth, ' &
      //           'source = sun. Umbral case.'               )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Moon'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the observer-target vector and one-way light time
!     so we can check OBSPOS and TRGEPC.
!
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', ET-LT, TIGHT, OK )

      CALL VMINUS ( TRGPOS, XOBSPS )

      CALL CHCKAD ( 'OBSPOS', OBSPOS, '~~/', XOBSPS, 3, TIGHT, OK )

!
!     In order the check the terminator points, we'll need
!     to express the target-source position in the body-fixed
!     frame at TRGEPC.
!
      CALL SPKPOS ( SOURCE, TRGEPC, FIXREF, ABCORR, TARGET, SRCPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the target radii.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the source radii.
!
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R = SRADII(1)

!
!     Get the expected terminator grid from ZZEDTERM. We
!     rely on ZZEDTERM to work correctly here.
!
      CALL ZZEDTERM ( 'UMBRAL', RADII(1),  RADII(2),  RADII(3), &
                      R,        SRCPOS,    NPTS,      XGRID    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the terminator points.
!
      DO I = 1, NPTS

         CALL CHCKAD ( 'TRMPTS', TRMGRD(1,I), '~~/', &
                                 XGRID (1,I), 3, TIGHT, OK )
      END DO




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: observer = earth, target = earth, ' &
      //           'source = sun. Umbral case.'               )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Earth'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the observer-target vector and one-way light time
!     so we can check OBSPOS and TRGEPC.
!
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', ET-LT, TIGHT, OK )

      CALL VMINUS ( TRGPOS, XOBSPS )

      CALL CHCKAD ( 'OBSPOS', OBSPOS, '~~/', XOBSPS, 3, TIGHT, OK )

!
!     In order the check the terminator points, we'll need
!     to express the target-source position in the body-fixed
!     frame at TRGEPC.
!
      CALL SPKPOS ( SOURCE, TRGEPC, FIXREF, ABCORR, TARGET, SRCPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the target radii.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the source radii.
!
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R = SRADII(1)

!
!     Get the expected terminator grid from ZZEDTERM. We
!     rely on ZZEDTERM to work correctly here.
!
      CALL ZZEDTERM ( 'UMBRAL', RADII(1),  RADII(2),  RADII(3), &
                      R,        SRCPOS,    NPTS,      XGRID    )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the terminator points.
!
      DO I = 1, NPTS

         CALL CHCKAD ( 'TRMPTS', TRMGRD(1,I), '~~/', &
                                 XGRID (1,I), 3, TIGHT, OK )
      END DO



!
!
!     EDTERM error cases:
!
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: No frame name definition.'  )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Moon'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3


      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    'X',      ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: No target name definition.'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, 'X',    ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: Target frame not centered at target.'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET,  ET, &
                    'J2000',  ABCORR, OBSRVR,  NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFIXREF)', OK )




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: Bad terminator type.'  )


      CALL EDTERM ( 'UMB',    SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: NPTS is non-positive'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, -1, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: target semi-axis length non-positive'  )


!
!     Fetch the target radii.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create bad radii for the target and store these
!     in the pool.
!
      CALL VEQU ( RADII, BADRAD )

      BADRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Restore good radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: source semi-axis length non-positive'  )


!
!     Fetch the source radii.
!
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create bad radii for the target and store these
!     in the pool.
!
      CALL VEQU ( RADII, BADRAD )

      BADRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY10_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Restore good radii.
!
      CALL PDPOOL ( 'BODY10_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: target radius count != 3'  )

!
!     Fetch the target radii.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the radius count to 1.
!
      CALL PDPOOL ( 'BODY399_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Set the radius count to 4.
!
      CALL PDPOOL ( 'BODY399_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Restore good radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: source radius count != 3'  )

!
!     Fetch the source radii.
!
      CALL BODVRD ( SOURCE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the radius count to 1.
!
      CALL PDPOOL ( 'BODY10_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Set the radius count to 4.
!
      CALL PDPOOL ( 'BODY10_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, 0, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Restore good radii.
!
      CALL PDPOOL ( 'BODY10_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: source is too close to target'  )

!
!     Fetch the source radii.
!
      CALL BODVRD ( SOURCE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the target radii to some awfully large values.
!
      CALL VSCL ( 1.D6, RADII, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )

!
!     Restore good radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: SPK lookup failure.'  )


      ET = 1.D9

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET, &
                    FIXREF,   ABCORR, OBSRVR, NPTS, &
                    TRGEPC,   OBSPOS, TRMGRD        )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'EDTERM: clean up: unload and delete kernels.'  )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END







!
!
!     Utility functions:
!
!
!
!     Check a terminator point on an ellipsoid.
!
      SUBROUTINE T_ETERM ( TITLE, UMBRAL, SRCPOS, SRCRAD, A, B, C, &
                           X,     OK                               )
      IMPLICIT NONE

      CHARACTER*(*)         TITLE
      LOGICAL               UMBRAL
      DOUBLE PRECISION      SRCPOS ( 3 )
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      X      ( 3 )
      LOGICAL               OK

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

!
!     Local parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-15 )

      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = VTIGHT )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      D
      DOUBLE PRECISION      H
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      NEARP  ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      SCALE

!
!     Get the ellipsoid's outward normal vector at X.
!
      CALL SURFNM ( A, B, C, X, N )

!
!     Form the tangent plane at X.
!
      CALL NVP2PL ( N, X, PLANE )

!
!     The center of the source should be below the tangent
!     plane in the umbral case and above the plane in the
!     penumbral case.
!
      LABEL = TITLE
      CALL SUFFIX ( '<N,SRCPOS>', 1, LABEL )

      CALL VSUB ( SRCPOS, X, OFFSET )

      IF ( UMBRAL ) THEN
         CALL CHCKSD ( LABEL, VDOT(N,OFFSET), '<', 0.D0, TOL, OK )
      ELSE


         CALL CHCKSD ( LABEL, VDOT(N,OFFSET), '>', 0.D0, TOL, OK )
      END IF

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     Find the nearest point on the plane to the center
!     of the light source.
!
      CALL VPRJP ( SRCPOS, PLANE, NEARP )

!
!     Find the plane's height relative to the source's
!     surface.
!
      H  =  VDIST ( NEARP, SRCPOS )  -  SRCRAD

!
!     Determine the scale that should be used for the
!     coming relative error computation.
!
      D     = VNORM ( SRCPOS )

      SCALE = MAX ( D, SRCRAD, A, B, C )

      LABEL = TITLE
      CALL SUFFIX ( 'ABS(H)/SCALE', 1, LABEL )


      CALL CHCKSD ( TITLE, ABS(H)/SCALE, '~', 0.D0, TOL, OK )

      RETURN
      END


