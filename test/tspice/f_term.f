C$Procedure      F_TERM ( Test terminator routines )
 
      SUBROUTINE F_TERM ( OK )
 
C$ Abstract
C
C     Test the SPICELIB terminator routines.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Currently this test suite is limited to routines that
C     deal with ellipsoids.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 2.0.0, 07-MAY-2012 (NJB)
C
C        Added test cases for EDTERM. Updated header
C        to conform to standard format.
C
C-    TSPICE Version 1.0.0, 31-JAN-2007 (NJB)
C
C-&
  
 
C
C     Test Utility Functions
C
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

C
C     Local parameters
C
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


C
C     Local Variables
C
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
C      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               NPTS

      LOGICAL               UMBRAL

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TERM' )

C******************************************************************
C
C     ZZEDTERM tests
C
C******************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Spherical target, D/R = 2, '
     .//           'on z-axis, umbral case'               )


      D = 2.D6

      CALL VPACK ( 0.D0, 0.D0, D, SRCPOS )
      
      A = 1.D4
      B = A
      C = A

      R = D/2  +  A


      NPTS = 360

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure each point found is a terminator point.
C
      UMBRAL =  .TRUE.
      PRVLON =  0.D0

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

C
C        Check that the grid point is a tangency point of
C        a plane that is also tangent to the light source. 
C        Also check that the center of the source is on the
C        correct side of the plane.
C
         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

C
C        Validate spacing of grid points:  longitude deltas
C        should be constant.  To avoid problems with singularities,
C        we work in a frame where SRCPOS is on the +Z axis and
C        the component of the first grid point orthogonal to the
C        +Z axis lies on the +X axis.
C
         IF ( I .EQ. 1 ) THEN
            
            CALL TWOVEC ( SRCPOS, 3, TRMGRD(1,1), 1, TRANS )
            
         END IF

         CALL MXV ( TRANS, TRMGRD(1,I), GRIDPT )

         CALL RECLAT ( GRIDPT, RADIUS, LON, LAT )

         IF ( LON .LT. 0.D0 ) THEN
            LON = LON + TWOPI()
         END IF

         IF ( I .EQ. 2 ) THEN
C
C           We don't know the expected sign of the difference, but we
C           know the expected magnitude.
C
            XDIFF  = LON - PRVLON

            IF ( XDIFF .GT. PI() ) THEN
                XDIFF = XDIFF - TWOPI()
            END IF

            CALL CHCKSD ( 'ABS(XDIFF)', ABS(XDIFF), '~', 
     .                    TWOPI()/NPTS, TOL,        OK   )

         ELSE IF ( I .GT. 2 ) THEN

            LONDIF = LON - PRVLON
            
            CALL CHCKSD ( 'LONDIF', LONDIF, '~', 
     .                    XDIFF,    TOL,    OK   )
            
         END IF

C         WRITE (*,*) RADIUS,LON*DPR(), LAT*DPR()

         PRVLON = LON

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Spherical target, D/R = 2, '
     .//           'on z-axis, penumbral case'            )


      D = 2.D6

      CALL VPACK ( 0.D0, 0.D0, D, SRCPOS )
      
      A = 1.D4
      B = A
      C = A

      R = D/2  +  A


      NPTS = 6

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure each point found is a terminator point.
C
      UMBRAL =  .FALSE.

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

         CALL RECLAT ( TRMGRD(1,I), RADIUS, LON, LAT )

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Slightly prolate target, '
     .//           'D/R = 2, on z-axis'                 )


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

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Geometry comparable to Sun-Earth, '
     .//           'umbral case' )


      D = 1.5D8

      CALL VPACK ( D,  0.D0, 0.D0, SRCPOS )
      
      A = 6378.D0
      B = A
      C = 6357.D0

      R = 1.D6


      NPTS = 360

      CALL ZZEDTERM ( 'UMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure each point found is a terminator point.
C
      UMBRAL =  .TRUE.

      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Geometry comparable to Sun-Earth, '
     .//           'penumbral case'                     )


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

C
C     Make sure each point found is a terminator point.
C
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Source and target have equal size, '
     .//           'umbral case'                     )


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

C
C     Make sure each point found is a terminator point.
C
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Source and target have equal size, '
     .//           'penumbral case'                     )


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

C
C     Make sure each point found is a terminator point.
C
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Source is smaller than target, '
     .//           'umbral case' )


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

C
C     Make sure each point found is a terminator point.
C
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Source is smaller than target, '
     .//           'penumbral case' )


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

C
C     Make sure each point found is a terminator point.
C
      DO I = 1, NPTS

         TITLE = 'Point *,'
         CALL REPMI ( TITLE, '*', I, TITLE )

         CALL T_ETERM ( TITLE,  UMBRAL, SRCPOS, R, 
     .                  A,      B,      C,      TRMGRD(1,I),  OK )

      END DO



C
C
C     Error handling tests follow.
C
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Bad terminator type.'  )

      CALL ZZEDTERM ( 'PNUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )
      
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Bad grid size'  )

      NPTS = 0
      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Bad ellipsoid axis'  )

      NPTS = 4
      
      CALL ZZEDTERM ( 'PENUMBRAL', 0.D0, B, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTERM ( 'PENUMBRAL', A, 0.D0, C, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTERM ( 'PENUMBRAL', A, B, 0.D0, R, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Bad light source radius'  )

      NPTS = 4
      
      CALL ZZEDTERM ( 'PENUMBRAL', A, B, C, 0.D0, SRCPOS, NPTS, TRMGRD )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZEDTERM: Light source and target are too close'  )

      NPTS = 4
      
      CALL VPACK ( 0.D0, 0.D0, 3.D0, SRCPOS )

      CALL ZZEDTERM ( 'UMBRAL', 2.D0, 1.D0, 1.D0, 1.5D0, 
     .                SRCPOS,   NPTS, TRMGRD             )
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )



C******************************************************************
C
C     EDTERM tests
C
C******************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: setup: create and load kernels.'  )

C
C     We'll need a generic test PCK and an SPK file.
C
      CALL TSTPCK ( PCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: observer = moon, target = earth, '
     .//           'source = sun. Umbral case.'               )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Moon'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3
      
      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the observer-target vector and one-way light time
C     so we can check OBSPOS and TRGEPC.
C
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', ET-LT, TIGHT, OK )

      CALL VMINUS ( TRGPOS, XOBSPS )

      CALL CHCKAD ( 'OBSPOS', OBSPOS, '~~/', XOBSPS, 3, TIGHT, OK )

C
C     In order the check the terminator points, we'll need
C     to express the target-source position in the body-fixed
C     frame at TRGEPC.
C
      CALL SPKPOS ( SOURCE, TRGEPC, FIXREF, ABCORR, TARGET, SRCPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the target radii.
C      
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the source radii.
C      
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R = SRADII(1)

C
C     Get the expected terminator grid from ZZEDTERM. We
C     rely on ZZEDTERM to work correctly here.
C
      CALL ZZEDTERM ( 'UMBRAL', RADII(1),  RADII(2),  RADII(3), 
     .                R,        SRCPOS,    NPTS,      XGRID    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the terminator points. 
C
      DO I = 1, NPTS

         CALL CHCKAD ( 'TRMPTS', TRMGRD(1,I), '~~/', 
     .                           XGRID (1,I), 3, TIGHT, OK )
      END DO




C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: observer = earth, target = earth, '
     .//           'source = sun. Umbral case.'               )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Earth'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3
      
      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the observer-target vector and one-way light time
C     so we can check OBSPOS and TRGEPC.
C
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', ET-LT, TIGHT, OK )

      CALL VMINUS ( TRGPOS, XOBSPS )

      CALL CHCKAD ( 'OBSPOS', OBSPOS, '~~/', XOBSPS, 3, TIGHT, OK )

C
C     In order the check the terminator points, we'll need
C     to express the target-source position in the body-fixed
C     frame at TRGEPC.
C
      CALL SPKPOS ( SOURCE, TRGEPC, FIXREF, ABCORR, TARGET, SRCPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the target radii.
C      
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the source radii.
C      
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R = SRADII(1)

C
C     Get the expected terminator grid from ZZEDTERM. We
C     rely on ZZEDTERM to work correctly here.
C
      CALL ZZEDTERM ( 'UMBRAL', RADII(1),  RADII(2),  RADII(3), 
     .                R,        SRCPOS,    NPTS,      XGRID    )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the terminator points. 
C
      DO I = 1, NPTS

         CALL CHCKAD ( 'TRMPTS', TRMGRD(1,I), '~~/', 
     .                           XGRID (1,I), 3, TIGHT, OK )
      END DO



C
C
C     EDTERM error cases:
C
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: No frame name definition.'  )

      ET = 1.D8

      ABCORR = 'LT+S'
      OBSRVR = 'Moon'
      TARGET = '399'
      SOURCE = '10'
      FIXREF = 'IAU_EARTH'
      NPTS   = 3


      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              'X',      ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: No target name definition.'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, 'X',    ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: Target frame not centered at target.'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET,  ET,      
     .              'J2000',  ABCORR, OBSRVR,  NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFIXREF)', OK )




C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: Bad terminator type.'  )

      
      CALL EDTERM ( 'UMB',    SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: NPTS is non-positive'  )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, -1, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: target semi-axis length non-positive'  )

      
C
C     Fetch the target radii.
C      
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create bad radii for the target and store these
C     in the pool.
C
      CALL VEQU ( RADII, BADRAD )

      BADRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Restore good radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: source semi-axis length non-positive'  )

      
C
C     Fetch the source radii.
C      
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create bad radii for the target and store these
C     in the pool.
C
      CALL VEQU ( RADII, BADRAD )

      BADRAD(1) = 0.D0

      CALL PDPOOL ( 'BODY10_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Restore good radii.
C
      CALL PDPOOL ( 'BODY10_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: target radius count != 3'  )
      
C
C     Fetch the target radii.
C      
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the radius count to 1.
C
      CALL PDPOOL ( 'BODY399_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Set the radius count to 4.
C
      CALL PDPOOL ( 'BODY399_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Restore good radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: source radius count != 3'  )
      
C
C     Fetch the source radii.
C      
      CALL BODVRD ( SOURCE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the radius count to 1.
C
      CALL PDPOOL ( 'BODY10_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Set the radius count to 4.
C
      CALL PDPOOL ( 'BODY10_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, 0, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Restore good radii.
C
      CALL PDPOOL ( 'BODY10_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: source is too close to target'  )
      
C
C     Fetch the source radii.
C      
      CALL BODVRD ( SOURCE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the target radii to some awfully large values.
C
      CALL VSCL ( 1.D6, RADII, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )

C
C     Restore good radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: SPK lookup failure.'  )


      ET = 1.D9

      CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, ET,      
     .              FIXREF,   ABCORR, OBSRVR, NPTS, 
     .              TRGEPC,   OBSPOS, TRMGRD        )      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'EDTERM: clean up: unload and delete kernels.'  )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      CALL T_SUCCESS ( OK )
      RETURN
      END







C
C
C     Utility functions:
C
C
C
C     Check a terminator point on an ellipsoid.
C
      SUBROUTINE T_ETERM ( TITLE, UMBRAL, SRCPOS, SRCRAD, A, B, C, 
     .                     X,     OK                               )
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

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

C
C     Local parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-15 )

      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = VTIGHT )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      D
      DOUBLE PRECISION      H
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      NEARP  ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      SCALE

C
C     Get the ellipsoid's outward normal vector at X.
C
      CALL SURFNM ( A, B, C, X, N )

C
C     Form the tangent plane at X.
C     
      CALL NVP2PL ( N, X, PLANE )

C
C     The center of the source should be below the tangent
C     plane in the umbral case and above the plane in the
C     penumbral case.
C
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

C
C     Find the nearest point on the plane to the center 
C     of the light source.
C
      CALL VPRJP ( SRCPOS, PLANE, NEARP )

C
C     Find the plane's height relative to the source's
C     surface.
C      
      H  =  VDIST ( NEARP, SRCPOS )  -  SRCRAD

C
C     Determine the scale that should be used for the
C     coming relative error computation.
C
      D     = VNORM ( SRCPOS )

      SCALE = MAX ( D, SRCRAD, A, B, C )

      LABEL = TITLE
      CALL SUFFIX ( 'ABS(H)/SCALE', 1, LABEL )


      CALL CHCKSD ( TITLE, ABS(H)/SCALE, '~', 0.D0, TOL, OK )

      RETURN
      END 

      
