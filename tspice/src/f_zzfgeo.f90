!$Procedure      F_ZZFGEO ( Test private "fast" geometry routines )

      SUBROUTINE F_ZZFGEO ( OK )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Test private SPICELIB "fast" geometry routines.
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
!     This routine tests the private routine routines
!
!        ZZINRYPL
!        ZZRYXSPH
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
!-    TSPICE Version 1.0.0, 11-JAN-2016 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
!      DOUBLE PRECISION      VNORM

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )


!      INTEGER               LNSIZE
!      PARAMETER           ( LNSIZE = 80 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 240 )

!
!     Local Variables
!
!      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      MAXD
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UNORML ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               CASENO
!      INTEGER               I
      INTEGER               NCASE
      INTEGER               NXPTS
      INTEGER               SEED
      INTEGER               XNXPTS

      LOGICAL               FOUND
      LOGICAL               XFOUND
!
!     Save all variables to avoid stack problems on some
!     platforms.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZFGEO' )


!**********************************************************************
!
!     ZZINRYPL tests
!
!**********************************************************************



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZINRYPL: basic test with plane parallel to X-Y ' &
      //           'axes.'                                           )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 1.D0

      CALL VPACK (  2.D0, 4.D0,  2.D0, VERTEX )
      CALL VPACK ( -1.D0, 0.D0, -1.D0, DIR    )

      CALL VHAT ( DIR, UDIR )

!
!     Compute expected result.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSI ( 'ZNXPTS', XNXPTS, '=', 1, 0, OK )

!
!     Compute result using ZZINRYPL.
!
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, &
                      CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )
      END IF

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZINRYPL: ray lies in plane.' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 0.D0

      CALL VPACK (  2.D0, 4.D0,  0.D0, VERTEX )

      CALL VPACK ( -1.D0, 0.D0,  0.D0, DIR    )

      CALL VHAT ( DIR, UDIR )


!
!     Compute result using ZZINRYPL.
!
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, &
                      CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

!
!     This is a non-intersection case for ZZINRYPL.
!
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'ZZINRYPL: ray vertex only lies in plane.' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 0.D0

      CALL VPACK (  2.D0, 4.D0,  0.D0, VERTEX )

      CALL VPACK ( -1.D0, 0.D0,  -1.D0, DIR    )

      CALL VHAT ( DIR, UDIR )


!
!     Compute result using ZZINRYPL.
!
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, &
                      CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

!
!     This is an intersection case for ZZINRYPL.
!
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', VERTEX, 3, TIGHT, OK )
      END IF


!
! --- Case --------------------------------------------------------
!

!
!     ZZINRYPL random tests
!

      SEED = -1

      NCASE = 10000

      DO CASENO = 1, NCASE

         SCALE      = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

         NORMAL(1)  = T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2)  = T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3)  = T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHAT ( NORMAL, UNORML )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CONST      = SCALE

         CALL NVC2PL ( UNORML, CONST, PLANE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         DIR(1)  = T_RANDD ( -1.D0, 1.D0, SEED )
         DIR(2)  = T_RANDD ( -1.D0, 1.D0, SEED )
         DIR(3)  = T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHAT ( DIR, UDIR )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         VERTEX(1)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(2)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(3)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE


!
! --- Case --------------------------------------------------------
!
         TITLE = 'Scale = #; unit normal = (#,#,#); const = #; ' &
         //      'UDIR = (#,#,#), VERTEX = (#,#,#).'

         CALL REPMD ( TITLE, '#', SCALE,     7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(3), 7, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(1),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(2),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(3),   7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(3), 7, TITLE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL TCASE ( TITLE  )


         CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

!
!        Compute result using ZZINRYPL.
!
         MAXD = 1.D10 * SCALE

         CALL ZZINRYPL ( VERTEX, UDIR, UNORML, &
                         CONST,  MAXD, NXPTS,  XPT )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         IF ( ( XNXPTS .EQ. 1 ) .OR. ( XNXPTS .EQ. 0 ) ) THEN

            CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

            IF ( OK .AND. ( NXPTS .EQ. 1 ) ) THEN
               CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )
            END IF

         END IF

      END DO






!**********************************************************************
!
!     ZZRYXSPH tests
!
!**********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Unit sphere, vertex at center' )

      CALL CLEARD ( 3, VERTEX )

      DIR(1) = 1.D0
      DIR(2) = 0.D0
      DIR(3) = 0.D0

      R      = 1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL VPACK ( 1.D0, 0.D0, 0.D0, XXPT )

      IF ( OK .AND. FOUND ) THEN

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

      END IF


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Unit sphere, vertex on Z axis at Z=2; DIR ' &
      //           'points in -Z direction.'                   )

      CALL VPACK ( 0.D0, 0.D0, 2.D0, VERTEX )

      DIR(1) =  0.D0
      DIR(2) =  0.D0
      DIR(3) = -1.D0

      R      =  1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XXPT )

      IF ( OK .AND. FOUND ) THEN

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

      END IF

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Unit sphere, vertex on line X=1,Y=2 at Z=2; DIR ' &
      //           'points in -Z direction. Miss expected.'         )

      CALL VPACK ( 1.D0, 2.D0, 2.D0, VERTEX )

      DIR(1) =  0.D0
      DIR(2) =  0.D0
      DIR(3) = -1.D0

      R      =  1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



!
! --- Case --------------------------------------------------------
!

!
!     ZZRYXSPH random tests
!

      SEED = -1

      NCASE = 10000

      DO CASENO = 1, NCASE

         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

         R      = T_RANDD ( 1.D0, 2.D0, SEED ) * SCALE / 2

         VERTEX(1)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(2)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(3)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE

         DIR(1) = - VERTEX(1) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2
         DIR(2) = - VERTEX(2) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2
         DIR(3) = - VERTEX(3) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2

         CALL VHAT ( DIR, UDIR )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

!
! --- Case --------------------------------------------------------
!
         TITLE = 'Scale = #; unit normal = (#,#,#); const = #; ' &
         //      'UDIR = (#,#,#), VERTEX = (#,#,#).'

         CALL REPMD ( TITLE, '#', SCALE,     7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(3), 7, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(1),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(2),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(3),   7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(3), 7, TITLE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL TCASE ( TITLE  )


         CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

!
!        Compute result using ZZINRYPL.
!
         CALL SURFPT ( VERTEX, UDIR, R, R, R, XXPT, XFOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL ZZRYXSPH ( VERTEX, UDIR, R, XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

!         WRITE (*,*) 'FOUND, ||V||-R = ', FOUND, VNORM(VERTEX)-R

         CALL CHCKSL ( 'FOUND', FOUND, XFOUND, OK )

         IF ( OK .AND. FOUND ) THEN

            CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

         END IF

      END DO





      CALL T_SUCCESS ( OK )
      RETURN
      END
