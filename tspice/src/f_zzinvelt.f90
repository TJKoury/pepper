!$Procedure F_ZZINVELT ( ZZINVELT tests )

      SUBROUTINE F_ZZINVELT ( OK )
      use pepper_legacy
      use pepper_inc_dskdsc
      use pepper_inc_dsktol

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINVELT.
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
!     This routine tests the private SPICELIB routine ZZINVELT.
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
!-    TSPICE Version 1.0.0, 03-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!

!
!     Local Variables
!
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      P      ( 3 )

      INTEGER               CORSYS

      INTEGER               EXCLUD
      LOGICAL               INSIDE
      LOGICAL               XIN

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINVELT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RECSYS: interior case. Exclude none. Zero MARGIN.' )


      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RECSYS: interior case. Exclude none. ' &
      //           'MARGIN > 0.' )


      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)


      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 0.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RECSYS: exterior case. Exclude none. ' &
      //           'MARGIN > 0.' )


      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)


      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 1.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RECSYS: interior case. Exclude Z. ' &
      //           'MARGIN > 0.' )


      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)


      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 1.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LATSYS: interior case. Exclude none. Zero MARGIN.' )


      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 7.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LATSYS: interior case. Exclude none. ' &
      //           'MARGIN > 0.'                          )


      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 0.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LATSYS: exterior case. Exclude none. ' &
      //           'MARGIN > 0.'                          )


      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'LATSYS: interior case. Exclude radius. ' &
      //           'MARGIN > 0.'                          )


      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'PDTSYS: interior case. Exclude none. Zero MARGIN.' )


      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0


      CALL GEOREC ( 0.D0, 0.D0, 0.5D0, CORPAR(1), CORPAR(2), P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'PDTSYS: interior case. Exclude none. ' &
      //           'MARGIN > 0.'                          )

      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0

      L           =   BOUNDS(2,3) - BOUNDS(1,3)


      CALL GEOREC ( 0.D0,      0.D0,      1.D0 + 0.5D0*L*MARGIN, &
                    CORPAR(1), CORPAR(2), P                       )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'PDTSYS: exterior case. Exclude none. ' &
      //           'MARGIN > 0.'                          )


      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0

      L           =   BOUNDS(2,3) - BOUNDS(1,3)


      CALL GEOREC ( 0.D0,      0.D0,      1.D0 + 1.5D0*L*MARGIN, &
                    CORPAR(1), CORPAR(2), P                       )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'PDTSYS: interior case. Exclude altitude. ' &
      //           'MARGIN > 0.'                              )


      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




!***********************************************************************
!
!     Error cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: Negative margin.'  )

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = -1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: Invalid coordinate system code.'  )

      MARGIN = 0.D0

      CALL ZZINVELT ( P,      -1,     CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL ZZINVELT ( P,       5,     CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: Invalid exclude index.'  )

      MARGIN = 0.D0

      EXCLUD = -1

      CALL ZZINVELT ( P,      PDTSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      EXCLUD = 4

      CALL ZZINVELT ( P,      PDTSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )



      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid planetodetic coordinate ' &
      //           'parameters.'                            )

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = -8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = -1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0


      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CORPAR(1) = 8.D0
      CORPAR(2) = 1.1D0

      CALL ZZINVELT ( P,      CORSYS, CORPAR, &
                      BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


