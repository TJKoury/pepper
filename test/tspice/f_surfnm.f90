!$Procedure F_SURFNM ( SURFNM tests )

      SUBROUTINE F_SURFNM ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine SURFNM.  SURFNM find the unit
!     outward normal at a specfied point on an ellipsoid.
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
! $ Files
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
! $ Particulars
!
!     This routine tests the SPICELIB routine SURFNM.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 21-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE                  ORIGIN

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_SURFNM' )


!
!     Run some simple tests where the correct results can be
!     determined by inspection.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'SURFNM simple case:  point is at top of unit sphere'
      CALL TCASE  ( TITLE )

      A = 1.D0
      B = 1.D0
      C = 1.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, SRFPT  )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'SURFNM simple case:  point is (sqrt(3)/3,sqrt(3)/3,' // &
              'sqrt(3)/3) on unit sphere'
      CALL TCASE  ( TITLE )

      A = 1.D0
      B = 1.D0
      C = 1.D0

      X = SQRT( 3.D0 ) / 3.D0

      CALL VPACK ( X, X, X, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'SURFNM simple case: repeat previous case, but scale ' // &
              'down sphere by 1.D-200'
      CALL TCASE  ( TITLE )

      SCALE = 1.D-200

      A = SCALE
      B = SCALE
      C = SCALE

      X = SQRT( 3.D0 ) / 3.D0

      Y = X * SCALE

      CALL VPACK ( Y, Y, Y, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'SURFNM simple case: repeat previous case, but scale ' // &
              'up sphere by 1.D200'
      CALL TCASE  ( TITLE )

      SCALE = 1.D200

      A = SCALE
      B = SCALE
      C = SCALE

      X = SQRT( 3.D0 ) / 3.D0

      Y = X * SCALE

      CALL VPACK ( Y, Y, Y, SRFPT  )
      CALL VPACK ( X, X, X, XNORML )

      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Triaxial case: axis lengths are 1, 2, 3.  Point ' // &
              'lies along direction vector (1, 0.3, 0.5)'

      CALL TCASE  ( TITLE )

      A  =  1.D0
      B  =  2.D0
      C  =  3.D0

      CALL VPACK ( 1.D0, 0.3D0, 0.5D0, U )

!
!     Find the surface point on the specified line.
!
      CALL SURFPT ( ORIGIN, U, A, B, C, SRFPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute the normal direction; unitize the expected normal vector.
!
      CALL VPACK ( SRFPT(1)/A**2, SRFPT(2)/B**2, SRFPT(3)/C**2, XNORML )

      CALL VHATIP ( XNORML )

!
!     See whether SURFNM agrees.
!
      CALL SURFNM ( A, B, C, SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, TIGHT, OK )




!
!     SURFNM error cases:
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INEDPL: ellipsoid has one zero-length axis.' )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SRFPT )

      CALL SURFNM (  0.D0, 1.D0,   1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  0.D0,  1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  1.D0,  0.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SURFNM: ellipsoid has one negative-length axis.' )

      CALL SURFNM ( -1.D0, 1.D0,   1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0, -1.D0,  1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL SURFNM (  1.D0,  1.D0, -1.D0,  SRFPT, NORMAL )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


