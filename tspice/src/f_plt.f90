!$Procedure F_PLT ( Plate routine tests )

      SUBROUTINE F_PLT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise plate routines.
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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB plate routines that
!     are not covered by their own test families. These are:
!
!        PLTAR
!        PLTNRM
!        PLTVOL
!        PLTEXP
!
!     The plate routine
!
!        PLTNP
!
!     has its own, separate test family.
!
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
!-    TSPICE Version 1.0.0, 24-OCT-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!
!      DOUBLE PRECISION      TIGHT
!      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 1000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      AREA
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EDGE1  ( 3 )
      DOUBLE PRECISION      EDGE2  ( 3 )
      DOUBLE PRECISION      IVERTS ( 3, 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      OVERTS ( 3, 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )
      DOUBLE PRECISION      VOL
      DOUBLE PRECISION      VOUT   ( LBCELL : 3*MAXV )
      DOUBLE PRECISION      VOUT1  ( LBCELL : 3*MAXV )
      DOUBLE PRECISION      VRTCES ( 3, MAXV )
      DOUBLE PRECISION      XAREA
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      XVERTS ( 3, 3 )
      DOUBLE PRECISION      XVOL

      INTEGER               I
      INTEGER               J
      INTEGER               NP
      INTEGER               NV
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               PLATS2 ( 3, MAXP )
      INTEGER               POUT   ( LBCELL : 3*MAXP )
      INTEGER               POUT1  ( LBCELL : 3*MAXP )

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_PLT' )



!**********************************************************************
!
!     PLTAR tests
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTAR Error: too few plates'

      CALL TCASE ( TITLE )

      CALL CLEARD ( 3*MAXV, VRTCES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( 3*MAXP, PLATES )

      NV = 3
      NP = -1

      AREA = PLTAR ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTAR Error: too few vertices'

      CALL TCASE ( TITLE )

      NV = 2
      NP = 1

      AREA = PLTAR ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWVERTICES)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTAR Error: bad vertex index in plate'

      CALL TCASE ( TITLE )
!
!     Initialize cells.
!
      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a plate set for a box.
!
      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NP = CARDI(POUT) / 3
      NV = CARDD(VOUT) / 3

!
!     Create a bad plate: first vertex index is 0.
!
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 1, 1 ) = 0

      AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

!
!     Restore the original plate. It's simplest to copy the
!     plate set. Set another invalid vertex index.
!
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 2, 1 ) = 0

      DO I = 1, NP

         DO J = 1, 3

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = 0

            AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = NV+1

            AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTAR: Find area of a box centered at the origin.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NV = CARDD( VOUT ) / 3
      NP = CARDI( POUT ) / 3

      AREA = PLTAR ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XAREA = 2 * ( ( A * B ) + ( B * C ) + ( A * C ) )

      CALL CHCKSD ( 'AREA', AREA, '~/', XAREA, VTIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTAR: Find area of a box that excludes the origin.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a vector by which to translate the vertices of the box.
!
      CALL VPACK ( 100.D0, 200.D0, -400.D0, OFFSET )

!
!     Create a translated box. The plate set doesn't change.
!
      CALL ZZPSXLAT ( VOUT, OFFSET, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute the area of the translated box.
!
      AREA = PLTAR ( NV, VOUT1(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AREA', AREA, '~/', XAREA, VTIGHT, OK )



!**********************************************************************
!
!     PLTVOL tests
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTVOL Error: too few plates'

      CALL TCASE ( TITLE )

      CALL CLEARD ( 3*MAXV, VRTCES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( 3*MAXP, PLATES )

      NV = 4
      NP = 3

      AREA = PLTVOL ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPLATES)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTVOL Error: too few vertices'

      CALL TCASE ( TITLE )

      NV = 3
      NP = 4

      AREA = PLTVOL ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWVERTICES)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLVOL Error: bad vertex index in plate'

      CALL TCASE ( TITLE )
!
!     Initialize cells.
!
      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a plate set for a box.
!
      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NP = CARDI(POUT) / 3
      NV = CARDD(VOUT) / 3

!
!     Create a bad plate: first vertex index is 0.
!
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 1, 1 ) = 0

      VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

!
!     Restore the original plate. It's simplest to copy the
!     plate set. Set another invalid vertex index.
!
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 2, 1 ) = 0

      DO I = 1, NP

         DO J = 1, 3

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = 0

            VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = NV+1

            VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTVOL: Find volume of a box centered at the origin.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NV = CARDD( VOUT ) / 3
      NP = CARDI( POUT ) / 3

      VOL = PLTVOL ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XVOL = A * B * C

      CALL CHCKSD ( 'VOL', VOL, '~/', XVOL, VTIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTVOL: Find volume of a box that excludes the origin.'

      CALL SSIZED ( 3*MAXV, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a vector by which to translate the vertices of the box.
!
      CALL VPACK ( 100.D0, 200.D0, -400.D0, OFFSET )

!
!     Create a translated box. The plate set doesn't change.
!
      CALL ZZPSXLAT ( VOUT, OFFSET, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute the volume of the translated box.
!
      VOL = PLTVOL ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XVOL = A * B * C

      CALL CHCKSD ( 'VOL', VOL, '~/', XVOL, VTIGHT, OK )



!**********************************************************************
!
!     PLTNRM tests
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'PLTNRM: Compute an upward normal of an equilateral ' &
      //       'triangle lying in the X-Y plane and centered ' &
      //       'at the origin.'

      CALL TCASE( TITLE )


      S = SQRT(3.D0)/2

      CALL VPACK (    S,  -0.5D0,  0.D0, V1 )
      CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 )
      CALL VPACK (   -S,  -0.5D0,  0.D0, V3 )


      CALL PLTNRM ( V1, V2, V3, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute an expected normal vector.
!
      CALL VSUB ( V2, V1, EDGE1 )
      CALL VSUB ( V3, V2, EDGE2 )

      CALL VCRSS ( EDGE1, EDGE2, XNORML )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, VTIGHT, OK )

!
!     Convert normal to unit length and compare to a known vector.
!
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL VHATIP ( NORMAL )

      CALL CHCKAD ( 'NORMAL (unit)', NORMAL, '~~/', &
                    XNORML, 3, VTIGHT, OK )




!**********************************************************************
!
!     PLTEXP tests
!
!**********************************************************************



      TITLE = 'PLTEXP: expand a plate that is parallel to, but not ' &
      //      'contained in, the X-Y plane.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -1.D0,  -1.D0,   3.D0,  IVERTS(1,1) )
      CALL VPACK (   2.D0,  -1.D0,   3.D0,  IVERTS(1,2) )
      CALL VPACK (   2.D0,   4.D0,   3.D0,  IVERTS(1,3) )

      DELTA = 1.D0

      CALL PLTEXP ( IVERTS, DELTA, OVERTS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Prepare the expected output.
!
      S = 1.D0 / 3.D0

      CALL VLCOM3 ( S, IVERTS(1,1), S, IVERTS(1,2), &
                    S, IVERTS(1,3),    CENTER      )

      DO I = 1, 3

         CALL VSUB  ( IVERTS(1,I), CENTER, OFFSET )

         CALL VLCOM ( 1.D0, CENTER, 2.D0, OFFSET, XVERTS(1,I) )

      END DO


      CALL CHCKAD ( 'OVERTS', OVERTS, '~~/', XVERTS, 9, VTIGHT, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


