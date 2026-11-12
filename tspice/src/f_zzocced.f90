!$Procedure      F_ZZOCCED ( Test ellipsoid occultation routine )

      SUBROUTINE F_ZZOCCED ( OK )
      use pepper_legacy
      use pepper_inc_zzocced
      IMPLICIT NONE

      LOGICAL               OK


! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZOCCED
!
! $ Version
!
!-    TSPICE Version 2.0.0, 07-MAR-2006 (NJB)
!
!        Added test case for observer within outer bounding sphere
!        of one target, but outside of that target.
!
!-    TSPICE Version 1.0.0, 08-AUG-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      CENTR1 ( 3 )
      DOUBLE PRECISION      CENTR2 ( 3 )
      DOUBLE PRECISION      MTEMP  ( 3, 3 )
      DOUBLE PRECISION      OAX1   ( 3, 3 )
      DOUBLE PRECISION      OAX2   ( 3, 3 )
      DOUBLE PRECISION      OCTR1  ( 3 )
      DOUBLE PRECISION      OCTR2  ( 3 )
      DOUBLE PRECISION      OVIEW  ( 3 )
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      SEMAX1 ( 3, 3 )
      DOUBLE PRECISION      SEMAX2 ( 3, 3 )
      DOUBLE PRECISION      VIEWPT ( 3 )

      INTEGER               CODE
      INTEGER               XCODE

!
!     Saved variables
!


!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZOCCED' )

!
!     We're going to start out with some very basic cases involving
!     spheres.  These will exercise the bounding code logic.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Disjoint spheres.  This should be handled using ' // &
              'maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!
      CALL IDENT  ( SEMAX1 )

      CALL CLEARD ( 9, SEMAX2 )

      SEMAX2(1,1) =   3.D0
      SEMAX2(2,2) =   3.D0
      SEMAX2(3,3) =   3.D0

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -2.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   4.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.
!
      VIEWPT(1)   =  10.D0
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation to be found.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Total occultation of first object by the second. ' // &
              'This should be handled using ' // &
              'minimum and maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   0.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   5.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first sphere
!     by the second to be found.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Total occultation of second object by the first. ' // &
              'Switch semi-axis matrices.'

      CALL TCASE  ( TITLE )


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the second sphere
!     by the first to be found.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Annular transit of first body across the second. ' // &
              'This should be handled using ' // &
              'minimum and maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit of the first sphere
!     across the second to be found.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Annular transit of second body across the first. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the first sphere
!     by the second to be found.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Partial occultation of first body by the second.'

      CALL TCASE  ( TITLE )

      CENTR1(1)   =  -4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   2.D0
      CENTR2(2)   =   3.D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation of the first sphere
!     by the second to be found.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Partial occultation of second body by the first. '// &
              'Switch centers and semi-axis matrices.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation of the second sphere
!     by the first to be found.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
!     At this point, we've done all we can with spherical targets.
!     We'll make the smaller ellipsoid prolate with vertical
!     sem-axis length 1.5.
!

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  disjoint case'

      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   1.5D0


      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation to be found.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Occultation is total. ' // &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0

      CENTR1(1)   =  -5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first ellipsoid to be found.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Occultation is total. ' // &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.  Switch roles of first and '// &
              'second targets.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the second ellipsoid to be found.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of small ellipsoid is annular. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0
      SEMAX2(2,2) =   5.0D-1
      SEMAX2(3,3) =   5.0D-1

      CENTR1(1)   =  -5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the first ellipsoid to be
!     found.
!
      XCODE =  ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of small ellipsoid is annular. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL. Switch roles of first and '// &
              'second targets.'


      CALL TCASE  ( TITLE )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the second ellipsoid to be
!     found.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of large ellipsoid is total. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0
      SEMAX2(2,2) =   5.0D-1
      SEMAX2(3,3) =   5.0D-1

      CENTR1(1)   =   5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the second ellipsoid to be found.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of large ellipsoid is total. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL. Switch roles of first and '// &
              'second targets.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first ellipsoid to be found.
!
      XCODE =  TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of small ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )


      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   3.0D0


      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   5.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the large ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of small ellipsoid by large ellipsoid.  Switch ' // &
              'argument positions.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the large ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of large ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )


      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   3.0D0


      CENTR1(1)   =   5.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of large ellipsoid by small ellipsoid.  Switch ' // &
              'positions of arguments.'

      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   7.5D-1


      CENTR1(1)   =   5.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  annular transit ' // &
              'of small ellipsoid across large ellipsoid.'

      CALL TCASE  ( TITLE )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   4.5D0

      CENTR1(1)   =   1.5D0
      CENTR1(2)   =   1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -3.D0
      CENTR2(2)   =   3.0D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the small ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  annular transit ' // &
              'of small ellipsoid across large ellipsoid. Switch ' // &
              'positions of arguments.'


      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   1.5D0
      SEMAX2(3,3) =   4.5D0

      CENTR1(1)   =   1.5D0
      CENTR1(2)   =   1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -3.D0
      CENTR2(2)   =   3.0D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the small ellipsoid in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  total occultation '    // &
              'of big ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D-1
      SEMAX2(3,3) =   2.0D-1

      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   2.0D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the small ellipsoid in front.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  total occultation '    // &
              'of big ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation. '// &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the small ellipsoid in front.
!
      XCODE =  TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  annular transit by '   // &
              'big ellipsoid across small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D-1
      SEMAX2(3,3) =   1.5D-1

      CENTR1(1)   =  -2.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   4.D0
      CENTR2(2)   =   2.0D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the big ellipsoid in front.
!
      XCODE =  ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  annular transit by '   // &
              'big ellipsoid across small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the big ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation of ' // &
              'small ellipsoid by big ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D0
      SEMAX2(3,3) =   2.D0

      CENTR1(1)   =  -2.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   4.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the big ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '    // &
              'of small ellipsoid by big ellipsoid.  This case '  // &
              'requires determination of maximum angular separation. '// &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the big ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '    // &
              'of large ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D0
      SEMAX2(3,3) =   2.D0

      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '      // &
              'of large ellipsoid by small ellipsoid.   This case '   // &
              'requires determination of maximum angular separation. '// &
              'Switch positions of arguments.'

      CALL TCASE  ( TITLE )


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #1:  annular transit across ' // &
              'large ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Side view of "potato chip" ellipsoids:
!
!
!        Annular transit:
!
!                               *
!                         *   *
!                       *   *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!

!     Observe that the center of the small ellipsoid is actually further
!     from the viewing point than the center of the large ellipsoid.
!
!
!     Set up the semi-axis matrices.
!

!
!     The first ellipsoid is the small potato chip.
!
      SEMAX1(1,1) =    1.D-8
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    1.D-8

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    SQRT( 2.D0 )
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =   -1.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     The large chip is 10 times larger.
!
      SEMAX2(1,1) =    1.D-7
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    1.D-7

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1 * SQRT( 2.D0 )
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =   -1.D1
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1


!
!     Assign the centers of the targets.
!
      CENTR1(1)   =  -3.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   7.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.
!
      VIEWPT(1)   =  50.D0
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #2:  annular transit across '         // &
              'large ellipsoid by small ellipsoid. '                  // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     We expect an annular transit with the second ellipsoid in front.
!
      XCODE = ANNLR1

      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #3:  total occultation of small ' // &
              'ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )
!
!
!        Total occultation:
!
!                               *
!                             *   *
!                           *   *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!



!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -11.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    7.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the second ellipsoid in front.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #4:  total occultation of small ' // &
              'ellipsoid by large ellipsoid.'                     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the first ellipsoid in front.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #5:  partial occultation of small ' // &
              'ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )


!
!        Partial occultation, small chip is in back:
!
!                                   *
!                               * *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!

!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -13.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   11.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second ellipsoid in
!     front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #6:  partial occultation of small ' // &
              'ellipsoid by large ellipsoid.'                       // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #7:  partial occultation of large ' // &
              'ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!        Partial occultation, small chip is in front:
!
!
!                            *  *
!                          *  *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!
!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -7.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #8:  partial occultation of large ' // &
              'ellipsoid by small ellipsoid.'                       // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second ellipsoid in
!     front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #9:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is '// &
              'in front.'

      CALL TCASE  ( TITLE )

!
!        No occultation, small chip is in front:
!
!
!                            *
!                          *    *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!
!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -7.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   11.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #10:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is ' // &
              'in front.'                                       // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #11:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is '// &
              'in back.'

      CALL TCASE ( TITLE )

!
!        No occultation, small chip is in back:
!
!
!                                   *
!                                 *
!                               *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!


!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -13.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   13.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #12:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is ' // &
              'in back.'                                        // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
!     The next series of tests exercises the logical branches of
!     ZZOCCED that deal with cases where the maximum angular separation
!     between the limb of the small target and the ray from the viewing
!     point to center of the large target is sought.
!

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #1:  large ' // &
              'ellipsoid totally occults small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!

!
!     The first ellipsoid is elongated in the y-direction and flattened
!     in the z-direction.  The ellipsoid is situated near the +z
!     portion of the limb of the large ellipsoid. This prevents a
!     simple bounding cone test from proving the ellipsoid is occulted.
!
      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    1.D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D-2

!
!     The large target is a large sphere. This ensures that the linear
!     transformation performed by ZZOCCED doesn't change the shape of
!     either target.
!
      SEMAX2(1,1) =    1.D1
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    0.D0

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =    0.D0
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1

!
!     Assign the centers of the targets.
!
      CENTR1(1)   = -12.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   9.25D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.  We set the viewing point far back
!     to reduce parallax.
!
      VIEWPT(1)   =   1.D3
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the second target.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #2:  large ' // &
              'ellipsoid totally occults small ellipsoid.'  // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )



!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the first target.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #3:  small '    // &
              'ellipsoid is in annular transit across large '  // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )


!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first target in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #4:  small '    // &
              'ellipsoid is in annular transit across large '  // &
              'ellipsoid.'                                     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )



!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the second target in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #5:  small '    // &
              'ellipsoid partially occults large '  // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )
!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-3
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    4.5D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D-3

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #6:  small '    // &
              'ellipsoid partially occults large '             // &
              'ellipsoid. '                                    // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE =  PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #7:  small '    // &
              'ellipsoid is partially occulted by large '      // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #8:  small '    // &
              'ellipsoid is partially occulted by large '      // &
              'ellipsoid. '                                    // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
!     The next set of cases all have negative maximum angular
!     separation between the limb of the small target and the ray from
!     the viewing point to center of the large target:  the ray
!     passes through the plane region bounded by the limb of the
!     small target.
!


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #9:  large ' // &
              'ellipsoid totally occults small ellipsoid. ' // &
              'Maximum angular separation is negative.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!
!     The small ellipsoid is elongated in the y-direction and flattened
!     in the z-direction.  The ellipsoid is situated near the x-z plane
!     with the center displaced in the +z direction. The small
!     ellipsoid is penetrated by the -x axis. The small ellipsoid has
!     sufficient extent in the y direction such that, if it were
!     rotated 90 degrees about the x-axis, it would not be totally
!     occulted.  This prevents a simple bounding cone test from proving
!     the ellipsoid is occulted.
!
      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    9.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     The large target is a large sphere. This ensures that the linear
!     transformation performed by ZZOCCED doesn't change the shape of
!     either target.
!
      SEMAX2(1,1) =    1.D1
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    0.D0

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =    0.D0
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1

!
!     Assign the centers of the targets.
!
      CENTR1(1)   = -12.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   1.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.  We set the viewing point far back
!     to reduce parallax.
!
      VIEWPT(1)   =   1.D3
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the second target.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #10:  large ' // &
              'ellipsoid totally occults small ellipsoid. '  // &
              'Maximum angular separation is negative. '     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the first target.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #11:  small '        // &
              'ellipsoid is in annular transit across large '       // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first target in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #12:  small '        // &
              'ellipsoid is in annular transit across large '       // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the second target in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )




!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #13:  small '        // &
              'ellipsoid partially occults large '                  // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =   10.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #14:  small '        // &
              'ellipsoid partially occults large '                  // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #15:  small '        // &
              'ellipsoid is partially occulted by large '           // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =   10.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #16:  small '        // &
              'ellipsoid is partially occulted by large '           // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Classify the occultation.
!
      CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
      CALL CHCKXC ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )





! *******
! *******
! *******
! *******  At this point, we've finished with the simple cases that
! *******  exercise all of the classification logic.  These simple
! *******  cases had the semi-axis matrices aligned with the standard
! *******  basis; they also employed other simplifications such as
! *******  placing the viewing location on the x-axis.
! *******
! *******  Now we'll repeat the tests with inputs transformed as
! *******  follows:
! *******
! *******     - The semi-axis matrices of the ellipsoids will have
! *******       their columns permuted so that these matrices won't
! *******       have diagonal form.
! *******
! *******       * The semi-axes of the first ellipsoid will be permuted
! *******         by a (312) permutation.
! *******
! *******       * The semi-axes of the second ellipsoid will be permuted
! *******         by a (231) permutation.
! *******
! *******     - All vectors, including the columns of the permuted
! *******       semi-axis matrices, will be rotated by a non-trivial
! *******       3-1-3 rotation.
! *******
! *******     - The viewing location and the ellipsoid centers will
! *******       be translated by an offset vector.
! *******
! *******  All of these tranformations preserve the viewing geometry,
! *******  so each test case should produce the same occultation
! *******  classification as the corresponding case above using "simple"
! *******  inputs.
! *******


!
!     We're going to start out with some very basic cases involving
!     spheres.  These will exercise the bounding code logic.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Disjoint spheres.  This should be handled using ' // &
              'maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!
      CALL IDENT  ( SEMAX1 )

      CALL CLEARD ( 9, SEMAX2 )

      SEMAX2(1,1) =   3.D0
      SEMAX2(2,2) =   3.D0
      SEMAX2(3,3) =   3.D0

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -2.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   4.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.
!
      VIEWPT(1)   =  10.D0
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0

!
!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation to be found.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Total occultation of first object by the second. ' // &
              'This should be handled using ' // &
              'minimum and maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   0.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   5.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first sphere
!     by the second to be found.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Total occultation of second object by the first. ' // &
              'Switch targets.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the second sphere
!     by the first to be found.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Annular transit of first body across the second. ' // &
              'This should be handled using ' // &
              'minimum and maximum bounding cones.'

      CALL TCASE  ( TITLE )

!
!     Assign the centers of the spheres.
!
      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit of the first sphere
!     across the second to be found.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Annular transit of second body across the first. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )
!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the first sphere
!     by the second to be found.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Partial occultation of first body by the second.'

      CALL TCASE  ( TITLE )

      CENTR1(1)   =  -4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   2.D0
      CENTR2(2)   =   3.D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK  )

!
!     We expect a partial occultation of the first sphere
!     by the second to be found.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Partial occultation of second body by the first. '// &
              'Switch centers and semi-axis matrices.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation of the second sphere
!     by the first to be found.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
!     At this point, we've done all we can with spherical targets.
!     We'll make the smaller ellipsoid prolate with vertical
!     sem-axis length 1.5.
!

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  disjoint case'

      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   1.5D0


      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation to be found.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Occultation is total. ' // &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0

      CENTR1(1)   =  -5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first ellipsoid to be found.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Occultation is total. ' // &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.  Switch roles of first and '// &
              'second targets.'

      CALL TCASE  ( TITLE )


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the second ellipsoid to be found.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of small ellipsoid is annular. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0
      SEMAX2(2,2) =   5.0D-1
      SEMAX2(3,3) =   5.0D-1

      CENTR1(1)   =  -5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the first ellipsoid to be
!     found.
!
      XCODE =  ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of small ellipsoid is annular. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL. Switch roles of first and '// &
              'second targets.'


      CALL TCASE  ( TITLE )
!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular occultation of the second ellipsoid to be
!     found.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of large ellipsoid is total. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   4.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      SEMAX2(1,1) =   5.0D0
      SEMAX2(2,2) =   5.0D-1
      SEMAX2(3,3) =   5.0D-1

      CENTR1(1)   =   5.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -6.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Set the viewing point far away to avoid parallax problems.
!
      VIEWPT(1)   =   50.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )


!
!     We expect a total occultation of the second ellipsoid to be found.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid presents larger limb than large '    // &
              'ellipsoid; occultation of large ellipsoid is total. '// &
              'This is a minimum angular separation case requiring '// &
              'a call to ZZASRYEL. Switch roles of first and '// &
              'second targets.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation of the first ellipsoid to be found.
!
      XCODE =  TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of small ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )


      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   3.0D0


      CENTR1(1)   =   0.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   5.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the large ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of small ellipsoid by large ellipsoid.  Switch ' // &
              'argument positions.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the large ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of large ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )


      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   3.0D0


      CENTR1(1)   =   5.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )



!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation ' // &
              'of large ellipsoid by small ellipsoid.  Switch ' // &
              'positions of arguments.'

      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   7.5D-1


      CENTR1(1)   =   5.D0
      CENTR1(2)   =  -0.8D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   3.1D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  annular transit ' // &
              'of small ellipsoid across large ellipsoid.'

      CALL TCASE  ( TITLE )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) =   1.0D0
      SEMAX1(2,2) =   1.0D0
      SEMAX1(3,3) =   1.5D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) =   3.0D0
      SEMAX2(2,2) =   3.0D0
      SEMAX2(3,3) =   4.5D0

      CENTR1(1)   =   1.5D0
      CENTR1(2)   =   1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -3.D0
      CENTR2(2)   =   3.0D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the small ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is prolate:  annular transit ' // &
              'of small ellipsoid across large ellipsoid. Switch ' // &
              'positions of arguments.'


      CALL TCASE  ( TITLE )

      SEMAX1(3,3) =   1.5D0
      SEMAX2(3,3) =   4.5D0

      CENTR1(1)   =   1.5D0
      CENTR1(2)   =   1.1D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -3.D0
      CENTR2(2)   =   3.0D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the small ellipsoid in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  total occultation '    // &
              'of big ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D-1
      SEMAX2(3,3) =   2.0D-1

      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   2.0D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the small ellipsoid in front.
!
      XCODE =  TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  total occultation '    // &
              'of big ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation. '// &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the small ellipsoid in front.
!
      XCODE =  TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  annular transit by '   // &
              'big ellipsoid across small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D-1
      SEMAX2(3,3) =   1.5D-1

      CENTR1(1)   =  -2.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   4.D0
      CENTR2(2)   =   2.0D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the big ellipsoid in front.
!
      XCODE =  ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  annular transit by '   // &
              'big ellipsoid across small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the big ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation of ' // &
              'small ellipsoid by big ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D0
      SEMAX2(3,3) =   2.D0

      CENTR1(1)   =  -2.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =   4.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the big ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '    // &
              'of small ellipsoid by big ellipsoid.  This case '  // &
              'requires determination of maximum angular separation. '// &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the big ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '    // &
              'of large ellipsoid by small ellipsoid.  This case '  // &
              'requires determination of maximum angular separation.'

      CALL TCASE  ( TITLE )

      SEMAX1(1,1) =   1.D0
      SEMAX1(2,2) =   3.D0
      SEMAX1(3,3) =   7.5D-1

      SEMAX2(1,1) =   4.D0
      SEMAX2(2,2) =   1.D0
      SEMAX2(3,3) =   2.D0

      CENTR1(1)   =   4.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   0.D0

      CENTR2(1)   =  -2.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )


!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Small ellipsoid is oblate:  partial occultation '      // &
              'of large ellipsoid by small ellipsoid.   This case '   // &
              'requires determination of maximum angular separation. '// &
              'Switch positions of arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the small ellipsoid in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #1:  annular transit across ' // &
              'large ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Side view of "potato chip" ellipsoids:
!
!
!        Annular transit:
!
!                               *
!                         *   *
!                       *   *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!

!     Observe that the center of the small ellipsoid is actually further
!     from the viewing point than the center of the large ellipsoid.
!
!
!     Set up the semi-axis matrices.
!

!
!     The first ellipsoid is the small potato chip.
!
      SEMAX1(1,1) =    1.D-8
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    1.D-8

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    SQRT( 2.D0 )
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =   -1.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     The large chip is 10 times larger.
!
      SEMAX2(1,1) =    1.D-7
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    1.D-7

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1 * SQRT( 2.D0 )
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =   -1.D1
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1


!
!     Assign the centers of the targets.
!
      CENTR1(1)   =  -3.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   7.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.
!
      VIEWPT(1)   =  50.D0
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first ellipsoid in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #2:  annular transit across '         // &
              'large ellipsoid by small ellipsoid. '                  // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     We expect an annular transit with the second ellipsoid in front.
!
      XCODE = ANNLR1

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #3:  total occultation of small ' // &
              'ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )
!
!
!        Total occultation:
!
!                               *
!                             *   *
!                           *   *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!



!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -11.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    7.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the second ellipsoid in front.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #4:  total occultation of small ' // &
              'ellipsoid by large ellipsoid.'                     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation with the first ellipsoid in front.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #5:  partial occultation of small ' // &
              'ellipsoid by large ellipsoid.'

      CALL TCASE  ( TITLE )


!
!        Partial occultation, small chip is in back:
!
!                                   *
!                               * *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!

!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -13.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   11.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second ellipsoid in
!     front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Potato chip test #6:  partial occultation of small ' // &
              'ellipsoid by large ellipsoid.'                     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #7:  partial occultation of large ' // &
              'ellipsoid by small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!        Partial occultation, small chip is in front:
!
!
!                            *  *
!                          *  *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!
!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -7.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first ellipsoid in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #8:  partial occultation of large ' // &
              'ellipsoid by small ellipsoid.'                       // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second ellipsoid in
!     front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #9:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is '// &
              'in front.'

      CALL TCASE  ( TITLE )

!
!        No occultation, small chip is in front:
!
!
!                            *
!                          *    *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!
!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -7.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   11.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #10:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is ' // &
              'in front.'                                       // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #11:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is '// &
              'in back.'

      CALL TCASE ( TITLE )

!
!        No occultation, small chip is in back:
!
!
!                                   *
!                                 *
!                               *
!                             *
!                           *
!                         *
!                       *
!  * viewing point    *
!                   *
!                 *
!               *
!             *
!           *
!


!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -13.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =   13.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Potato chip test #12:  no occultation of large ' // &
              'ellipsoid by small ellipsoid.  Small target is ' // &
              'in back.'                                        // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect no occultation.
!
      XCODE = NOOCC

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
!     The next series of tests exercises the logical branches of
!     ZZOCCED that deal with cases where the maximum angular separation
!     between the limb of the small target and the ray from the viewing
!     point to center of the large target is sought.
!

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #1:  large ' // &
              'ellipsoid totally occults small ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!

!
!     The first ellipsoid is elongated in the y-direction and flattened
!     in the z-direction.  The ellipsoid is situated near the +z
!     portion of the limb of the large ellipsoid. This prevents a
!     simple bounding cone test from proving the ellipsoid is occulted.
!
      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    1.D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D-2

!
!     The large target is a large sphere. This ensures that the linear
!     transformation performed by ZZOCCED doesn't change the shape of
!     either target.
!
      SEMAX2(1,1) =    1.D1
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    0.D0

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =    0.D0
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1

!
!     Assign the centers of the targets.
!
      CENTR1(1)   = -12.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   9.25D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.  We set the viewing point far back
!     to reduce parallax.
!
      VIEWPT(1)   =   1.D3
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the second target.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #2:  large ' // &
              'ellipsoid totally occults small ellipsoid.'  // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )



!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the first target.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #3:  small '    // &
              'ellipsoid is in annular transit across large '  // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )


!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first target in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #4:  small '    // &
              'ellipsoid is in annular transit across large '  // &
              'ellipsoid.'                                     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )



!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the second target in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #5:  small '    // &
              'ellipsoid partially occults large '  // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )
!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-3
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    4.5D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D-3

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #6:  small '    // &
              'ellipsoid partially occults large '             // &
              'ellipsoid. '                                    // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #7:  small '    // &
              'ellipsoid is partially occulted by large '      // &
              'ellipsoid.'

      CALL TCASE  ( TITLE )

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #8:  small '    // &
              'ellipsoid is partially occulted by large '      // &
              'ellipsoid. '                                    // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    9.25D0


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )


!
!     The next set of cases all have negative maximum angular
!     separation between the limb of the small target and the ray from
!     the viewing point to center of the large target:  the ray
!     passes through the plane region bounded by the limb of the
!     small target.
!


!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #9:  large ' // &
              'ellipsoid totally occults small ellipsoid. ' // &
              'Maximum angular separation is negative.'

      CALL TCASE  ( TITLE )

!
!     Set up the semi-axis matrices.
!
!     The small ellipsoid is elongated in the y-direction and flattened
!     in the z-direction.  The ellipsoid is situated near the x-z plane
!     with the center displaced in the +z direction. The small
!     ellipsoid is penetrated by the -x axis. The small ellipsoid has
!     sufficient extent in the y direction such that, if it were
!     rotated 90 degrees about the x-axis, it would not be totally
!     occulted.  This prevents a simple bounding cone test from proving
!     the ellipsoid is occulted.
!
      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =    9.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     The large target is a large sphere. This ensures that the linear
!     transformation performed by ZZOCCED doesn't change the shape of
!     either target.
!
      SEMAX2(1,1) =    1.D1
      SEMAX2(2,1) =    0.D0
      SEMAX2(3,1) =    0.D0

      SEMAX2(1,2) =    0.D0
      SEMAX2(2,2) =    1.D1
      SEMAX2(3,2) =    0.D0

      SEMAX2(1,3) =    0.D0
      SEMAX2(2,3) =    0.D0
      SEMAX2(3,3) =    1.D1

!
!     Assign the centers of the targets.
!
      CENTR1(1)   = -12.D0
      CENTR1(2)   =   0.D0
      CENTR1(3)   =   1.D0

      CENTR2(1)   =   0.D0
      CENTR2(2)   =   0.D0
      CENTR2(3)   =   0.D0

!
!     Assign the viewing point.  We set the viewing point far back
!     to reduce parallax.
!
      VIEWPT(1)   =   1.D3
      VIEWPT(2)   =   0.D0
      VIEWPT(3)   =   0.D0


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the second target.
!
      XCODE = TOTAL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #10:  large ' // &
              'ellipsoid totally occults small ellipsoid. '  // &
              'Maximum angular separation is negative. '     // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )



!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a total occultation by the first target.
!
      XCODE = TOTAL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #11:  small '        // &
              'ellipsoid is in annular transit across large '       // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the first target in front.
!
      XCODE = ANNLR2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #12:  small '        // &
              'ellipsoid is in annular transit across large '       // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect an annular transit with the second target in front.
!
      XCODE = ANNLR1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )




!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #13:  small '        // &
              'ellipsoid partially occults large '                  // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =   10.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     Assign the center of the small target.
!
      CENTR1(1)   =   12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #14:  small '        // &
              'ellipsoid partially occults large '                  // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )



!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #15:  small '        // &
              'ellipsoid is partially occulted by large '           // &
              'ellipsoid; maximum angular separation is negative.'

      CALL TCASE  ( TITLE )


!
!     Assign the semi-axes of the small target.
!

      SEMAX1(1,1) =    1.D-2
      SEMAX1(2,1) =    0.D0
      SEMAX1(3,1) =    0.D0

      SEMAX1(1,2) =    0.D0
      SEMAX1(2,2) =   10.25D0
      SEMAX1(3,2) =    0.D0

      SEMAX1(1,3) =    0.D0
      SEMAX1(2,3) =    0.D0
      SEMAX1(3,3) =    1.D0

!
!     Assign the center of the small target.
!
      CENTR1(1)   =  -12.D0
      CENTR1(2)   =    0.D0
      CENTR1(3)   =    1.D0

!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR1, OAX1, OCTR2, OAX2 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the second target in front.
!
      XCODE = PARTL1

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )

!
! --- Case: ------------------------------------------------------
!

      TITLE = 'Maximum angular separation case #16:  small '        // &
              'ellipsoid is partially occulted by large '           // &
              'ellipsoid; maximum angular separation is negative. ' // &
              'Switch arguments.'

      CALL TCASE  ( TITLE )


!
!     Apply rotations and translations to the inputs.
!
      CALL XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                    OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
!
!     Classify the occultation.
!
      CODE = ZZOCCED ( OVIEW, OCTR2, OAX2, OCTR1, OAX1 )
      CALL CHCKXC  ( .FALSE.,    ' ', OK    )

!
!     We expect a partial occultation with the first target in front.
!
      XCODE = PARTL2

      CALL CHCKSI ( 'ZZOCCED', CODE, '=', XCODE, 0, OK )





! *******
! *******
! *******
! *******
! *******
! *******  Error handling cases:
! *******
! *******
! *******
! *******
! *******

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Viewing point is inside outer bounding sphere of '  // &
              'first ellipsoid, but outside the ellipsoid itself. '// &
              'Make sure NO error is signaled.'
      CALL TCASE  ( TITLE )


      CALL VPACK ( 0.D0, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 0.D0, -2.D0,  0.D0, CENTR1 )
      CALL VPACK ( 0.D0,  2.D0,  0.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) = 1.D0
      SEMAX1(2,2) = 1.D0
      SEMAX1(3,3) = 4.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) = 1.D0
      SEMAX2(2,2) = 1.D0
      SEMAX2(3,3) = 1.D0

      CALL ROTATE ( HALFPI(), 2, RMAT )


      CALL MXM   ( RMAT,  SEMAX1,  MTEMP  )
      CALL MOVED ( MTEMP, 9,       SEMAX1 )



      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )

      CALL CHCKXC  ( .FALSE., ' ', OK    )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Viewing point is inside first ellipsoid.'
      CALL TCASE  ( TITLE )


      CALL VPACK ( 1.D0,  2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0, -2.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) = 1.D0
      SEMAX1(2,2) = 1.D0
      SEMAX1(3,3) = 1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) = 1.D0
      SEMAX2(2,2) = 1.D0
      SEMAX2(3,3) = 1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )

      CALL CHCKXC  ( .TRUE., 'SPICE(NOTDISJOINT)', OK    )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Viewing point is inside second ellipsoid.'
      CALL TCASE  ( TITLE )


      CALL VPACK ( 1.D0, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0, -2.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) = 1.D0
      SEMAX1(2,2) = 1.D0
      SEMAX1(3,3) = 1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) = 1.D0
      SEMAX2(2,2) = 1.D0
      SEMAX2(3,3) = 1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )

      CALL CHCKXC  ( .TRUE., 'SPICE(NOTDISJOINT)', OK    )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Target bodies intersect'
      CALL TCASE  ( TITLE )


      CALL VPACK ( 1.D1, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0,  1.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) = 1.D0
      SEMAX1(2,2) = 1.D0
      SEMAX1(3,3) = 1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) = 1.D0
      SEMAX2(2,2) = 1.D0
      SEMAX2(3,3) = 1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )

      CALL CHCKXC  ( .TRUE., 'SPICE(NOTDISJOINT)', OK    )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Non-positive radii'
      CALL TCASE  ( TITLE )

      CALL VPACK ( 1.D1, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0,  1.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) =  0.D0
      SEMAX1(2,2) =  1.D0
      SEMAX1(3,3) =  1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) =  1.D0
      SEMAX2(2,2) =  1.D0
      SEMAX2(3,3) =  1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX1(1,1) =  1.D0
      SEMAX1(2,2) =  0.D0

      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX1(2,2) =  1.D0
      SEMAX1(3,3) =  0.D0

      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX1(3,3) =  1.D0


      SEMAX2(1,1) =  0.D0

      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX2(1,1) =  1.D0
      SEMAX2(2,2) =  0.D0

      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX2(2,2) =  1.D0
      SEMAX2(3,3) =  0.D0

      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(BADAXISLENGTH)', OK    )

      SEMAX2(3,3) =  1.D0


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'First matrix is not a rotation'
      CALL TCASE  ( TITLE )

      CALL VPACK ( 1.D1, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0, -1.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) = -1.D0
      SEMAX1(2,2) =  1.D0
      SEMAX1(3,3) =  1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) =  1.D0
      SEMAX2(2,2) =  1.D0
      SEMAX2(3,3) =  1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(NOTAROTATION)', OK    )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Second matrix is not a rotation'
      CALL TCASE  ( TITLE )

      CALL VPACK ( 1.D1, -2.D0,  3.D0, VIEWPT )
      CALL VPACK ( 1.D0,  2.1D0, 3.D0, CENTR1 )
      CALL VPACK ( 1.D0,  1.1D0, 3.D0, CENTR2 )

      CALL CLEARD ( 9, SEMAX1 )
      SEMAX1(1,1) =  1.D0
      SEMAX1(2,2) =  1.D0
      SEMAX1(3,3) =  1.D0

      CALL CLEARD ( 9, SEMAX2 )
      SEMAX2(1,1) =  1.D0
      SEMAX2(2,2) =  1.D0
      SEMAX2(3,3) = -1.D0


      CODE =  ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
      CALL CHCKXC  ( .TRUE., 'SPICE(NOTAROTATION)', OK    )


      CALL T_SUCCESS ( OK )
      RETURN
      END


!
!     Utility for "deranging" inputs to more fully exercise
!     the frame transformation code in ZZOCCED.
!
      SUBROUTINE XINPUT ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2, &
                          OVIEW,  OCTR1,  OAX1,   OCTR2,  OAX2    )
      use pepper_legacy
      IMPLICIT NONE

      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      CENTR1 ( 3 )
      DOUBLE PRECISION      SEMAX1 ( 3, 3 )
      DOUBLE PRECISION      CENTR2 ( 3 )
      DOUBLE PRECISION      SEMAX2 ( 3, 3 )
      DOUBLE PRECISION      OVIEW  ( 3 )
      DOUBLE PRECISION      OCTR1  ( 3 )
      DOUBLE PRECISION      OAX1   ( 3, 3 )
      DOUBLE PRECISION      OCTR2  ( 3 )
      DOUBLE PRECISION      OAX2   ( 3, 3 )

!
!     SPICELIB functions
!

!
!     Local variables
!
      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      M1     ( 3, 3 )
      DOUBLE PRECISION      M2     ( 3, 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      P231   ( 3, 3 )
      DOUBLE PRECISION      P312   ( 3, 3 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )

      INTEGER               AXES   ( 3 )
      INTEGER               I

      LOGICAL               PASS1

!
!     Initial values
!
!
!     Saved variables
!
      SAVE                  ANGLES
      SAVE                  AXES
      SAVE                  OFFSET
      SAVE                  P231
      SAVE                  P312
      SAVE                  PASS1
      SAVE                  R

!
!     Initial values
!
      DATA                  ANGLES / 30.D0, -50.D0,  85.D0   /
      DATA                  AXES   /     3,      1,      3   /

      DATA                  P231   /  0.D0,  1.D0,  0.D0, &
                                      0.D0,  0.D0,  1.D0, &
                                      1.D0,  0.D0,  0.D0     /

      DATA                  P312   /  0.D0,  0.D0,  1.D0, &
                                      1.D0,  0.D0,  0.D0, &
                                      0.D0,  1.D0,  0.D0     /

      DATA                  PASS1  / .TRUE.                  /

      DATA                  OFFSET /  1.D3,   -7.D4,   3.D10 /


      IF ( PASS1 ) THEN
!
!        Create the rotation matrix that we'll use to transform
!        all input vectors.
!
         CALL EUL2M ( ANGLES(3)*RPD(), &
                      ANGLES(2)*RPD(), &
                      ANGLES(1)*RPD(), &
                      AXES(3), &
                      AXES(2), &
                      AXES(1), &
                      R               )

         PASS1  =  .FALSE.

      END IF

!
!     Permute the columns of the first semi-axis matrix using
!     the matrix P312.  Apply P231 to the second matrix.
!
      CALL MXM   ( SEMAX1, P312, M1 )
      CALL MXM   ( SEMAX2, P231, M2 )

!
!     Rotate all input vectors using R.
!
      CALL MXV ( R, VIEWPT, OVIEW  )
      CALL MXV ( R, CENTR1, OCTR1  )
      CALL MXV ( R, CENTR2, OCTR2  )

      DO I = 1, 3
         CALL MXV ( R, M1(1,I), OAX1(1,I) )
         CALL MXV ( R, M2(1,I), OAX2(1,I) )
      END DO

!
!     Translate all position vectors using OFFSET.
!
      CALL VADD ( OFFSET, OVIEW, VTEMP )
      CALL VEQU ( VTEMP,         OVIEW )

      CALL VADD ( OFFSET, OCTR1, VTEMP )
      CALL VEQU ( VTEMP,         OCTR1 )

      CALL VADD ( OFFSET, OCTR2, VTEMP )
      CALL VEQU ( VTEMP,         OCTR2 )

      END



