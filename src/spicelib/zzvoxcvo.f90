!$Procedure  ZZVOXCVO ( Voxel to coarse voxel offset )

      SUBROUTINE ZZVOXCVO ( VIXYZ, NVOX, CGRSCL, CGXYZ, CGOFF, CGOF1D )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Map voxel coordinates to coarse voxel coordinates and offset
!     relative to coarse voxel. Offset coordinates are 1-based.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     PRIVATE
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               VIXYZ  ( 3 )
      INTEGER               NVOX   ( 3 )
      INTEGER               CGRSCL
      INTEGER               CGXYZ  ( 3 )
      INTEGER               CGOFF  ( 3 )
      INTEGER               CGOF1D

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VIXYX      I   Fine voxel integer coordinates.
!     NVOX       I   Fine voxel grid dimensions.
!     CGRSCL     I   Coarse voxel scale.
!     CGXYZ      O   Coarse voxel coordinates.
!     CGOFF      O   3-D voxel offset relative to coarse voxel.
!     CGOF1D     O   1-D voxel offset relative to coarse voxel.
!
! $ Detailed_Input
!
!     VIXYZ      is an array containing integer Cartesian coordinates
!                of a voxel in the fine voxel grid. Coordinates are
!                1-based.
!
!     NVOX       is an array containing the integer extents of the fine
!                voxel grid. Units are voxels.
!
!     CGRSCL     is the coarse voxel scale. This is the integer ratio
!                of the coarse voxel edge length to the fine voxel
!                edge length.
!
! $ Detailed_Output
!
!     CGXYZ      is an array containing integer Cartesian coordinates
!                of the coarse voxel that contains the fine voxel
!                indexed by VIXYZ. Coordinates are 1-based.
!
!     CGOFF      is an array containing the integer Cartesian
!                coordinates of the fine voxel relative to the coarse
!                voxel that contains it. Coordinates are 1-based.
!
!     CGOF1D     is the 1-dimensional offset of the input fine voxel
!                relative to the coarse voxel that contains it. The
!                offset is 1-based.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If any input fine voxel grid dimension is non-positive,
!        the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2) If the coarse voxel scale is non-positive, the error
!        SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine encapsulates a frequently used type 2 DSK address
!     calculation. The output offset CGOF1D is useful for locating
!     a fine voxel's pointer into the voxel-plate list.
!
! $ Examples
!
!     See usage in the routine ZZMKSPIN.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB)
!
!        $Exceptions section was updated.
!
!     Based on DSKLIB Version 1.0.0, 20-MAR-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     map voxel to coarse voxel offset
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               I


      IF ( RETURN() ) THEN
         RETURN
      END IF

      DO I = 1, 3

         IF ( NVOX(I) .LT. 1 ) THEN

            CALL CHKIN  ( 'ZZVOXCVO' )

            CALL SETMSG ( 'Voxel grid dimensions must be positive ' &
            //            'but were # # #.'                        )
            CALL ERRINT ( '#', NVOX(1)                             )
            CALL ERRINT ( '#', NVOX(2)                             )
            CALL ERRINT ( '#', NVOX(3)                             )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )

            CALL CHKOUT ( 'ZZVOXCVO' )
            RETURN

         END IF

      END DO


      DO I = 1, 3

         IF ( ( VIXYZ(I) .LT. 1 ) .OR. ( VIXYZ(I) .GT. NVOX(I) ) ) THEN

            CALL CHKIN  ( 'ZZVOXCVO' )

            CALL SETMSG ( 'Voxel grid coordinates must be inside ' &
            //            'grid having dimensions # x # x # ' &
            //            'but were # # #.'                        )
            CALL ERRINT ( '#', NVOX(1)                             )
            CALL ERRINT ( '#', NVOX(2)                             )
            CALL ERRINT ( '#', NVOX(3)                             )
            CALL ERRINT ( '#', VIXYZ(1)                            )
            CALL ERRINT ( '#', VIXYZ(2)                            )
            CALL ERRINT ( '#', VIXYZ(3)                            )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )

            CALL CHKOUT ( 'ZZVOXCVO' )
            RETURN

         END IF

      END DO


      IF ( CGRSCL .LT. 1 ) THEN

         CALL CHKIN  ( 'ZZVOXCVO' )

         CALL SETMSG ( 'Coarse voxel grid scale must be positive ' &
         //            'but was #.'                               )
         CALL ERRINT ( '#', NVOX(1)                               )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                   )

         CALL CHKOUT ( 'ZZVOXCVO' )
         RETURN

      END IF

      DO I = 1, 3
!
!        Set the Ith coarse grid coordinate. Recall these coordinates
!        are 1-based.
!
         CGXYZ(I) = ( ( VIXYZ(I) - 1 ) /  CGRSCL  )  +  1

!
!        Set the Ith coarse grid coordinate offset. These offsets
!        are 1-based as well.
!
         CGOFF(I) =  VIXYZ(I)  -  CGRSCL*( CGXYZ(I) - 1 )

      END DO

!
!     Convert the coarse grid-relative offset to a relative
!     ID. The ID is a one-dimensional offset.
!
      CGOF1D =    (CGOFF(3)-1) * CGRSCL * CGRSCL &
                + (CGOFF(2)-1) * CGRSCL &
                +  CGOFF(1)

      END
