!$Procedure  ZZINGRD  ( is a voxel inside the grid? )

       LOGICAL FUNCTION ZZINGRD ( NVOX, VOXEL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return true if voxel is inside the voxel grid. This routine
!     operates on integer voxel coordinates.
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
!     VOXEL
!
! $ Declarations

      IMPLICIT              NONE

      INTEGER               NVOX  ( 3 )
      INTEGER               VOXEL ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NVOX       I   Dimensions of voxel grid.
!     VOXEL      I   Coordinates of voxel in voxel grid units.
!
!     The function returns .TRUE. if the voxel is inside the grid.
!
! $ Detailed_Input
!
!     NVOX       Dimensions of voxel grid.
!
!     VOXEL      Coordinates of voxel in voxel grid units. The
!                coordinates are 1-based integers.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if the voxel is inside the grid.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the SPICELIB routine XDDA.
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
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB) (JAB)
!
!        Renamed routine.
!
!        Based on DSKLIB Version 1.0.0, 03-FEB-1999 (JAB)
!
!-&

! $ Index_Entries
!
!      voxel inside grid
!
!-&


      INTEGER               I


      ZZINGRD = .FALSE.

!
!     Determine if voxel is outside the voxel grid
!     in any direction.
!
      DO I = 1, 3

         IF (      ( VOXEL(I) .LT. 1       ) &
              .OR. ( VOXEL(I) .GT. NVOX(I) )  ) THEN

            RETURN

         END IF

      END DO

      ZZINGRD = .TRUE.

      RETURN
      END
