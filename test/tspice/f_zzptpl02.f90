!$Procedure F_ZZPTPL02 ( ZZPTPL02 tests )

      SUBROUTINE F_ZZPTPL02 ( OK )

! $ Abstract
!
!     Exercise the routine ZZPTPL02.
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

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
!     This routine tests the SPICELIB routine ZZPTPL02.
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
!-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzptpl02_0.bds' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzptpl02.tpc' )


      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    FRNAME
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      BOXCTR ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DEFMRG
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      P2     ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      VERTS  ( 3, 3 )
      DOUBLE PRECISION      XV2    ( 3, 3 )
      DOUBLE PRECISION      XVERTS ( 3, 3 )

      INTEGER               BODYID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               PLATE  ( 3 )
      INTEGER               PLID
      INTEGER               SURFID
      INTEGER               XPLATE ( 3 )

      LOGICAL               FOUND


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
      CALL TOPEN ( 'F_ZZPTPL02' )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
!     The following simple cases are meant to assist debugging.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create tessellated ellipsoid DSK.'  )


!
!     Make sure no kernels are loaded before we start.
!
      CALL KCLEAR

!
!     Create, load, and discard generic test PCK.
!
      CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create DSK.
!
!     TARGET = 'MARS'
      FRNAME = 'IAU_MARS'
      BODYID = 499
      SURFID = 1

      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL ( DSK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Append a segment for Phobos. Make this segment large enough
!     so that the buffering capability in ZZPTPL02 will be
!     exercised.
!
!     TARGET = 'Phobos'
      FRNAME = 'IAU_PHOBOS'
      BODYID = 401
      SURFID = 1

      NLON   = 200
      NLAT   = 100

      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars segment: map plate centroids to plates.' )

!
!     This should be easy....
!
!     Get DLA descriptor.
!
      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Get the DSK descriptor, which will be needed a bit later.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get vertex and plate counts.
!
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the plate's centroid.
!
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), &
                       C, XVERTS(1,3), P              )

!
!        Find the plate associated with P. We're expecting plate I.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Make sure the centroid was found on the correct plate.
!
         LABEL = 'centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!        Check the returned plate.
!
         LABEL = 'plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!        Check the returned vertices. We expect an exact match.
!
         LABEL = 'vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars segment: map off-plate points near plate ' &
      //           'centroids to plates.'                         )


      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the plate's centroid.
!
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), &
                       C, XVERTS(1,3), P              )

!
!        Get the normal vector of plate I; add a small upward
!        displacement

!
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCALE = 1.D-10

         CALL VLCOM ( 1.D0, P, SCALE, NORMAL, P2 )

!
!        Find the plate associated with P2. We're expecting plate I.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Make sure the offst centroid was found on the correct plate.
!
         LABEL = '(up) offset centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!        Check the returned plate.
!
         LABEL = '(up) plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!        Check the returned vertices. We expect an exact match.
!
         LABEL = '(up) vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )



!
!        Add a small downward displacement to the centroid.
!
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM ( 1.D0, P, -SCALE, NORMAL, P2 )

!
!        Find the plate associated with P2. We're expecting plate I.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Make sure the offset centroid was found on the correct plate.
!
         LABEL = '(down) offset centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!        Check the returned plate.
!
         LABEL = '(down) plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!        Check the returned vertices. We expect an exact match.
!
         LABEL = '(down) vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )


      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars segment: attempt to map off-plate points ' &
      //           'near centroids to plates, using offsets that ' &
      //           'are too large. No matches should be found.'    )


!
!     Get default membership margin.
!
      CALL DSKGTL ( KEYPTM, DEFMRG )

!
!     Get the segment's bounding radius.
!
      CALL ZZLATBOX ( DSKDSC(MN1IDX), BOXCTR, LR, &
                      LT,             LZ,     MAXR )

!
!     Derive the applicable margin.
!
      MARGIN = DEFMRG * MAXR


      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the plate's centroid.
!
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), &
                       C, XVERTS(1,3), P              )

!
!        Get the normal vector of plate I; add a small upward
!        displacement

!
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCALE = 2 * MARGIN

         CALL VLCOM ( 1.D0, P, SCALE, NORMAL, P2 )

!
!        Try to find the plate associated with P2. We expect no match.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
!        Add a small downward displacement to the centroid.
!
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM ( 1.D0, P, -SCALE, NORMAL, P2 )

!
!        Try to find the plate associated with P2. We expect no match.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars: map plate interior points near vertices to ' &
      //           'plates.'                                         )

!
!     We're still working with the same segment as last time.
!
      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Shrink the plate very slightly. The vertices of the new plate
!        will be inside the boundary of the original.
!
!        We can't put the vertices too near the edges of the original
!        plates, or the expansion of other plates may cause those
!        plates to "capture" our vertices.
!
!        Note the sign of DELTA.
!
         DELTA = -1.D-9

         CALL PLTEXP ( XVERTS, DELTA, XV2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the plates associated with the vertices of the scaled
!        plate.
!
         DO J = 1, 3

            CALL VEQU ( XV2(1,J), P )
!
!           Find the plate associated with P. We're expecting plate I.
!
            CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P, &
                            PLID,   PLATE,  VERTS,  FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!           Make sure the vertex was found on the correct plate.
!
            LABEL = 'vertex # PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!           Check the returned plate.
!
            LABEL = 'vertex # plate PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!           Check the returned vertices. We expect an exact match.
!
            LABEL = 'vertex # vertices PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars: map plate interior points near vertices to ' &
      //           'plates. Turn off default plate expansion and use ' &
      //           'a very small shrinkage factor.'                   )

!
!     Shut down plate expansion.
!
      CALL DSKSTL ( KEYXFR, 0.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We're still working with the same segment as last time.
!
      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Shrink the plate very slightly. The vertices of the new plate
!        will be inside the boundary of the original.
!
!        Use a very small scale factor. This factor won't work with
!        default plate expansion turned on.
!
!        Note the sign of DELTA.
!
         DELTA = -1.D-12

         CALL PLTEXP ( XVERTS, DELTA, XV2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the plates associated with the vertices of the scaled
!        plate.
!
         DO J = 1, 3

            CALL VEQU ( XV2(1,J), P )
!
!           Find the plate associated with P. We're expecting plate I.
!
            CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P, &
                            PLID,   PLATE,  VERTS,  FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!           Make sure the vertex was found on the correct plate.
!
            LABEL = 'vertex # PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!           Check the returned plate.
!
            LABEL = 'vertex # plate PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!           Check the returned vertices. We expect an exact match.
!
            LABEL = 'vertex # vertices PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

         END DO

      END DO

!
!     Restore default plate expansion.
!
      CALL DSKSTL ( KEYXFR, XFRACT )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Phobos segment: map plate centroids to plates.' )

!
!     Make sure that a segment change is not a problem.
!
!     Get DLA descriptor.
!
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

!
!     Get the DSK descriptor, which will be needed a bit later.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get vertex and plate counts.
!
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NP
!
!        Get Ith plate and its vertices.
!
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

!
!        Compute the plate's centroid.
!
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), &
                       C, XVERTS(1,3), P              )

!
!        Find the plate associated with P. We're expecting plate I.
!
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P, &
                         PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Make sure the centroid was found on the correct plate.
!
         LABEL = 'centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

!
!        Check the returned plate.
!
         LABEL = 'plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

!
!        Check the returned vertices. We expect an exact match.
!
         LABEL = 'vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

      END DO





!**********************************************************************
!
!     Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case setup: get valid descriptors.' )
!
!     Get DLA descriptor.
!
      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Get the DSK descriptor, which will be needed a bit later.
!
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: exceptions are only for bad segment parameters,
!     bad DSK descriptor and file read errors.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid file handle'  )

      CALL ZZPTPL02 ( HANDLE+1, DLADSC, DSKDSC, P, &
                      PLID,     PLATE,  VERTS,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system.'  )

      DSKDSC(SYSIDX) = -1

      CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P, &
                      PLID,   PLATE,  VERTS,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Restore coordinate system.
!
      DSKDSC(SYSIDX) = LATSYS


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.'  )

!
!     Close DSK file; open was performed using DASOPR.
!
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


