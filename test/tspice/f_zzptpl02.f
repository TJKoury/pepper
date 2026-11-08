C$Procedure F_ZZPTPL02 ( ZZPTPL02 tests )
 
      SUBROUTINE F_ZZPTPL02 ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZPTPL02.
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
            
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'
      
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
C     This routine tests the SPICELIB routine ZZPTPL02.
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
C-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters 
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzptpl02_0.bds' )

      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzptpl02.tpc' )

 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
C
C     Local Variables 
C  
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


C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
 


C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZPTPL02' )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C     The following simple cases are meant to assist debugging.
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create tessellated ellipsoid DSK.'  )
  

C
C     Make sure no kernels are loaded before we start.
C
      CALL KCLEAR

C
C     Create, load, and discard generic test PCK.
C     
      CALL TSTPCK ( PCK0, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create DSK.
C
C     TARGET = 'MARS'
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

C
C     Append a segment for Phobos. Make this segment large enough
C     so that the buffering capability in ZZPTPL02 will be 
C     exercised.
C
C     TARGET = 'Phobos'
      FRNAME = 'IAU_PHOBOS'
      BODYID = 401
      SURFID = 1

      NLON   = 200
      NLAT   = 100

      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars segment: map plate centroids to plates.' )
  
C
C     This should be easy....
C
C     Get DLA descriptor.
C
      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Get the DSK descriptor, which will be needed a bit later.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get vertex and plate counts.
C
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compute the plate's centroid.
C
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), 
     .                 C, XVERTS(1,3), P              )

C
C        Find the plate associated with P. We're expecting plate I.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Make sure the centroid was found on the correct plate.
C
         LABEL = 'centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C        Check the returned plate.
C
         LABEL = 'plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C        Check the returned vertices. We expect an exact match.
C
         LABEL = 'vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )
         
      END DO


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars segment: map off-plate points near plate '
     .//           'centroids to plates.'                         )
  
 
      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compute the plate's centroid.
C
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), 
     .                 C, XVERTS(1,3), P              )

C
C        Get the normal vector of plate I; add a small upward
C        displacement

C        
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCALE = 1.D-10

         CALL VLCOM ( 1.D0, P, SCALE, NORMAL, P2 )         

C
C        Find the plate associated with P2. We're expecting plate I.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Make sure the offst centroid was found on the correct plate.
C
         LABEL = '(up) offset centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C        Check the returned plate.
C
         LABEL = '(up) plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C        Check the returned vertices. We expect an exact match.
C
         LABEL = '(up) vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )
         


C
C        Add a small downward displacement to the centroid.
C        
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM ( 1.D0, P, -SCALE, NORMAL, P2 )         

C
C        Find the plate associated with P2. We're expecting plate I.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Make sure the offset centroid was found on the correct plate.
C
         LABEL = '(down) offset centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C        Check the returned plate.
C
         LABEL = '(down) plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C        Check the returned vertices. We expect an exact match.
C
         LABEL = '(down) vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )
         

      END DO


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars segment: attempt to map off-plate points '
     .//           'near centroids to plates, using offsets that '
     .//           'are too large. No matches should be found.'    )
  
 
C
C     Get default membership margin.
C
      CALL DSKGTL ( KEYPTM, DEFMRG )

C
C     Get the segment's bounding radius.
C
      CALL ZZLATBOX ( DSKDSC(MN1IDX), BOXCTR, LR, 
     .                LT,             LZ,     MAXR )

C
C     Derive the applicable margin.
C
      MARGIN = DEFMRG * MAXR


      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compute the plate's centroid.
C
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), 
     .                 C, XVERTS(1,3), P              )

C
C        Get the normal vector of plate I; add a small upward
C        displacement

C        
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCALE = 2 * MARGIN

         CALL VLCOM ( 1.D0, P, SCALE, NORMAL, P2 )         

C
C        Try to find the plate associated with P2. We expect no match.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

         
C
C        Add a small downward displacement to the centroid.
C        
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM ( 1.D0, P, -SCALE, NORMAL, P2 )         

C
C        Try to find the plate associated with P2. We expect no match.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P2,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )         

      END DO



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars: map plate interior points near vertices to '
     .//           'plates.'                                         )
  
C
C     We're still working with the same segment as last time.
C 
      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Shrink the plate very slightly. The vertices of the new plate
C        will be inside the boundary of the original.
C
C        We can't put the vertices too near the edges of the original
C        plates, or the expansion of other plates may cause those
C        plates to "capture" our vertices.
C
C        Note the sign of DELTA.
C
         DELTA = -1.D-9

         CALL PLTEXP ( XVERTS, DELTA, XV2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the plates associated with the vertices of the scaled
C        plate.
C
         DO J = 1, 3

            CALL VEQU ( XV2(1,J), P )
C
C           Find the plate associated with P. We're expecting plate I.
C
            CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P,
     .                      PLID,   PLATE,  VERTS,  FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C           Make sure the vertex was found on the correct plate.
C
            LABEL = 'vertex # PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C           Check the returned plate.
C
            LABEL = 'vertex # plate PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C           Check the returned vertices. We expect an exact match.
C
            LABEL = 'vertex # vertices PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

         END DO
         
      END DO


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Mars: map plate interior points near vertices to '
     .//           'plates. Turn off default plate expansion and use '
     .//           'a very small shrinkage factor.'                   )
  
C
C     Shut down plate expansion.
C
      CALL DSKSTL ( KEYXFR, 0.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We're still working with the same segment as last time.
C 
      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Shrink the plate very slightly. The vertices of the new plate
C        will be inside the boundary of the original.
C
C        Use a very small scale factor. This factor won't work with
C        default plate expansion turned on.
C
C        Note the sign of DELTA.
C
         DELTA = -1.D-12

         CALL PLTEXP ( XVERTS, DELTA, XV2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the plates associated with the vertices of the scaled
C        plate.
C
         DO J = 1, 3

            CALL VEQU ( XV2(1,J), P )
C
C           Find the plate associated with P. We're expecting plate I.
C
            CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P,
     .                      PLID,   PLATE,  VERTS,  FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C           Make sure the vertex was found on the correct plate.
C
            LABEL = 'vertex # PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C           Check the returned plate.
C
            LABEL = 'vertex # plate PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C           Check the returned vertices. We expect an exact match.
C
            LABEL = 'vertex # vertices PLID #.'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )

         END DO
         
      END DO

C
C     Restore default plate expansion.
C
      CALL DSKSTL ( KEYXFR, XFRACT )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Phobos segment: map plate centroids to plates.' )
  
C
C     Make sure that a segment change is not a problem.
C
C     Get DLA descriptor.
C
      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

C
C     Get the DSK descriptor, which will be needed a bit later.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get vertex and plate counts.
C
      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NP
C
C        Get Ith plate and its vertices.
C
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, XPLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         DO J = 1, 3

            CALL DSKV02 ( HANDLE, DLADSC, XPLATE(J), 1, N, XVERTS(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compute the plate's centroid.
C
         C = 1.D0/3

         CALL VLCOM3 ( C, XVERTS(1,1), C, XVERTS(1,2), 
     .                 C, XVERTS(1,3), P              )

C
C        Find the plate associated with P. We're expecting plate I.
C
         CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P,
     .                   PLID,   PLATE,  VERTS,  FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Make sure the centroid was found on the correct plate.
C
         LABEL = 'centroid PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, PLID, '=', I, 0, OK )

C
C        Check the returned plate.
C
         LABEL = 'plate PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, PLATE, '=', XPLATE, 3, OK )

C
C        Check the returned vertices. We expect an exact match.
C
         LABEL = 'vertices PLID #.'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, VERTS, '=', XVERTS, 3, 0.D0, OK )
         
      END DO





C**********************************************************************
C
C     Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Error case setup: get valid descriptors.' )
C
C     Get DLA descriptor.
C
      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Get the DSK descriptor, which will be needed a bit later.
C
      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: exceptions are only for bad segment parameters,
C     bad DSK descriptor and file read errors.
C       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Invalid file handle'  )

      CALL ZZPTPL02 ( HANDLE+1, DLADSC, DSKDSC, P,
     .                PLID,     PLATE,  VERTS,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDHANDLE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad coordinate system.'  )

      DSKDSC(SYSIDX) = -1

      CALL ZZPTPL02 ( HANDLE, DLADSC, DSKDSC, P,
     .                PLID,   PLATE,  VERTS,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Restore coordinate system.
C
      DSKDSC(SYSIDX) = LATSYS


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.'  )
  
C
C     Close DSK file; open was performed using DASOPR.
C
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


