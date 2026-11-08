!$Procedure ZZDSKSGX ( DSK, ray-segment intercept )

      SUBROUTINE ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, VERTEX, &
                            RAYDIR, XPT,    DC,    IC, FOUND   )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find the intersection of a ray and the surface described by
!     a single DSK segment.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               DLADSC ( * )
      INTEGER               DTYPE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      DC     ( * )
      INTEGER               IC     ( * )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DSK file handle.
!     DLADSC     I   DLA descriptor of segment.
!     DTYPE      I   Data type code.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     XPT        O   Surface intercept, if found.
!     DC         O   D.p. component of source info.
!     IC         O   Integer component of source info.
!     FOUND      O   Found flag.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of a DSK file containing a segment
!                to be used in a ray-surface intercept computation.
!
!     DLASDC     is the DLA descriptor of the DSK segment to be used.
!
!     DTYPE      is the data type code of the segment. While this
!                information can be retrieved from the DSK descriptor
!                of the segment, the availability of this argument
!                saves the time needed to do so.
!
!     ET         is the epoch of the intersection computation,
!                expressed as seconds past J2000 TDB. This epoch is
!                used for DSK segment selection.
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray's direction vector must be
!                represented in the reference frame of the segment. The
!                vertex is considered to be an offset from the center
!                of the reference frame associated with the segment.
!
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the surface described
!                by the segment, if such an intercept exists. If the
!                ray intersects the surface at multiple points, the
!                one closest to the ray's vertex is selected. XPT is
!                valid if and only if FOUND is .TRUE.
!
!                XPT is expressed in the reference frame associated
!                with the specified segment. It represents an offset
!                from the center of this frame. Note that the frame
!                center may differ from the central body of the
!                segment.
!
!
!     DC         is the double precision component of the data
!                source information. Contents are data type-
!                dependent. DC is valid if and only if FOUND
!                is .TRUE.
!
!
!     IC         is the integer component of the data
!                source information. Contents are data type-
!                dependent. IC is valid if and only if FOUND
!                is .TRUE.
!
!                For type 2 segments, IC contains just the
!                intercept plate ID in element 1.
!
!
!     FOUND      is a logical flag that is set to .TRUE. if and only
!                if a ray-surface intercept was found.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the DSK segment data type is not recognized, the error
!         SPICE(TYPENOTSUPPORTED) is signaled.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - DSK data: the DSK file designated by HANDLE and containing
!          the segment having the DLA descriptor DLADSC must be loaded
!          at the time this routine is called.
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine sits on top of data DSK type-specific ray-segment
!     intercept routines such as DSKX02.
!
! $ Examples
!
!     See usage in ZZDSKBUX.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-FEB-2016 (NJB)
!
!        Based on first version 20-JAN-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray with surface defined by dsk segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      TOUCHD

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      RETVAL

      INTEGER               PLID


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKSGX' )

!
!     Note: input argument ET is provided to support time-dependent
!     data types.
!
      RETVAL = TOUCHD( ET )
      DC(1)  = TOUCHD( DC )

      IF ( DTYPE .EQ. 2 ) THEN
!
!        The intercept plate ID is returned in element 1 of
!        IC, if an intercept is found.
!
         CALL DSKX02 ( HANDLE, DLADSC, VERTEX, &
                       RAYDIR, PLID,   XPT,    FOUND )

         IF ( FOUND ) THEN
            IC(1) = PLID
         END IF

      ELSE

         CALL SETMSG ( 'DSK ray-surface intercepts are not ' &
         //            'supported for DSK data type #.'     )
         CALL ERRINT ( '#', DTYPE                           )
         CALL SIGERR ( 'SPICE(TYPENOTSUPPORTED)'            )
         CALL CHKOUT ( 'ZZDSKSGX'                           )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZDSKSGX' )
      RETURN
      END

