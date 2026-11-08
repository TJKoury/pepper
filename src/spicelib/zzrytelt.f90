!$Procedure ZZRYTELT ( DSK, ray touches coordinate volume element )

      SUBROUTINE ZZRYTELT ( VERTEX, RAYDIR, DSKDSC, &
                            MARGIN, NXPTS,  XPT    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to ray's vertex of ray and
!     a coordinate volume element. If the vertex is inside
!     the element, the vertex is considered to be the solution.
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
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      MARGIN
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    ( 3 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     DSKDSC     I   DSK descriptor of segment.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame of the segment to which the
!                volume element boundaries correspond. The vertex is
!                considered to be an offset from the center of the
!                reference frame associated with the segment.
!
!
!     DSKDSC     is a DSK segment descriptor. The coordinate system
!                and spatial boundaries of a the segment are specified
!                by members of this descriptor.
!
!
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the boundary of the
!                input volume element, if such an intercept exists. If
!                the ray's vertex is inside the element, XPT is set
!                equal to the vertex. XPT is valid if and only if FOUND
!                is .TRUE.
!
!                XPT is expressed in the reference frame associated
!                with the inputs VERTEX and RAYDIR. XPT represents
!                an offset from the origin of the coordinate system.
!
!                XPT is valid only if NXPTS is set to 1.
!
!
!     NXPTS      is the number of intercept points of the ray and
!                the volume element.
!
!                Currently there are only two possible values for
!                NXPTS:
!
!                   1 for an intersection
!                   0 for no intersection
!
!                If the vertex is inside the element, NXPTS is
!                set to 1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the coordinate system code in the segment descriptor
!        is not recognized, the error SPICE(NOTSUPPORTED) will be
!        signaled.
!
! $ Files
!
!     None. However, the input segment boundaries normally have
!     been obtained from a loaded DSK file.
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
!-    SPICELIB Version 1.0.0, 19-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on dsk volume element
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               CORSYS


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZRYTELT' )


      CORSYS = NINT( DSKDSC(SYSIDX) )

      IF ( CORSYS .EQ. LATSYS ) THEN

         CALL ZZRYTLAT ( VERTEX, RAYDIR, DSKDSC(MN1IDX), &
                         MARGIN, NXPTS,  XPT              )


      ELSE IF ( CORSYS .EQ. RECSYS ) THEN

         CALL ZZRYTREC ( VERTEX, RAYDIR, DSKDSC(MN1IDX), &
                         MARGIN, NXPTS,  XPT              )


      ELSE IF ( CORSYS .EQ. PDTSYS ) THEN

         CALL ZZRYTPDT ( VERTEX,         RAYDIR, &
                         DSKDSC(MN1IDX), DSKDSC(PARIDX), &
                         MARGIN,         NXPTS,          XPT )
      ELSE

         CALL SETMSG( 'Coordinate system # is not supported.' )
         CALL ERRINT( '#', CORSYS                             )
         CALL SIGERR( 'SPICE(BADCOORDSYS)'                    )
         CALL CHKOUT( 'ZZRYTELT'                              )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZRYTELT' )
      RETURN
      END
