!$Procedure ZZSEGBOX (Bounding box for DSK segment volume element)

      SUBROUTINE ZZSEGBOX ( DSKDSC, BOXCTR, MAXR )

! $ Abstract
!
!     Create a bounding box for a DSK segment volume element.
!     The outputs are the box's center and radius. The center
!     is relative to the segment's frame center.
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
!     GEOMETRY
!     MATH
!
! $ Declarations

      INCLUDE 'dskdsc.inc'

      DOUBLE PRECISION      DSKDSC ( * )
      DOUBLE PRECISION      BOXCTR ( 3 )
      DOUBLE PRECISION      MAXR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DSKDSC     I   DSK segment descriptor.
!     CENTER     O   Center of bounding box.
!     RADIUS     O   Radius of box.
!
! $ Detailed_Input
!
!     DSKDSC     is the DSK descriptor for the segment of interest.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of a box tangent to and containing the volume
!                specified by the coordinate system, coordinate
!                parameters, if any, and bounds specified in the DSK
!                segment descriptor.
!
!                The box bounds the surface specified by the input
!                descriptor; the offset between the center of the
!                segment's reference frame and the segment's central
!                body plays no role.
!
!     RADIUS     is the radius of the sphere that circumscribes the
!                box. RADIUS is equal to the length of a line segment
!                connecting the center of the box to any corner.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If coordinate system specified in the descriptor is not
!         recognized, the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  Any errors that occur while deriving the bounding box
!         will be signaled by routines in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine consolidates the logic branch needed to compute a
!     bounding box for a DSK segment. Using this routine can eliminate
!     the need to test higher-level routines using each type of
!     coordinate system.
!
!     The box bounds the surface specified by the input descriptor; the
!     offset between the center of the segment's reference frame and
!     the segment's central body plays no role.
!
!     To compute a body's DSK bounding sphere, centered at the body,
!     and taking into account all loaded segments, use ZZDSKSPH.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 17-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box for dsk segment volume element
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      L1
      DOUBLE PRECISION      L2
      DOUBLE PRECISION      L3

      INTEGER               CORSYS


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSEGBOX' )


      CORSYS = NINT( DSKDSC(SYSIDX) )

      IF ( CORSYS .EQ. LATSYS ) THEN

         CALL ZZLATBOX ( DSKDSC(MN1IDX), BOXCTR, L1, &
                         L2,             L3,     MAXR )

      ELSE IF ( CORSYS .EQ. RECSYS ) THEN

         CALL ZZRECBOX ( DSKDSC(MN1IDX), BOXCTR, L1, &
                         L2,             L3,     MAXR )

      ELSE IF ( CORSYS .EQ. PDTSYS ) THEN

         CALL ZZPDTBOX ( DSKDSC(MN1IDX), DSKDSC(PARIDX), BOXCTR, &
                         L1,             L2,             L3, &
                         MAXR                                   )

      ELSE

         CALL SETMSG ( 'Coordinate system # is not supported.' )
         CALL ERRINT ( '#',  CORSYS                            )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                   )
         CALL CHKOUT ( 'ZZSEGBOX'                              )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZSEGBOX' )
      RETURN
      END
