!$Procedure ZZDSKBBL ( DSK, build BSR segment list )

      SUBROUTINE ZZDSKBBL ( BODYID )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Force the ZZDSKBSR subsystem to build a complete, current segment
!     list for a specified body.
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
!     BODY
!     SEGMENT
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzctr.inc'
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsk02.inc'

      INTEGER               BODYID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID code.
!
! $ Detailed_Input
!
!     BODYID     is the ID code of the body for which a complete
!                segment list is to be built by the ZZDSKBSR subsystem.
!
! $ Detailed_Output
!
!     None. See Particulars for a description of the side effects of
!     this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while building the segment list for
!         the specified body, the error will be diagnosed by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     Loaded DSK files are searched by ZZDSKBSR entry points as a
!     result of a call to this routine.
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine forces the ZZDSKBSR subsystem to form a complete
!     segment list for a specified body. This will allow a ZZDSKBSR
!     segment search to obtain information about all loaded segments
!     for that body.
!
!     This routine enables routines that use the DSK API segment
!     buffers to accumulate complete segment data from the ZZDSKBSR
!     subsystem.
!
!     This routine may be rather slow, since it can cause a great
!     number of physical file reads to occur.  It should be called only
!     when the set of loaded DSKs is known to have changed.
!
! $ Examples
!
!     See usage in ZZDSKSBF entry points ZZSBFXR and ZZSBFNRM.
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
!-    SPICELIB Version 1.0.0, 08-JUL-2016 (NJB)
!
!        Updated to use ZZDSKNOT. Bug fix: change of body
!        now causes an update.
!
!        05-FEB-2016 (NJB)
!
!           Based on Version 1.0.0 25-SEP-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     force zzdskbsr to build segment list for body
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     EXTERNAL routines
!
      EXTERNAL ZZDSKNOT

!
!     Local parameters
!

!
!     Local variables
!
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )

      INTEGER               CTR    ( CTRSIZ )
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               PRVBOD

      LOGICAL               FIRST
      LOGICAL               NEWBOD
      LOGICAL               SEGFND
      LOGICAL               UPDATE

!
!     Saved variables
!
      SAVE                  CTR
      SAVE                  FIRST
      SAVE                  PRVBOD

!
!     Initial values
!
      DATA                  CTR    / CTRSIZ * 0 /
      DATA                  FIRST  / .TRUE.     /
      DATA                  PRVBOD / 0 /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSKBBL' )


      IF ( FIRST ) THEN

         CALL ZZCTRUIN ( CTR )

         FIRST  = .FALSE.
         NEWBOD = .TRUE.

      ELSE
!
!        PRVBOD is the last body for which a successful list
!        build was performed.
!
         NEWBOD = BODYID .NE. PRVBOD

      END IF

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      CALL ZZDSKCHK ( CTR, UPDATE )

      IF ( UPDATE .OR. NEWBOD ) THEN
!
!        Force the BSR subsystem to build a complete segment list
!        for the current body. We'll do this by using a matching
!        function that indicates no segment matches: this forces
!        ZZDSKSNS to search all files for segments and build a
!        complete segment search.
!
!        Start a segment search.
!
         CALL ZZDSKBSS ( BODYID )

         CALL ZZDSKSNS ( ZZDSKNOT, HANDLE, DLADSC, DSKDSC, SEGFND )


         IF ( .NOT. FAILED() ) THEN

            PRVBOD = BODYID

         END IF

      END IF


      CALL CHKOUT ( 'ZZDSKBBL' )
      RETURN
      END
