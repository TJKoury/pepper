!$Procedure DLASSG ( DLA, same segment? )

      LOGICAL FUNCTION DLASSG ( HAN1, HAN2, DSC1, DSC2 )

! $ Abstract
!
!     Return a logical value indicating whether a two DLA
!     segments, each identified by DAS handle and DLA descriptor,
!     are in fact the same segment.
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
!     DAS
!     DLA
!
! $ Keywords
!
!     DAS
!     DLA
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'

      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               DSC1 ( DLADSZ )
      INTEGER               DSC2 ( DLADSZ )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HAN1       I   Handle of an open DLA file.
!     HAN2       I   Handle of a second open DLA file.
!     DSC1       I   DLA descriptor of a segment in the first file.
!     DSC2       I   DLA descriptor of a segment in the second file.
!
!     The function returns .TRUE. if and only if the DLA segments
!     match.
!
! $ Detailed_Input
!
!     HAN1     is the integer handle associated with a DLA file.
!              The file is open for read access.
!
!     HAN2     is the integer handle associated with a second DLA
!              file. The file is open for read access.
!
!     DSC1     is the DLA descriptor of a segment in the file
!              associated with HAN1.
!
!     DSC2     is the DLA descriptor of a segment in the file
!              associated with HAN2.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the DLA segments
!     match. The segments are considered to match if and only if the
!     input handles match and all elements of the DLA descriptors
!     match.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the inputs are invalid, this routine will
!         fail in an unspecified manner.
!
! $ Files
!
!     See description of input arguments HAN1 and HAN2.
!
! $ Particulars
!
!     DLA files are built using the DAS low-level format; DLA files are
!     a specialized type of DAS file in which data are organized as a
!     doubly linked list of segments. Each segment's data belong to
!     contiguous components of character, double precision, and integer
!     type.
!
!     This routine supports DLA and DSK routines by enabling
!     them to determine whether a given DLA segment matches one
!     they've previously examined. This may allow such routines
!     to avoid buffering information redundantly.
!
! $ Examples
!
!     1)  A typical use of this routine is to enable a subroutine
!         to determine whether a DLA segment identified by a
!         handle and DLA descriptor matches one seen previously.
!         The logic of such a test can be implemented as follows:
!
!
!                   SUBROUTINE SUBA ( HANDLE, DLADSC )
!                   IMPLICIT NONE
!
!                   INCLUDE 'dla.inc'
!
!                   INTEGER               HANDLE
!                   INTEGER               DLADSC ( * )
!
!             C
!             C     SPICELIB functions
!             C
!                   LOGICAL               DLASSG
!                   LOGICAL               FAILED
!             C
!             C     Local variables
!             C
!                   INTEGER               PRVDSC ( DLADSZ )
!                   INTEGER               PRVHAN
!
!             C
!             C     Saved variables
!             C
!                   SAVE                  PRVDSC
!                   SAVE                  PRVHAN
!
!             C
!             C     Initial values
!             C
!                   DATA                  PRVHAN / 0 /
!
!                   ...
!
!                   IF ( .NOT. DLASSG( HANDLE, PRVHAN,
!                  .                   DLADSC, PRVDSC ) ) THEN
!
!                      [Examine segment]
!
!                      IF ( .NOT. FAILED() ) THEN
!             C
!             C           Save values only if no error occurred.
!             C
!                         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
!                         PRVHAN = HANDLE
!
!                      END IF
!
!                   END IF
!
!                   [Normal case]
!
!                   ...
!
!                   END
!
! $ Restrictions
!
!     1)  This routine relies on uniqueness of DAS file handles.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 22-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-MAY-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     test DLA segments for match
!
!-&


!
!     Local variables
!
      INTEGER               I

!
!     Give the function an initial value.
!
      DLASSG = .FALSE.

!
!     If the handles don't match, we're done.
!
      IF ( HAN1 .NE. HAN2 ) THEN
         RETURN
      END IF
!
!     Compare the DLA descriptors. All elements, including pointers,
!     must match in order to have a matching result.
!
      DO I = 1, DLADSZ

         IF ( DSC1(I) .NE. DSC2(I) ) THEN
            RETURN
         END IF

      END DO
!
!     At this point, everything's a match.
!
      DLASSG = .TRUE.

      RETURN
      END
