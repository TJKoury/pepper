!$Procedure ZZSWFINI ( Private, switch frame initialization )

      SUBROUTINE ZZSWFINI ( HDFRAM, FRPOOL, FIDLST, BASBEG, &
                            FREE,   PRVAT,  PRVFRM, SAMFRM  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize switch frame bookkeeping data structures.
!
!     Arrays of base frame attributes are not initialized by this
!     routine.
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
!     FRAMES
!     KERNEL
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     Private
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzswtchf.inc'

      INTEGER               HDFRAM ( * )
      INTEGER               FRPOOL ( LBSNGL : * )
      INTEGER               FIDLST ( * )
      INTEGER               BASBEG ( * )
      INTEGER               FREE
      INTEGER               PRVAT
      INTEGER               PRVFRM
      LOGICAL               SAMFRM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HDFRAM     O   Array of hash collision list head nodes.
!     FRPOOL     O   Hash collision list pool.
!     FIDLST     O   Frame IDs corresponding to list nodes.
!     BASBEG     O   Start indices of base lists of switch frames.
!     FREE       O   First free index in base and time lists.
!     PRVAT      O   Index of previous input frame in frame list.
!     PRVFRM     O   Previous input frame ID.
!     SAMFRM     O   Flag indicating input frame matches previous one.
!     LBSNGL     P   Lower bound of singly linked list pool.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     HDFRAM      is the initialized head node array.
!
!     FRPOOL      is the initialized hash collision list pool.
!
!     FIDLST      is the initialized switch frame ID list.
!
!     BASBEG      is the initialized base frame start index list.
!
!     FREE        is the common index of the first free entries in the
!                 arrays of base frame attributes. The index is set to
!                 1.
!
!     PRVAT       is the index of the previous ZZSWFXFM input frame
!                 in the frame list.
!
!     PRVFRM      is the frame ID of the previous ZZSWFXFM input frame.
!
!     SAMFRM      is a logical flag indicating whether the current
!                 ZZSWFXFM input frame matches the previous one.
!
! $ Parameters
!
!     LBSNGL      is the lower bound of a singly linked list pool
!                 managed by the integer hash subsystem.
!
!     See the include file zzswtchf.inc for SPICE-private parameters
!     defining sizes of buffers used by the switch frame subsystem.
!
! $ Exceptions
!
!     1)  If integer hash initialization fails, the error is signaled
!         by a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine centralizes initialization of bookkeeping data
!     structures maintained by ZZSWFXFM.
!
! $ Examples
!
!     None. See usage in ZZSWFXFM.
!
! $ Restrictions
!
!     1)  This routine is SPICE-private. User applications must not
!         call it.
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
!-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize switch frame bookkeeping data structures
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSWFINI' )

!
!     Initialize the frame hash head node array, hash collision list
!     pool, and parallel frame ID list. Initialize the array of start
!     indices of associated base frame information. Indicate the base
!     frame data buffers are empty by setting FREE to 1.
!
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )

      IF ( FAILED() ) THEN
!
!        This code should be unreachable. It's provided for safety.
!
         CALL CHKOUT ( 'ZZSWFINI' )
         RETURN

      END IF

      CALL CLEARI ( MAXFRM, FIDLST )
      CALL CLEARI ( MAXFRM, BASBEG )

      FREE = 1

!
!     Initialize all saved frame identity information.
!
      PRVFRM = 0
      PRVAT  = 0
      SAMFRM = .FALSE.

      CALL CHKOUT ( 'ZZSWFINI' )
      RETURN
      END
