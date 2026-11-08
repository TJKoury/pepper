!$Procedure ZZSWFCLN ( Private, switch frame clean up )

      SUBROUTINE ZZSWFCLN ( HDFRAM, FRPOOL, BASBEG, FRAMAT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Perform partial data structure clean up in response to failures
!     detected in the switch frame data fetch routine ZZSWFFET.
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
      INTEGER               BASBEG ( * )
      INTEGER               FRAMAT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HDFRAM     O   Array of hash collision list head nodes.
!     FRPOOL     O   Hash collision list pool.
!     BASBEG     O   Start indices of base lists of switch frames.
!     FRAMAT     O   Switch frame index in switch frame ID array.
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
!     BASBEG      is the initialized base frame start index list.
!
!     FRAMAT      is the initialized frame ID index. This index is
!                 set to zero, which is never valid.
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
!     1)  If integer hash initialization fails, the error will be
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is essentially a macro that centralizes
!     initialization of bookkeeping data structures maintained by
!     ZZSWFFET. Only those initializations that are required by
!     ZZSWFFET are performed. See ZZSWFINI for the complete set of
!     initialization actions.
!
! $ Examples
!
!     None. See usage in ZZSWFFET.
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
!     perform switch frame clean up
!
!-&

!
!     This routine must perform its function after an error has been
!     signaled, so it does not return upon entry after a SPICE error
!     occurs.
!
      CALL CHKIN ( 'ZZSWFCLN' )

      FRAMAT = 0

!
!     Both of the following routines will execute even when a SPICE
!     error condition exists.
!
      CALL CLEARI   ( MAXFRM, BASBEG )
      CALL ZZHSIINI ( MAXFRM, HDFRAM, FRPOOL )

      CALL CHKOUT ( 'ZZSWFCLN' )
      RETURN
      END
