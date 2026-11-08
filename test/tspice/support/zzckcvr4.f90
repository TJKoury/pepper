!$Procedure ZZCKCVR4 ( Private --- C-kernel segment coverage, type 04 )

      SUBROUTINE ZZCKCVR4 ( HANDLE, ARRBEG, ARREND, SCHEDL )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 04 C-kernel segment.
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
!     CK
!     DAF
!
! $ Keywords
!
!     CK
!     UTILITY
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'ckparam.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HANDLE
      INTEGER               ARRBEG
      INTEGER               ARREND
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
!     SCHEDL    I/O  An initialized window/schedule of interval
!     CK4RSZ     P   C-kernel Type 04 Maximum Record Size
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 04 segment
!
!     ARREND     is the ending address of a type 04 segment.
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment.
!
! $ Parameters
!
!     CK4RSZ     is the maximum length of a CK4 record (with angular
!                velocity). Defined in the include file 'ckparam.inc'.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     Routines in the call tree of this routine may signal errors
!     if in sufficient room in SCHEDL exists or other error
!     conditions relating to file access arise.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 04 C-kernel segment.
!
! $ Examples
!
!     See CKBRIEF's main driver.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 29-AUG-2002 (FST)
!
!-&

      INTEGER               INTMAX
      LOGICAL               RETURN

      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( 5 )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      VALUES ( CK4RSZ )

      INTEGER               ENDS   ( 2 )
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NREC

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCKCVR4' )

!
!     Build a descriptor record that satisfies the requirements
!     of CKNR04 and SGFPKT.
!
!     Note: This is a hack dependent on the implementation of
!     the generic segments routines.  But for C-kernels it
!     should always work, as ND and NI aren't changing any
!     time soon.
!
      IC ( 1 ) = INTMAX()
      IC ( 2 ) = INTMAX()
      IC ( 3 ) = 4
      IC ( 4 ) = INTMAX()
      IC ( 5 ) = ARRBEG
      IC ( 6 ) = ARREND

      DC ( 1 ) = 0.0D0
      DC ( 2 ) = 0.0D0

      CALL DAFPS ( ND, NI, DC, IC, DESCR )

!
!     Determine the number of records in the array.
!
      CALL CKNR04 ( HANDLE, DESCR, NREC )

      DO I = 1, NREC

!
!        Extract each packet of pointing coefficients.
!
         CALL SGFPKT ( HANDLE, DESCR, I, I, VALUES, ENDS )

!
!        Compute the left and right end points of the interval
!        of coverage related to this packet.
!
         LEFT  = VALUES ( 1 ) - VALUES ( 2 )
         RIGHT = VALUES ( 1 ) + VALUES ( 2 )

!
!        Store the results in the schedule.
!
         CALL WNINSD ( LEFT, RIGHT, SCHEDL )

      END DO

      CALL CHKOUT ( 'ZZCKCVR4' )
      RETURN
      END
