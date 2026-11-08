!$Procedure SPKR12 ( Read SPK record from segment, type 12 )

      subroutine spkr12 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single data record from a type 12 SPK segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer(ip), parameter :: maxrec = 129

      integer(ip)           handle
      real(wp)              descr    ( * )
      real(wp)              et
      real(wp)              record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of the open SPK file.
!     DESCR      I   Descriptor of the segment with the desired record.
!     ET         I   Epoch used to identify the desired record.
!     RECORD     O   The desired type 12 SPK record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the open SPK file which contains
!              the segment of interest.
!
!     DESCR    is the descriptor for a type 12 SPK segment that
!              contains the record of interest.
!
!     ET       is the target epoch used to determine the
!              particular record to be obtained from the SPK
!              segment.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | number of states (n) |
!                 +----------------------+
!                 | start epoch          |
!                 +----------------------+
!                 | step size            |
!                 +----------------------+
!                 | state 1 (6 elts.)    |
!                 +----------------------+
!                 | state 2 (6 elts.)    |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | state n (6 elts.)    |
!                 +----------------------+
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while looking up SPK data, the error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This subroutine will read a single record from a type 12 SPK
!     segment. The record read will provide the data necessary to
!     compute the state for the body designated by DESCR at epoch
!     ET.
!
!     The exact format and structure of a type 12 SPK segment is
!     described in the SPK Required Reading.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in a raw form, taken
!     directly from the segment. As such, it will be not be directly
!     useful to a user unless they have a complete understanding of the
!     structure of the data type. Given that understanding, however,
!     the SPKRnn routines could be used to "dump" and check segment data
!     for a particular epoch, as in the example which follows.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 12 ) THEN
!              CALL SPKR12 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  This subroutine should not be called directly by a casual
!         user. It is intended for use by the subroutine SPKPVN, and
!         certain tests for error conditions are not performed here, as
!         SPKPVN will have already performed them.
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
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated
!        $Exceptions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_12 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     The type 8 reader knows how to obtain a type 12 record.
!
      call spkr08 ( handle, descr, et, record )

      return
      end
