!$Procedure PCKR03 ( PCK, read record from type 3 segment )

      subroutine pckr03 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single PCK data record from a segment of type 03.
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      real(wp)              record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle for a PCK file.
!     DESCR      I   Descriptor for a type 03 PCK segment.
!     ET         I   Target epoch for orientation information.
!     RECORD     O   Data record associated with epoch ET.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle for a type 03 PCK segment.
!
!     DESCR    is the segment descriptor for a type 03 PCK segment.
!
!     ET       is a target epoch, for which a data record from
!              the specified segment is required.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which,
!              when evaluated at epoch ET, will give the RA, DEC,
!              W and body fixed angular rates for the body associated
!              with the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  It is assumed that the descriptor and handle supplied are
!         for a properly constructed type 03 segment. No checks are
!         performed to ensure this.
!
!     2)  If the input ET value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
!     3)  If any issue is detected while reading the PCK data, an error
!         is signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This subroutine reads a type 03 PCK record from the segment
!     specified by HANDLE and DESCR. The record read will contain
!     sufficient information to to compute RA, DEC, W and body fixed
!     angular rates for the body associated with the segment for epoch
!     ET.
!
!     See the PCK Required Reading file for a description of the
!     structure of a type 03 PCK segment.
!
! $ Examples
!
!     The data returned by the PCKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the PCKRnn
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 03 ) THEN
!              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  It is assumed that the descriptor and handle supplied are
!         for a properly constructed type 03 segment. No checks are
!         performed to ensure this.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     read record from type_03 PCK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local Parameters
!
!     The number of constant values stored with a type 03 segment
!     segment.
!
      integer(ip), parameter :: nconst = 1
!
!     The beginning location in the output record for the non-constant
!     segment data.
!
      integer(ip), parameter :: begel1 = nconst + 1

!
!     Local Variables
!
      real(wp)              value

      integer(ip)           ends
      integer(ip)           indx

      logical               found

!
!     Standard SPICE error handling.
!
      if ( return () ) return
!
!     Check the request time against the time bounds in the segment
!     descriptor. In order to get the right data back from the generic
!     segment calls below, we need to be sure that the desired epoch
!     falls within the bounds of the segment, as specified by the
!     descriptor. The first two elements of the descriptor are the start
!     time for the segment and the stop time for the segment,
!     respectively.
!
      if ( ( et < descr(1) )  .or.  ( et > descr(2) ) )  then

         call setmsg( 'Request time # is outside of descriptor'    // &
                      ' bounds # : #.'                              )
         call errdp ( '#',  et                                      )
         call errdp ( '#',  descr(1)                                )
         call errdp ( '#',  descr(2)                                )
         call sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if
!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      call sgfcon ( handle, descr, 1,  nconst, record(1) )
!
!     Locate the time in the file less than or equal to the input ET.
!
      call sgfrvi ( handle, descr, et, value, indx, found )
!
!     Fetch the data record.
!
      call sgfpkt ( handle, descr, indx, indx, record(begel1), ends )

      return

      end
