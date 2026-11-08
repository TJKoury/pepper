!$Procedure SPKR01 ( Read SPK record from segment, type 1 )

      subroutine spkr01 ( handle, descr, et, record )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 1
!     (Difference Lines).
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

      integer               handle
      double precision      descr    ( 5 )
      double precision      et
      double precision      record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 1.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
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
!     See the SPK Required Reading file for a description of the
!     structure of a data type 1 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
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
!           IF ( TYPE .EQ. 1 ) THEN
!              CALL SPKR01 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added entry #1
!        to $Exceptions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
!
!        SPK01 was removed from the $Required_Reading section of the
!        header. The information in the SPK01 Required Reading file
!        is now part of the SPK Required Reading file.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_1 SPK segment
!
!-&


!
!     SPICELIB functions
!
      integer               lstltd
      logical               return

!
!     Local variables
!

      double precision      dc       (   2 )
      integer               ic       (   6 )
      integer               begin
      integer               end

      double precision      data     ( 100 )
      integer               nrec
      integer               ndir
      integer               offd
      integer               offe
      integer               off

      integer               recno
      integer               offr
      integer               i



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKR01' )
      end if

!
!     Unpack the segment descriptor.
!
      call dafus ( descr, 2, 6, dc, ic )

      begin = ic(5)
      end   = ic(6)

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFD      The offset of the first directory epoch.
!
!        OFFE      The offset of the first epoch.
!
!
!     the number of directory epochs.
!
      call dafgda ( handle, end, end, data )
      nrec = int ( data(1) )

      ndir  = nrec / 100
      offd  = end - ndir - 1
      offe  = end - ndir - nrec - 1

!
!     What we want is the record number: once we have that, we can
!     compute the offset of the record from the beginning of the
!     segment, grab it, and go. But how to find it?
!
!     Ultimately, we want the first record whose epoch is greater
!     than or equal to ET. If there are 100 or fewer records, all
!     the record epochs can be examined in a single group.
!
      if ( nrec .le. 100 ) then
         call dafgda ( handle, offe+1, offe+nrec, data )
         recno = lstltd ( et, nrec, data ) + 1

         offr = (begin - 1) + (recno - 1) * 71
         call dafgda ( handle, offr+1, offr+71, record )

         call chkout ( 'SPKR01' )
         return
      end if

!
!     Searching directories is a little more difficult.
!
!     The directory contains epochs 100, 200, and so on. Once we
!     find the first directory epoch greater than or equal to ET,
!     we can grab the corresponding set of 100 record epochs, and
!     search them.
!
      do i = 1, ndir
         call dafgda ( handle, offd+i, offd+i, data )

         if ( data(1) .ge. et ) then
            off = offe + (i - 1) * 100
            call dafgda ( handle, off+1, off+100, data )

            recno = ((i - 1) * 100) + lstltd ( et, 100, data ) + 1

            offr = (begin - 1) + (recno - 1) * 71
            call dafgda ( handle, offr+1, offr+71, record )

            call chkout ( 'SPKR01' )
            return
         end if
      end do

!
!     If ET is greater than the final directory epoch, we want one
!     of the final records.
!
      i = mod ( nrec, 100 )

      call dafgda ( handle, end-ndir-i, end-ndir-1, data )
      recno = (ndir * 100) + lstltd ( et, i, data ) + 1

      offr = (begin - 1) + (recno - 1) * 71
      call dafgda ( handle, offr+1, offr+71, record )


      call chkout ( 'SPKR01' )
      return
      end
