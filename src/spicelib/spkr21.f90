!$Procedure SPKR21 ( Read SPK record from segment, type 21 )

      subroutine spkr21 ( handle, descr, et, record )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 21
!     (Extended Difference Lines).
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk21.inc'

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
!     ET         I   Evaluation epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 21.
!
!     ET       is an epoch for which a data record from a specific
!              segment is required. The epoch is represented as
!              seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of an
!              ephemeris object, relative to its center of motion,
!              in an inertial reference frame.
!
!              The contents of RECORD are as follows:
!
!                 RECORD(1):         The difference table size per
!                                    Cartesian component. Call this
!                                    size MAXDIM; then the difference
!                                    line (MDA) size DLSIZE is
!
!                                      ( 4 * MAXDIM ) + 11
!
!                 RECORD(2)
!                    ...
!                 RECORD(1+DLSIZE):  An extended difference line.
!                                    The contents are:
!
!                    Dimension  Description
!                    ---------  ----------------------------------
!                    1          Reference epoch of difference line
!                    MAXDIM     Stepsize function vector
!                    1          Reference position vector,  x
!                    1          Reference velocity vector,  x
!                    1          Reference position vector,  y
!                    1          Reference velocity vector,  y
!                    1          Reference position vector,  z
!                    1          Reference velocity vector,  z
!                    MAXDIM,3   Modified divided difference
!                               arrays (MDAs)
!                    1          Maximum integration order plus 1
!                    3          Integration order array
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the maximum table size of the input record exceeds
!         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
!
!     2)  If an error occurs while looking up SPK data, the error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 21 segment.
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
!              CALL SPKR21 ( HANDLE, DESCR, ET, RECORD )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     F.T. Krogh         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      integer               lstltd
      logical               return

!
!     Local parameters
!
      integer               bufsiz
      parameter           ( bufsiz = 100 )

!
!     Local variables
!
      double precision      data   ( bufsiz )
      double precision      dc     ( 2 )

      integer               begin
      integer               dflsiz
      integer               end
      integer               i
      integer               ic     ( 6 )
      integer               maxdim
      integer               ndir
      integer               nrec
      integer               off
      integer               offd
      integer               offe
      integer               offr
      integer               recno

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKR21' )

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
!     We'll fetch the difference table dimension as well.
!
      call dafgda ( handle, end-1, end, data )

      nrec   = nint ( data(2) )
      ndir   = nrec / bufsiz
      offd   = end  - ndir - 2
      offe   = offd - nrec

      maxdim = nint ( data(1) )

      if ( maxdim .gt. maxtrm ) then

         call setmsg ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         call errint ( '#',  maxdim                            )
         call errint ( '#',  maxtrm                            )
         call sigerr ( 'SPICE(DIFFLINETOOLARGE)'               )
         call chkout ( 'SPKR21'                                )
         return

      end if

!
!     The difference line dimension per component is the
!     first element of the output record.
!
      record( 1 ) = maxdim

!
!     Set the difference line size.
!
      dflsiz = ( 4 * maxdim ) + 11

!
!     What we want is the record number: once we have that, we can
!     compute the offset of the record from the beginning of the
!     segment, grab it, and go. But how to find it?
!
!     Ultimately, we want the first record whose epoch is greater
!     than or equal to ET. If there are BUFSIZ or fewer records, all
!     the record epochs can be examined in a single group.
!
      if ( nrec .le. bufsiz ) then

         call dafgda ( handle, offe+1, offe+nrec, data )
         recno = lstltd ( et, nrec, data ) + 1

         offr = (begin - 1) + (recno - 1) * dflsiz
         call dafgda ( handle, offr+1, offr+dflsiz, record(2) )

         call chkout ( 'SPKR21' )
         return

      end if

!
!     Searching directories is a little more difficult.
!
!     The directory contains epochs BUFSIZ, 2*BUFSIZ, and so on. Once
!     we find the first directory epoch greater than or equal to ET, we
!     can grab the corresponding set of BUFSIZ record epochs, and
!     search them.
!
      do i = 1, ndir

         call dafgda ( handle, offd+i, offd+i, data )

         if ( data(1) .ge. et ) then

            off = offe + (i - 1) * bufsiz
            call dafgda ( handle, off+1, off+bufsiz, data )

            recno = ((i - 1) * bufsiz) + lstltd ( et, bufsiz, data ) + 1

            offr = (begin - 1) + (recno - 1) * dflsiz
            call dafgda ( handle, offr+1, offr+dflsiz, record(2) )

            call chkout ( 'SPKR21' )
            return

         end if

      end do

!
!     If ET is greater than the final directory epoch, we want one
!     of the final records.
!
      i = mod ( nrec, bufsiz )

      call dafgda ( handle, end-ndir-i-1, end-ndir-2, data )

      recno = (ndir * bufsiz) + lstltd ( et, i, data ) + 1

      offr = (begin - 1) + (recno - 1) * dflsiz

      call dafgda ( handle, offr+1, offr+dflsiz, record(2) )


      call chkout ( 'SPKR21' )
      return
      end
