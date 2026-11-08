!$Procedure SPKUDS ( SPK - unpack segment descriptor )

      subroutine spkuds ( descr, &
                          body,  center, frame,  type, &
                          first, last,   baddrs, eaddrs )

! $ Abstract
!
!     Unpack the contents of an SPK segment descriptor.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      implicit none

      double precision      descr ( * )
      integer               body
      integer               center
      integer               frame
      integer               type
      double precision      first
      double precision      last
      integer               baddrs
      integer               eaddrs

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DESCR      I   An SPK segment descriptor.
!     BODY       O   The NAIF ID code for the body of the segment.
!     CENTER     O   The center of motion for BODY.
!     FRAME      O   The code for the frame of this segment.
!     TYPE       O   The type of SPK segment.
!     FIRST      O   The first epoch for which the segment is valid.
!     LAST       O   The last  epoch for which the segment is valid.
!     BADDRS     O   Beginning DAF address of the segment.
!     EADDRS     O   Ending DAF address of the segment.
!
! $ Detailed_Input
!
!     DESCR    is an SPK segment descriptor.
!
! $ Detailed_Output
!
!     BODY     is the NAIF ID code for the body of the segment.
!
!     CENTER   is the center of motion for BODY.
!
!     FRAME    is the SPICE integer code for the frame to which states
!              for the body are be referenced.
!
!     TYPE     is the type of SPK segment.
!
!     FIRST    is the first epoch for which the segment has
!              ephemeris data.
!
!     LAST     is the last epoch for which the segment has
!              ephemeris data.
!
!     BADDRS   is the starting address of the data associated
!              with this descriptor.
!
!     EADDRS   is the last address of the data associated with
!              this descriptor.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input descriptor DESCR is invalid, it's possible for
!         the output times to contain bit patterns that don't represent
!         normal double precision values. This error is not diagnosed.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine extracts the contents of an SPK segment
!     descriptor into the components needed for reading and
!     evaluating the data stored in the segment. It serves
!     as a macro for expanding the SPK segment descriptor.
!
! $ Examples
!
!     Suppose you wished to summarize a particular SPK segment
!     and that you have the descriptor for that segment in hand.
!     The following code fragment shows how you might use this
!     routine to create a summary message concerning the segment.
!
!        CALL SPKUDS ( DESCR, BODY,  CENTER, FRAME,
!       .              TYPE,  FIRST, LAST,   BADDR, EADDR )
!
!     Convert the start and stop times to ephemeris calendar strings
!
!        CALL ETCAL ( FIRST, FSTCAL )
!        CALL ETCAL ( LAST,  LSTCAL )
!
!        WRITE (*,*)
!        WRITE (*,*) 'Body     : ', BODY
!        WRITE (*,*) 'Center   : ', CENTER
!        WRITE (*,*) 'Frame ID : ', FRAME
!        WRITE (*,*) 'Data Type: ', TYPE
!        WRITE (*,*)
!        WRITE (*,*) 'Segment Start : ', FSTCAL
!        WRITE (*,*) 'Segment Stop  : ', LSTCAL
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 02-OCT-2021 (JDR) (NJB)
!
!        Changed output argument names "BEGIN" and "END" to "BADDRS" and
!        "EADDRS" for consistency with other routines.
!
!        Edited the header to comply with NAIF standard. Added entry
!        #1 to $Exceptions section and declared the routine error free.
!
!-    SPICELIB Version 1.0.0, 04-JAN-1994 (WLT) (KRG)
!
!-&


! $ Index_Entries
!
!     Unpack and SPK segment descriptor
!
!-&


!
!     Spicelib Functions
!
      logical               failed
      logical               return

!
!     Local Parameters
!
!     Values of ND and NI for SPK files.
!
      integer               nd
      parameter           ( nd = 2 )

      integer               ni
      parameter           ( ni = 6 )

!
!     Local Variables
!
      double precision      dppart ( nd )

      integer               ipart  ( ni )

!
!     Standard introductory error handling preparations.
!
      if ( return() ) then
         return
      end if
      call chkin ( 'SPKUDS' )

!
!     No judgments are made about the descriptor when we
!     unpack it.  If things were done right when the descriptor
!     was created, it should be fine now.
!
      call dafus ( descr, nd, ni, dppart, ipart )

      if ( failed() ) then
         call chkout ( 'SPKUDS' )
         return
      end if

      body   = ipart(1)
      center = ipart(2)
      frame  = ipart(3)
      type   = ipart(4)
      baddrs = ipart(5)
      eaddrs = ipart(6)

      first  = dppart(1)
      last   = dppart(2)

      call chkout ( 'SPKUDS' )
      return

      end
