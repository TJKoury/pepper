!$Procedure SPKS10 ( S/P Kernel, subset, type 10 )

      subroutine spks10 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in a type 10 SPK segment into a new
!     type 10 segment.
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

      include              'sgparam.inc'
      integer(ip)           srchan
      real(wp)              srcdsc(*)
      integer(ip)           dsthan
      real(wp)              dstdsc(*)
      character(*)          dstsid

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SRCHAN     I   Handle of the SPK file with the source segment.
!     SRCDSC     I   Descriptor for the source segment.
!     DSTHAN     I   Handle of the SPK file for the destination segment.
!     DSTDSC     I   Descriptor for the destination segment.
!     DSTSID     I   Segment identifier for the new segment.
!
! $ Detailed_Input
!
!     SRCHAN   is the handle of the SPK file containing the source
!              segment.
!
!     SRCDSC   is the SPK descriptor for the source segment.
!
!     DSTHAN   is the handle of the SPK file containing the new segment.
!
!     DSTDSC   is the SPK descriptor for the destination segment. It
!              contains the desired start and stop times for the
!              requested subset.
!
!     DSTSID   is the segment identifier for the destination segment.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     See arguments SRCHAN, DSTHAN.
!
! $ Particulars
!
!     This subroutine copies a subset of the data form one SPK segment
!     to another.
!
!     The exact structure of a segment of SPK type 10 is detailed in
!     the SPK Required Reading. Please see this document for details.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  We assume that the source descriptor actually describes a
!         segment in the source SPK file containing the time coverage
!         that is desired for the subsetting operation.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1997 (KRG)
!
!-&


! $ Index_Entries
!
!     subset type_10 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local Parameters
!
      integer(ip)           wdsize
      parameter           ( wdsize = 40 )
!
!     DAF ND and NI values for SPK files.
!
      integer(ip)           nd
      parameter           ( nd = 2 )

      integer(ip)           ni
      parameter           ( ni = 6 )

!
!     The number of geophysical constants:
!
      integer(ip)           nconst
      parameter           ( nconst = 8 )
!
!     The number of elements per two-line set:
!
      integer(ip)           nelems
      parameter           ( nelems = 10 )

      integer(ip)           nangs
      parameter           ( nangs  =  4 )

      integer(ip)           pktsiz
      parameter           ( pktsiz = nelems + nangs )

!
!     Local Variables
!
      character(wdsize)     time

      real(wp)              begtim
      real(wp)              consts ( nconst )
      real(wp)              dtemp  ( nd     )
      real(wp)              endtim
      real(wp)              myref
      real(wp)              packet ( pktsiz )

      integer(ip)           begidx
      integer(ip)           dummy
      integer(ip)           endidx
      integer(ip)           i
      integer(ip)           itemp  ( ni )
      integer(ip)           nepoch

      logical               found



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKS10' )
      end if
!
!     First, unpack the destination segment descriptor and set some
!     local variables.
!
      call dafus( dstdsc, nd, ni, dtemp, itemp )

      begtim = dtemp(1)
      endtim = dtemp(2)
!
!     Get the constants for the input segment and send them to the
!     output segment by beginning a fixed packet size segment.
!
      call sgfcon ( srchan, srcdsc, 1, nconst, consts )
      call sgbwfs ( dsthan, dstdsc,    dstsid, &
                    nconst, consts,    pktsiz, expcls )

      if ( failed() ) then
         call chkout ( 'SPKS10' )
         return
      end if

!
!     Get the beginning and ending indices for the packets we need for
!     the destination segment.  Note we need to get the preceding
!     and succeeding packets (if there are any) corresponding to the
!     start and end times of the output segments
!
      call sgfrvi ( srchan, srcdsc, begtim, myref, begidx, found )

      if ( .not. found ) then

         call etcal  ( begtim, time )
         call setmsg ( 'An error has occurred while attempting ' &
         //            'to subset the a type 10 SPK segment. The ' &
         //            'error occurred while attempting to ' &
         //            'locate a packet for the epoch #.  There ' &
         //            'does not appear to be such a packet. ' )
         call errch  ( '#', time )
         call sigerr ( 'SPICE(CANNOTGETPACKET)'  )
         call chkout ( 'SPKS10' )
         return

      end if

      if ( myref > begtim ) then
         begidx = max ( 1, begidx - 1 )
      end if


      call sgfrvi ( srchan, srcdsc, endtim, myref, endidx, found )

      if ( .not. found ) then

         call etcal  ( endtim, time )
         call setmsg ( 'An error has occurred while attempting ' &
         //            'to subset the a type 10 SPK segment. The ' &
         //            'error occurred while attempting to ' &
         //            'locate a packet for the epoch #.  There ' &
         //            'does not appear to be such a packet. ' )
         call errch  ( '#', time )
         call sigerr ( 'SPICE(CANNOTGETPACKET)'  )
         call chkout ( 'SPKS10' )
         return

      end if

!
!     Get the total number of epochs.
!
      call sgmeta ( srchan, srcdsc, nref, nepoch )

      if ( myref < endtim ) then
         endidx = min ( nepoch, endidx + 1 )
      end if

!
!     Now we get the data one record at a time from the source segment
!     and write it out to the destination segment.
!
      do i = begidx, endidx

         call sgfpkt ( srchan, srcdsc, i, i,   packet, dummy )
         call sgfref ( srchan, srcdsc, i, i,           myref )
         call sgwfpk ( dsthan, 1,      packet, 1,      myref )

      end do

!
!     Now all we need to do is end the segment.
!
      call sgwes  ( dsthan   )

      call chkout ( 'SPKS10' )
      return

      end
