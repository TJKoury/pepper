!$Procedure SPKS14 ( S/P Kernel, subset, type 14 )

      subroutine spks14 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in a type 14 SPK segment into a new
!     type 14 segment.
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

      integer(ip)           srchan
      real(wp)              srcdsc(*)
      integer(ip)           dsthan
      real(wp)              dstdsc(*)
      character*(*)         dstsid

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
!     1)  If the length of the SPK record that is to be moved is larger
!         than MAXREC, the error SPICE(SPKRECTOOLARGE) is signaled.
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
!     The exact structure of a segment of SPK type 14 is detailed in
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
!-    SPICELIB Version 1.0.0, 08-MAR-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     subset type_14 SPK segment
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
!     This is the maximum size type 14 record that we can move. This
!     allows a 20th degree Chebyshev Polynomial, which should be more
!     than sufficient. This should be the same as the value in SPKPV.
!
      integer(ip)           maxrec
      parameter           ( maxrec = 128 )
!
!     Reference frame name size. See CHGIRF.
!
      integer(ip)           frmsiz
      parameter           ( frmsiz = 16 )
!
!     DAF ND and NI values for SPK files.
!
      integer(ip)           nd
      parameter           ( nd = 2 )

      integer(ip)           ni
      parameter           ( ni = 6 )
!
!     Length of a state.
!
      integer(ip)           nstate
      parameter           ( nstate = 6 )
!
!     Local Variables
!
      character*(frmsiz)    myfram

      real(wp)              begtim
      real(wp)              dtemp  ( nd     )
      real(wp)              endtim
      real(wp)              myref
      real(wp)              record ( maxrec )

      integer(ip)           begidx
      integer(ip)           body
      integer(ip)           center
      integer(ip)           chbdeg
      integer(ip)           dummy
      integer(ip)           endidx
      integer(ip)           i
      integer(ip)           iframe
      integer(ip)           itemp  ( ni )
      integer(ip)           recsiz

      logical               found

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKS14' )
      end if
!
!     First, unpack the destination segment descriptor and set some
!     local variables.
!
      call dafus( dstdsc, nd, ni, dtemp, itemp )

      begtim = dtemp(1)
      endtim = dtemp(2)
      body   = itemp(1)
      center = itemp(2)
      iframe = itemp(3)

      call irfnam ( iframe, myfram )
!
!     If we can't find the code, it can't be an SPK file.
!
      if ( failed() ) then
         call chkout ( 'SPKS14' )
         return
      end if
!
!     Get the constants for this segment. There is only one.
!
      call sgfcon ( srchan, srcdsc, 1, 1, dtemp )

      if ( failed() ) then
         call chkout ( 'SPKS14' )
         return
      end if
!
!     The first element of DTEMP now contains the number of coefficients
!     used for the Chebyshev polynomials. We need the degree of the
!     polynomial which is one less than the number of coefficients.
!
      chbdeg = int ( dtemp(1) ) - 1
!
!     Compute the size of the SPK record and signal an error if there is
!     not enough room in the variable RECORD to hold it.
!
      recsiz = (chbdeg + 1) * nstate + 2

      if ( recsiz .gt. maxrec ) then
         call setmsg ( 'Storage for # double precision numbers' &
         //            ' is needed for an SPK data record and' &
         //            ' only # locations were available. Update' &
         //            ' the parameter MAXREC in the subroutine' &
         //            ' SPKS14 and notify the NAIF group of this' &
         //            ' problem.'                                )
         call errint ( '#', recsiz                                )
         call errint ( '#', maxrec                                )
         call sigerr ( 'SPICE(SPKRECTOOLARGE)'                    )
         call chkout ( 'SPKS14'                                   )
         return
      end if

!
!     Get the beginning and ending indices for the packets we need for
!     the destination segment.
!
      call sgfrvi ( srchan, srcdsc, begtim, myref, begidx, found )
      call sgfrvi ( srchan, srcdsc, endtim, myref, endidx, found )
!
!     Begin the destination segment.
!
      call spk14b ( dsthan, dstsid, body, center, myfram, &
                            begtim, endtim, chbdeg        )

      if ( failed() ) then
         call chkout ( 'SPKS14' )
         return
      end if
!
!     Now we get the data one record at a time from the source segment
!     and write it out to the destination segment.
!
      do i = begidx, endidx

         call sgfpkt ( srchan, srcdsc, i, i, record, dummy )
         call sgfref ( srchan, srcdsc, i, i, myref         )
         call spk14a ( dsthan, 1, record, myref            )

         if ( failed() ) then
            call chkout ( 'SPKS14' )
            return
         end if

      end do
!
!     Now all we need to do is end the segment.
!
      call spk14e ( dsthan )

      call chkout ( 'SPKS14' )
      return

      end
