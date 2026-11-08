!$Procedure SPKS20 ( S/P Kernel, subset, type 20 )

      subroutine spks20 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 20
!     into a new segment.
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

      integer(ip)           handle
      integer(ip)           baddr
      integer(ip)           eaddr
      real(wp)              begin
      real(wp)              end

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of source segment.
!     BADDR      I   Beginning address of source segment.
!     EADDR      I   Ending address of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!
! $ Detailed_Input
!
!     HANDLE,
!     BADDR,
!     EADDR    are the file handle assigned to a SPK file, and the
!              beginning and ending addresses of a segment within
!              the file. Together they determine a complete set of
!              ephemeris data, from which a subset is to be
!              extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time)
!              of the subset to be extracted.
!
! $ Detailed_Output
!
!     None. This routine writes data to the SPK file currently
!     open for write access.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading data from the source SPK
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
!     2)  If an error occurs while writing data to the output SPK file,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     The exact structure of a segment of data type 20 is detailed in
!     the SPK Required Reading file.
!
! $ Examples
!
!     None.
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
!     R.E. Thurman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 23-DEC-2013 (NJB) (RET)
!
!-&

! $ Index_Entries
!
!     subset type_20 SPK segment
!
!-&

!
!     SPICELIB functions
!
      real(wp)              j2000
      real(wp)              spd

      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: bufsiz = 100

!
!     Local variables
!
      real(wp)              btime
      real(wp)              data   ( bufsiz )
      real(wp)              dscale
      real(wp)              tscale
      real(wp)              initjd
      real(wp)              initfr
      real(wp)              intlen
      real(wp)              intrvl
      real(wp)              subbeg
      real(wp)              subifr
      real(wp)              subijd

      integer(ip)           recsiz
      integer(ip)           nrec
      integer(ip)           first
      integer(ip)           last
      integer(ip)           remain
      integer(ip)           addr
      integer(ip)           move

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      call dafgda ( handle, eaddr-6, eaddr, data )

      dscale =      data( 1 )
      tscale =      data( 2 )
      initjd =      data( 3 )
      initfr =      data( 4 )
      intlen =      data( 5 )
      recsiz = int( data( 6 ) )
      nrec   = int( data( 7 ) )

      btime  = ( ( initjd - j2000() ) + initfr ) * spd()
      intrvl = intlen * spd()

      first = int( (begin - btime) / intrvl ) + 1
      first = max( 1,  min( first, nrec )  )

      last  = int( (end   - btime) / intrvl ) + 1
      last  = max( 1,  min( last,  nrec )  )

!
!     The number of records to be moved.
!
      nrec = last - first + 1

!
!     We're going to move the data in chunks of BUFSIZ d.p. words.
!     Compute the number of words left to move, the address of the
!     beginning of the records to move, and the number to move this
!     time.
!
      remain = nrec * recsiz
      addr   = baddr + ( first - 1 )*recsiz
      move   = min( bufsiz, remain )

      do while ( remain > 0 )

         call dafgda ( handle, addr, addr + move - 1, data )
         call dafada ( data, move )
         remain = remain - move
         addr   = addr   + move
         move   = min( bufsiz, remain )

      end do

!
!     That's all the records we have to move. But there are still seven
!     final numbers left to write:
!
!        1)  The distance scale (DSCALE).
!        2)  The time scale (TSCALE).
!        3)  The initial integer Julian date of the start time of the
!            first record.
!        4)  The fractional part of the state time of the first
!            record.
!        5)  The time interval length for each polynomial in days
!            (INTLEN).
!        6)  The record size (RECSIZ).
!        7)  The number of records (NREC).
!
!     Let SUBBEG be the subset begin time expressed as a TDB Julian
!     date.
!
      subbeg =  j2000()  +  (  btime + ( first - 1 )*intrvl  ) / spd()

      subijd =  aint( subbeg )
      subifr =  subbeg - subijd

      data( 1 ) = dscale
      data( 2 ) = tscale
      data( 3 ) = subijd
      data( 4 ) = subifr
      data( 5 ) = intlen
      data( 6 ) = real( recsiz, wp )
      data( 7 ) = real( nrec, wp   )

      call dafada ( data, 7 )

      return
      end
