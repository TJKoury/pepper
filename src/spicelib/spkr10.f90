!$Procedure SPKR10 ( SPK, read record from SPK type 10 segment )

      subroutine spkr10 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 10
!     (NORAD two line element sets).
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

      include               'sgparam.inc'
      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      real(wp)              record   ( * )

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
!              a SPK segment of type 10.
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
!     1)  It is assumed that the descriptor and handle supplied are
!         for a properly constructed type 10 segment. No checks are
!         performed to ensure this.
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
!     structure of a data type 10 segment.
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
!              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
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
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated entry
!        #2 in $Exceptions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.1.0, 09-MAR-2009 (EDW)
!
!        Removed declaration of unused variable DOINT.
!
!-    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     read record from type_10 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           nconst
      parameter           ( nconst = 8  )

      integer(ip)           nelems
      parameter           ( nelems = 10 )
!
!     We have 2 nutation/obliquity terms and their rates giving us
!     four angle components for each packet.
!
      integer(ip)           nangs
      parameter           ( nangs =   4 )
!
!     BEGEL1 is the location in the record where the first
!     two-line element set will begin.
!
      integer(ip)           begel1
      parameter           ( begel1 = nconst + 1      )
!
!     BEGEL2 is the location in the record where the second
!     two-line element set will begin.
!
      integer(ip)           begel2
      parameter           ( begel2 = begel1 + nelems + nangs )
!
!     ENSET1 and ENSET2 are the locations in the record where the
!     last element of set 1 and set 2 will be located.
!
      integer(ip)           enset1
      parameter           ( enset1 = begel1 + nelems - 1 )

      integer(ip)           enset2
      parameter           ( enset2 = begel2 + nelems - 1 )


      real(wp)              value


      integer(ip)           ends    ( 2 )
      integer(ip)           from
      integer(ip)           indx
      integer(ip)           nepoch
      integer(ip)           to
      integer(ip)           putelm
      integer(ip)           getelm
      integer(ip)           i
      integer(ip)           set1
      integer(ip)           set2

      logical               found

      save


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      call sgfcon ( handle, descr, 1,  nconst, record(1) )

!
!     Locate the time in the file closest to the input ET.
!
      call sgfrvi ( handle, descr, et, value, indx, found )

!
!     Determine which pair of element sets to choose so that
!     they will bracket ET.
!
      if ( et <= value ) then

         from = max( indx-1, 1 )
         to   = indx

      else

         call sgmeta ( handle, descr, nref, nepoch )
         from = indx
         to   = min( indx+1, nepoch  )

      end if

!
!     Fetch the element sets
!
      call sgfpkt ( handle, descr, from, to, record(begel1), ends )
!
!     If the size of the packets is not 14, this is an old style
!     two-line element set without nutation information.  We simply
!     set all of the angles to zero.
!
      if ( ends(1) == nelems ) then
!
!        First shift the elements to their proper locations in RECORD
!        so there will be room to fill in the zeros.
!
         putelm = enset2
         getelm = enset1 + nelems

         do while ( getelm > enset1 )

            record(putelm) = record(getelm)
            putelm         = putelm - 1
            getelm         = getelm - 1

         end do

         set1 = enset1 + 1
         set2 = enset2 + 1

         do i = 1, nangs
            record(set1) = 0.0_wp
            record(set2) = 0.0_wp
            set1         = set1 + 1
            set2         = set2 + 1
         end do

      end if

!
!     If we only got one element set, ET  was either before the
!     first one in the segment or after the last one in the
!     segment.  We simply copy the one fetched a second time so
!     that the record is properly constructed.
!
      if ( from == to ) then
         call moved ( record(begel1), nelems + nangs, record(begel2) )
      end if

      return
      end
