!$Procedure CKR01 ( C-kernel, read pointing record, data type 1 )

      subroutine ckr01 ( handle, descr, sclkdp, tol, needav, &
                         record, found                       )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a pointing record from a CK segment, data type 1.
!
! $ Required_Reading
!
!     CK
!     DAF
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              descr  ( * )
      real(wp)              sclkdp
      real(wp)              tol
      logical               needav
      real(wp)              record ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Spacecraft clock time.
!     TOL        I   Time tolerance.
!     NEEDAV     I   .TRUE. when angular velocity data is requested.
!     RECORD     O   Pointing data record.
!     FOUND      O   .TRUE. when data is found.
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle of the CK file containing the
!              segment.
!
!     DESCR    is the descriptor of the segment.
!
!     SCLKDP   is an encoded spacecraft clock time for which
!              pointing is being requested. The SPICELIB routines
!              SCENCD and SCDECD are used to encode and decode SCLK
!              times.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              The record returned by CKR01 is the one whose time is
!              closest to SCLKDP and within TOL units of SCLKDP.
!
!     NEEDAV   is .TRUE. when angular velocity data is requested.
!
! $ Detailed_Output
!
!     RECORD   is the pointing record. Contents are as follows:
!
!                 RECORD( 1 ) = CLKOUT
!
!                 RECORD( 2 ) = q0
!                 RECORD( 3 ) = q1
!                 RECORD( 4 ) = q2
!                 RECORD( 5 ) = q3
!
!                 RECORD( 6 ) = Av1  ]
!                 RECORD( 7 ) = Av2  |-- Returned optionally
!                 RECORD( 8 ) = Av3  ]
!
!              CLKOUT is the encoded spacecraft clock time for the
!              returned pointing values. CLKOUT will be the closest
!              time in the segment to the input time as long as it is
!              within the input tolerance (see FOUND below). If SCLKDP
!              falls at the exact midpoint of two times, the record
!              for the greater of the two will be returned.
!
!              The quantities q0 - q3 represent a quaternion.
!              The quantities Av1, Av2, and Av3 represent the angular
!              velocity vector, and are returned if the segment
!              contains angular velocity data and NEEDAV is .TRUE.
!              The components of the angular velocity vector are
!              specified relative to the inertial reference frame
!              for the segment.
!
!     FOUND    is .TRUE. if a record was found to satisfy the pointing
!              request. FOUND will be .FALSE. when there is no pointing
!              instance within the segment whose time falls within
!              the requested time tolerance on either side of the
!              input time.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified handle does not belong to any file that is
!         currently known to be open, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If DESCR is not a valid, packed descriptor of a segment in
!         the CK file specified by HANDLE, the results of this routine
!         are unpredictable.
!
!     3)  If the segment is not of data type 1, as specified in the
!         third integer component of the segment descriptor,
!         the error SPICE(WRONGDATATYPE) is signaled.
!
!     4)  If there is a need for angular velocity data and the segment
!         contains no such data, the error SPICE(NOAVDATA) is signaled.
!
! $ Files
!
!     The file containing the segment is specified by its handle, and
!     should be opened for read, either by CKLPF or DAFOPR.
!
! $ Particulars
!
!     See the CK Required Reading file for a detailed description of
!     the structure of a type 1 pointing segment.
!
!     This routine searches a type 1 segment for the pointing instance
!     whose associated time is closest to the time that pointing was
!     requested for. If this time is within the tolerance specified by
!     the user, it sets FOUND equal to true and returns information in
!     the array RECORD that CKE01 uses to evaluate the pointing at the
!     time CLKOUT.
!
! $ Examples
!
!     The CKRnn routines are usually used in tandem with the CKEnn
!     routines, which evaluate the record returned by CKRnn to give
!     the pointing information and output time.
!
!     The following code fragment searches backwards through a file
!     (represented by HANDLE) for all segments applicable to the
!     Voyager 2 wide angle camera, for a particular spacecraft clock
!     time, which have data type 1. It then evaluates the pointing for
!     that epoch and prints the result.
!
!     The search performed here does not mimic the behavior of the CK
!     reader APIs CKGP and CKGPAV, which consider data from multiple CK
!     files, when available. See the CK Required reading for details.
!
!     C
!     C     - Get the spacecraft clock time. Must encode it for use
!     C       in the C-kernel.
!     C
!     C     - Set the time tolerance high to catch anything close to
!     C       the input time.
!     C
!     C     - We don't need angular velocity data.
!     C
!           SC     = -32
!           INST   = -32002
!           TOL    =  1000.D0
!           NEEDAV = .FALSE.
!           DTYPE  =  1
!     C
!     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
!     C
!           CALL FURNSH ( 'VGR_SCLK.TSC'        )
!           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
!     C
!     C     Convert the input request time to ticks.
!     C
!           WRITE (*,*) 'Enter spacecraft clock time string:'
!           READ (*,FMT='(A)') SCLKCH
!           CALL SCENCD ( SC, SCLKCH, SCLKDP )
!
!     C
!     C     Search backwards from the end of the CK file through all
!     C     of the segments.
!     C
!           CALL DAFBBS ( HANDLE )
!           CALL DAFFPA ( SFND   )
!
!           DO WHILE ( SFND )
!
!              CALL DAFGN ( IDENT                 )
!              CALL DAFGS ( DESCR                 )
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!              IF ( INST          .EQ. ICD( 1 )  .AND.
!          .        DTYPE         .EQ. ICD( 3 )  .AND.
!          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
!          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
!
!                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                   RECORD, FOUND )
!
!                 IF ( FOUND ) THEN
!
!                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
!
!                    WRITE (*,*) 'Segment descriptor and identifier:'
!                    WRITE (*,*) DCD, ICD
!                    WRITE (*,*) IDENT
!
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*) CMAT
!
!                 END IF
!
!              END IF
!
!              CALL DAFFPA ( SFND )
!
!           END DO
!
! $ Restrictions
!
!     1)  The file containing the segment should be opened for read,
!         either by CKLPF or DAFOPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.2, 12-AUG-2021 (NJB) (JDR)
!
!        Updated code example to use backwards search. Added
!        note regarding difference between this search and those
!        performed by the CK reader APIs CKGP and CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 30-AUG-1991 (JML)
!
!        This routine now checks the segment descriptor to
!        determine if it has been given a type 1 segment.
!
!        The FOUND flag is set to .FALSE. at the beginning of
!        the routine.
!
!        The $Particulars section was changed to provide a more
!        general description of the function of this routine. The
!        information that was originally in $Particulars was moved
!        to the body of the code.
!
!        The example program was changed so that the tolerance
!        and data type are used in selecting which segments to read.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!        The example program was corrected so that the input
!        instrument code was tested against ICD(1) instead of
!        ICD(3).
!
!-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
!
!-&

! $ Index_Entries
!
!     read CK type_1 pointing data record
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
!
!        1) This routine now checks the segment descriptor, ICD(3),
!           to determine if it has been given a type 1 segment.
!
!        2) The FOUND flag is set to .FALSE. at the beginning of
!           the routine. This is done so that if a SPICE error
!           is signaled, the FOUND flag will definitely be false.
!
!        3) The $Particulars section was changed to provide a more
!           general description of the function of this routine. The
!           information that was originally in $Particulars was moved
!           to the body of the code.
!
!        4) The example program was changed so that the tolerance
!           and data type are used in selecting which segments to read.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!        1) The example program was corrected so that the input
!           instrument code was tested against ICD(1) instead of
!           ICD(3).
!        2) ROTATIONS was removed from the Required Reading section.
!
!-    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM)
!
!        The following changes were made as a result of the
!        NAIF CK Code and Documentation Review:
!
!        1) The variable SCLK was changed to SCLKDP.
!        2) The declarations for the parameters QSIZ, QAVSIZ, NDC, and
!           NIC were moved from the "Declarations" section of the
!           header to the "Local parameters" section of the code below
!           the header. These parameters are not meant to modified by
!           users.
!        3) The variable DIRSIZ has been parameterized in the code
!           following the header. DIRSIZ is still 100.
!        5) The header was improved and updated to reflect the changes.
!        6) The in-code comments were improved.
!
!-    Beta Version 1.0.0, 17-MAY-1990 (RET) (IMU)
!
!-&

!
!     SPICELIB functions
!
      logical               return
      integer(ip)           lstled
      integer(ip)           lstcld

!
!     Local parameters
!
!        DIRSIZ     is the directory size.
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        QSIZ       is the number of double precision numbers making up
!                   the quaternion portion of a pointing record.
!
!        QAVSIZ     is the number of double precision numbers making up
!                   the quaternion and angular velocity portion of a
!                   pointing record.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!

      integer(ip), parameter :: dirsiz = 100

      integer(ip), parameter :: ndc = 2

      integer(ip), parameter :: nic = 6

      integer(ip), parameter :: qsiz = 4

      integer(ip), parameter :: qavsiz = 7

      integer(ip), parameter :: dtype = 1

!
!     Local variables
!
      integer(ip)           icd    ( nic )
      integer(ip)           beg
      integer(ip)           end
      integer(ip)           nrec
      integer(ip)           ndir
      integer(ip)           psiz
      integer(ip)           group
      integer(ip)           dirloc
      integer(ip)           remain
      integer(ip)           skip
      integer(ip)           grpndx
      integer(ip)           i
      integer(ip)           n

      real(wp)              dcd    ( ndc )
      real(wp)              buffer ( dirsiz )

      logical               fnd

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     To minimize the number of file reads performed during the search,
!     a buffer of 100 double precision numbers is used to read the SCLK
!     times from the C-kernel.  If there are 10,001 or fewer pointing
!     records, at most four reads will be needed to satisfy the request:
!     one to read NREC, one to read in 100 or fewer directory times,
!     one to read 100 or fewer actual times, and then after the
!     appropriate record has been located, one to read the quaternion
!     and angular velocity data.
!
!     One more read would be required for every other group of 10,000
!     records in the segment.
!

!
!     Start off with FOUND set to FALSE.
!
      found = .false.
!
!     We need to look at a few of the descriptor components.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!
      call dafus ( descr, ndc, nic, dcd, icd )
!
!     Check to make sure that the segment is type 1.
!
      if ( icd( 3 ) /= dtype ) then

         call setmsg ( 'The segment is not a type 1 segment.  ' // &
                       'Type is #'                                 )
         call errint ( '#', icd(3)                                 )
         call sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         return

      end if

!
!     The size of the record returned depends on whether or not the
!     segment contains angular velocity data.
!
!     This is a convenient place to check if the need for angular
!     velocity data matches the availability.
!
      if ( icd( 4 ) == 1 ) then

         psiz = qavsiz

      else

         psiz = qsiz

         if ( needav ) then

            call setmsg ( 'Segment does not contain angular velocity '// &
                          'data.'           )
            call sigerr ( 'SPICE(NOAVDATA)' )
            return

         end if

      end if

!
!     The beginning and ending addresses of the segment are in the
!     descriptor.
!
      beg = icd( 5 )
      end = icd( 6 )

!
!     Get the number of records in this segment, and from that determine
!     the number of directory epochs.
!
      call dafgda ( handle, end, end, buffer )
      nrec = int( buffer(1) )
      ndir = ( nrec - 1 )/dirsiz

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records. The way the directory is constructed guarantees
!     that we will definitely find the closest time in the segment to
!     SCLKDP in the indicated group.
!
!     There is only one group if there are no directory epochs.
!
      if ( ndir == 0 ) then
         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, need to go through all of the
!        pointing numbers (PSIZ*NREC of them), then through all of
!        the SCLKDP times (NREC more) to get to the first SCLK
!        directory.
!
         dirloc = beg + ( psiz + 1 ) * nrec

!
!        Locate the first directory epoch greater than SCLKDP. Read in
!        as many as DIRSIZ directory epochs at a time for comparison.
!
         fnd    = .false.
         remain =  ndir
         group  =  0

         do while ( .not. fnd )
!
!           The number of records to read in the buffer.
!
            n = min( remain, dirsiz )

            call dafgda ( handle, dirloc, dirloc + n - 1, buffer )

            remain = remain - n

!
!           If we find the first directory time greater than or equal
!           to the epoch, we're done.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
            i = lstled( sclkdp, n, buffer )

            if ( i < n ) then
               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then
               group =  ndir + 1
               fnd   = .true.

            else
               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) times to look at.
!     Out of the NREC SCLKDP times, the number that we should skip over
!     to get to the proper group is DIRSIZ*( GROUP - 1 ).
!
      skip = dirsiz*( group - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, need to pass through
!     PSIZ*NREC pointing numbers to get to the first SCLKDP time.
!     Then we skip over the number just computed above.
!
      grpndx = beg + (nrec * psiz) + skip

!
!     The number of times that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
      n = min( dirsiz, nrec - skip )

      call dafgda ( handle, grpndx, grpndx + n - 1, buffer )

!
!     Find the time in the group closest to the input time, and see
!     if it's within tolerance.
!
      i = lstcld( sclkdp, n, buffer )

      if ( abs( sclkdp - buffer( i ) ) > tol ) return

!
!     Now we know the exact record that we want.
!
!     RECORD( 1 ) holds CLKOUT.
!
      found       = .true.
      record( 1 ) =  buffer( i )

!
!     We need the Ith pointing record out of this group of DIRSIZ.
!     This group of DIRSIZ is SKIP records into the beginning
!     of the segment. And each record is PSIZ big.
!
      n = beg + psiz*( skip + i - 1 )

      call dafgda ( handle, n, n + psiz - 1, record( 2 ) )

!
!     That is all.
!
      return
      end
