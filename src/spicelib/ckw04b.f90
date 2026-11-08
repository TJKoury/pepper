!$Procedure CKW04B ( CK type 04: Begin a segment )

      subroutine ckw04b ( handle, begtim, inst, ref, avflag, segid )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Begin a type CK04 segment in the DAF file associated with
!     HANDLE. See also CKW04A and CKW04E.
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

      include               'sgparam.inc'
      include               'ckparam.inc'

      integer(ip)           handle
      character(*)          segid
      integer(ip)           inst
      logical               avflag
      character(*)          ref
      real(wp)              begtim

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of an DAF file open for writing.
!     SEGID      I   The string to use for segment identifier.
!     INST       I   The NAIF ID code for the SC or instrument.
!     AVFLAG     I   The angular rates flag.
!     REF        I   The reference frame for this segment.
!     BEGTIM     I   The segment coverage start encoded SCLK time
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a CK file that has been
!              opened for writing.
!
!     SEGID    is the segment identifier. CK segment identifier
!              may contain up to 40 printing ASCII characters.
!
!     INST     is the SPICE ID for the SC structure or instrument
!              whose orientation are to be recorded in a CK file.
!
!     AVFLAG   is a logical flag that indicates whether segment will
!              contain angular rate information.
!
!     REF      is the name of a reference frame that pointing is
!              given with respect to, for example 'J2000'.
!
!     BEGTIM   is the encoded SCLK time for the start of the segment
!              coverage.
!
! $ Detailed_Output
!
!     None.
!
!     The input data is used to create the segment summary for the
!     segment being started in the DAF file associated with HANDLE.
!
!     See the $Particulars section for details about the structure of a
!     type 4 CK segment.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the files
!     'sgparam.inc' and 'ckparam.inc'.
!
! $ Exceptions
!
!     1)  If a file access error occurs while this routine begins a new
!         type 04 segment, the error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If numeric ID for given reference frame cannot be resolved
!         from it's name, the error SPICE(INVALIDREFFRAME) is signaled.
!
!     3)  If SEGID is more than 40 characters long, the error
!         SPICE(SEGIDTOOLONG) is signaled.
!
!     4)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
! $ Files
!
!     See HANDLE in the $Detailed_Input section.
!
! $ Particulars
!
!     This routine begins writing a type 4 CK segment to the open DAF
!     file that is associated with HANDLE. The file must have been
!     opened with write access.
!
!     This routine is one of a set of three routines for creating and
!     adding data to type 4 CK segments. These routines are:
!
!        CKW04B: Begin a type 4 CK segment. This routine must be
!                called before any data may be added to a type 4
!                segment.
!
!        CKW04A: Add data to a type 4 CK segment. This routine may be
!                called any number of times after a call to CKW04B to
!                add type 4 records to the CK segment that was
!                started.
!
!        CKW04E: End a type 4 CK segment. This routine is called to
!                make the type 4 segment a permanent addition to the
!                DAF file. Once this routine is called, no further type
!                4 records may be added to the segment. A new segment
!                must be started.
!
!     A type 4 CK segment consists of coefficient sets for variable
!     order Chebyshev polynomials over consecutive time intervals of
!     a variable length. The gaps between intervals are allowed.
!     The Chebyshev polynomials represent individual quaternion
!     components q0, q1, q2 and q3 and individual angular velocities
!     AV1, AV2 and AV3 if they are included with the data.
!
!     The pointing data supplied to the type 4 CK writer (CKW04A)
!     is packed into an array as a sequence of records,
!
!        ----------------------------------------------------
!        | Record 1 | Record 2 | .. | Record N-1 | Record N |
!        ----------------------------------------------------
!
!     with each record in data packets has the following format.
!
!        ----------------------------------------------------
!        | The midpoint of the approximation interval       |
!        ----------------------------------------------------
!        | The radius of the approximation interval         |
!        ----------------------------------------------------
!        | Number of coefficients for q0                    |
!        ----------------------------------------------------
!        | Number of coefficients for q1                    |
!        ----------------------------------------------------
!        | Number of coefficients for q2                    |
!        ----------------------------------------------------
!        | Number of coefficients for q3                    |
!        ----------------------------------------------------
!        | Number of coefficients for AV1                   |
!        ----------------------------------------------------
!        | Number of coefficients for AV2                   |
!        ----------------------------------------------------
!        | Number of coefficients for AV3                   |
!        ----------------------------------------------------
!        | q0 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q1 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q2 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q3 Cheby coefficients                            |
!        ----------------------------------------------------
!        | AV1 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!        | AV2 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!        | AV3 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!
! $ Examples
!
!     Assume that we have:
!
!        HANDLE   is the handle of an CK file opened with write
!                 access.
!
!        SEGID    is a character string of no more than 40 characters
!                 which provides a pedigree for the data in the CK
!                 segment we will create.
!
!        INST     is the SPICE ID code for the instrument whose
!                 pointing data is to be placed into the file.
!
!        AVFLAG   angular rates flag.
!
!        REFFRM   is the name of the SPICE reference frame for the
!                 pointing data.
!
!        BEGTIM   is the starting encoded SCLK time for which the
!                 segment is valid.
!
!        ENDTIM   is the ending encoded SCLK time for which the segment
!                 is valid.
!
!        N        is the number of type 4 records that we want to
!                 put into a segment in an CK file.
!
!        NPKTS    is integer array which contains the lengths of
!                 variable size data packets
!
!        RECRDS   contains N type 4 records packaged for the CK
!                 file.
!
!        SCSTRT   contains the initial encoded SC time for each of
!                 the records contained in RECRDS, where
!
!                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1
!
!                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST
!
!     Then the following code fragment demonstrates how to create
!     a type 4 CK segment if all of the data for the segment is
!     available at one time.
!
!     C
!     C     Begin the segment.
!     C
!           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )
!     C
!     C     Add the data to the segment all at once.
!     C
!           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT )
!     C
!     C     End the segment, making the segment a permanent
!     C     addition to the CK file.
!     C
!           CALL CKW04E ( HANDLE, ENDTIM )
!
! $ Restrictions
!
!     1)  The file containing the segment should be opened for read
!         or write access either by CKOPN or DAFOPW.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     Y.K. Zaiko         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
!
!        Minor header edits.
!
!-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
!
!-&

! $ Index_Entries
!
!     begin writing a type_4 CK segment
!
!-&

!
!     Spicelib functions
!
      integer(ip)           lastnb
      logical               return

!
!     Local Parameters
!

!
!     DAF ND and NI values for CK files and length of a DAF descriptor.
!
      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 6

      integer(ip), parameter :: ndescr = nd + ( ni + 1 ) / 2

!
!     The number of generic segment constants in a type 4 CK segment.
!
      integer(ip), parameter :: nconst = 0

!
!     The integer codes of the first and last printable ASCII
!     characters.
!
      integer(ip), parameter :: fprint = 32

      integer(ip), parameter :: lprint = 126

!
!     The maximum number of characters allowed in a CK segment
!     identifier.
!
      integer(ip), parameter :: sidlen = 40

!
!     Local variables
!
      real(wp)              dcd    ( nd )
      real(wp)              dcoeff
      real(wp)              descr  ( ndescr )

      integer(ip)           i
      integer(ip)           icd    ( ni )
      integer(ip)           refcod
      integer(ip)           value

!
!     Standard SPICELIB error handling.
!
      if ( return() ) return

!
!     Create a descriptor for the segment we are about to write. First
!     assign start and stop times.
!
      dcd( 1 ) = begtim
      dcd( 2 ) = 0.0_wp

!
!     Second, resolve reference frame ID code from its name and
!     assign it to the corresponding descriptor component. Signal
!     an error if frame is not recognized.
!
      call namfrm ( ref, refcod )

      if ( refcod == 0 ) then
         call setmsg ( 'The reference frame # is not supported.' )
         call errch  ( '#', ref                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                  )
         return
      end if

      icd ( 2 ) = refcod

!
!     Third, assign values to the rest of the integer components of
!     the segment descriptor.
!
      icd ( 1 ) = inst
      icd ( 3 ) = ck4dtp

      if ( avflag ) then
         icd ( 4 ) = 1
      else
         icd ( 4 ) = 0
      end if

!
!     Now pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

!
!     Check that all characters in the SEGID are printable.
!
      do i = 1, lastnb(segid)

         value = ichar( segid(i:i) )

         if ( ( value < fprint ) .or. ( value > lprint ) ) then

            call setmsg ( 'The segment identifier contains '      // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            return

         end if

      end do

!
!     Also check if the segment identifier is too long.
!
      if ( lastnb( segid ) > sidlen ) then

         call setmsg ( 'Segment identifier contains more than '   // &
                       '40 characters.'                           )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                      )
         return

      end if

!
!     We've got a valid descriptor and identifier and can begin
!     the segment. For this data type, we want to use an explicit
!     reference value index where the reference epochs are in
!     increasing order. We also want the index returned for a
!     particular request epoch to be the index of the greatest
!     reference epoch less than or equal to the request epoch. These
!     characteristics are prescribed by the mnemonic EXPLE. See the
!     include file 'sgparam.inc' for more details.
!
      call sgbwvs ( handle, descr, segid, nconst, dcoeff, exple )

!
!     No need to check FAILED() here, since all we do after this
!     point is checking out.
!

      return

      end
