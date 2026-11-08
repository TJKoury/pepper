!$Procedure SPK14B ( SPK type 14: Begin a segment.)

      subroutine spk14b ( handle, segid, &
                          body,   center, frame, &
                          first,  last,   chbdeg  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Begin a type 14 SPK segment in the SPK file associated with
!     HANDLE.
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

      include 'sgparam.inc'

      integer(ip)           handle
      character*(*)         segid
      integer(ip)           body
      integer(ip)           center
      character*(*)         frame
      real(wp)              first
      real(wp)              last
      integer(ip)           chbdeg

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of an SPK file open for writing.
!     SEGID      I   The string to use for segment identifier.
!     BODY       I   The NAIF ID code for the body of the segment.
!     CENTER     I   The center of motion for BODY.
!     FRAME      I   The reference frame for this segment.
!     FIRST      I   The first epoch for which the segment is valid.
!     LAST       I   The last epoch for which the segment is valid.
!     CHBDEG     I   The degree of the Chebyshev Polynomial used.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file that has been
!              opened for writing.
!
!     SEGID    is the segment identifier. An SPK segment identifier
!              may contain up to 40 printing ASCII characters.
!
!     BODY     is the SPICE ID for the body whose states are
!              to be recorded in an SPK file.
!
!     CENTER   is the SPICE ID for the center of motion associated
!              with BODY.
!
!     FRAME    is the reference frame that states are referenced to,
!              for example 'J2000'.
!
!     FIRST    is the starting epoch, in seconds past J2000, for
!              the ephemeris data to be placed into the segment.
!
!     LAST     is the ending epoch, in seconds past J2000, for
!              the ephemeris data to be placed into the segment.
!
!     CHBDEG   is the degree of the Chebyshev Polynomials used to
!              represent the ephemeris information stored in the
!              segment.
!
! $ Detailed_Output
!
!     None. The input data is used to create the segment summary for the
!     segment being started in the SPK file associated with HANDLE.
!
!     See the $Particulars section for details about the structure of a
!     type 14 SPK segment.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If the degree of the Chebyshev Polynomial to be used for this
!         segment is negative, the error SPICE(INVALIDARGUMENT) is
!         signaled.
!
!     2)  If there are issues in the structure or content of the inputs
!         other than the degree of the Chebyshev Polynomial, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If a file access error occurs, the error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     See HANDLE in the $Detailed_Input section.
!
! $ Particulars
!
!     This routine begins writing a type 14 SPK segment to the open SPK
!     file that is associated with HANDLE. The file must have been
!     opened with write access.
!
!     This routine is one of a set of three routines for creating and
!     adding data to type 14 SPK segments. These routines are:
!
!        SPK14B: Begin a type 14 SPK segment. This routine must be
!                called before any data may be added to a type 14
!                segment.
!
!        SPK14A: Add data to a type 14 SPK segment. This routine may be
!                called any number of times after a call to SPK14B to
!                add type 14 records to the SPK segment that was
!                started.
!
!        SPK14E: End a type 14 SPK segment. This routine is called to
!                make the type 14 segment a permanent addition to the
!                SPK file. Once this routine is called, no further type
!                14 records may be added to the segment. A new segment
!                must be started.
!
!     A type 14 SPK segment consists of coefficient sets for fixed order
!     Chebyshev polynomials over consecutive time intervals, where the
!     time intervals need not all be of the same length. The Chebyshev
!     polynomials represent the position, X, Y, and Z coordinates, and
!     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to
!     CENTER.
!
!     The ephemeris data supplied to the type 14 SPK writer is packed
!     into an array as a sequence of records,
!
!        -----------------------------------------------------
!        | Record 1 | Record 2 | ... | Record N-1 | Record N |
!        -----------------------------------------------------
!
!     with each record has the following format.
!
!           ------------------------------------------------
!           |  The midpoint of the approximation interval  |
!           ------------------------------------------------
!           |  The radius of the approximation interval    |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the X coordinate  |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Y coordinate  |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Z coordinate  |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the X velocity    |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Y velocity    |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Z velocity    |
!           ------------------------------------------------
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to create an SPK type 14 kernel
!        containing only one segment, given a set of Chebyshev
!        coefficients and their associated epochs.
!
!
!        Example code begins here.
!
!
!              PROGRAM SPK14B_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 42 )
!
!        C
!        C     Define the segment identifier parameters.
!        C
!              CHARACTER*(*)         SPK14
!              PARAMETER           ( SPK14  = 'spk14b_ex1.bsp' )
!
!              CHARACTER*(*)         REF
!              PARAMETER           ( REF    = 'J2000'          )
!
!              INTEGER               BODY
!              PARAMETER           ( BODY   = 3  )
!
!              INTEGER               CENTER
!              PARAMETER           ( CENTER = 10 )
!
!              INTEGER               CHBDEG
!              PARAMETER           ( CHBDEG = 2  )
!
!              INTEGER               NRECS
!              PARAMETER           ( NRECS  = 4  )
!
!              INTEGER               RECSIZ
!              PARAMETER           ( RECSIZ = 2 + 6*(CHBDEG+1) )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    SEGID
!
!              DOUBLE PRECISION      EPOCHS ( NRECS + 1 )
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      LAST
!              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS )
!
!              INTEGER               HANDLE
!              INTEGER               NCOMCH
!
!        C
!        C     Define the epochs and coefficients.
!        C
!              DATA                  EPOCHS /
!             .                100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /
!
!              DATA                  RECRDS /
!             .     150.0D0, 50.0D0, 1.0101D0, 1.0102D0, 1.0103D0,
!             .                      1.0201D0, 1.0202D0, 1.0203D0,
!             .                      1.0301D0, 1.0302D0, 1.0303D0,
!             .                      1.0401D0, 1.0402D0, 1.0403D0,
!             .                      1.0501D0, 1.0502D0, 1.0503D0,
!             .                      1.0601D0, 1.0602D0, 1.0603D0,
!             .     250.0D0, 50.0D0, 2.0101D0, 2.0102D0, 2.0103D0,
!             .                      2.0201D0, 2.0202D0, 2.0203D0,
!             .                      2.0301D0, 2.0302D0, 2.0303D0,
!             .                      2.0401D0, 2.0402D0, 2.0403D0,
!             .                      2.0501D0, 2.0502D0, 2.0503D0,
!             .                      2.0601D0, 2.0602D0, 2.0603D0,
!             .     350.0D0, 50.0D0, 3.0101D0, 3.0102D0, 3.0103D0,
!             .                      3.0201D0, 3.0202D0, 3.0203D0,
!             .                      3.0301D0, 3.0302D0, 3.0303D0,
!             .                      3.0401D0, 3.0402D0, 3.0403D0,
!             .                      3.0501D0, 3.0502D0, 3.0503D0,
!             .                      3.0601D0, 3.0602D0, 3.0603D0,
!             .     450.0D0, 50.0D0, 4.0101D0, 4.0102D0, 4.0103D0,
!             .                      4.0201D0, 4.0202D0, 4.0203D0,
!             .                      4.0301D0, 4.0302D0, 4.0303D0,
!             .                      4.0401D0, 4.0402D0, 4.0403D0,
!             .                      4.0501D0, 4.0502D0, 4.0503D0,
!             .                      4.0601D0, 4.0602D0, 4.0603D0 /
!
!
!        C
!        C     Set the start and end times of interval covered by
!        C     segment.
!        C
!              FIRST = EPOCHS(1)
!              LAST  = EPOCHS(NRECS + 1)
!
!        C
!        C     NCOMCH is the number of characters to reserve for the
!        C     kernel's comment area. This example doesn't write
!        C     comments, so set to zero.
!        C
!              NCOMCH = 0
!
!        C
!        C     Internal file name and segment ID.
!        C
!              IFNAME = 'Type 14 SPK internal file name.'
!              SEGID  = 'SPK type 14 test segment'
!
!        C
!        C     Open a new SPK file.
!        C
!              CALL SPKOPN( SPK14, IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Begin the segment.
!        C
!              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REF,
!             .              FIRST,  LAST,  CHBDEG            )
!
!        C
!        C     Add the data to the segment all at once.
!        C
!              CALL SPK14A ( HANDLE, NRECS, RECRDS, EPOCHS )
!
!        C
!        C     End the segment, making the segment a permanent addition
!        C     to the SPK file.
!        C
!              CALL SPK14E ( HANDLE )
!
!        C
!        C     Close the SPK file.
!        C
!              CALL SPKCLS ( HANDLE )
!
!              END
!
!
!        When this program is executed, no output is presented on
!        screen. After run completion, a new SPK type 14 exists in
!        the output directory.
!
! $ Restrictions
!
!     1)  The SPK file must be open with write access.
!
!     2)  Only one segment may be written to a particular SPK file at a
!         time. All of the data for the segment must be written and the
!         segment must be ended before another segment may be started in
!         the file.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added
!        complete example code from existing fragment.
!
!        Removed references to other routines from $Abstract section
!        (present in $Particulars).
!
!-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
!
!        Removed comments from the $Declarations section.
!
!-    SPICELIB Version 1.0.1, 30-OCT-2006 (BVS)
!
!        Deleted "inertial" from the FRAME description in the $Brief_I/O
!        section of the header.
!
!-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     begin writing a type_14 SPK segment
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
!     DAF ND and NI values for SPK files.
!
      integer(ip)           nd
      parameter           ( nd = 2 )

      integer(ip)           ni
      parameter           ( ni = 6 )
!
!     Length of an SPK descriptor.
!
      integer(ip)           ndescr
      parameter           ( ndescr =  nd + (ni+1)/2 )
!
!     Length of a state.
!
      integer(ip)           nstate
      parameter           ( nstate = 6 )
!
!     The type of this segment
!
      integer(ip)           type
      parameter           ( type = 14 )
!
!     The number of constants:
!
      integer(ip)           nconst
      parameter           ( nconst = 1 )
!
!     Local variables
!
      real(wp)              dcoeff
      real(wp)              descr(ndescr)

      integer(ip)           ncoeff
      integer(ip)           pktsiz
!
!     Standard SPICELIB error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'SPK14B' )
      end if
!
!     First, check the degree of the polynomial to be sure that it is
!     not negative.
!
      if ( chbdeg .lt. 0 ) then

         call setmsg ( 'The degree of the Chebyshev Polynomial'  // &
                       ' was negative, #. The degree of the'     // &
                       ' polynomial must be greater than or'     // &
                       ' equal to zero.'                          )
         call errint ( '#', chbdeg                                )
         call sigerr ( 'SPICE(INVALIDARGUMENT)'                   )
         call chkout ( 'SPK14B'                                   )
         return

      end if
!
!     Create a descriptor for the segment we are about to write.
!
      call spkpds ( body,  center, frame, type, first, last, descr )

      if ( failed() ) then

         call chkout ( 'SPK14B' )
         return

      end if
!
!     We've got a valid descriptor, so compute a few things and begin
!     the segment.
!
      ncoeff = chbdeg + 1
      pktsiz = nstate * ncoeff + 2
      dcoeff = real( ncoeff, wp )
!
!     For this data type, we want to use an explicit reference value
!     index where the reference epochs are in increasing order. We also
!     want to have as the index for a particular request epoch the index
!     of the greatest reference epoch less than or equal to the request
!     epoch. These characteristics are prescribed by the mnemonic EXPLE.
!     See the include file 'sgparam.inc' for more details.
!
      call sgbwfs ( handle, descr,  segid,  nconst, &
                            dcoeff, pktsiz, exple   )

!
!     No need to check FAILED() here, since all we do is check out.
!     Leave it up to the caller.
!

      call chkout ( 'SPK14B' )
      return

      end
