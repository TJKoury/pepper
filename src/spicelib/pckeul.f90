!$Procedure PCKEUL ( PCK, get Euler angles at time from PCK file )

      subroutine pckeul ( body, et, found, ref, eulang )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     This routine is obsolete. It supports only the type 02 binary
!     PCK format. It is maintained only for backward compatibility
!
!     Return Euler angles and their derivatives and their reference
!     frame, given an input time and body and reference frame from
!     a PCK binary file.
!
! $ Required_Reading
!
!     NAIF_IDS
!     ROTATION
!     TIME
!     PCK
!
! $ Keywords
!
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      integer(ip)          body
      real(wp)             et
      logical              found
      character(*)         ref
      real(wp)             eulang   ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of body
!     ET         I   Epoch of transformation
!     FOUND      O   .TRUE. if ET, BODY found in a PCK file
!     REF        O   Name of inertial ref. frame of state
!     EULANG     O   Euler angles and their derivatives.
!
! $ Detailed_Input
!
!     BODY     is the integer ID code of the body for which the
!              state transformation matrix is requested. Bodies
!              are numbered according to the standard NAIF
!              numbering scheme. The numbering scheme is
!              explained in the NAIF_IDS required reading file.
!
!     ET       is the epoch at which the state transformation
!              matrix is requested.
!
! $ Detailed_Output
!
!     FOUND    if the Euler angles for the requested time
!              and body are found in a PCK binary file,
!              FOUND is .TRUE. Otherwise, it's false.
!
!     REF      is the name of an inertial ref. frame.
!              (See the routine CHGIRF for a full list of names.)
!
!     EULANG   is the Euler angles and their derivatives at
!              time ET. The rotation matrix is
!              [ EULANG(3) ]  [EULANG(2)] [EULANG(1)]
!                           3            1           3
!
!              and   dEULANG(1)/dt = EULANG(4)
!                    dEULANG(2)/dt = EULANG(5)
!                    dEULANG(3)/dt = EULANG(6)
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
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     Here we load a binary PCK files and use PCKEUL to get the
!     Euler angles.
!
!     C
!     C  Load binary PCK file.
!     C
!        CALL PCKLOF ('example.pck', HANDLE)
!
!     C  Call routine to get Euler angles phi, delta, w.
!
!        CALL PCKEUL ( BODY, ET, FOUND, REF, EULANG )
!
!     The Euler angles and their derivatives are returned
!     in EULANG.
!
! $ Restrictions
!
!     1)  A binary PCK kernel must be loaded with PCKLOF before
!         calling this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 2.0.0, 21-MAR-1995 (KSZ)
!
!        PCKEUL modified to check in. PCKMAT takes
!        over for PCKEUL in many cases. REF now a character.
!
!-    SPICELIB Version 1.1.0, 18-OCT-1994 (KSZ)
!
!        Fixed bug which incorrectly modded DW by two pi.
!
!-    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ)
!
!-&


! $ Index_Entries
!
!     get Euler angles and their derivatives
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Parameters
!
!     ND    number of double precision components of descriptor
!     NI    number of integer components of descriptor
!     NR    component number of reference frame in integer
!           portion of descriptor
!     NS    size of a packed PCK segment descriptor
!     NT    component number of data type in integer portion
!           of descriptor
!
      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 5

      integer(ip), parameter :: nr = 2

      integer(ip), parameter :: ns = 5

      integer(ip), parameter :: nt = 3

      integer(ip), parameter :: filen = 128

!
!  Local Variables
!

      character(40)   ident

      real(wp)              dcd     ( nd )
      real(wp)              descr   ( ns )
      real(wp)              record  ( 130 )

      integer(ip) type
      integer(ip) handle
      integer(ip) icd ( ni )
      integer(ip) iref

!
!     Standard SPICE Error handling.
!
      if ( return () ) return

!
!     Get a segment applicable to a specified body and epoch.
!
      call pcksfs ( body, et, handle, descr, ident, found )

      if ( found ) then

!
!        Look at parts of the descriptor.
!
         call dafus ( descr, nd, ni, dcd, icd )
         type   = icd( nt )
         iref   = icd( nr )
         call irfnam ( iref, ref )

         if ( type == 2 ) then

!
!           Read in Chebyshev coefficients from segment.
!
            call pckr02 ( handle, descr, et, record )

!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke02 ( et, record, eulang )

         else
!
!           If appropriate data was not found, found is false.
!
            found = .false.

         end if

      end if

      return
      end
