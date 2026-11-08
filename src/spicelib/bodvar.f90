!$Procedure BODVAR ( Return values from the kernel pool )

      subroutine bodvar ( body, item, dim, values )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Deprecated: This routine has been superseded by BODVCD and
!     BODVRD. This routine is supported for purposes of backward
!     compatibility only.
!
!     Return the values of some item for any body in the
!     kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!     PCK
!     SPK
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations

      implicit none

      integer(ip)           body
      character(*)          item
      integer(ip)           dim
      real(wp)              values  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of body.
!     ITEM       I   Item for which values are desired.
!     DIM        O   Number of values returned.
!     VALUES     O   Values.
!
! $ Detailed_Input
!
!     BODY     is the ID code of the body for which ITEM is
!              requested.
!
!     ITEM     is the item to be returned. Together, the body and
!              item name combine to form a variable name, e.g.,
!
!                 'BODY599_RADII'
!                 'BODY401_POLE_RA'
!
! $ Detailed_Output
!
!     DIM      is the number of values associated with the variable.
!
!     VALUES   are the values associated with the variable.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the requested item is not found, the error
!         SPICE(KERNELVARNOTFOUND) is signaled.
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
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Retrieve the Earth's radii values from the kernel pool
!
!        Use the PCK kernel below to load the required triaxial
!        ellipsoidal shape model for the Earth.
!
!           pck00008.tpc
!
!
!        Example code begins here.
!
!
!              PROGRAM BODVAR_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              LOGICAL               BODFND
!
!        C
!        C     Local constants.
!        C
!              INTEGER               BODYID
!              PARAMETER           ( BODYID = 399 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      RADII  (3)
!
!              INTEGER               DIM
!
!              LOGICAL               FOUND
!
!        C
!        C     Load a PCK file.
!        C
!              CALL FURNSH ( 'pck00008.tpc' )
!
!        C
!        C     Test if Earth's radii values exist in the
!        C     kernel pool.
!        C
!        C     The procedure searches for the kernel variable
!        C     BODY399_RADII.
!        C
!              FOUND = BODFND( BODYID, 'RADII' )
!
!        C
!        C     If found, retrieve the values.
!        C
!              IF ( FOUND ) THEN
!
!                 CALL BODVAR ( BODYID, 'RADII', DIM, RADII )
!
!                 WRITE(*,'(I3,A,3F11.3)') BODYID, ' RADII:', RADII(1),
!             .                            RADII(2), RADII(3)
!
!              ELSE
!
!                 WRITE(*,*) 'No RADII data found for object ', BODYID
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        399 RADII:   6378.140   6378.140   6356.750
!
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.6, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added
!        complete code example. Updated input argument BODY
!        detailed description. Added SPK to the list of required
!        readings.
!
!-    SPICELIB Version 1.0.5, 18-MAY-2010 (BVS)
!
!        Index lines now state that this routine is deprecated.
!
!-    SPICELIB Version 1.0.4, 27-OCT-2005 (NJB)
!
!        Routine is now deprecated.
!
!-    SPICELIB Version 1.0.3, 08-JAN-2004 (EDW)
!
!        Trivial typo corrected.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
!
!        Detailed Input section of the header was updated. The
!        description for the variable BODY was incorrect.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     DEPRECATED fetch constants for a body from the kernel pool
!     DEPRECATED physical constants for a body
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      character(16)         code
      character(32)         varnam
      logical               found



!
!     Standard SPICE error handling.
!
      if ( return () ) then
        return
      else
        call chkin ( 'BODVAR' )
      end if

!
!     Construct the variable name from BODY and ITEM.
!
      varnam = 'BODY'

      call intstr ( body, code )
      call suffix ( code, 0, varnam )
      call suffix ( '_',  0, varnam )
      call suffix ( item, 0, varnam )

!
!     Grab the items. Complain if they aren't there.
!
      call rtpool ( varnam, dim, values, found )

      if ( .not. found ) then
         call setmsg ( 'The variable # could not be found in the ' // &
                       'kernel pool.'                             )
         call errch  ( '#', varnam                                )
         call sigerr ( 'SPICE(KERNELVARNOTFOUND)'                 )
      end if

      call chkout ( 'BODVAR' )
      return
      end
