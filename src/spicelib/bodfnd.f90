!$Procedure BODFND ( Find values from the kernel pool )

      logical function bodfnd ( body, item )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether values exist for some item for any body
!     in the kernel pool.
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

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of body.
!     ITEM       I   Item to find ('RADII', 'NUT_AMP_RA', etc.).
!
!     The function returns .TRUE. if ITEM is in the kernel pool, .FALSE.
!     otherwise.
!
! $ Detailed_Input
!
!     BODY     is the ID code of the body for which the item is
!              requested.
!
!     ITEM     is the item to be returned. Together, the body and item
!              name combine to form a variable name, e.g.,
!
!                    'BODY599_RADII'
!                    'BODY4_POLE_RA'
!
! $ Detailed_Output
!
!     The function returns .TRUE. if ITEM is in the kernel pool, and
!     .FALSE. if it is not.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     BODVCD, which returns values from the kernel pool, causes an
!     error to be signaled whenever the specified item is not found.
!     In many cases, this is appropriate. However, sometimes the
!     program may attempt to recover, by providing default values,
!     prompting for replacements, and so on.
!
! $ Examples
!
!     In the following example, default values are substituted for
!     bodies for which axes are not found.
!
!        IF ( BODFND ( TARGET, 'RADII' ) ) THEN
!           CALL BODVCD ( TARGET, 'RADII', 3, N, RADII )
!        ELSE
!           CALL VPACK ( 100.D0, 100.D0, 100.D0, RADII )
!        END IF
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Updated
!        input argument BODY detailed description. Added SPK and PCK to
!        the list of required readings.
!
!-    SPICELIB Version 1.2.1, 24-OCT-2005 (NJB)
!
!        Header update: calls to BODVAR in example code were replaced
!        with calls to BODVCD. The string 'AXES' and variable AXES
!        were replaced with the string 'RADII' and variable 'RADII'
!        throughout the header.
!
!-    SPICELIB Version 1.2.0, 15-MAR-2002 (NJB)
!
!        Bug fix: routine was updated to work with string-valued
!        kernel variables.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     find constants for a body in the kernel pool
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip)           vnmlen
      parameter           ( vnmlen = 32 )

      integer(ip)           numlen
      parameter           ( numlen = 16 )

!
!     Local variables
!
      character(1)          dtype
      character(numlen)     code
      character(vnmlen)     varnam

      integer(ip)           n

      logical               found



!
!     Standard SPICE error handling.
!
      if ( return () ) then
        bodfnd = .false.
        return
      else
        call chkin ( 'BODFND' )
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
!     Search the kernel pool for the item.
!
      call dtpool ( varnam, found, n, dtype )

!
!     Was anything there?
!
      bodfnd = found

      call chkout ( 'BODFND' )
      return
      end
