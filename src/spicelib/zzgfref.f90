!$Procedure ZZGFREF ( Private --- GF, update REFVAL )

      subroutine zzgfref( refval )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set reference value in the GF sub-system.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     STORE_VALUE
!     GEOMETRY
!
! $ Declarations

      implicit none

      include 'zzholdd.inc'

      double precision      refval

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ZZPUT      P   ZZHOLDD stores a DP value.
!     GF_REF     P   ZZHOLDD acts on the reference value
!                    for a GF search.
!     REFVAL     I   The value to set as the reference value.
!
! $ Detailed_Input
!
!     REFVAL     the double precision scalar value to set as the
!                reference value for a geometry finder search.
!
! $ Detailed_Output
!
!    None.
!
! $ Parameters
!
!    None.
!
! $ Exceptions
!
!    None.
!
! $ Files
!
!    None.
!
! $ Particulars
!
!    This routine wraps a put call to ZZHOLDD for the GF_REF
!    value.
!
! $ Examples
!
!    None.
!
! $ Restrictions
!
!    None.
!
! $ Literature_References
!
!    None.
!
! $ Author_and_Institution
!
!    E.D. Wright    (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW)
!
!       Change in ZZHOLDD functionality required edit to ZZHOLDD
!       call and inclusion of "zzholdd.inc" parameters file.
!
!-   SPICELIB Version 1.0.0  28-NOV-2009 (EDW)
!
!-&

! $ Index_Entries
!
!    store a double precision reference value
!
!-&

!
!     Local Variables
!

      logical               ok

!
!     Store the REFVAL value for use in ZZGFUDLT.
!
      call zzholdd ( zzput, gf_ref, ok, refval )

      end

