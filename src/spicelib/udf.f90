!$Procedure UDF ( Dummy function for UDFUNS )

      subroutine udf ( x, value )

! $ Abstract
!
!     Serve as a dummy function for GF routines expecting an UDFUNS
!     argument. It is a no-op routine with an argument signature
!     matching UDFUNS.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      double precision      x
      double precision      value

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   Double precision value, unused.
!     VALUE      I   Double precision value, unused.
!
! $ Detailed_Input
!
!     X        is a double precision value, unused.
!
!     VALUE    is a double precision value, unused.
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
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The routine performs no evaluations. It exists for routines
!     expecting an UDFUNS argument. In the cases where UDFUNC is
!     unneeded or unavailable, this routine provides a null operation
!     alternative.
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
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 21-OCT-2013 (EDW)
!
!-&


! $ Index_Entries
!
!     dummy function for UDFUNS signature arguments
!
!-&


      x     = x     + 0.d0
      value = value + 0.d0

      return

      end
