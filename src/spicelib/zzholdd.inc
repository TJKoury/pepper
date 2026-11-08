! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This file contains parameter declarations for the ZZHOLDD
!     routine.
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
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     GEN       general value, primarily for testing.
!
!     GF_REF    user defined GF reference value.
!
!     GF_TOL    user defined GF convergence tolerance.
!
!     GF_DT     user defined GF step for numeric differentiation.
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
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0  03-DEC-2013 (EDW)
!
!-&


!
!     OP codes. The values exist in the integer domain
!     [ -ZZNOP, -1],
!
      integer               zzget
      parameter           ( zzget     = -1 )

      integer               zzput
      parameter           ( zzput     = -2 )

      integer               zzreset
      parameter           ( zzreset   = -3 )

!
!     Current number of OP codes.
!
      integer               zznop
      parameter           ( zznop     =  3 )


!
!     ID codes. The values exist in the integer domain
!     [ 1, NID],
!

!
!     General use, primarily testing.
!
      integer               gen
      parameter           ( gen       = 1 )

!
!     The user defined GF reference value.
!
      integer               gf_ref
      parameter           ( gf_ref    = 2 )

!
!     The user defined GF convergence tolerance.
!
      integer               gf_tol
      parameter           ( gf_tol    = 3 )

!
!     The user defined GF step for numeric differentiation.
!
      integer               gf_dt
      parameter           ( gf_dt     = 4 )

!
!     Current number of ID codes, dimension of array
!     in ZZHOLDD. Bad things can happen if this parameter
!     does not have the proper value.
!
      integer               nid
      parameter           ( nid       = 4 )


!
!     End of file zzholdd.inc.
!
