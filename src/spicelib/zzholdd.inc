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
      INTEGER               ZZGET
      PARAMETER           ( ZZGET     = -1 )

      INTEGER               ZZPUT
      PARAMETER           ( ZZPUT     = -2 )

      INTEGER               ZZRESET
      PARAMETER           ( ZZRESET   = -3 )

!
!     Current number of OP codes.
!
      INTEGER               ZZNOP
      PARAMETER           ( ZZNOP     =  3 )


!
!     ID codes. The values exist in the integer domain
!     [ 1, NID],
!

!
!     General use, primarily testing.
!
      INTEGER               GEN
      PARAMETER           ( GEN       = 1 )

!
!     The user defined GF reference value.
!
      INTEGER               GF_REF
      PARAMETER           ( GF_REF    = 2 )

!
!     The user defined GF convergence tolerance.
!
      INTEGER               GF_TOL
      PARAMETER           ( GF_TOL    = 3 )

!
!     The user defined GF step for numeric differentiation.
!
      INTEGER               GF_DT
      PARAMETER           ( GF_DT     = 4 )

!
!     Current number of ID codes, dimension of array
!     in ZZHOLDD. Bad things can happen if this parameter
!     does not have the proper value.
!
      INTEGER               NID
      PARAMETER           ( NID       = 4 )


!
!     End of file zzholdd.inc.
!
