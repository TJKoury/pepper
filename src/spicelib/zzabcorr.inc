
! $ Abstract
!
!     Include file zzabcorr.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines.  Users should not include this file directly due
!     to the volatile nature of this file
!
!     The parameters below define the structure of an aberration
!     correction attribute block.
!
! $ Parameters
!
!     An aberration correction attribute block is an array of logical
!     flags indicating the attributes of the aberration correction
!     specified by an aberration correction string.  The attributes
!     are:
!
!        - Is the correction "geometric"?
!
!        - Is light time correction indicated?
!
!        - Is stellar aberration correction indicated?
!
!        - Is the light time correction of the "converged
!          Newtonian" variety?
!
!        - Is the correction for the transmission case?
!
!        - Is the correction relativistic?
!
!    The parameters defining the structure of the block are as
!    follows:
!
!       NABCOR    Number of aberration correction choices.
!
!       ABATSZ    Number of elements in the aberration correction
!                 block.
!
!       GEOIDX    Index in block of geometric correction flag.
!
!       LTIDX     Index of light time flag.
!
!       STLIDX    Index of stellar aberration flag.
!
!       CNVIDX    Index of converged Newtonian flag.
!
!       XMTIDX    Index of transmission flag.
!
!       RELIDX    Index of relativistic flag.
!
!    The following parameter is not required to define the block
!    structure, but it is convenient to include it here:
!
!       CORLEN    The maximum string length required by any aberration
!                 correction string
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&


!     Number of aberration correction choices:
!
      INTEGER               NABCOR
      PARAMETER           ( NABCOR = 15 )

!
!     Aberration correction attribute block size
!     (number of aberration correction attributes):
!
      INTEGER               ABATSZ
      PARAMETER           ( ABATSZ = 6 )

!
!     Indices of attributes within an aberration correction
!     attribute block:
!
      INTEGER               GEOIDX
      PARAMETER           ( GEOIDX = 1 )

      INTEGER               LTIDX
      PARAMETER           ( LTIDX  = GEOIDX + 1 )

      INTEGER               STLIDX
      PARAMETER           ( STLIDX = LTIDX  + 1 )

      INTEGER               CNVIDX
      PARAMETER           ( CNVIDX = STLIDX + 1 )

      INTEGER               XMTIDX
      PARAMETER           ( XMTIDX = CNVIDX + 1 )

      INTEGER               RELIDX
      PARAMETER           ( RELIDX = XMTIDX + 1 )

!
!     Maximum length of an aberration correction string:
!
      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 5 )

!
!     End of include file zzabcorr.inc
!
