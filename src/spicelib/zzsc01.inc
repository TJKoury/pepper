!
!     Include file zzsclk01.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines. Users should not include this file directly due
!     to the volatile nature of this file.
!
!     Include file for SPICE-private declarations of SC01.
!
!-    Version 1.0.0, 11-NOV-2020
!

!
!    Parameters copied from sclk.inc:
!
!        NOTE: these declarations are duplicated in the include file
!        sclk.inc. Those declarations must be kept in sync with
!        these ones.
!
!
!     Number of supported SCLK field delimiters:
!
      INTEGER               NDELIM
      PARAMETER           ( NDELIM = 5 )

!
!     Supported SCLK string field delimiters:
!
      CHARACTER*(NDELIM)    DELIMS
      PARAMETER           ( DELIMS = '.:-, ' )

!
!     Time system codes
!
      INTEGER               TDB
      PARAMETER           ( TDB    =      1 )

      INTEGER               TDT
      PARAMETER           ( TDT    =      2 )

!
!     Maximum number of partitions:
!
      INTEGER               MXPART
      PARAMETER           ( MXPART = 9999 )

!
!     Maximum number of coefficient records:
!
      INTEGER               MXCOEF
      PARAMETER           ( MXCOEF = 100000 )

!
!     Maximum number of fields in an SCLK string:
!
      INTEGER               MXNFLD
      PARAMETER           ( MXNFLD = 10 )

!
!     Length of strings used to represent D.P. numbers:
!
      INTEGER               DPLEN
      PARAMETER           ( DPLEN = 30 )

!
!     End of duplicated declarations.
!

!
!     Indices of integer data items, relative to base index into
!     the integer data buffer:
!
!
!     Index of number of fields
!
      INTEGER               IXNFLD
      PARAMETER           ( IXNFLD = 1 )

!
!     Index of delimiter code
!
      INTEGER               IXDLIM
      PARAMETER           ( IXDLIM = IXNFLD + 1 )
!
!     Index of time system code
!
      INTEGER               IXTSYS
      PARAMETER           ( IXTSYS = IXDLIM + 1 )

!
!     Index of coefficient count. The count is 3 x the number of
!     coefficient records.
!
      INTEGER               IXNCOF
      PARAMETER           ( IXNCOF = IXTSYS + 1 )

!
!     Index of the number of partitions
!
      INTEGER               IXNPRT
      PARAMETER           ( IXNPRT = IXNCOF + 1 )

!
!     Index of the base index in the double precision buffer of the
!     coefficient set
!
      INTEGER               IXBCOF
      PARAMETER           ( IXBCOF = IXNPRT + 1 )

!
!     Index of the base index in the double precision buffer of the
!     partition start times
!
      INTEGER               IXBSTR
      PARAMETER           ( IXBSTR = IXBCOF + 1 )

!
!     Index of the base index in the double precision buffer of the
!     partition end times
!
      INTEGER               IXBEND
      PARAMETER           ( IXBEND = IXBSTR + 1 )

!
!     Index of the base index in the double precision buffer of the
!     SCLK field moduli
!
      INTEGER               IXBMOD
      PARAMETER           ( IXBMOD  = IXBEND + 1 )

!
!     Index of the base index in the double precision buffer of the
!     SCLK field offsets
!
      INTEGER               IXBOFF
      PARAMETER           ( IXBOFF   = IXBMOD + 1 )

!
!     Number of integer values per clock
!
      INTEGER               NIVALS
      PARAMETER           ( NIVALS = IXBOFF )


!
!     Data structure parameters
!

!
!     Maximum number of clocks
!
      INTEGER               MXNCLK
      PARAMETER           ( MXNCLK = 100 )

!
!     DP buffer size
!
!     The buffer is large enough to hold data for one clock having the
!     maximum amount of data.
!
      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ =   ( 3 * MXCOEF ) &
                                     + ( 2 * MXPART ) &
                                     + ( 2 * MXNFLD ) )

!
!     Integer buffer size
!
      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = NIVALS * MXNCLK )

!
!     Lower bound of control area of singly linked list:
!
      INTEGER               LBSNGL
      PARAMETER           ( LBSNGL = -5 )

!
!     The add-only hash pool for frame IDs is singly linked.
!
      INTEGER               CPLSIZ
      PARAMETER           ( CPLSIZ = MXNCLK - LBSNGL + 1 )

!
!     End include file zzsc01.inc
!
