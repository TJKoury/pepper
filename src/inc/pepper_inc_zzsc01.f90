!> The parameters of SPICELIB's include file zzsc01.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_zzsc01

   implicit none

   public

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
   integer               ndelim
   parameter           ( ndelim = 5 )

!
!     Supported SCLK string field delimiters:
!
   character(ndelim), parameter :: delims = '.:-, '

!
!     Time system codes
!
   integer               tdb
   parameter           ( tdb    =      1 )

   integer               tdt
   parameter           ( tdt    =      2 )

!
!     Maximum number of partitions:
!
   integer               mxpart
   parameter           ( mxpart = 9999 )

!
!     Maximum number of coefficient records:
!
   integer               mxcoef
   parameter           ( mxcoef = 100000 )

!
!     Maximum number of fields in an SCLK string:
!
   integer               mxnfld
   parameter           ( mxnfld = 10 )

!
!     Length of strings used to represent D.P. numbers:
!
   integer               dplen
   parameter           ( dplen = 30 )

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
   integer               ixnfld
   parameter           ( ixnfld = 1 )

!
!     Index of delimiter code
!
   integer               ixdlim
   parameter           ( ixdlim = ixnfld + 1 )
!
!     Index of time system code
!
   integer               ixtsys
   parameter           ( ixtsys = ixdlim + 1 )

!
!     Index of coefficient count. The count is 3 x the number of
!     coefficient records.
!
   integer               ixncof
   parameter           ( ixncof = ixtsys + 1 )

!
!     Index of the number of partitions
!
   integer               ixnprt
   parameter           ( ixnprt = ixncof + 1 )

!
!     Index of the base index in the double precision buffer of the
!     coefficient set
!
   integer               ixbcof
   parameter           ( ixbcof = ixnprt + 1 )

!
!     Index of the base index in the double precision buffer of the
!     partition start times
!
   integer               ixbstr
   parameter           ( ixbstr = ixbcof + 1 )

!
!     Index of the base index in the double precision buffer of the
!     partition end times
!
   integer               ixbend
   parameter           ( ixbend = ixbstr + 1 )

!
!     Index of the base index in the double precision buffer of the
!     SCLK field moduli
!
   integer               ixbmod
   parameter           ( ixbmod  = ixbend + 1 )

!
!     Index of the base index in the double precision buffer of the
!     SCLK field offsets
!
   integer               ixboff
   parameter           ( ixboff   = ixbmod + 1 )

!
!     Number of integer values per clock
!
   integer               nivals
   parameter           ( nivals = ixboff )

!
!     Data structure parameters
!

!
!     Maximum number of clocks
!
   integer               mxnclk
   parameter           ( mxnclk = 100 )

!
!     DP buffer size
!
!     The buffer is large enough to hold data for one clock having the
!     maximum amount of data.
!
   integer               dbufsz
   parameter           ( dbufsz =   ( 3 * mxcoef ) &
   + ( 2 * mxpart ) &
   + ( 2 * mxnfld ) )

!
!     Integer buffer size
!
   integer               ibufsz
   parameter           ( ibufsz = nivals * mxnclk )

!
!     Lower bound of control area of singly linked list:
!
   integer               lbsngl
   parameter           ( lbsngl = -5 )

!
!     The add-only hash pool for frame IDs is singly linked.
!
   integer               cplsiz
   parameter           ( cplsiz = mxnclk - lbsngl + 1 )

!
!     End include file zzsc01.inc
!

end module pepper_inc_zzsc01
