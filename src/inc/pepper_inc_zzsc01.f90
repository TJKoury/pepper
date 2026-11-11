!> Include file for SPICE-private declarations of SC01.
!>
!>     SPICE private file intended solely for the support of SPICE
!>     routines. Users should not include this file directly due
!>     to the volatile nature of this file.
!>
!>    Parameters copied from sclk.inc:
!>
!>## Version
!>
!>-    Version 1.0.0, 11-NOV-2020
!>
!>        NOTE: these declarations are duplicated in the include file
!>        sclk.inc. Those declarations must be kept in sync with
!>        these ones.
module pepper_inc_zzsc01

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: ndelim = 5  !! Number of supported SCLK field delimiters:

   character(ndelim), parameter :: delims = '.:-, '  !! Supported SCLK string field delimiters:

   integer(ip), parameter :: tdb = 1  !! Time system codes

   integer(ip), parameter :: tdt = 2

   integer(ip), parameter :: mxpart = 9999  !! Maximum number of partitions:

   integer(ip), parameter :: mxcoef = 100000  !! Maximum number of coefficient records:

   integer(ip), parameter :: mxnfld = 10  !! Maximum number of fields in an SCLK string:

   integer(ip), parameter :: dplen = 30  !! Length of strings used to represent D.P. numbers:

!
!     End of duplicated declarations.
!

   integer(ip), parameter :: ixnfld = 1  !! Indices of integer data items, relative to base index into
                                         !! the integer data buffer:
                                         !!
                                         !! Index of number of fields

   integer(ip), parameter :: ixdlim = ixnfld + 1  !! Index of delimiter code
   integer(ip), parameter :: ixtsys = ixdlim + 1  !! Index of time system code

   integer(ip), parameter :: ixncof = ixtsys + 1  !! Index of coefficient count. The count is 3 x the number of
                                                  !! coefficient records.

   integer(ip), parameter :: ixnprt = ixncof + 1  !! Index of the number of partitions

   integer(ip), parameter :: ixbcof = ixnprt + 1  !! Index of the base index in the double precision buffer of the
                                                  !! coefficient set

   integer(ip), parameter :: ixbstr = ixbcof + 1  !! Index of the base index in the double precision buffer of the
                                                  !! partition start times

   integer(ip), parameter :: ixbend = ixbstr + 1  !! Index of the base index in the double precision buffer of the
                                                  !! partition end times

   integer(ip), parameter :: ixbmod = ixbend + 1  !! Index of the base index in the double precision buffer of the
                                                  !! SCLK field moduli

   integer(ip), parameter :: ixboff = ixbmod + 1  !! Index of the base index in the double precision buffer of the
                                                  !! SCLK field offsets

   integer(ip), parameter :: nivals = ixboff  !! Number of integer values per clock

!
!     Data structure parameters
!

   integer(ip), parameter :: mxnclk = 100  !! Maximum number of clocks

   integer(ip), parameter :: dbufsz = ( 3 * mxcoef ) &
      + ( 2 * mxpart ) &
      + ( 2 * mxnfld )
      !! DP buffer size
      !!
      !! The buffer is large enough to hold data for one clock having the
      !! maximum amount of data.

   integer(ip), parameter :: ibufsz = nivals * mxnclk  !! Integer buffer size

   integer(ip), parameter :: lbsngl = -5  !! Lower bound of control area of singly linked list:

   integer(ip), parameter :: cplsiz = mxnclk - lbsngl + 1  !! The add-only hash pool for frame IDs is singly linked.

end module pepper_inc_zzsc01
