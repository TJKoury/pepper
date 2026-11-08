!$Procedure ZZSCIN01 ( Private, SCLK database initialization, type 01 )

      subroutine zzscin01 ( hdsclk, scpool, clklst, &
                            dpfree, ifree,  prvsc  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize the SCLK type 1 database.
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

      include 'zzsc01.inc'

      integer(ip)           hdsclk ( * )
      integer(ip)           scpool ( lbsngl : * )
      integer(ip)           clklst ( * )
      integer(ip)           dpfree
      integer(ip)           ifree
      integer(ip)           prvsc

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HDSCLK     O   Array of heads of hash collision lists.
!     SCPOOL     O   Singly linked collision list pool.
!     CLKLST     O   Array of SCLK codes.
!     DPFREE     O   Index of first free entry in d.p. data buffer.
!     IFREE      O   Index of first free entry in integer data buffer.
!     PRVSC      O   Previous SCLK ID code.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     HDSCLK      is an array containing the head node indices of the
!                 SCLK ID collision lists stored in SCPOOL. On output,
!                 this array is zero-filled.
!
!     SCPOOL      is a singly linked list pool containing collision
!                 lists for sets of SCLK IDs that have the same hash
!                 values. On output, this data structure has the
!                 initial state set by ZZHSIINI.
!
!     CLKLST      is an array of SCLK IDs, which is parallel to the
!                 data portion of SCPOOL. Each allocated node of
!                 SCPOOL is the index in CLKLST of the associated
!                 SCLK ID. On output, this array is zero-filled.
!
!     DPFREE      is the index in the database's double precision
!                 buffer of the first free element. On output, this
!                 index is 1.
!
!     IFREE       is index in the database's integer buffer of the
!                 first free element. On output, this index is 1.
!
!     PRVSC       is the previous ID code passed to the SC01 subsystem.
!                 On output, this code is set to 0, which is never a
!                 valid SCLK ID code.
!
! $ Parameters
!
!     See the include file zzsc01.inc.
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
!     None.
!
! $ Examples
!
!     None. See usage in SC01.
!
! $ Restrictions
!
!     This is a SPICE-private routine. It should not
!     be called directly by user application code.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize sclk type 1 database
!
!-&

! $ Revisions
!
!     None.
!`
!-&

!
!     This routine must execute when SPICE error handling code is in
!     the process of responding to an error, so this routine doesn't
!     participate in SPICE error handling or tracing. The routines
!     called here are error-free.
!
!     Initialize SC01 data structures.
!
!     ZZHSIINI won't signal an error as long as the parameter MXNCLK
!     is positive, so we don't check FAILED here.
!
      call zzhsiini ( mxnclk, hdsclk, scpool )
      call cleari   ( mxnclk, clklst         )

      dpfree = 1
      ifree  = 1
      prvsc  = 0

      return
      end
