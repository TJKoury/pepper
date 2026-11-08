!$Procedure  ZZEKQCNJ ( Private: EK, read conjunction sizes from query )

      subroutine zzekqcnj ( eqryi, n, size )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read conjunction sizes from an encoded EK query.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekquery.inc'
      include 'ekbool.inc'

      integer(ip), parameter :: lbcell = -5

      integer(ip)           eqryi ( lbcell : * )
      integer(ip)           n
      integer(ip)           size

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     N          I   Index within FROM clause of table name to read.
!     SIZE       O   Size of Nth conjunction in WHERE clause.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     N              is the index, within the FROM clause of the query,
!                    of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     SIZE           is the size of the Nth conjunction of
!                    constraints in the input encoded query.  The size
!                    applies to the constraints after `normalization'.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     2)  If the input query has not been parsed, the error
!         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         conjunctions in the query, the error SPICE(INVALIDINDEX)
!         will be signaled.  The outputs will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_CONJUNCTIONS',  N  )
!
!     may be used to get the conjunction count from an encoded query.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKSRCH.
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           iparse
      integer(ip)           loc
      integer(ip)           ncnj
      integer(ip)           ncns
      integer(ip)           ntab

      call zzekreqi ( eqryi, 'PARSED', iparse )

      if ( failed() ) return

      if ( iparse == ifalse ) then

         call setmsg ( 'Encoded query has not yet been parsed.'  )
         call sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( eqryi, 'NUM_TABLES',         ntab   )
      call zzekreqi ( eqryi, 'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( eqryi, 'NUM_CONSTRAINTS',    ncns   )

      if (  ( n < 1 ) .or. ( n > ncnj )  ) then

         call setmsg ( 'Table index # is out of valid range 1:#.'  )
         call errint ( '#',  n                                     )
         call errint ( '#',  ncnj                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

!
!     Compute the location of the requested conjunction size value.
!
      loc    =     eqvbas &
                 + ntab  *  2  *  eqvdsz &
                 + ncns        *  eqcdsz &
                 + n

      size   =  eqryi ( loc )

      return
      end
