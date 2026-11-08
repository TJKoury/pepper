!$Procedure   ZZEKWEQI ( Private: EK, write to encoded query, integer )

      subroutine zzekweqi ( name, value, eqryi )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Write scalar integer value to encoded EK query.
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

      integer(ip), parameter :: lbcell = -5

      character(*)          name
      integer(ip)           value
      integer(ip)           eqryi ( lbcell : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of scalar item to write.
!     VALUE      I   Value to write.
!     EQRYI     I-O  Integer component of query.
!
! $ Detailed_Input
!
!     NAME           is the name of the item whose value is to be set.
!                    This item is some element of the integer portion
!                    of an encoded query.  The currently supported set
!                    of names is:
!
!                       ARCHITECTURE
!                       INITIALIZED
!                       PARSED
!                       NAMES_RESOLVED
!                       TIMES_RESOLVED
!                       SEM_CHECKED
!                       NUM_TABLES
!                       NUM_CONJUNCTIONS
!                       NUM_CONSTRAINTS
!                       NUM_SELECT_COLS
!                       NUM_ORDERBY_COLS
!                       NUM_BUF_SIZE
!                       FREE_NUM
!                       CHR_BUF_SIZE
!                       FREE_CHR
!
!     VALUE          is an integer value to assign to the quantity
!                    designated by NAME.
!
!     EQRYI          is the integer portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query,
!                    updated to reflect the requested assignment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input name is not recognized, the error
!         SPICE(INVALIDNAME) is signaled.  The encoded query is not
!         modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is intended to hide from calling routines the
!     specifics of the EK encoded query structure.  See the include
!     file ekquery.inc if details of this structure are desired.
!
! $ Examples
!
!     See ZZEKNRES.
!
! $ Restrictions
!
!     1) Uses EK encoded query architecture version 2.
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
!-    Beta Version 1.0.0, 01-AUG-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           isrchc

!
!     Local parameters
!
      integer(ip), parameter :: namlen = 32

      integer(ip), parameter :: maxnam = 15
!
!     Local variables
!
      character(namlen)     namlst ( maxnam )
      character(namlen)     tmpnam

      integer(ip)           i
      integer(ip)           namidx ( maxnam )

!
!     Saved variables
!
      save

!
!     Initial values
!
      data  (   namlst(i),          namidx(i),   i = 1, maxnam  )  / &
                 'ARCHITECTURE',      eqarch, &
                 'INITIALIZED',       eqinit, &
                 'PARSED',            eqpars, &
                 'NAMES_RESOLVED',    eqnres, &
                 'TIMES_RESOLVED',    eqtres, &
                 'SEM_CHECKED',       eqschk, &
                 'NUM_TABLES',        eqntab, &
                 'NUM_CONJUNCTIONS',  eqncnj, &
                 'NUM_CONSTRAINTS',   eqncns, &
                 'NUM_SELECT_COLS',   eqnsel, &
                 'NUM_ORDERBY_COLS',  eqnord, &
                 'NUM_BUF_SIZE',      eqnsiz, &
                 'FREE_NUM',          eqnptr, &
                 'CHR_BUF_SIZE',      eqcsiz, &
                 'FREE_CHR',          eqcptr                       /


!
!     Use discovery check-in.
!
!
!     Find the location of the named item.
!
      call ljust ( name,   tmpnam )
      call ucase ( tmpnam, tmpnam )

      i  =  isrchc ( tmpnam, maxnam, namlst )

      if ( i == 0 ) then

         call setmsg ( 'Item # not found.'  )
         call errch  ( '#',  name           )
         call sigerr ( 'SPICE(INVALIDNAME)' )
         return

      end if

!
!     Do the deed.
!
      eqryi( namidx(i) ) = value

      return
      end
