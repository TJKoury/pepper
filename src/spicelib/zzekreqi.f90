!$Procedure   ZZEKREQI ( Private: EK, read from encoded query, integer )

      subroutine zzekreqi ( eqryi, name, value )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read scalar integer value from encoded EK query.
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

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               eqryi ( lbcell : * )
      character*(*)         name
      integer               value

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     NAME       I   Name of scalar item to read.
!     VALUE      O   Value of item.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!
!     NAME           is the name of the item whose value is to be read.
!                    This item is some element of the integer portion
!                    of an encoded query.
!
! $ Detailed_Output
!
!     VALUE          is the integer value designated by NAME.
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
!     This routine is the inverse of ZZEKWEQI.
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
      integer               isrchc

!
!     Local parameters
!
      integer               namlen
      parameter           ( namlen = 32 )

      integer               maxnam
      parameter           ( maxnam = 15 )
!
!     Local variables
!
      character*(namlen)    namlst ( maxnam )
      character*(namlen)    tmpnam

      integer               i
      integer               namidx ( maxnam )

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

      if ( i .eq. 0 ) then

         call chkin  ( 'ZZEKREQI'           )
         call setmsg ( 'Item # not found.'  )
         call errch  ( '#',  name           )
         call sigerr ( 'SPICE(INVALIDNAME)' )
         call chkout ( 'ZZEKREQI'           )
         return

      end if

!
!     Do the deed.
!
      value  =  eqryi( namidx(i) )

      return
      end
