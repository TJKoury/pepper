!$Procedure   ZZEKQINI ( Private: EK, initialize encoded query )

      subroutine zzekqini ( isize, dsize, eqryi, eqryc, eqryd )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize encoded EK query.
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
      include 'ekqlimit.inc'
      include 'ekbool.inc'

      integer(ip), parameter :: lbcell = -5

      integer(ip)           isize
      integer(ip)           dsize
      integer(ip)           eqryi ( lbcell : * )
      character(*)          eqryc
      real(wp)              eqryd ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ISIZE      I   Size of integer component of encoded query.
!     DSIZE      I   Size of d.p. component of encoded query.
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     EQRYD     I-O  D.p. component of query.
!
! $ Detailed_Input
!
!     ISIZE          is the size of the cell comprising the integer
!                    component of the encoded query.
!
!     DSIZE          is the size of the array comprising the double
!                    precision component of the encoded query.
!
!     EQRYI          is an integer array that is to serve as the
!                    integer portion of an encoded EK query.  EQRYI
!                    will be initialized as an integer cell having
!                    size ISIZE.
!
!     EQRYC          is a character string that is to serve as
!                    the character portion of an encoded EK query.
!                    EQRYC will be set to blank.
!
!     EQRYD          is a double precision array that is to serve as
!                    the numeric portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI,
!     EQRYD,
!     EQRYC          are the components of an initialized EK query.
!                    The query's architecture code will be set to 1.
!                    All counts will be set to zero.  The array
!                    EQRYD will be zeroed out.  The character string
!                    EQRYC will be blank.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) ISIZE must be large enough to accommodate a query with the
!        maximum number of tables, constraints, and tokens, and
!        indicated by the parameters in ekqlimit.inc.  If ISIZE is
!        too small, the error SPICE(CELLTOOSMALL) will be signaled.
!
!     2) DSIZE must be large enough to accommodate the largest number
!        of numeric tokens that can occur in a query.  If DSIZE is
!        too small, the error SPICE(CELLTOOSMALL) will be signaled.
!
!     3) EQRYC must be long enough to accommodate all of the character
!        data that can occur in a query.  If EQRYC is too short, the
!        error  SPICE(STRINGTOOSHORT) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See ZZEKPARS.
!
! $ Restrictions
!
!     1) This routine is private to the EK library.  No routines
!        external to the EK library should call this routine.
!
!     2) Uses EK encoded query architecture version 1.
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
!
!     Minimum upper bound for the integer cell of an encoded query:
!
      integer(ip)           mnisiz
      parameter           ( mnisiz =     eqvbas &
                                      +  maxtab * eqvdsz * 2 &
                                      +  maxcon * eqcdsz &
                                      +  maxcon &
                                      +  maxord * eqodsz &
                                      +  maxsel * eqsdsz )

!
!     Local variables
!
      integer(ip)           i


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Check sizes:
!
      if ( isize < mnisiz ) then

         call setmsg ( 'Size of integer component of encoded query ' // &
                       'is #; at least # elements are required.'      )
         call errint ( '#',   isize                                   )
         call errint ( '#',   mnisiz                                  )
         call sigerr ( 'SPICE(CELLTOOSMALL)'                          )
         return

      end if


      if ( dsize < maxqnm ) then

         call setmsg ( 'Size of d.p. component of encoded query ' // &
                       'is #; at least # elements are required.'      )
         call errint ( '#',   dsize                                   )
         call errint ( '#',   maxqnm                                  )
         call sigerr ( 'SPICE(CELLTOOSMALL)'                          )
         return

      end if


      if ( len(eqryc) < maxcln ) then

         call setmsg ( 'Size of character component of encoded query '// &
                       'is #; a length of at least # characters is '  // &
                       'required.'                                    )
         call errint ( '#',   len(eqryc)                              )
         call errint ( '#',   maxcln                                  )
         call sigerr ( 'SPICE(STRINGTOOSHORT)'                        )
         return

      end if

!
!     Initialize the integer cell, the d.p. array, and the string.
!
      call ssizei ( isize, eqryi )
      call cleard ( dsize, eqryd )
      eqryc = ' '

!
!     Append enough elements to the integer cell to contain the
!     fixed-size portion of the encoded query:
!
      do i = 1, eqvbas
         call appndi ( 0, eqryi )
      end do

!
!     Clear out the fixed-size portion of the integer cell.
!
      call cleari ( eqvbas, eqryi(1) )

!
!     Fill in the architecture version.
!
      eqryi( eqarch )  =  1

!
!     Set the parse completion and name and time resolution flags to
!     indicate `not done':
!
      eqryi(eqpars)    =  ifalse
      eqryi(eqnres)    =  ifalse
      eqryi(eqtres)    =  ifalse

!
!     Set the buffer sizes:
!
      eqryi(eqcsiz)    =  len(eqryc)
      eqryi(eqnsiz)    =  dsize

!
!     Set the free pointers:
!
      eqryi(eqnptr)    =  1
      eqryi(eqcptr)    =  1

!
!     Indicate that initialization has been done:
!
      eqryi(eqinit)    =  itrue

      return
      end
