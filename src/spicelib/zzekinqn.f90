!$Procedure   ZZEKINQN ( Private: EK, insert into query, numeric )

      subroutine zzekinqn ( value, type,  lexbeg, lexend, &
                            eqryi, eqryd, descr           )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Insert a numeric value into a specified encoded EK query, and
!     obtain a descriptor for the stored value.
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
      include 'ektype.inc'
      include 'ekbool.inc'

      integer(ip), parameter :: lbcell = -5

      real(wp)              value
      integer(ip)           type
      integer(ip)           lexbeg
      integer(ip)           lexend
      integer(ip)           eqryi  ( lbcell : * )
      real(wp)              eqryd  ( * )
      integer(ip)           descr  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VALUE      I   Numeric value.
!     TYPE       I   Data type.  May be INT, DP, or TIME.
!     LEXBEG,
!     LEXEND     I   Begin and end positions of value's lexeme.
!     EQRYI     I-O  Integer portion of encoded query.
!     EQRYD     I-O  Numeric portion of encoded query.
!     DESCR      O   Descriptor for value.
!
! $ Detailed_Input
!
!     VALUE          is a numeric value to be inserted into an
!                    encoded query.
!
!     TYPE           indicates the data type of the numeric value.
!                    TYPE may be INT, DP, or TIME.
!
!     LEXBEG,
!     LEXEND         are the begin and end character positions in the
!                    original query of the lexeme that generated the
!                    input value.  These indices may be used for error
!                    correction.
!
!     EQRYI          is the integer portion of an encoded EK query
!
!     EQRYD          is the numeric portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query,
!                    updated to reflect the addition of a value to the
!                    encoded query's numeric buffer.
!
!     EQRYD          is the numeric portion of an encoded EK query,
!                    with the input numeric value added.
!
!     DESCR          is a descriptor for the input value.  The
!                    descriptor contains EQVDSZ elements.
!
! $ Parameters
!
!     See the INCLUDE files.
!
! $ Exceptions
!
!     1)  If the input query is uninitialized, the error
!         SPICE(NOTINITIALIZED) will be signaled.
!
!     2)  If there is insufficient space in the encoded query's
!         numeric component, the error SPICE(BUFFERTOOSMALL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should always be used to insert numeric values
!     into an encoded query; the insertion should never be done
!     directly.
!
! $ Examples
!
!     See ZZEKNRML.
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
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      integer(ip)           free
      integer(ip)           init
      integer(ip)           room
      integer(ip)           size

!
!     Use discovery check-in.
!
      call zzekreqi ( eqryi, 'INITIALIZED', init )

      if ( init /= itrue ) then

         call setmsg ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         call sigerr ( 'SPICE(NOTINITIALIZED)'                       )
         return

      end if

!
!     Get the numeric free pointer; make sure there's enough room.
!
      call zzekreqi ( eqryi, 'FREE_NUM',     free )
      call zzekreqi ( eqryi, 'NUM_BUF_SIZE', size )

      room  =  size - free + 1

      if ( room <= 0 ) then

         call setmsg ( 'Out of room in numeric portion of encoded ' // &
                       'query; only # elements were available.'      )
         call errint ( '#',  size                                    )
         call sigerr ( 'SPICE(BUFFERTOOSMALL)'                       )
         return

      end if

!
!     Insert the value into the double precision portion of the encoded
!     query.
!
      eqryd(free)      =  value

!
!     Fill in the descriptor.
!
      call cleari ( eqvdsz, descr )

      descr( eqdtyp )  =  type
      descr( eqblex )  =  lexbeg
      descr( eqelex )  =  lexend
      descr( eqvptr )  =  free

!
!     Update the numeric free pointer.
!
      call zzekweqi ( 'FREE_NUM', free+1, eqryi )


      return
      end
