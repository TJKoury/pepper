!$Procedure   ZZEKINQC ( Private: EK, insert into query, character )

      subroutine zzekinqc ( value, length, lexbeg, lexend, &
                            eqryi, eqryc,  descr           )
! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Insert a character value into a specified encoded EK query, and
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

      integer               lbcell
      parameter           ( lbcell = -5 )

      character*(*)         value
      integer               length
      integer               lexbeg
      integer               lexend
      integer               eqryi  ( lbcell : * )
      character*(*)         eqryc
      integer               descr  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VALUE      I   Character value.
!     LENGTH     I   Length of item to insert.
!     LEXBEG,
!     LEXEND     I   Begin and end positions of value's lexeme.
!     EQRYI     I-O  Integer portion of encoded query.
!     EQRYC     I-O  Character portion of encoded query.
!     DESCR      O   Descriptor for value.
!
! $ Detailed_Input
!
!     VALUE          is a character value to be inserted into an
!                    encoded query.
!
!     LENGTH         indicates the length of the input character value.
!                    If LENGTH exceeds LEN(VALUE), the stored value
!                    is padded with trailing blanks.  This allows
!                    faithful representation of literal strings.
!
!     LEXBEG,
!     LEXEND         are the begin and end character positions in the
!                    original query of the lexeme that generated the
!                    input value.  These indices may be used for error
!                    correction.
!
!     EQRYI          is the integer portion of an encoded EK query
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query,
!                    updated to reflect the addition of a value to the
!                    encoded query's character buffer.
!
!     EQRYC          is the character portion of an encoded EK query,
!                    with the input value added.
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
!     2)  If the input character count LENGTH is non-positive, the
!         error SPICE(INVALIDCOUNT) is signaled.
!
!     3)  If there is insufficient space in the encoded query's
!         character component, the error SPICE(BUFFERTOOSMALL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should always be used to insert character values
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
      integer               free
      integer               init
      integer               l
      integer               room
      integer               size

!
!     Use discovery check-in.
!
      call zzekreqi ( eqryi, 'INITIALIZED', init )

      if ( init .ne. itrue ) then

         call chkin  ( 'ZZEKINQC'                                    )
         call setmsg ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         call sigerr ( 'SPICE(NOTINITIALIZED)'                       )
         call chkout ( 'ZZEKINQC'                                    )
         return

      end if

!
!     Check the input length value.
!
      if ( length .lt. 1 ) then

         call chkin  ( 'ZZEKINQC'                                    )
         call setmsg ( 'Length of string value was #; must be > 0.'  )
         call errint ( '#',  length                                  )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                         )
         call chkout ( 'ZZEKINQC'                                    )
         return

      end if

!
!     Get the character free pointer; make sure there's enough room.
!
      call zzekreqi ( eqryi, 'FREE_CHR',     free )
      call zzekreqi ( eqryi, 'CHR_BUF_SIZE', size )

      room  =  size - free + 1

      if ( length .gt. room ) then

         call chkin  ( 'ZZEKINQC'                                    )
         call setmsg ( 'Out of room in character portion of encoded ' // &
                       'query; only # elements were available; # '    // &
                       'are needed.'                                 )
         call errint ( '#',  room                                    )
         call errint ( '#',  length                                  )
         call sigerr ( 'SPICE(BUFFERTOOSMALL)'                       )
         call chkout ( 'ZZEKINQC'                                    )
         return

      end if

!
!     Insert the value into the character portion of the encoded query.
!
      l                =  min ( length, len(value) )
      eqryc(free:)     =  value(:l)

!
!     Fill in the descriptor.
!
      call cleari ( eqvdsz, descr )

      descr( eqdtyp )  =  chr
      descr( eqblex )  =  lexbeg
      descr( eqelex )  =  lexend
      descr( eqbstr )  =  free
      descr( eqestr )  =  free + length - 1

!
!     Update the character free pointer.
!
      call zzekweqi ( 'FREE_CHR', free+length, eqryi )


      return
      end
