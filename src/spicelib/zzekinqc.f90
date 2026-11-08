!$Procedure   ZZEKINQC ( Private: EK, insert into query, character )

      SUBROUTINE ZZEKINQC ( VALUE, LENGTH, LEXBEG, LEXEND, &
                            EQRYI, EQRYC,  DESCR           )
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

      INCLUDE 'ekquery.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekbool.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         VALUE
      INTEGER               LENGTH
      INTEGER               LEXBEG
      INTEGER               LEXEND
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      INTEGER               DESCR  ( * )

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
      INTEGER               FREE
      INTEGER               INIT
      INTEGER               L
      INTEGER               ROOM
      INTEGER               SIZE

!
!     Use discovery check-in.
!
      CALL ZZEKREQI ( EQRYI, 'INITIALIZED', INIT )

      IF ( INIT .NE. ITRUE ) THEN

         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'                       )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN

      END IF

!
!     Check the input length value.
!
      IF ( LENGTH .LT. 1 ) THEN

         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Length of string value was #; must be > 0.'  )
         CALL ERRINT ( '#',  LENGTH                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                         )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN

      END IF

!
!     Get the character free pointer; make sure there's enough room.
!
      CALL ZZEKREQI ( EQRYI, 'FREE_CHR',     FREE )
      CALL ZZEKREQI ( EQRYI, 'CHR_BUF_SIZE', SIZE )

      ROOM  =  SIZE - FREE + 1

      IF ( LENGTH .GT. ROOM ) THEN

         CALL CHKIN  ( 'ZZEKINQC'                                    )
         CALL SETMSG ( 'Out of room in character portion of encoded ' // &
                       'query; only # elements were available; # '    // &
                       'are needed.'                                 )
         CALL ERRINT ( '#',  ROOM                                    )
         CALL ERRINT ( '#',  LENGTH                                  )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'ZZEKINQC'                                    )
         RETURN

      END IF

!
!     Insert the value into the character portion of the encoded query.
!
      L                =  MIN ( LENGTH, LEN(VALUE) )
      EQRYC(FREE:)     =  VALUE(:L)

!
!     Fill in the descriptor.
!
      CALL CLEARI ( EQVDSZ, DESCR )

      DESCR( EQDTYP )  =  CHR
      DESCR( EQBLEX )  =  LEXBEG
      DESCR( EQELEX )  =  LEXEND
      DESCR( EQBSTR )  =  FREE
      DESCR( EQESTR )  =  FREE + LENGTH - 1

!
!     Update the character free pointer.
!
      CALL ZZEKWEQI ( 'FREE_CHR', FREE+LENGTH, EQRYI )


      RETURN
      END
