!$Procedure   ZZEKINQN ( Private: EK, insert into query, numeric )

      SUBROUTINE ZZEKINQN ( VALUE, TYPE,  LEXBEG, LEXEND, &
                            EQRYI, EQRYD, DESCR           )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Insert a numeric value into a specified encoded EK query, and
!     obtain a descriptor for the stored value.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      DOUBLE PRECISION      VALUE
      INTEGER               TYPE
      INTEGER               LEXBEG
      INTEGER               LEXEND
      INTEGER               EQRYI  ( LBCELL : * )
      DOUBLE PRECISION      EQRYD  ( * )
      INTEGER               DESCR  ( * )

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
      INTEGER               FREE
      INTEGER               INIT
      INTEGER               ROOM
      INTEGER               SIZE

!
!     Use discovery check-in.
!
      CALL ZZEKREQI ( EQRYI, 'INITIALIZED', INIT )

      IF ( INIT .NE. ITRUE ) THEN

         CALL CHKIN  ( 'ZZEKINQN'                                    )
         CALL SETMSG ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'                       )
         CALL CHKOUT ( 'ZZEKINQN'                                    )
         RETURN

      END IF

!
!     Get the numeric free pointer; make sure there's enough room.
!
      CALL ZZEKREQI ( EQRYI, 'FREE_NUM',     FREE )
      CALL ZZEKREQI ( EQRYI, 'NUM_BUF_SIZE', SIZE )

      ROOM  =  SIZE - FREE + 1

      IF ( ROOM .LE. 0 ) THEN

         CALL CHKIN  ( 'ZZEKINQN'                                    )
         CALL SETMSG ( 'Out of room in numeric portion of encoded ' // &
                       'query; only # elements were available.'      )
         CALL ERRINT ( '#',  SIZE                                    )
         CALL SIGERR ( 'SPICE(BUFFERTOOSMALL)'                       )
         CALL CHKOUT ( 'ZZEKINQN'                                    )
         RETURN

      END IF

!
!     Insert the value into the double precision portion of the encoded
!     query.
!
      EQRYD(FREE)      =  VALUE

!
!     Fill in the descriptor.
!
      CALL CLEARI ( EQVDSZ, DESCR )

      DESCR( EQDTYP )  =  TYPE
      DESCR( EQBLEX )  =  LEXBEG
      DESCR( EQELEX )  =  LEXEND
      DESCR( EQVPTR )  =  FREE

!
!     Update the numeric free pointer.
!
      CALL ZZEKWEQI ( 'FREE_NUM', FREE+1, EQRYI )


      RETURN
      END
