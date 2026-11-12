
!$Procedure NCODEI ( Encode integer value into integer item )

      SUBROUTINE NCODEI ( VALUE, ITEM )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Encode an integer value into an integer item.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations

      INTEGER               VALUE
      INTEGER               ITEM

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VALUE      I   Non-negative integer value.
!     ITEM       O   Item into which VALUE has been encoded.
!
! $ Detailed_Input
!
!     VALUE       is an arbitrary non-negative integer value.
!
! $ Detailed_Output
!
!     ITEM        is an integer item, into which the value has been
!                 been encoded. The value can be recovered by calling
!                 subroutine DCODE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the number to be encoded is negative, the error
!        'SPICE(OUTOFRANGE)' is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NCODE (and its inverse, DCODE) provide a uniform way to store
!     and retrieve values from the control areas of cells and cell-
!     based data types. This makes it possible to write templates
!     for a generic data type, suitable for instantiation by a
!     pre-compiler.
!
! $ Examples
!
!     The following code fragment illustrates how NCODE and DCODE
!     can be used to create instantiable subroutine templates.
!
!        C
!        C     Check the bolzat counter, to see if the elements
!        C     have been freebished; if not, do it now, and set
!        C     the counter to zero.
!        C
!              CALL DCODE@ ( STRUCT(-4), BCOUNT )
!
!              IF ( BCOUNT .GT. 0 ) THEN
!                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  )
!                 CALL NCODE@ (                0, STRUCT(-4) )
!              END IF
!
!     By replacing all occurrences of `@' with the appropriate
!     type ending (C, D, or I), this single template can give
!     rise to three separate pieces of type-dependent code.
!
!     The alternative to using NCODE and DCODE is to use simple
!     assignments for numeric cells, and calls to ENCHAR and
!     DECHAR for character cells, destroying the symmetry inherent
!     in the rest of the code.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
!
!-&

!
!     SPICELIB functions
!

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NCODEI' )
      END IF

      IF ( VALUE .GE. 0 ) THEN
         ITEM = VALUE

      ELSE
         CALL SETMSG ( 'Cannot encode #; must be non-negative.' )
         CALL ERRINT ( '#', VALUE                               )
         CALL SIGERR ( 'SPICE(OUTOFRANGE)'                      )
      END IF

      CALL CHKOUT ( 'NCODEI' )
      RETURN




!$Procedure DCODEI ( Decode integer value from integer item )

      ENTRY DCODEI ( ITEM, VALUE )

! $ Abstract
!
!     Decode the integer value stored in an integer item by a
!     previous call to NCODEI.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations
!
!     INTEGER               ITEM
!     INTEGER               VALUE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item into which an integer value has been encoded.
!     VALUE      O   Encoded value.
!
! $ Detailed_Input
!
!     ITEM        is an integer item, into which an integer value
!                 has been encoded by a previous call to NCODEI.
!
! $ Detailed_Output
!
!     VALUE       is the encoded value.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NCODE (and its inverse, DCODE) provide a uniform way to store
!     and retrieve values from the control areas of cells and cell-
!     based data types. This makes it possible to write templates
!     for a generic data type, suitable for instantiation by a
!     pre-compiler.
!
! $ Examples
!
!     The following code fragment illustrates how NCODE and DCODE
!     can be used to create instantiable subroutine templates.
!
!        C
!        C     Check the bolzat counter, to see if the elements
!        C     have been freebished; if not, do it now, and set
!        C     the counter to zero.
!        C
!              CALL DCODE@ ( STRUCT(-4), BCOUNT )
!
!              IF ( BCOUNT .GT. 0 ) THEN
!                 CALL FREEB@ ( CARD@ ( STRUCT ), STRUCT(1)  )
!                 CALL NCODE@ (                0, STRUCT(-4) )
!              END IF
!
!     By replacing all occurrences of `@' with the appropriate
!     type ending (C, D, or I), this single template can give
!     rise to three separate pieces of type-dependent code.
!
!     The alternative to using NCODE and DCODE is to use simple
!     assignments for numeric cells, and calls to ENCHAR and
!     DECHAR for character cells, destroying the symmetry inherent
!     in the rest of the code.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DCODEI' )
      END IF

      VALUE = ITEM

      CALL CHKOUT ( 'DCODEI' )
      RETURN
      END
