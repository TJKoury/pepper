
!$Procedure CBINIT ( Character buffer, initialize  )

      SUBROUTINE CBINIT_1 ( DIM, BUFFER )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Initialize a character buffer.
!
! $ Required_Reading
!
!     CB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )

      INTEGER               DIM
      CHARACTER*(*)         BUFFER      ( LBCBUF:DIM )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DIM        I   Dimension of the character buffer array.
!     BUFFER    I,O  Character buffer.
!
! $ Detailed_Input
!
!     DIM         is the dimension of the array containing the
!                 character buffer to be initialized.
!
!     BUFFER      is the array.
!
! $ Detailed_Output
!
!     BUFFER      is an initialized character buffer.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) The error 'SPICE(NOTLEGALCB)' is signalled whenever any of
!        the following conditions is detected.
!
!           -- The length of the individual array elements is less
!              than eight.
!
!           -- DIM is less than one.
!
! $ Particulars
!
!     A character buffer must be initialized to allow subsequent
!     operations on the buffer to detect possible overflows.
!
! $ Examples
!
!     The following code fragment illustrates the initialization
!     of a character buffer.
!
!        INTEGER               LBCBUF
!        PARAMETER           ( LBCBUF =    0 )
!
!        INTEGER               BUFDIM
!        PARAMETER           ( BUFDIM =  256 )
!
!        INTEGER               BUFLEN
!        PARAMETER           ( BUFLEN = 1024 )
!
!        CHARACTER*(BUFLEN)    BUFFER   ( LBCBUF:BUFDIM )
!         .
!         .
!
!        CALL CBINIT ( BUFDIM, BUFFER )
!
!     In this example, the buffer contains 256K characters of available
!     storage (256 array elements of 1024 characters each). Note that
!     it is only necessary to supply the dimension of the array (256),
!     and not the length of the individual elements (1024).
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
!     Dagny Taggart, (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&

!
!     SPICELIB functions
!


!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBINIT_1' )

         IF ( LEN ( BUFFER(0) ) .LT. 8 ) THEN
            CALL SETMSG ( 'Length is #.'         )
            CALL ERRINT ( '#', LEN ( BUFFER(0) ) )

            CALL SIGERR ( 'SPICE(NOTLEGALCB)' )
            CALL CHKOUT ( 'CBINIT_1'            )
            RETURN

         ELSE IF ( DIM .LT. 1 ) THEN
            CALL SETMSG ( 'Dimension is #.' )
            CALL ERRINT ( '#', DIM          )

            CALL SIGERR ( 'SPICE(NOTLEGALCB)' )
            CALL CHKOUT ( 'CBINIT_1'            )
            RETURN
         END IF
      END IF

!
!     Store only the dimension.
!
      CALL ENCHAR ( DIM, BUFFER(0)(1:8) )

      CALL CHKOUT ( 'CBINIT_1' )
      RETURN
      END
