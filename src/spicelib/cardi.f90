!$Procedure CARDI ( Cardinality of an integer cell )

      INTEGER FUNCTION CARDI ( CELL )

! $ Abstract
!
!     Return the cardinality (number of elements) of an integer cell.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER          CELL  ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CELL       I   Input cell.
!
!     The function returns the cardinality of the input cell.
!
! $ Detailed_Input
!
!     CELL     is a cell.
!
! $ Detailed_Output
!
!     The function returns the cardinality of (number of elements in)
!     the input cell.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input array has invalid cardinality, the error
!         SPICE(INVALIDCARDINALITY) is signaled. CARDI returns
!         an unspecified value in this case.
!
!     2)  If the input array has invalid size, the error
!         SPICE(INVALIDSIZE) is signaled. CARDI returns
!         an unspecified value in this case.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The cardinality (CARD) functions are typically used to process
!     each of the elements of a cell. In the following example, CARDC
!     is used to step through the individual elements of the character
!     cell NAMES.
!
!           DO I = 1, CARDC ( NAMES )
!            .
!            .
!           END DO
!
!     In conjunction with the size (SIZE) functions, they may be used
!     to predict (and subsequently avoid) overflows when manipulating
!     cells. In the following example, SIZEI is used to determine
!     whether the integer cell ORIGINAL can be safely copied into
!     the integer cell SAVE before actually attempting the operation.
!     If ORIGINAL contains more elements than SAVE can hold, then
!     the operation would fail.
!
!           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
!              CALL COPYI ( ORIGINAL, SAVE )
!
!           ELSE
!            .
!            .
!           END IF
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 29-JUL-2002 (NJB)
!
!        Errors in code fragments in the $Examples section of
!        the header were corrected.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!       If the value of the function RETURN is .TRUE. upon execution of
!       this module, this function is assigned a default value of
!       either 0, 0.0D0, .FALSE., or blank depending on the type of the
!       function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     cardinality of an integer cell
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
!
!        Check for valid input cell added. The input cell must
!        have valid size and cardinality values.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN



      IF ( RETURN() ) THEN
         CARDI = 0
         RETURN
      ELSE
         CALL CHKIN ( 'CARDI' )
      END IF

!
!     Set return value, regardless of validity.
!
      CARDI = CELL(0)


!
!     Squeal if something is awry.
!

      IF (  CELL(-1) .LT. 0  )  THEN

         CALL SETMSG ( 'Invalid cell size.  The size was #.')
         CALL ERRINT ( '#', CELL(-1) )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'CARDI' )
         RETURN

      ELSE IF ( CELL(0) .LT. 0  ) THEN

         CALL SETMSG ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         CALL ERRINT ( '#', CELL(0)  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'CARDI' )
         RETURN

      ELSE IF ( CELL(0) .GT. CELL(-1)  ) THEN

         CALL SETMSG ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         CALL ERRINT ( '#', CELL( 0)  )
         CALL ERRINT ( '#', CELL(-1)  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'CARDI' )
         RETURN


      END IF


      CALL CHKOUT ( 'CARDI' )
      RETURN
      END
