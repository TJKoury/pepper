!$Procedure CARDD ( Cardinality of a double precision cell )

      INTEGER FUNCTION CARDD ( CELL )

! $ Abstract
!
!     Return the cardinality (number of elements) of a double
!     precision cell.
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

      DOUBLE PRECISION CELL  ( LBCELL:* )

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
!         SPICE(INVALIDCARDINALITY) is signaled. CARDD returns
!         an unspecified value in this case.
!
!     2)  If the input array has invalid size, the error
!         SPICE(INVALIDSIZE) is signaled. CARDD returns
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
!     cells. In the following example, SIZED is used to determine
!     whether the d.p. cell ORIGINAL can be safely copied into
!     the d.p. cell SAVE before actually attempting the operation.
!     If ORIGINAL contains more elements than SAVE can hold, then
!     the operation would fail.
!
!           IF ( CARDD ( ORIGINAL ) .LE. SIZED ( SAVE ) ) THEN
!              CALL COPYD ( ORIGINAL, SAVE )
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
!     cardinality of a d.p. cell
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
         CARDD = 0
         RETURN
      ELSE
         CALL CHKIN ( 'CARDD' )
      END IF

!
!     Set return value, regardless of validity.
!
      CARDD = INT ( CELL(0) )

!
!     Squeal if something is awry.
!

      IF (  INT( CELL(-1) ) .LT. 0  )  THEN

         CALL SETMSG ( 'Invalid cell size.  The size was #.')
         CALL ERRINT ( '#', INT( CELL(-1) ) )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
         CALL CHKOUT ( 'CARDD' )
         RETURN

      ELSE IF ( INT( CELL(0) ) .LT. 0  ) THEN

         CALL SETMSG ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         CALL ERRINT ( '#', INT( CELL(0) )  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'CARDD' )
         RETURN

      ELSE IF ( INT(CELL(0)) .GT. INT(CELL(-1))  ) THEN

         CALL SETMSG ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         CALL ERRINT ( '#', INT(CELL( 0))  )
         CALL ERRINT ( '#', INT(CELL(-1))  )
         CALL SIGERR ( 'SPICE(INVALIDCARDINALITY)' )
         CALL CHKOUT ( 'CARDD' )
         RETURN


      END IF


      CALL CHKOUT ( 'CARDD' )
      RETURN
      END
