!$Procedure DATANH  ( Double precision arc hyperbolic tangent )

      DOUBLE PRECISION FUNCTION  DATANH ( X )

! $ Abstract
!
!     Return the inverse hyperbolic tangent of a double precision
!     argument.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     HYPERBOLIC
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   X

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   Number whose inverse hyperbolic tangent is
!                    desired.
!
!     The function returns the inverse hyperbolic tangent of a double
!     precision number.
!
! $ Detailed_Input
!
!     X        is any double precision.
!
!              X must be within the range -1 < X < +1.
!
! $ Detailed_Output
!
!     The function returns the inverse hyperbolic tangent of the double
!     precision number X.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If X is not between -1.0 and 1.0, the error
!         SPICE(INVALIDARGUMENT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This function simply implements the definition of the inverse
!     hyperbolic tangent as follows:
!
!        DATANH = 0.5D0 * DLOG ( (1+X) / (1-X) )
!
!     If the input value is not valid, an error is signaled.
!
! $ Examples
!
!     The following table gives a few values for X and the resulting
!     value of DATANH.
!
!         X                       DATANH(X)
!        ----------------------------------------------
!        -0.2000000000000000     -0.2027325540540822
!        -0.1000000000000000     -0.1003353477310756
!         0.0000000000000000E+00  0.0000000000000000E+00
!         0.1000000000000000      0.1003353477310756
!         0.2000000000000000      0.2027325540540822
!         0.4000000000000000      0.4236489301936018
!        0.8000000000000000       1.098612288668110
!
! $ Restrictions
!
!     1)  The value of the input variable X must be between -1.0 and
!         1.0, otherwise an error is signaled.
!
! $ Literature_References
!
!     [1]  W.H. Beyer, "CRC Standard Mathematical Tables," CRC Press,
!          1987.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        Set the default function value to either 0, 0.0D0, .FALSE.,
!        or blank depending on the type of the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     d.p. arc hyperbolic_tangent
!
!-&


!
!     SPICELIB functions
!
      LOGICAL            RETURN





!
!     Set up the error processing.
!
      IF ( RETURN () ) THEN
         DATANH = 0.0D0
         RETURN
      ELSE
         CALL CHKIN ( 'DATANH' )
         DATANH = 0.0D0
      END IF

!
!     Check that -1 < X < +1.
!
      IF ( DABS(X) .GE. 1.0D0 ) THEN

         CALL SETMSG ( 'DATANH: Argument out of range.' )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )

         CALL CHKOUT ( 'DATANH' )
         RETURN
      END IF

      DATANH = 0.5D0 * DLOG ( (1.D0+X) / (1.D0-X) )

      CALL CHKOUT ( 'DATANH' )

      RETURN
      END
