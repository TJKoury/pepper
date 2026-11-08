!$Procedure DACOSN (arc cosine of bracketed argument)

      DOUBLE PRECISION FUNCTION DACOSN ( ARG, TOL )

! $ Abstract
!
!     Compute arc cosine of a bracketed argument.
!
!     This routine produces a SPICE error if the |argument| exceeds
!     1.D0 by more than TOL. If ARG exceeds 1.D0, the argument is
!     evaluated as if it equaled 1.D0, if ARG is less than -1.,
!     the argument is evaluated as if it equaled -1.D0.
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
!     None.
!
! $ Keywords
!
!     INTERVALS
!     INVERSE TRIGONOMETRIC FUNCTION
!     NUMBERS
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   ARG
      DOUBLE PRECISION   TOL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARG        I   Argument to be evaluated.
!     TOL        I   Tolerance.
!
!     The function returns the arc cosine of ARG.
!
! $ Detailed_Input
!
!     ARG      is the arc cosine argument that is to be evaluated such
!              that if it is less than -1.D0 by more than TOL or greater
!              than 1.D0 by more than TOL, an error results.
!
!     TOL      is a tolerance such that |ARG| is considered to be equal
!              to 1.D0 if |ARG| <= 1.D0 + TOL. TOL must be non-negative.
!
! $ Detailed_Output
!
!     The function returns the arc cosine of ARG. If
!
!        |ARG| >= 1.D0,
!
!     it returns DACOS (1.D0) or DACOS (-1.D0) as appropriate. Values
!     range from 0 to PI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If |ARG| > 1.D0 + TOL, the error SPICE(INPUTOUTOFBOUNDS) is
!         signaled.
!
!     2)  If TOL is less than zero, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines whether |ARG| > 1.D0 + TOL. If it is, an
!     error will be flagged. In addition, the values of ARG are
!     constrained to [-1.D0, 1.D0].
!
! $ Examples
!
!     The following illustrate the operation of DACOSN.
!
!           DACOSN (  -1.D0,        1.D-7 )  =   PI
!           DACOSN (  -1.00001D0,   1.D-3 )  =   PI
!           DACOSN (  -1.00001D0,   1.D-7 )  =   PI   (error flagged)
!           DACOSN (   0.D0,        1.D-7 )  =   PI/2
!           DACOSN (   1.00001D0,   1.D-3 )  =   0.
!           DACOSN (   1.00001D0,   1.D-7 )  =   0. (error flagged)
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
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 28-FEB-2006 (LSE)
!
!-&


! $ Index_Entries
!
!     check a d.p. argument for ACOS before evaluation
!
!-&


!
!     Bracket ARG.
!

      DACOSN      =  ACOS ( MAX ( - 1.D0, MIN ( 1.D0, ARG ) ) )

!
!     Check that tolerance is non negative.
!

      IF ( TOL .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'DACOSN'                           )
         CALL SETMSG ( 'TOL was #; must be non-negative.' )
         CALL ERRDP  ( '#',  TOL                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
         CALL CHKOUT ( 'DACOSN'                           )
         RETURN

      END IF

!
!     Check to see if |ARG| is within TOL of 1.D0. Signal error if
!     appropriate.
!

      IF  ( ( ABS ( ARG ) - TOL ) .GT. 1.D0) THEN

         CALL CHKIN  ( 'DACOSN'                           )
         CALL SETMSG ( 'The |argument| specified was greater '// &
                       'than 1.D0 by more than #. The value '// &
                       'of the argument is #. '           )
         CALL ERRDP  ( '#',  TOL                          )
         CALL ERRDP  ( '#',  ARG                          )
         CALL SIGERR ( 'SPICE(INPUTOUTOFBOUNDS)'          )
         CALL CHKOUT ( 'DACOSN'                           )
         RETURN

      END IF

      RETURN
      END
