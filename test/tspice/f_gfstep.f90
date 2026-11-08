!$Procedure      F_GFSTEP ( Test GFSTEP )

      SUBROUTINE F_GFSTEP ( OK )

! $ Abstract
!
!     Test the GF step size routine GFSTEP and step setting
!     routine GFSSTP.
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
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Test the GF step size routine GFSTEP and step setting
!     routine GFSSTP.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 01-FEB-2009 (NJB)
!
!        Initialized ET to avoid portability problems.
!        Changed value for negative step case.
!
!-    SPICELIB Version 2.0.0, 22-AUG-2008 (NJB)
!
!        "Not initialized" test case has been commented
!        out. This case can be used only when this family
!        is the first one that uses GFSTEP.
!
!-    SPICELIB Version 1.0.0, 02-AUG-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX

!
!     Local parameters
!


!
!     Local Variables
!
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      XSTEP


!
!     Saved variables
!

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFSTEP' )




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************



!
!---- Case -------------------------------------------------------------
!


!      CALL TCASE ( 'Call GFSTEP while step is uninitialized.' )

!      CALL GFSTEP ( 0.D0, STEP )

!      CALL CHCKXC ( .TRUE., 'SPICE(NOTINITIALIZED)', OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set zero step.' )

!
!     Give ET an initial value.
!
      ET = 1.D9

!
!     Set initial, valid step value.
!
      XSTEP = 3.D2
      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSSTP ( 0.D0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

!
!     Retrieve step.
!
      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP', STEP, '=', XSTEP, 0.D0, OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Set negative step.' )

!
!     Set initial, valid step value.
!
      XSTEP = 3.D2
      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSSTP ( -1.D0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

!
!     Retrieve step.
!
      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP', STEP, '=', XSTEP, 0.D0, OK )




!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure positive step sizes are accepted ' &
      //           'and saved.' )


!
!     ET should be ignored. Use an unusual value of ET.
!
      ET    = DPMAX()

      XSTEP = 1.D0

      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP (1)', STEP, '=', XSTEP, 0.D0, OK )


      ET    = - ( DPMAX()/2 )
      XSTEP = DPMAX()

      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP (DPMAX())', STEP, '=', XSTEP, 0.D0, OK )





      CALL T_SUCCESS ( OK )
      RETURN
      END




