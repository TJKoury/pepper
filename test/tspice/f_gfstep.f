C$Procedure      F_GFSTEP ( Test GFSTEP )
  
      SUBROUTINE F_GFSTEP ( OK )

C$ Abstract
C
C     Test the GF step size routine GFSTEP and step setting
C     routine GFSSTP.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

 
      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Test the GF step size routine GFSTEP and step setting
C     routine GFSSTP.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 01-FEB-2009 (NJB)
C
C        Initialized ET to avoid portability problems.
C        Changed value for negative step case.
C
C-    SPICELIB Version 2.0.0, 22-AUG-2008 (NJB)
C
C        "Not initialized" test case has been commented
C        out. This case can be used only when this family
C        is the first one that uses GFSTEP.
C
C-    SPICELIB Version 1.0.0, 02-AUG-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX

C     
C     Local parameters
C
 
 
C
C     Local Variables
C
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      XSTEP


C
C     Saved variables
C
 
C
C     Initial values
C
 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFSTEP' )



      
C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************



C
C---- Case -------------------------------------------------------------
C


C      CALL TCASE ( 'Call GFSTEP while step is uninitialized.' )

C      CALL GFSTEP ( 0.D0, STEP )

C      CALL CHCKXC ( .TRUE., 'SPICE(NOTINITIALIZED)', OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set zero step.' )

C
C     Give ET an initial value.
C
      ET = 1.D9

C
C     Set initial, valid step value.
C
      XSTEP = 3.D2
      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSSTP ( 0.D0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

C
C     Retrieve step.
C
      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP', STEP, '=', XSTEP, 0.D0, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Set negative step.' )

C
C     Set initial, valid step value.
C
      XSTEP = 3.D2
      CALL GFSSTP ( XSTEP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFSSTP ( -1.D0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

C
C     Retrieve step.
C
      CALL GFSTEP ( ET, STEP )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'STEP', STEP, '=', XSTEP, 0.D0, OK )




C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Make sure positive step sizes are accepted '
     .//           'and saved.' )


C
C     ET should be ignored. Use an unusual value of ET.
C
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




