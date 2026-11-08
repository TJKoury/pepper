C$Procedure      F_SPKEZP (Family of tests for SPKEZP)
 
      SUBROUTINE F_SPKEZP( OK )
 
C$ Abstract
C
C     This routine performs a series of tests on the routine
C     SPKEZP to make sure that it behaves as expected.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
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
C     This routine tests the SPICELIB routine SPKEZP.
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
C     N.J. Bachman     (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0 27-AUG-2013 (NJB)
C
C        In the comparison of SPKEZP results against those
C        from the combination of SPKSSB and SPKAPP, subsituted
C        SPKAPO for SPKAPP for the cases where an exact match
C        is expected.
C
C-    TSPICE Version 2.0.1 11-JAN-2002 (EDW)
C
C        Reduced the vector tolerance for Cases 1 and 6
C        by two orders of magnitude, to 10^12 and 10^-10
C        respectively. Roundoff error resulted in
C        relative differences between test and exp vectors
C        to exceed the original tolerances on some
C        platforms.
C     
C-    TSPICE Version 2.0.0 18-DEC-2001 (NJB)
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ERROR
 
      INTEGER               HANDLE
 
      DOUBLE PRECISION      ET
 
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      ELT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ESTATE ( 6 )
      DOUBLE PRECISION      TSTATE ( 6 )
      DOUBLE PRECISION      SOBS   ( 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      CLT
      DOUBLE PRECISION      ISTATE ( 6 )
 
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKEZP' )
 
      CALL KILFIL ( 'phoenix2.bsp' )
      CALL KILFIL ( 'phoenix2.ker' )
 
      CALL TSTSPK ( 'phoenix2.bsp', .TRUE.,  HANDLE )
 
      CALL TSTPCK ( 'phoenix2.ker', .TRUE., .FALSE. )
      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )


 
      CALL TCASE ( 'Make sure that SPKEZP and SPKGEO return the '
     .//           'same information when the correction '
     .//           'requested is NONE. ' )
 
      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'NONE', 301001,
     .             STATE,  LT )
 
      CALL SPKGEO( 401001, ET, 'IAU_EARTH', 301001, ESTATE, ELT )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-13, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )
 
 



      CALL TCASE ( 'Make sure that SPKEZP returns the same thing '
     .//           'as the combination of SPKSSB and SPKAPO when '
     .//           'an inertial frame is the requested output '
     .//           'frame. Light time only correction.' )
 
      CALL SPKEZP( 301001, ET, 'J2000', 'LT', 399001,
     .             STATE,  LT )
 
      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'LT', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )

      CALL SPKEZP( 301001, ET, 'J2000', 'XLT', 399001,
     .             STATE,  LT )
 
      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'XLT', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )





      CALL TCASE ( 'Make sure that SPKEZP returns the same thing '
     .//           'as the combination of SPKSSB and SPKAPO when '
     .//           'an inertial frame is the requested output '
     .//           'frame. Converged Newtonian plus stellar aberation' )
 
      CALL SPKEZP( 301001, ET, 'J2000', 'CN+S', 399001,
     .             STATE,  LT )
 
      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'CN+S', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )
 


      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )

      CALL SPKEZP( 301001, ET, 'J2000', 'XCN+S', 399001,
     .             STATE,  LT )
 
      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'XCN+S', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )
 



      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. ' )
 
 
      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'CN+S', 301001,
     .             STATE,  LT )
 
      CALL SPKSSB( 301001, ET, 'J2000',  SOBS )
      CALL SPKAPP( 401001, ET, 'J2000',  SOBS, 'CN+S', ISTATE, ELT )
      CALL SPKAPP( 399,    ET, 'J2000',  SOBS, 'CN+S', TSTATE, CLT )
      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-CLT,       XFORM       )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-11, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-12, OK )
 
 

      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )
 
 
      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'XCN+S', 301001,
     .             STATE,  LT )
 
      CALL SPKSSB( 301001, ET, 'J2000',  SOBS )
      CALL SPKAPP( 401001, ET, 'J2000',  SOBS, 'XCN+S', ISTATE, ELT )
      CALL SPKAPP( 399,    ET, 'J2000',  SOBS, 'XCN+S', TSTATE, CLT )
      CALL SXFORM( 'J2000', 'IAU_EARTH', ET+CLT,        XFORM       )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-12, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-12, OK )
 
      CALL SPKUEF (  HANDLE        )
      CALL KILFIL ( 'phoenix2.bsp' )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
