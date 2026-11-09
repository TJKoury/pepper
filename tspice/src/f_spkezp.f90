!$Procedure      F_SPKEZP (Family of tests for SPKEZP)

      SUBROUTINE F_SPKEZP( OK )

! $ Abstract
!
!     This routine performs a series of tests on the routine
!     SPKEZP to make sure that it behaves as expected.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
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
!     This routine tests the SPICELIB routine SPKEZP.
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
!     N.J. Bachman     (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0 27-AUG-2013 (NJB)
!
!        In the comparison of SPKEZP results against those
!        from the combination of SPKSSB and SPKAPP, subsituted
!        SPKAPO for SPKAPP for the cases where an exact match
!        is expected.
!
!-    TSPICE Version 2.0.1 11-JAN-2002 (EDW)
!
!        Reduced the vector tolerance for Cases 1 and 6
!        by two orders of magnitude, to 10^12 and 10^-10
!        respectively. Roundoff error resulted in
!        relative differences between test and exp vectors
!        to exceed the original tolerances on some
!        platforms.
!
!-    TSPICE Version 2.0.0 18-DEC-2001 (NJB)
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!
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




!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKEZP' )

      CALL KILFIL ( 'phoenix2.bsp' )
      CALL KILFIL ( 'phoenix2.ker' )

      CALL TSTSPK ( 'phoenix2.bsp', .TRUE.,  HANDLE )

      CALL TSTPCK ( 'phoenix2.ker', .TRUE., .FALSE. )
      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )



      CALL TCASE ( 'Make sure that SPKEZP and SPKGEO return the ' &
      //           'same information when the correction ' &
      //           'requested is NONE. ' )

      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'NONE', 301001, &
                   STATE,  LT )

      CALL SPKGEO( 401001, ET, 'IAU_EARTH', 301001, ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-13, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )





      CALL TCASE ( 'Make sure that SPKEZP returns the same thing ' &
      //           'as the combination of SPKSSB and SPKAPO when ' &
      //           'an inertial frame is the requested output ' &
      //           'frame. Light time only correction.' )

      CALL SPKEZP( 301001, ET, 'J2000', 'LT', 399001, &
                   STATE,  LT )

      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'LT', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' // &
                   'case.'                                          )

      CALL SPKEZP( 301001, ET, 'J2000', 'XLT', 399001, &
                   STATE,  LT )

      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'XLT', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )





      CALL TCASE ( 'Make sure that SPKEZP returns the same thing ' &
      //           'as the combination of SPKSSB and SPKAPO when ' &
      //           'an inertial frame is the requested output ' &
      //           'frame. Converged Newtonian plus stellar aberation' )

      CALL SPKEZP( 301001, ET, 'J2000', 'CN+S', 399001, &
                   STATE,  LT )

      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'CN+S', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' // &
                   'case.'                                          )

      CALL SPKEZP( 301001, ET, 'J2000', 'XCN+S', 399001, &
                   STATE,  LT )

      CALL SPKSSB( 399001, ET, 'J2000',  SOBS )
      CALL SPKAPO( 301001, ET, 'J2000',  SOBS, 'XCN+S', ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 3, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )




      CALL TCASE ( 'Perform an independent test to see if ' &
      //           '"apparent" positions in non-inertial frames ' &
      //           'are properly computed. ' )


      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'CN+S', 301001, &
                   STATE,  LT )

      CALL SPKSSB( 301001, ET, 'J2000',  SOBS )
      CALL SPKAPP( 401001, ET, 'J2000',  SOBS, 'CN+S', ISTATE, ELT )
      CALL SPKAPP( 399,    ET, 'J2000',  SOBS, 'CN+S', TSTATE, CLT )
      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-CLT,       XFORM       )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 3, 1.0D-11, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-12, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' // &
                   'case.'                                          )


      CALL SPKEZP( 401001, ET, 'IAU_EARTH', 'XCN+S', 301001, &
                   STATE,  LT )

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
