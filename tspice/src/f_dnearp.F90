!$Procedure F_DNEARP ( Family of DNEARP tests )

      SUBROUTINE F_DNEARP ( OK )

! $ Abstract
!
!     This routine performs a set of rudimentary checks on the
!     routine DNEARP.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine DNEARP.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 2.8.1, 17-FEB-2021 (JDR)
!
!        Edited the header and comments to comply with NAIF standard.
!
!-    TSPICE Version 2.8.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 2.7.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 2.6.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 2.4.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 2.3.0, 27-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VLCOM call.
!
!-    TSPICE Version 2.2.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments (only FORTRAN ones because
!        MAC-OSX C environment has the same problem with the
!        focal point test as the Linux and classic Mac C.)
!
!-    TSPICE Version 2.2.0, 02-JAN-2002 (NJB) (EDW)
!
!        Added PC-LINUX_C and MACPPC_C to the set of environments
!        for which the focal point test is omitted.
!
!-    TSPICE Version 2.1.0, 20-OCT-1999 (WLT)
!
!         Made the test environment specific because we can't get
!         a point on the focal set of the ellipsoid on our edition
!         of PC-LINUX or the MAC
!
!-&


! $ Index_Entries
!
!     Test family for DNEARP routine
!
!-&


!
!     Spicelib Functions
!
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDOT


!
!     Local Variables
!

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DALT   ( 2 )
      DOUBLE PRECISION      DNEAR  ( 6 )
      DOUBLE PRECISION      EALT   ( 2 )
      DOUBLE PRECISION      EDNEAR ( 6 )
      DOUBLE PRECISION      NPOINT1( 3 )
      DOUBLE PRECISION      NPOINT2( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TMPSTA ( 6 )

      LOGICAL               FOUND


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DNEARP' )

!
!     *****************************************************************
!
!     Normal cases: DNEARP
!
!     *****************************************************************


!
!     --- Case: -------------------------------------------------------
!
#if defined(SPICE_COMPILER_INTEL)
      CALL TCASE ( 'Make sure that the found flag is set to ' &
      //           'FALSE for a point on the focal set of the ' &
      //           'ellipsoid. ' )



      A          = 10.0D0
      B          = 20.0D0
      C          = 20.0D0

      STATE( 1 ) = 0.0D0
      STATE( 2 ) = 0.0D0
      STATE( 3 ) = 0.0D0
      STATE( 4 ) = 1.0D0
      STATE( 5 ) = 2.0D0
      STATE( 6 ) = 3.0D0

      CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )


!
!     --- Case: -------------------------------------------------------
!
#else
#endif
      CALL TCASE ( 'Check the results of DNEARP when the body in ' &
      //           'question is the unit sphere. ' )

      A           = 1.0D0
      B           = 1.0D0
      C           = 1.0D0

      STATE ( 1 ) = 10.0D0
      STATE ( 2 ) = 11.0D0
      STATE ( 3 ) = 12.0D0
      STATE ( 4 ) =  2.0D0
      STATE ( 5 ) =  3.0D0
      STATE ( 6 ) = -1.0D0
!
!     When the body is the unit sphere, the near point is just
!     the unit vector that is parallel to the position component
!     of the state vector.  The velocity of the near point
!     is the derivative of this unit vector  DVHAT handles the
!     whole problem.
!
      CALL DVHAT ( STATE, EDNEAR )

!
!     The latitude will be the distance from the origin minus 1.
!     The rate of change of
!
      EALT(1) = VNORM(STATE) - 1.0D0
      EALT(2) = VDOT (EDNEAR, STATE(4) )

      CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )


      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
      CALL CHCKAD ( 'DNEAR', DNEAR, '~/', EDNEAR, 6, 1.0D-13, OK )
      CALL CHCKAD ( 'DALT',  DALT,  '~/', EALT,   2, 1.0D-13, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check state of near point over a vertex of ' &
      //           'an ellipsoid. ' )

      A           = 4.0D0
      B           = 4.0D0
      C           = 9.0D0


      STATE ( 1 ) =  0.0D0
      STATE ( 2 ) =  0.0D0
      STATE ( 3 ) = 12.0D0
      STATE ( 4 ) =  1.0D0
      STATE ( 5 ) =  2.0D0
      STATE ( 6 ) =  5.0D0


      EDNEAR( 1 ) =  0.0D0
      EDNEAR( 2 ) =  0.0D0
      EDNEAR( 3 ) =  9.0D0
      EDNEAR( 4 ) =  0.75D0
      EDNEAR( 5 ) =  1.50D0
      EDNEAR( 6 ) =  0.0D0

      EALT  ( 1 ) =  3.0D0
      EALT  ( 2 ) =  5.0D0

      CALL NEARPT ( STATE, A, B, C, NPOINT1, ALT )
      CALL VLCOM  ( 1.0D0, STATE, 0.00001D0, STATE(4), TMPSTA )
      CALL MOVED  ( TMPSTA,       6,                   STATE  )
      CALL NEARPT ( STATE, A, B, C, NPOINT2, ALT )

      CALL VLCOM  ( 100000.0D0, NPOINT2, &
                   -100000.0D0, NPOINT1, EDNEAR(4) )

      STATE ( 1 ) =  0.0D0
      STATE ( 2 ) =  0.0D0
      STATE ( 3 ) = 12.0D0
      STATE ( 4 ) =  1.0D0
      STATE ( 5 ) =  2.0D0
      STATE ( 6 ) =  5.0D0

      CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
      CALL CHCKAD ( 'DNEAR', DNEAR, '~',  EDNEAR, 6, 1.0D-5,  OK )
      CALL CHCKAD ( 'DALT',  DALT,  '~/', EALT,   2, 1.0D-13, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
