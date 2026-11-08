C$Procedure F_DNEARP ( Family of DNEARP tests )
 
      SUBROUTINE F_DNEARP ( OK )
 
C$ Abstract
C
C     This routine performs a set of rudimentary checks on the
C     routine DNEARP.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine DNEARP.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    TSPICE Version 2.8.1, 17-FEB-2021 (JDR)
C
C        Edited the header and comments to comply with NAIF standard.
C
C-    TSPICE Version 2.8.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 2.7.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 2.6.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 2.4.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 2.3.0, 27-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VLCOM call.
C
C-    TSPICE Version 2.2.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments (only FORTRAN ones because
C        MAC-OSX C environment has the same problem with the
C        focal point test as the Linux and classic Mac C.)
C
C-    TSPICE Version 2.2.0, 02-JAN-2002 (NJB) (EDW)
C
C        Added PC-LINUX_C and MACPPC_C to the set of environments
C        for which the focal point test is omitted.
C
C-    TSPICE Version 2.1.0, 20-OCT-1999 (WLT)
C
C         Made the test environment specific because we can't get
C         a point on the focal set of the ellipsoid on our edition
C         of PC-LINUX or the MAC
C
C-&
 
 
C$ Index_Entries
C
C     Test family for DNEARP routine
C
C-&
 
 
C
C     Spicelib Functions
C
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDOT
 
 
C
C     Local Variables
C
 
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
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DNEARP' )
 
C
C     *****************************************************************
C
C     Normal cases: DNEARP
C
C     *****************************************************************
 
 
C
C     --- Case: -------------------------------------------------------
C
#if defined(SPICE_COMPILER_INTEL)
      CALL TCASE ( 'Make sure that the found flag is set to '
     .//           'FALSE for a point on the focal set of the '
     .//           'ellipsoid. ' )
 
 
 
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
 
 
C
C     --- Case: -------------------------------------------------------
C
#else
#endif
      CALL TCASE ( 'Check the results of DNEARP when the body in '
     .//           'question is the unit sphere. ' )
 
      A           = 1.0D0
      B           = 1.0D0
      C           = 1.0D0
 
      STATE ( 1 ) = 10.0D0
      STATE ( 2 ) = 11.0D0
      STATE ( 3 ) = 12.0D0
      STATE ( 4 ) =  2.0D0
      STATE ( 5 ) =  3.0D0
      STATE ( 6 ) = -1.0D0
C
C     When the body is the unit sphere, the near point is just
C     the unit vector that is parallel to the position component
C     of the state vector.  The velocity of the near point
C     is the derivative of this unit vector  DVHAT handles the
C     whole problem.
C
      CALL DVHAT ( STATE, EDNEAR )
 
C
C     The latitude will be the distance from the origin minus 1.
C     The rate of change of
C
      EALT(1) = VNORM(STATE) - 1.0D0
      EALT(2) = VDOT (EDNEAR, STATE(4) )
 
      CALL DNEARP ( STATE, A, B, C, DNEAR, DALT, FOUND )
 
 
      CALL CHCKXC ( .FALSE., ' ',            OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
      CALL CHCKAD ( 'DNEAR', DNEAR, '~/', EDNEAR, 6, 1.0D-13, OK )
      CALL CHCKAD ( 'DALT',  DALT,  '~/', EALT,   2, 1.0D-13, OK )
 
 
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Check state of near point over a vertex of '
     .//           'an ellipsoid. ' )
 
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
 
      CALL VLCOM  ( 100000.0D0, NPOINT2,
     .             -100000.0D0, NPOINT1, EDNEAR(4) )
 
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
