C$Procedure F_EQNCPV (Family of tests for EQNCPV)

      SUBROUTINE F_EQNCPV ( OK )

C$ Abstract
C
C     This routine performs a basic comparison of EQNCPV when
C     the pole of the reference frame is aligned with the
C     z-axis of the inertial frame and the rate of change of
C     the longitude of the node and argument of periapse is
C     zero.
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
C     None.
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
C
C$ Version
C
C-    TSPICE Version 1.2.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.1.0, 27-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MXV calls.
C
C-    TSPICE Version 1.0.0, 14-JAN-1998 (WLT)
C
C        The relative error tests were relaxed from 1.0D-12 to 5.0D-12
C        so that comparisons will pass on the PC which for some reason
C        isn't able to achieve the same performance as the UNIX
C        machines.
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD

C
C     Local Variables
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      ARGP
      DOUBLE PRECISION      DECPOL
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      ELTS(8)
      DOUBLE PRECISION      EQEL(9)
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIVDPD
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      INC
      DOUBLE PRECISION      M0
      DOUBLE PRECISION      N
      DOUBLE PRECISION      NODE
      DOUBLE PRECISION      P
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      TENDPD
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      ROT    ( 3,3 )
      DOUBLE PRECISION      TOINRT ( 3,3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      TMPSTA ( 6 )

      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      ROTTO  ( 6, 6 )
      DOUBLE PRECISION      ROTFRM ( 6, 6 )
      DOUBLE PRECISION      TEMP   ( 6, 6 )

      DOUBLE PRECISION      K ( 3 )
      DOUBLE PRECISION      X ( 3 )
      DOUBLE PRECISION      Y ( 3 )
      DOUBLE PRECISION      Z ( 3 )


      INTEGER               I
      INTEGER               J



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_EQNCPV' )



      CALL TCASE ( 'This compares EQNCPV with CONICS for the '
     .//           'case when the rates of node and argument of '
     .//           'periapse are zero and the pole of the '
     .//           'central frame is aligned with the pole of an '
     .//           'inertial frame. ' )



         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0


         ELTS(1) = P
         ELTS(2) = ECC
         ELTS(3) = INC
         ELTS(4) = NODE
         ELTS(5) = ARGP
         ELTS(6) = M0
         ELTS(7) = T0
         ELTS(8) = GM


         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = 0.0D0
         EQEL(8) = N
         EQEL(9) = 0.0D0

         RAPOL    = -HALFPI()
         DECPOL   =  HALFPI()



         ET = T0 - 10000.0D0

         DO I = 1, 100

            ET = ET + 250.0D0

            CALL CONICS ( ELTS, ET, STATE1 )
            CALL EQNCPV ( ET,   T0, EQEL, RAPOL, DECPOL, STATE2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ('POSITION', STATE2(1), '~/', STATE1(1), 3,
     .                    5.0D-12,   OK )
            CALL CHCKAD ('VELOCITY', STATE2(4), '~/', STATE1(4), 3,
     .                    5.0D-12,   OK )

         END DO

      CALL TCASE ( 'Test to make sure we can accurately compute '
     .//           'the state of an object that has non-zero '
     .//           'rates for the longitude of the ascending '
     .//           'node. ' )


         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0


         ELTS(1) = P
         ELTS(2) = ECC
         ELTS(3) = INC
         ELTS(4) = NODE
         ELTS(5) = ARGP
         ELTS(6) = M0
         ELTS(7) = T0
         ELTS(8) = GM
C
C        We want a rate for the node of 10 degrees/day.
C
         TENDPD  = ( 10.0D0 / 86400.0D0 ) * RPD()

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = TENDPD
         EQEL(8) = N + TENDPD
         EQEL(9) = TENDPD

         RAPOL    = -HALFPI()
         DECPOL   =  HALFPI()


         ET = T0 - 10000.0D0

         DO I = 1, 100

            ET    =  ET + 250.0D0

            THETA = (ET - T0) * TENDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*TENDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*TENDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*TENDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*TENDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL CONICS ( ELTS,  ET,   STATE )
            CALL MXVG   ( XFORM,       STATE,    6, 6,   STATE1 )

            CALL EQNCPV ( ET,   T0, EQEL, RAPOL, DECPOL, STATE2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ('POSITION', STATE2(1), '~/', STATE1(1), 3,
     .                    5.0D-12,   OK )
            CALL CHCKAD ('VELOCITY', STATE2(4), '~/', STATE1(4), 3,
     .                    5.0D-12,   OK )

         END DO

      CALL TCASE ( 'Test to make sure that we can accurately '
     .//           'compute the state of an object that has a '
     .//           'non-zero rate for the argument of periapse. ' )


         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0


         ELTS(1) = P
         ELTS(2) = ECC
         ELTS(3) = INC
         ELTS(4) = NODE
         ELTS(5) = ARGP
         ELTS(6) = M0
         ELTS(7) = T0
         ELTS(8) = GM
C
C        We want a rate for the node of 10 degrees/day.
C
         FIVDPD  = ( 5.0D0 / 86400.0D0 ) * RPD()

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD
         EQEL(8) = N + FIVDPD
         EQEL(9) = 0.0D0

         RAPOL   = -HALFPI()
         DECPOL  =  HALFPI()

         ROT(1,1) =   DCOS(NODE)
         ROT(2,1) =   DSIN(NODE)
         ROT(3,1) =   0.0D0

         ROT(1,2) =  -DCOS(INC) * DSIN(NODE)
         ROT(2,2) =   DCOS(INC) * DCOS(NODE)
         ROT(3,2) =   DSIN(INC)

         ROT(1,3) =   DSIN(INC) * DSIN(NODE)
         ROT(2,3) =  -DSIN(INC) * DCOS(NODE)
         ROT(3,3) =   DCOS(INC)


         DO I = 1,3
            DO J = 1,3
               ROTTO(I,  J  ) = ROT(I,J)
               ROTTO(I+3,J+3) = ROT(I,J)
               ROTTO(I+3,J  ) = 0.0D0
               ROTTO(I,  J+3) = 0.0D0
            END DO
         END DO

         CALL XPOSEG ( ROTTO, 6, 6, ROTFRM )

         ET = T0 - 10000.0D0

         DO I = 1, 100

            ET      =  ET        + 250.0D0
            THETA   =  (ET - T0) * FIVDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*FIVDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*FIVDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL MXMG ( XFORM, ROTFRM, 6, 6, 6, TEMP  )
            CALL MXMG ( ROTTO, TEMP,   6, 6, 6, XFORM )

            CALL CONICS ( ELTS,  ET,   STATE )
            CALL MXVG   ( XFORM,       STATE,    6, 6,   STATE1 )

            CALL EQNCPV ( ET,   T0, EQEL, RAPOL, DECPOL, STATE2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ('POSITION', STATE2(1), '~/', STATE1(1), 3,
     .                    5.0D-12,   OK )
            CALL CHCKAD ('VELOCITY', STATE2(4), '~/', STATE1(4), 3,
     .                    5.0D-12,   OK )

         END DO

      CALL TCASE ( 'Test the equinoctial propagator when '
     .//           'precession of both the node and argument of '
     .//           'periapse are non-zero. ' )



         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0


         ELTS(1) = P
         ELTS(2) = ECC
         ELTS(3) = INC
         ELTS(4) = NODE
         ELTS(5) = ARGP
         ELTS(6) = M0
         ELTS(7) = T0
         ELTS(8) = GM
C
C        We want a rate for the node of 10 degrees/day.
C
         FIVDPD  = ( 5.0D0 / 86400.0D0 ) * RPD()

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD + TENDPD
         EQEL(8) = N + FIVDPD + TENDPD
         EQEL(9) = TENDPD

         RAPOL   = -HALFPI()
         DECPOL  =  HALFPI()

         ROT(1,1) =   DCOS(NODE)
         ROT(2,1) =   DSIN(NODE)
         ROT(3,1) =   0.0D0

         ROT(1,2) =  -DCOS(INC) * DSIN(NODE)
         ROT(2,2) =   DCOS(INC) * DCOS(NODE)
         ROT(3,2) =   DSIN(INC)

         ROT(1,3) =   DSIN(INC) * DSIN(NODE)
         ROT(2,3) =  -DSIN(INC) * DCOS(NODE)
         ROT(3,3) =   DCOS(INC)


         DO I = 1,3
            DO J = 1,3
               ROTTO(I,  J  ) = ROT(I,J)
               ROTTO(I+3,J+3) = ROT(I,J)
               ROTTO(I+3,J  ) = 0.0D0
               ROTTO(I,  J+3) = 0.0D0
            END DO
         END DO

         CALL XPOSEG ( ROTTO, 6, 6, ROTFRM )

         ET = T0 - 10000.0D0

         DO I = 1, 100

            ET      =  ET        + 250.0D0
            THETA   =  (ET - T0) * FIVDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*FIVDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*FIVDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL MXMG ( XFORM, ROTFRM, 6, 6, 6, TEMP  )
            CALL MXMG ( ROTTO, TEMP,   6, 6, 6, XFORM )

            CALL CONICS ( ELTS,  ET,   STATE1 )
            CALL MXVG   ( XFORM,       STATE1,    6, 6,   STATE )

            THETA = (ET - T0) * TENDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*TENDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*TENDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*TENDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*TENDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL MXVG   ( XFORM, STATE,  6, 6,           STATE1 )

            CALL EQNCPV ( ET,   T0, EQEL, RAPOL, DECPOL, STATE2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ('POSITION', STATE2(1), '~/', STATE1(1), 3,
     .                    5.0D-12,   OK )
            CALL CHCKAD ('VELOCITY', STATE2(4), '~/', STATE1(4), 3,
     .                    5.0D-12,   OK )

         END DO

      CALL TCASE ( 'Apply the same test as the previous case '
     .//           'with the RA and DEC of the pole of the '
     .//           'equatorial frame set so that the axes of the '
     .//           'equatorial frame are not aligned with the '
     .//           'inertial frame. ' )





         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0


         ELTS(1) = P
         ELTS(2) = ECC
         ELTS(3) = INC
         ELTS(4) = NODE
         ELTS(5) = ARGP
         ELTS(6) = M0
         ELTS(7) = T0
         ELTS(8) = GM
C
C        We want a rate for the node of 10 degrees/day.
C
         FIVDPD  = ( 5.0D0 / 86400.0D0 ) * RPD()

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD + TENDPD
         EQEL(8) = N + FIVDPD + TENDPD
         EQEL(9) = TENDPD

         RAPOL   = 30.D0 * RPD()
         DECPOL  = 60.D0 * RPD()

         ROT(1,1) =   DCOS(NODE)
         ROT(2,1) =   DSIN(NODE)
         ROT(3,1) =   0.0D0

         ROT(1,2) =  -DCOS(INC) * DSIN(NODE)
         ROT(2,2) =   DCOS(INC) * DCOS(NODE)
         ROT(3,2) =   DSIN(INC)

         ROT(1,3) =   DSIN(INC) * DSIN(NODE)
         ROT(2,3) =  -DSIN(INC) * DCOS(NODE)
         ROT(3,3) =   DCOS(INC)


         Z(1)     =   DCOS(RAPOL)*DCOS(DECPOL)
         Z(2)     =   DSIN(RAPOL)*DCOS(DECPOL)
         Z(3)     =   DSIN(DECPOL)


         K(1)     = 0.0D0
         K(2)     = 0.0D0
         K(3)     = 1.0D0

         CALL UCRSS ( K, Z, X )
         CALL UCRSS ( Z, X, Y )

         TOINRT(1,1) = X(1)
         TOINRT(2,1) = X(2)
         TOINRT(3,1) = X(3)

         TOINRT(1,2) = Y(1)
         TOINRT(2,2) = Y(2)
         TOINRT(3,2) = Y(3)

         TOINRT(1,3) = Z(1)
         TOINRT(2,3) = Z(2)
         TOINRT(3,3) = Z(3)


         DO I = 1,3
            DO J = 1,3
               ROTTO(I,  J  ) = ROT(I,J)
               ROTTO(I+3,J+3) = ROT(I,J)
               ROTTO(I+3,J  ) = 0.0D0
               ROTTO(I,  J+3) = 0.0D0
            END DO
         END DO

         CALL XPOSEG ( ROTTO, 6, 6, ROTFRM )

         ET = T0 - 10000.0D0

         DO I = 1, 100

            ET      =  ET        + 250.0D0
            THETA   =  (ET - T0) * FIVDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*FIVDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*FIVDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*FIVDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL MXMG ( XFORM, ROTFRM, 6, 6, 6, TEMP  )
            CALL MXMG ( ROTTO, TEMP,   6, 6, 6, XFORM )

            CALL CONICS ( ELTS,  ET,   STATE1 )
            CALL MXVG   ( XFORM,       STATE1,    6, 6,   STATE )

            THETA = (ET - T0) * TENDPD

            XFORM ( 1, 1 ) =  DCOS(THETA)
            XFORM ( 2, 1 ) =  DSIN(THETA)
            XFORM ( 3, 1 ) =  0.0D0
            XFORM ( 4, 1 ) = -DSIN(THETA)*TENDPD
            XFORM ( 5, 1 ) =  DCOS(THETA)*TENDPD
            XFORM ( 6, 1 ) =  0.0D0

            XFORM ( 1, 2 ) = -DSIN(THETA)
            XFORM ( 2, 2 ) =  DCOS(THETA)
            XFORM ( 3, 2 ) =  0.0D0
            XFORM ( 4, 2 ) = -DCOS(THETA)*TENDPD
            XFORM ( 5, 2 ) = -DSIN(THETA)*TENDPD
            XFORM ( 6, 2 ) =  0.0D0

            XFORM ( 1, 3 ) =  0.0D0
            XFORM ( 2, 3 ) =  0.0D0
            XFORM ( 3, 3 ) =  1.0D0
            XFORM ( 4, 3 ) =  0.0D0
            XFORM ( 5, 3 ) =  0.0D0
            XFORM ( 6, 3 ) =  0.0D0

            XFORM ( 1, 4 ) =  0.0D0
            XFORM ( 2, 4 ) =  0.0D0
            XFORM ( 3, 4 ) =  0.0D0
            XFORM ( 4, 4 ) =  DCOS(THETA)
            XFORM ( 5, 4 ) =  DSIN(THETA)
            XFORM ( 6, 4 ) =  0.0D0

            XFORM ( 1, 5 ) =  0.0D0
            XFORM ( 2, 5 ) =  0.0D0
            XFORM ( 3, 5 ) =  0.0D0
            XFORM ( 4, 5 ) = -DSIN(THETA)
            XFORM ( 5, 5 ) =  DCOS(THETA)
            XFORM ( 6, 5 ) =  0.0D0

            XFORM ( 1, 6 ) =  0.0D0
            XFORM ( 2, 6 ) =  0.0D0
            XFORM ( 3, 6 ) =  0.0D0
            XFORM ( 4, 6 ) =  0.0D0
            XFORM ( 5, 6 ) =  0.0D0
            XFORM ( 6, 6 ) =  1.0D0

            CALL MXVG   ( XFORM,  STATE,  6, 6,          STATE1    )
            CALL MXV    ( TOINRT, STATE1,                TMPSTA    )
            CALL MXV    ( TOINRT, STATE1(4),             TMPSTA(4) )
            CALL MOVED  ( TMPSTA, 6,                     STATE1    )

            CALL EQNCPV ( ET,   T0, EQEL, RAPOL, DECPOL, STATE2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKAD ('POSITION', STATE2(1), '~/', STATE1(1), 3,
     .                    5.0D-12,   OK )
            CALL CHCKAD ('VELOCITY', STATE2(4), '~/', STATE1(4), 3,
     .                    5.0D-12,   OK )

         END DO




      CALL T_SUCCESS ( OK )
      RETURN
      END
