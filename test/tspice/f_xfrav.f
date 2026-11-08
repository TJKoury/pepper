C$Procedure F_XFRAV ( Frame conversion to angular velocity )

      SUBROUTINE F_XFRAV ( OK )

C$ Abstract
C
C     This routine tests the "macro" routines XF2RAV and RAV2XF.
C     The tests performed check
C
C     1)  That the routines are nearly inverses of one another.
C
C     2)  That they produce equivalent objects for converting
C         states from one frame to another.
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
C-    TSPICE Version 1.2.0, 29-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.1.0, 27-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VMINUS call.
C
C-    TSPICE Version 1.1.0, 20-OCT-1999 (WLT)
C
C        Declared PI to be an EXTERNAL Functions.
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
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      HALFPI

C
C     Local Variables
C
      INTEGER               I
      INTEGER               J


      DOUBLE PRECISION      AV1   (    3 )
      DOUBLE PRECISION      AV2   (    3 )

      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLONG
      DOUBLE PRECISION      DMAG

      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      R

      DOUBLE PRECISION      ROT1  ( 3, 3 )
      DOUBLE PRECISION      ROT2  ( 3, 3 )

      DOUBLE PRECISION      STATE ( 6 )
      DOUBLE PRECISION      STATE1( 6 )
      DOUBLE PRECISION      STATE2( 6 )

      DOUBLE PRECISION      V1    ( 3 )
      DOUBLE PRECISION      V2    ( 3 )
      DOUBLE PRECISION      VTEMP ( 3 )

      DOUBLE PRECISION      X     (    3 )
      DOUBLE PRECISION      XFORM ( 6, 6 )
      DOUBLE PRECISION      XFORM1( 6, 6 )
      DOUBLE PRECISION      XFORM2( 6, 6 )


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_XFRAV' )

      CALL TCASE ( 'This case tests that converting from a '
     .//           'rotation and angular velocity to a state '
     .//           'transformation and then back to rotation and '
     .//           'angular velocity is very nearly the identity. '
     .//           '200 different sub-cases are examined.' )

         R     =  1.0D0
         LAT   = -HALFPI()
         LONG  =  0.0D0
         MAG   =  1.0D-1

         DLAT  = PI()/11.0D0
         DLONG = PI()/10.0D0
         DMAG  = 0.1D0

         DO I = 1, 10

            LAT  = LAT + DLAT
            LONG = 0.0D0

            DO J = 1, 20

               CALL TSTMSG ( '#', 'The values of I and J are # '
     .         //                 'and # respectively. ' )
               CALL TSTMSI ( I )
               CALL TSTMSI ( J )

               CALL LATREC ( R,  LONG,  LAT,       ROT1(1,1) )
               CALL FRAME  ( ROT1(1,1), ROT1(1,2), ROT1(1,3) )

               CALL LATREC ( R,   LAT, LONG, X   )
               CALL VSCL   ( MAG, X,         AV1 )

               CALL RAV2XF ( ROT1,  AV1,  XFORM )
               CALL XF2RAV ( XFORM, ROT2, AV2   )

               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKAD ( 'ROT2', ROT2, '~', ROT1, 9, 1.0D-14, OK )
               CALL CHCKAD ( 'AV2',  AV2,  '~', AV1,  3, 1.0D-14, OK )

               CALL XPOSE ( ROT1, ROT2 )
               CALL MXV   ( ROT1, AV1, AV2 )
               CALL VSCLIP( -1.D0,     AV2 )
               CALL RAV2XF( ROT2, AV2, XFORM2 )
               CALL INVSTM( XFORM,     XFORM1 )

               CALL CHCKAD('XFORM1',XFORM1, '~', XFORM2, 9, 1.0D-14, OK)


               LONG = LONG + DLONG
               MAG  = MAG  + DMAG

            END DO


         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'In this case we construct a rotation, '
     .//           'angular velocity and state transformation '
     .//           'matrix.  We use these to compute velocities '
     .//           'in two different ways and compare the '
     .//           'results. 200 sub-cases are examined.' )


         R        =  1.0D0
         LAT      = -HALFPI()
         LONG     =  0.0D0
         MAG      =  1.0D-1

         DLAT     = PI()/11.0D0
         DLONG    = PI()/10.0D0
         DMAG     = 0.1D0

         STATE(1) = 6.0D0
         STATE(2) = 5.0D0
         STATE(3) = 4.0D0
         STATE(4) = 3.0D0
         STATE(5) = 2.0D0
         STATE(6) = 1.0D0

         DO I = 1, 10

            LAT  = LAT + DLAT
            LONG = 0.0D0

            DO J = 1, 20

               CALL TSTMSG ( '#', 'The values of I and J are # '
     .         //                 'and # respectively. ' )
               CALL TSTMSI ( I )
               CALL TSTMSI ( J )

               CALL LATREC ( R,  LONG,  LAT,       ROT1(1,1) )
               CALL FRAME  ( ROT1(1,1), ROT1(1,2), ROT1(1,3) )

               CALL LATREC ( R,   LAT, LONG, X   )
               CALL VSCL   ( MAG, X,         AV1 )

               CALL RAV2XF ( ROT1,  AV1,  XFORM )
C
C              First transform states using the state transformation
C              matrix.
C

               CALL MXVG   ( XFORM, STATE, 6, 6, STATE1 )
C
C              Now transform states using the rotation and angular
C              velocity.  Recall that the angular velocity of
C              FRAME1 with respect to FRAME2 is the opposite of
C              the angular velocity of FRAME2 with respect to FRAME1.
C
               CALL VMINUS ( AV1,   AV2         )
C
C              A part of the position and velocity is obtained by
C              simply rotating the original position and velocity.
C
               CALL MXV    ( ROT1,  STATE,     STATE2    )
               CALL MXV    ( ROT1,  STATE(4),  V1        )

C
C              The rest of the velocity is obtained by crossing
C              the angular velocity with the current position
C              in  FRAME2.
C
               CALL VCRSS  ( AV2,   STATE,    VTEMP        )
               CALL MXV    ( ROT1,  VTEMP,    V2           )
C
C              Add the results together to get the full velocity.
C
               CALL VADD   ( V1,    V2,       STATE2(4) )


               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKAD ( 'POSITION', STATE1,
     .                       '~~/',      STATE2,
     .                        3,         1.0D-14,   OK )
               CALL CHCKAD ( 'VELOCITY', STATE1(4),
     .                        '~~/',     STATE2(4),
     .                        3,         1.0D-14,   OK )

               CALL MOVED ( STATE2, 6, STATE )

               LONG = LONG + DLONG
               MAG  = MAG  + DMAG

            END DO


         END DO

         CALL TSTMSG ( '#', ' ' )


      CALL T_SUCCESS ( OK )
      RETURN
      END
