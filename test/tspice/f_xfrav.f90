!$Procedure F_XFRAV ( Frame conversion to angular velocity )

      SUBROUTINE F_XFRAV ( OK )

! $ Abstract
!
!     This routine tests the "macro" routines XF2RAV and RAV2XF.
!     The tests performed check
!
!     1)  That the routines are nearly inverses of one another.
!
!     2)  That they produce equivalent objects for converting
!         states from one frame to another.
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
!     None.
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
!
! $ Version
!
!-    TSPICE Version 1.2.0, 29-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.1.0, 27-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VMINUS call.
!
!-    TSPICE Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL Functions.
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB Functions
!
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      HALFPI

!
!     Local Variables
!
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


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_XFRAV' )

      CALL TCASE ( 'This case tests that converting from a ' &
      //           'rotation and angular velocity to a state ' &
      //           'transformation and then back to rotation and ' &
      //           'angular velocity is very nearly the identity. ' &
      //           '200 different sub-cases are examined.' )

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

               CALL TSTMSG ( '#', 'The values of I and J are # ' &
               //                 'and # respectively. ' )
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

      CALL TCASE ( 'In this case we construct a rotation, ' &
      //           'angular velocity and state transformation ' &
      //           'matrix.  We use these to compute velocities ' &
      //           'in two different ways and compare the ' &
      //           'results. 200 sub-cases are examined.' )


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

               CALL TSTMSG ( '#', 'The values of I and J are # ' &
               //                 'and # respectively. ' )
               CALL TSTMSI ( I )
               CALL TSTMSI ( J )

               CALL LATREC ( R,  LONG,  LAT,       ROT1(1,1) )
               CALL FRAME  ( ROT1(1,1), ROT1(1,2), ROT1(1,3) )

               CALL LATREC ( R,   LAT, LONG, X   )
               CALL VSCL   ( MAG, X,         AV1 )

               CALL RAV2XF ( ROT1,  AV1,  XFORM )
!
!              First transform states using the state transformation
!              matrix.
!

               CALL MXVG   ( XFORM, STATE, 6, 6, STATE1 )
!
!              Now transform states using the rotation and angular
!              velocity.  Recall that the angular velocity of
!              FRAME1 with respect to FRAME2 is the opposite of
!              the angular velocity of FRAME2 with respect to FRAME1.
!
               CALL VMINUS ( AV1,   AV2         )
!
!              A part of the position and velocity is obtained by
!              simply rotating the original position and velocity.
!
               CALL MXV    ( ROT1,  STATE,     STATE2    )
               CALL MXV    ( ROT1,  STATE(4),  V1        )

!
!              The rest of the velocity is obtained by crossing
!              the angular velocity with the current position
!              in  FRAME2.
!
               CALL VCRSS  ( AV2,   STATE,    VTEMP        )
               CALL MXV    ( ROT1,  VTEMP,    V2           )
!
!              Add the results together to get the full velocity.
!
               CALL VADD   ( V1,    V2,       STATE2(4) )


               CALL CHCKXC ( .FALSE., ' ',  OK )
               CALL CHCKAD ( 'POSITION', STATE1, &
                             '~~/',      STATE2, &
                              3,         1.0D-14,   OK )
               CALL CHCKAD ( 'VELOCITY', STATE1(4), &
                              '~~/',     STATE2(4), &
                              3,         1.0D-14,   OK )

               CALL MOVED ( STATE2, 6, STATE )

               LONG = LONG + DLONG
               MAG  = MAG  + DMAG

            END DO


         END DO

         CALL TSTMSG ( '#', ' ' )


      CALL T_SUCCESS ( OK )
      RETURN
      END
