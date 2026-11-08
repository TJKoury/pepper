!$Procedure CKE02 ( C-kernel, evaluate pointing record, data type 2 )

      subroutine cke02 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Evaluate a pointing record returned by CKR02 from a CK data type 2
!     segment. Return the C-matrix and angular velocity vector
!     associated with the time CLKOUT.
!
! $ Required_Reading
!
!     CK
!     ROTATION
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      logical               needav
      real(wp)              record ( *    )
      real(wp)              cmat   ( 3, 3 )
      real(wp)              av     ( 3    )
      real(wp)              clkout

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NEEDAV     I   .TRUE. if angular velocity is requested.
!     RECORD     I   Data type 2 pointing record.
!     CMAT       O   C-matrix.
!     AV         O   Angular velocity vector.
!     CLKOUT     O   SCLK associated with C-matrix.
!
! $ Detailed_Input
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
!     RECORD   is a set of double precision numbers returned by CKR02
!              that contain sufficient information from a data type
!              2 pointing segment to evaluate the C-matrix and the
!              angular velocity vector for a particular instance.
!
!              The contents of RECORD are as follows:
!
!                 RECORD( 1  ) = start SCLKDP of interval
!
!                 RECORD( 2  ) = SCLK for which pointing was found
!
!                 RECORD( 3  ) = seconds / tick rate
!
!                 RECORD( 4  ) = q0
!                 RECORD( 5  ) = q1
!                 RECORD( 6  ) = q2
!                 RECORD( 7  ) = q3
!
!                 RECORD( 8  ) = av1
!                 RECORD( 9  ) = av2
!                 RECORD( 10 ) = av3
!
!              The quantities q0 - q3 are the components of the
!              quaternion that represents the C - matrix associated
!              with the start of the interval. The quantities av1,
!              av2, and av3 are the components of the angular velocity
!              vector.
!
! $ Detailed_Output
!
!     CMAT     is a rotation matrix that transforms the components
!              of a vector expressed in the inertial frame given in
!              the segment to components expressed in the instrument
!              fixed frame at the returned time.
!
!              Thus, if a vector v has components x, y, z in the
!              inertial frame, then v has components x', y', z' in the
!              instrument fixed frame where:
!
!                   [ x' ]     [          ] [ x ]
!                   | y' |  =  |   CMAT   | | y |
!                   [ z' ]     [          ] [ z ]
!
!              If the x', y', z' components are known, use the
!              transpose of the C-matrix to determine x, y, z as
!              follows.
!
!                   [ x ]      [          ]T    [ x' ]
!                   | y |  =   |   CMAT   |     | y' |
!                   [ z ]      [          ]     [ z' ]
!                            (Transpose of CMAT)
!
!     AV       is the angular velocity vector. The angular velocity
!              contained in RECORD is returned only if NEEDAV is .TRUE.
!
!              The direction of the angular velocity vector gives
!              the right-handed axis about which the instrument fixed
!              reference frame is rotating. The magnitude of AV is
!              the magnitude of the instantaneous velocity of the
!              rotation, in radians per second.
!
!              The angular velocity vector is returned in component
!              form
!
!                       AV = [ AV1  , AV2  , AV3  ]
!
!              which is in terms of the inertial coordinate frame
!              specified in the segment descriptor.
!
!     CLKOUT   is the encoded SCLK associated with the returned
!              C-matrix and angular velocity vector.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  No checking is done to determine whether RECORD is valid.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     For a detailed description of the structure of a type 2 pointing
!     segment, see the CK Required Reading.
!
!     Pointing data in a type 2 segment consists of intervals during
!     which the orientation of the spacecraft structure can be described
!     by an initial C-matrix and a constant angular velocity vector.
!     From the information contained in the pointing record returned by
!     CKR02, this subroutine calculates and returns the C-matrix
!     associated with the time returned by CKR02. It also returns the
!     angular velocity vector contained in the pointing record.
!
! $ Examples
!
!     A call to a CKEnn routine is almost always preceded by a call to
!     the corresponding CKRnn routine, which gets the logical record
!     that CKEnn evaluates.
!
!     The following code fragment searches through a file (represented
!     by HANDLE) for all segments applicable to the Voyager 2 wide angle
!     camera, for a particular spacecraft clock time, that are of data
!     types 1 or 2. It then evaluates the pointing for that epoch and
!     prints the result.
!
!
!           SC     = -32
!           INST   = -32002
!     C
!     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
!     C
!           CALL FURNSH ( 'VGR_SCLK.TSC'        )
!           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
!
!     C
!     C     Get the spacecraft clock time. Must encode it for use
!     C     in the C-kernel.
!     C
!
!           WRITE (*,*) 'Enter spacecraft clock time string:'
!           READ (*,FMT='(A)') SCLKCH
!           CALL SCENCD ( SC, SCLKCH, SCLKDP )
!
!     C
!     C     Search from the beginning through all segments.
!     C
!           CALL DAFBFS ( HANDLE )
!           CALL DAFFNA ( SFND   )
!
!           DO WHILE ( SFND )
!
!              CALL DAFGN ( IDENT                 )
!              CALL DAFGS ( DESCR                 )
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!              IF ( INST          .EQ. ICD( 1 )  .AND.
!          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
!          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
!
!                 DTYPE = ICD ( 3 )
!
!                 IF ( DTYPE .EQ. 1 ) THEN
!
!                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                      RECORD, FOUND                       )
!
!                    IF ( FOUND ) THEN
!                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
!                    END IF
!
!                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
!
!                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
!          .                      RECORD, FOUND )
!
!                    IF ( FOUND ) THEN
!                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
!                    END IF
!
!                 END IF
!
!                 IF ( FOUND ) THEN
!
!                    WRITE (*,*) 'Segment descriptor and identifier:'
!                    WRITE (*,*) DCD, ICD
!                    WRITE (*,*) IDENT
!
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*) CMAT
!
!                 END IF
!
!              END IF
!
!              CALL DAFFNA ( SFND )
!
!           END DO
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
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
!
!        Removed non-standard end-of-declarations marker
!        'C%&END_DECLARATIONS' from comments.
!
!-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
!
!-&


! $ Index_Entries
!
!     evaluate CK type_2 pointing data record
!
!-&


!
!     SPICELIB functions
!
      logical               return

      real(wp)              vnorm




!
!     Local variables
!
      real(wp)              quat   ( 4    )
      real(wp)              cbase  ( 3, 3 )
      real(wp)              rot    ( 3, 3 )
      real(wp)              avtemp (    3 )
      real(wp)              angle
      real(wp)              time


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Copy the returned encoded SCLK time into CLKOUT.
!
      clkout = record(2)

!     The quaternion stored in RECORD represents the C - matrix
!     corresponding to the start time of the interval.  The angular
!     velocity vector is constant throughout the interval and gives
!     the axis and rate by which the spacecraft is rotating.
!
!     Copy the quaternion and the angular velocity from RECORD.
!
!        RECORD ( 4 ) = q0
!        RECORD ( 5 ) = q1
!        RECORD ( 6 ) = q2
!        RECORD ( 7 ) = q3
!
!        RECORD ( 8  ) = av1
!        RECORD ( 9  ) = av2
!        RECORD ( 10 ) = av3
!
      call vequg ( record(4), 4, quat )
      call vequ  ( record(8), avtemp  )

!
!     Calculate the angle of the rotation.
!
!        RECORD ( 1 ) = The start time of the interval.
!        RECORD ( 2 ) = The time that pointing was returned for.
!        RECORD ( 3 ) = The number of seconds per SCLK tick.
!
      time  =  ( record(2) - record(1) ) * record(3)

      angle =    time * vnorm ( avtemp )

!
!     Construct a matrix which rotates vectors by ANGLE radians about
!     AVTEMP.
!
      call axisar ( avtemp, angle, rot )

!
!     Convert the quaternion to a C - matrix.
!
      call q2m ( quat, cbase )
!
!     Rotate each of the axis vectors of the spacecraft instrument frame
!     by ANGLE radians about AVTEMP. (AVTEMP is given in the same
!     inertial frame as the C - matrix.)  The resulting matrix is the
!     transpose of the requested C - matrix.
!
!        [       ]       [       ] T         [        ] T
!        [  ROT  ]   *   [ CBASE ]     =     [  CMAT  ]
!        [       ]       [       ]           [        ]
!
!     OR
!
!        [       ]       [       ] T         [        ]
!        [ CBASE ]   *   [  ROT  ]     =     [  CMAT  ]
!        [       ]       [       ]           [        ]
!

      call mxmt ( cbase, rot, cmat )

!
!     Return the angular velocity only if it is requested.
!
      if ( needav ) then
         call vequ ( avtemp, av )
      end if


      return
      end
