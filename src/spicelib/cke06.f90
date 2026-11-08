!$Procedure CKE06 ( C-Kernel, evaluate, type 6 )

      subroutine cke06 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Evaluate a single data record from a type 6 CK segment.
!
! $ Required_Reading
!
!     CK
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      include 'ck06.inc'
      include 'ckparam.inc'


      logical               needav
      real(wp)              record   ( * )
      real(wp)              cmat     ( 3, 3 )
      real(wp)              av       ( 3 )
      real(wp)              clkout

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NEEDAV     I   .TRUE. if angular velocity is requested.
!     RECORD    I-O  Data type 6 record.
!     CMAT       O   C-matrix.
!     AV         O   Angular velocity vector.
!     CLKOUT     O   SCLK associated with C-matrix.
!
! $ Detailed_Input
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
!     RECORD   is a record from a type 6 CK segment which, when
!              evaluated at the epoch contained in its first
!              element, will give the attitude and angular velocity
!              of a spacecraft structure or instrument relative to a
!              base reference frame.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | evaluation epoch     |
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | nominal SCLK rate    |
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                 | packet 2             |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
!                See the CK Required Reading or the include file
!                ck06.inc for details on CK type 6 packet contents.
!
! $ Detailed_Output
!
!     RECORD   has been modified due to its use as a workspace array.
!              The contents are undefined.
!
!
!     CMAT     is a rotation matrix that transforms the components
!              of a vector expressed in the base frame given in
!              the segment to components expressed in the instrument
!              fixed frame at the returned time.
!
!              Thus, if a vector v has components x, y, z in the
!              base frame, then v has components x', y', z' in the
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
!
!     AV       is the angular velocity vector of the instrument fixed
!              frame defined by CMAT. The angular velocity is
!              returned only if NEEDAV is .TRUE.
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
!              which is in terms of the base coordinate frame
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
!     1)  If the input record contains an unrecognized subtype code, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the record subtype is one for which quaternion derivatives
!         are stored (subtypes 0 and 2), and if the Ith quaternion in
!         the input record is farther than its negative from the (I-1)st
!         quaternion in the record, an error is signaled by a routine
!         in the call tree of this routine.
!
!         For subtypes 1 and 3, this condition is not considered an
!         error: the closer to the preceding quaternion of the two
!         quaternion representations is used for interpolation.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of CK type 6 (MEX/Rosetta Attitude
!     file interpolation) CK segments is described in the CK Required
!     Reading.
!
! $ Examples
!
!     The CKEnn routines are almost always used in conjunction with
!     the corresponding CKRnn routines, which read the records from
!     CK files.
!
!     The following code fragment searches through all of the segments
!     in a file applicable to the Mars Express spacecraft bus that
!     are of data type 6, for a particular spacecraft clock time.
!     It then evaluates the pointing for that epoch and prints the
!     result.
!
!           CHARACTER*(20)        SCLKCH
!           CHARACTER*(20)        SCTIME
!           CHARACTER*(40)        IDENT
!
!           INTEGER               I
!           INTEGER               SC
!           INTEGER               INST
!           INTEGER               HANDLE
!           INTEGER               DTYPE
!           INTEGER               ICD      (    6 )
!
!           DOUBLE PRECISION      SCLKDP
!           DOUBLE PRECISION      TOL
!           DOUBLE PRECISION      CLKOUT
!           DOUBLE PRECISION      DESCR    (    5 )
!           DOUBLE PRECISION      DCD      (    2 )
!           DOUBLE PRECISION      RECORD   (   17 )
!           DOUBLE PRECISION      CMAT     ( 3, 3 )
!           DOUBLE PRECISION      AV       (    3 )
!
!           LOGICAL               NEEDAV
!           LOGICAL               FND
!           LOGICAL               SFND
!
!
!           SC     = -41
!           INST   = -41000
!           DTYPE  =  6
!           NEEDAV = .FALSE.
!
!     C
!     C     Load the MEX SCLK kernel and the C-kernel.
!     C
!           CALL FURNSH ( 'MEX_SCLK.TSC'       )
!           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE )
!     C
!     C     Get the spacecraft clock time. Then encode it for use
!     C     in the C-kernel.
!     C
!           WRITE (*,*) 'Enter spacecraft clock time string:'
!           READ (*,FMT='(A)') SCLKCH
!
!           CALL SCENCD ( SC, SCLKCH, SCLKDP )
!     C
!     C     Use a tolerance of 2 seconds ( half of the nominal
!     C     separation between MEX pointing instances ).
!     C
!           CALL SCTIKS ( SC, '0000000002:000', TOL )
!
!     C
!     C     Search from the beginning of the CK file through all
!     C     of the segments.
!     C
!           CALL DAFBFS ( HANDLE )
!           CALL DAFFNA ( SFND   )
!
!           FND    = .FALSE.
!
!           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
!
!     C
!     C        Get the segment identifier and descriptor.
!     C
!              CALL DAFGN ( IDENT )
!              CALL DAFGS ( DESCR )
!     C
!     C        Unpack the segment descriptor into its integer and
!     C        double precision components.
!     C
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!     C
!     C        Determine if this segment should be processed.
!     C
!              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
!          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
!          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
!          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
!
!
!                 CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                   RECORD, FND )
!
!                 IF ( FND ) THEN
!
!                    CALL CKE06 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
!
!                    CALL SCDECD ( SC, CLKOUT, SCTIME )
!
!                    WRITE (*,*)
!                    WRITE (*,*) 'Segment identifier: ', IDENT
!                    WRITE (*,*)
!                    WRITE (*,*) 'Pointing returned for time: ',
!          .                      SCTIME
!                    WRITE (*,*)
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*)
!                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
!                    WRITE (*,*)
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
!     1)  This routine performs minimal error checking. The input data
!         are assumed to have been checked when the source CK file was
!         created.
!
!     2)  With the exception of the check described in item 2 of
!         the $Exceptions section above, the input data are assumed to
!         be suitable for the interpolation method specified by the
!         input record's subtype and packet count (which implies an
!         interpolating polynomial degree).
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     evaluate type_6 ck_segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'CKE06' )

!
!     Given that our nominally type 6 input record is actually a
!     valid type 5 record, we let the type 5 evaluator do the
!     work.
!
      call cke05 ( needav, record, cmat, av, clkout )

      call chkout ( 'CKE06' )
      return
      end
