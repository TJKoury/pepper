!$Procedure SPKE19 ( SPK, evaluate record, type 19 )

      SUBROUTINE SPKE19 ( ET, RECORD, STATE )

! $ Abstract
!
!     Evaluate a single data record from a type 19 SPK segment.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     SPK
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk19.inc'
      INCLUDE 'spkrec.inc'

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXREC     P   Maximum size of SPK record.
!     ET         I   Epoch for which a state is desired.
!     RECORD     I   Record from a type 19 SPK segment valid for ET.
!     STATE      O   State (position and velocity) at epoch ET.
!
! $ Detailed_Input
!
!     ET       is the epoch for which a state vector is desired,
!              expressed as seconds past J2000 TDB.
!
!     RECORD   is a record from a type 19 SPK segment which, when
!              evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative to
!              some center, in the reference frame associated
!              with the data packets. Usually the body, center
!              and frame are those of the SPK segment from which
!              the packets were read.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epoch 1              |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | epoch n              |
!                 +----------------------+
!
! $ Detailed_Output
!
!     STATE    is the state vector at epoch ET. Its contents are, in
!              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
!
! $ Parameters
!
!     MAXREC   is the maximum size of SPK record. See the SPICELIB
!              routine SPKPVN for details.
!
! $ Exceptions
!
!     1)  Most types of errors in the input record cannot be diagnosed
!         by this routine. This routine assumes that the input record
!         is valid.
!
!     2)  If the subtype code in the input record is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 19 (ESOC/DDID piecewise
!     interpolation) SPK segments is described in the SPK Required
!     Reading.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in a raw form, taken
!     directly from the segment. As such, it will be not be directly
!     useful to a user unless they have a complete understanding of the
!     structure of the data type. Given that understanding, however,
!     the SPKRnn routines could be used to "dump" and check segment data
!     for a particular epoch before evaluating the record to obtain a
!     state vector, as in the example that follows.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 19 ) THEN
!
!              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE19 ( ET, RECORD, STATE )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  This routine assumes that the input record is valid. Any
!         checking of the input data is assumed to have been performed
!         when the source SPK file was created.
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
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2. Now performs
!        computations in-line, rather than calling
!        SPKE18.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     evaluate type_19 spk_segment
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      LGRINT
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Index of subtype code in record:
!
      INTEGER               SBTIDX
      PARAMETER           ( SBTIDX = 1 )

!
!     Index of packet count in record:
!
      INTEGER               CNTIDX
      PARAMETER           ( CNTIDX = 2 )

!
!     Index at which packets start:
!
      INTEGER               PKTIDX
      PARAMETER           ( PKTIDX = 3 )

!
!     Local variables
!
      DOUBLE PRECISION      LOCREC ( MAXREC )
      DOUBLE PRECISION      WORK   ( MAXREC * 2,  2 )
      DOUBLE PRECISION      VBUFF  ( 6 )

      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               PACKSZ
      INTEGER               SUBTYP
      INTEGER               TO
      INTEGER               XSTART
      INTEGER               YSTART


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKE19' )

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      SUBTYP =  NINT( RECORD(SBTIDX) )

      IF ( SUBTYP .EQ. S19TP0 ) THEN

         PACKSZ = S19PS0

      ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN

         PACKSZ = S19PS1

      ELSE IF ( SUBTYP .EQ. S19TP2 ) THEN

         PACKSZ = S19PS2

      ELSE

         CALL SETMSG ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                      )
         CALL CHKOUT ( 'SPKE19'                                   )
         RETURN

      END IF

!
!     Get the packet count.
!
      N = NINT( RECORD(CNTIDX) )


      IF ( SUBTYP .EQ. S19TP0 ) THEN
!
!        We interpolate each state component in turn.  Position and
!        velocity are interpolated separately.
!
         XSTART = 3  +  PACKSZ * N

         DO I = 1, 3

            DO J = 1, N
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               FROM         = 2 + PACKSZ*(J-1) + I
               TO           =     2 * J        - 1

               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 3 )

            END DO

!
!           Interpolate the Ith position and velocity components of the
!           state.  We'll keep the position and overwrite the velocity.
!
            CALL HRMINT ( N, &
                          RECORD(XSTART), &
                          LOCREC, &
                          ET, &
                          WORK, &
                          STATE(I  ), &
                          STATE(I+3)      )

         END DO

!
!        Now interpolate velocity, using separate velocity data and
!        acceleration.
!
         DO I = 1, 3

            DO J = 1, N
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               FROM         = 2  +  PACKSZ*(J-1)  +  PACKSZ/2  +  I
               TO           =       2 * J         -  1

               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 3 )

            END DO

!
!           Interpolate the Ith velocity and acceleration components of
!           the state.  We'll capture the result in a temporary buffer,
!           then transfer the velocity to the output state array.
!
            CALL HRMINT ( N, &
                          RECORD(XSTART), &
                          LOCREC, &
                          ET, &
                          WORK, &
                          VBUFF(I  ), &
                          VBUFF(I+3)     )
         END DO

!
!        Fill in the velocity in the output state using the results of
!        interpolating velocity and acceleration.
!
         CALL VEQU ( VBUFF, STATE(4) )


      ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN
!
!        We perform Lagrange interpolation on each state component.
!
!        We'll transpose the state information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.
!
         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) )

!
!        We interpolate each state component in turn.
!
         XSTART   =   3   +  N * PACKSZ

         DO I = 1, PACKSZ

            YSTART    =   3   +  N * (I-1)

            STATE(I)  =   LGRINT ( N, &
                                   RECORD(XSTART), &
                                   RECORD(YSTART), &
                                   LOCREC, &
                                   ET              )
         END DO


      ELSE IF ( SUBTYP .EQ. S19TP2 ) THEN
!
!        We perform Hermite interpolation on each position component
!        and corresponding velocity component.
!
         XSTART   =   3   +  N * PACKSZ

         DO I = 1, 3

            DO J = 1, N
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               FROM         = 2 + PACKSZ*(J-1) + I
               TO           =     2 * J        - 1

               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 3 )

            END DO

!
!           Interpolate the Ith position and velocity components of the
!           state.
!
            CALL HRMINT ( N, &
                          RECORD(XSTART), &
                          LOCREC, &
                          ET, &
                          WORK, &
                          STATE(I), &
                          STATE(I+3)      )
         END DO


      ELSE
!
!         This is a backstop case.
!
         CALL SETMSG ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                      )
         CALL CHKOUT ( 'SPKE19'                                   )
         RETURN

      END IF

      CALL CHKOUT ( 'SPKE19' )
      RETURN
      END
