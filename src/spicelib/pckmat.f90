!$Procedure PCKMAT ( PCK, get transformation matrix at time )

      SUBROUTINE PCKMAT ( BODY, ET, REF, TSIPM, FOUND )

! $ Abstract
!
!     Return the name of an inertial reference frame and the 6 x 6
!     state transformation matrix from that frame to the body fixed
!     frame of a given body at a specified epoch.
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
!     NAIF_IDS
!     ROTATION
!     TIME
!     PCK
!
! $ Keywords
!
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      IMPLICIT NONE

      INTEGER              BODY
      DOUBLE PRECISION     ET
      INTEGER              REF
      DOUBLE PRECISION     TSIPM ( 6, 6 )
      LOGICAL              FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of some body.
!     ET         I   Epoch of transformation.
!     REF        O   Integer code for inertial reference frame.
!     TSIPM      O   Transformation from Inertial to PM for BODY at ET.
!     FOUND      O   .TRUE. if data for BODY and ET are found.
!
! $ Detailed_Input
!
!     BODY     is the integer ID code of the body for which the
!              state transformation matrix is requested. Bodies
!              are numbered according to the standard NAIF
!              numbering scheme. The numbering scheme is
!              explained in the NAIF_IDS required reading file.
!
!     ET       is the epoch at which the state transformation
!              matrix is requested.
!
! $ Detailed_Output
!
!     REF      is the integer code for the inertial reference frame
!              of the state transformation matrix TSIPM. (See the
!              routine CHGIRF for a full list of inertial reference
!              frame names.)
!
!     TSIPM    is a 6x6 transformation matrix. It is used to
!              transform states from inertial coordinates to body
!              fixed (also called equator and prime meridian --- PM)
!              coordinates.
!
!              Given a state S in the inertial reference frame
!              specified by REF, the corresponding state in the body
!              fixed reference frame is given by the matrix vector
!              product:
!
!                 TSIPM * S
!
!              See the PCK required reading for further details
!              concerning PCK reference frames.
!
!              NOTE: The inverse of TSIPM is NOT its transpose. The
!              matrix, TSIPM, has the structure shown below:
!
!                          -            -
!                         |       :      |
!                         |   R   :  0   |
!                         | ......:......|
!                         |       :      |
!                         | dR_dt :  R   |
!                         |       :      |
!                          -            -
!
!              where R is a time varying rotation matrix and dR_dt
!              is its derivative. The inverse of this matrix is:
!
!                          -              -
!                         |     T  :       |
!                         |    R   :  0    |
!                         | .......:.......|
!                         |        :       |
!                         |      T :   T   |
!                         | dR_dt  :  R    |
!                         |        :       |
!                          -              -
!
!              The SPICE routine INVSTM is available for producing
!              this inverse.
!
!      FOUND      if the data allowing the computation of a state
!              transformation matrix for the requested time and body
!              are found in a binary PCK file, FOUND will have the
!              value .TRUE., otherwise it will have the value
!              .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the size of the type 20 PCK record to be  retrieved is too
!         large to fit into RECORD, the error SPICE(PCKRECTOOLARGE)
!         is signaled.
!
!     2)  If any issue is detected while reading PCK data, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If the requested transformation matrix cannot be computed
!         using data from loaded binary PCK files, FOUND is returned
!         with the value .FALSE. This is not a SPICE error.
!
! $ Files
!
!     This routine computes transformation matrices using data
!     provided by a loaded binary PCK kernel.
!
! $ Particulars
!
!     The matrix for transforming an inertial state into a body fixed
!     states is the 6x6 matrix shown below as a block structured
!     matrix.
!
!                 -            -
!                |       :      |
!                | TIPM  :  0   |
!                | ......:......|
!                |       :      |
!                | DTIPM : TIPM |
!                |       :      |
!                 -            -
!
!     If a binary PCK file record can be found for the time and body
!     requested, it will be used. The most recently loaded binary PCK
!     file has first priority, followed by previously loaded binary PCK
!     files in backward time order. If no binary PCK file has been
!     loaded, the text P_constants kernel file is used.
!
! $ Examples
!
!     Here we load a binary PCK files and use PCKEUL to get the
!     Euler angles.
!
!     C
!     C  Load binary PCK file.
!     C
!        CALL PCKLOF ('example.pck', HANDLE)
!
!     C  Call routine to get transformation matrix.
!
!        CALL PCKMAT ( BODY, ET, REF, TIPM, FOUND )
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
!     K.R. Gehringer     (JPL)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 03-JAN-2014 (NJB) (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!        Updated to support type 20. Changed long error message
!        for the case of RECORD having insufficient room: the
!        user is no longer advised to modify the record size.
!
!-    SPICELIB Version 2.0.0, 22-MAR-1995 (KRG) (KSZ)
!
!        Added PCK type 03. Added a new exception. Made some minor
!        comment changes.
!
!-    SPICELIB Version 1.0.0, 21-MAR-1995 (KSZ)
!
!        Replaces PCKEUL and returns the transformation
!        matrix rather than the Euler angles.
!
!-&


! $ Index_Entries
!
!     get state transformation matrix from binary PCK file
!
!-&


!
!     SPICELIB functions
!
      LOGICAL              FAILED
      LOGICAL              RETURN

!
!     Local Parameters
!
!     ND and NI values for a PCK file.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
!
!     Index for the reference frame code in the integer summary.
!
      INTEGER               NR
      PARAMETER           ( NR = 2 )
!
!     Length of the descriptor for a PCK file.
!
      INTEGER               NS
      PARAMETER           ( NS = ND + (NI+1)/2 )
!
!     Index for the data type code in the integer summary.
!
      INTEGER               NT
      PARAMETER           ( NT = 3 )
!
!     Maximum size allowed for a record in a segment of a binary PCK
!     file.
!
      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 130 )
!
!     Number of components in a state vector.
!
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )

!
!     Local Variables
!
      CHARACTER*(40)        IDENT

      DOUBLE PRECISION      DCD    (     ND )
      DOUBLE PRECISION      DESCR  (     NS )
      DOUBLE PRECISION      ESTATE ( NSTATE )
      DOUBLE PRECISION      EULANG ( NSTATE )
      DOUBLE PRECISION      RECORD ( MAXREC )

      INTEGER               HANDLE
      INTEGER               ICD    (     NI )
      INTEGER               RECSIZ
      INTEGER               TYPE

!
!     Standard SPICE Error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'PCKMAT' )

!
!     Get a segment applicable to a specified body and epoch.
!
      CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )

      IF ( FAILED() ) THEN
         FOUND = .FALSE.
         CALL CHKOUT ( 'PCKMAT' )
         RETURN
      END IF

      IF ( FOUND ) THEN

!
!        Look at parts of the descriptor.
!
         CALL DAFUS ( DESCR, ND, NI, DCD, ICD )

         TYPE = ICD( NT )
         REF  = ICD( NR )

         IF ( TYPE .EQ. 02 ) THEN
!
!           Read in Chebyshev coefficients from segment.
!
            CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            CALL PCKE02 ( ET, RECORD, EULANG )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF
!
!           From the PCK type two file the Euler angles are
!           retrieved in a particular order.  The routine to
!           get the TSIPM matrix from expects them in another
!           order.  Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            ESTATE (1) = EULANG (3)
            ESTATE (2) = EULANG (2)
            ESTATE (3) = EULANG (1)
            ESTATE (4) = EULANG (6)
            ESTATE (5) = EULANG (5)
            ESTATE (6) = EULANG (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            CALL EUL2XF ( ESTATE, 3, 1, 3, TSIPM )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. 03 ) THEN
!
!           Fetch the number of Chebyshev coefficients, compute the
!           record size needed, and signal an error if there is not
!           enough storage in RECORD. The number of coefficients is the
!           first constant value in the generic segment.
!
            CALL SGFCON ( HANDLE, DESCR, 1, 1, RECORD(1) )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF

            RECSIZ = NSTATE * INT(RECORD(1)) + 2

            IF ( RECSIZ .GT. MAXREC ) THEN

               CALL SETMSG ( 'Storage for # double precision numbers' &
               //            ' is needed for a PCK data record and' &
               //            ' only # locations were available.' &
               //            ' Notify the NAIF' &
               //            ' group of this problem.'                 )
               CALL ERRINT ( '#', RECSIZ                               )
               CALL ERRINT ( '#', MAXREC                               )
               CALL SIGERR ( 'SPICE(PCKKRECTOOLARGE)'                  )
               CALL CHKOUT ( 'PCKMAT'                                  )
               RETURN

            END IF

            CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
            CALL PCKE03 ( ET, RECORD, TSIPM         )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. 20 ) THEN
!
!           Read in Chebyshev coefficients from segment.
!
            CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            CALL PCKE20 ( ET, RECORD, EULANG )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF
!
!           From the PCK type 20 file the Euler angles are
!           retrieved in a particular order. The routine to
!           get the TSIPM matrix from expects them in another
!           order. Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            ESTATE (1) = EULANG (3)
            ESTATE (2) = EULANG (2)
            ESTATE (3) = EULANG (1)
            ESTATE (4) = EULANG (6)
            ESTATE (5) = EULANG (5)
            ESTATE (6) = EULANG (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            CALL EUL2XF ( ESTATE, 3, 1, 3, TSIPM )

            IF ( FAILED() ) THEN
               FOUND = .FALSE.
               CALL CHKOUT ( 'PCKMAT' )
               RETURN
            END IF


         ELSE
!
!           If data matching the requested body and time was not
!           found, FOUND is false.
!
            FOUND = .FALSE.

         END IF

      END IF

      CALL CHKOUT ( 'PCKMAT' )
      RETURN
      END
