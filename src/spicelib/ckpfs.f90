!$Procedure CKPFS ( C-kernel, get pointing from segment )

      SUBROUTINE CKPFS ( HANDLE, DESCR, SCLKDP, TOL,  NEEDAV, &
                         CMAT,   AV,    CLKOUT, FOUND         )

! $ Abstract
!
!     Evaluate pointing data from a segment for a given time.
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
!     CK
!     DAF
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'ckparam.inc'

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( *    )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      TOL
      LOGICAL               NEEDAV
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   CK file handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Spacecraft clock time.
!     TOL        I   Time tolerance.
!     NEEDAV     I   .TRUE. when angular velocity data is requested.
!     CMAT       O   C-matrix.
!     AV         O   Angular velocity vector.
!     CLKOUT     O   Output spacecraft clock time.
!     FOUND      O   .TRUE. when requested pointing is available.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              desired segment. The file should have been opened
!              for read access, either by CKLPF or DAFOPR.
!
!     DESCR    is the packed descriptor of the segment.
!
!     SCLKDP   is the encoded spacecraft clock time for which
!              pointing is desired.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock. The C-matrix returned by
!              CKPFS is the one whose time is closest to SCLKDP and
!              within TOL units of SCLKDP.
!
!     NEEDAV   is .TRUE. when angular velocity data is requested.
!
! $ Detailed_Output
!
!     CMAT     is a rotation matrix that transforms the components of
!              of a vector expressed in the reference frame given in
!              the segment to components expressed in the instrument
!              fixed frame at time CLKOUT.
!
!              Thus, if a vector v has components x, y, z in the
!              CK base frame, then v has components x', y', z' in
!              the instrument fixed frame at time CLKOUT:
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
!                           (Transpose of CMAT)
!
!     AV       is the angular velocity vector. This is returned only
!              if it has been requested, as indicated by NEEDAV. In
!              other words, if NEEDAV is .TRUE., then the pointing
!              records in the segment must contain AV data.
!
!              The angular velocity vector is the right-handed axis
!              about which the reference frame tied to the instrument
!              is instantaneously rotating at time CLKOUT. The
!              magnitude of AV is the magnitude of the instantaneous
!              velocity of the rotation, in radians per second.
!
!              The components of AV are given relative to the
!              reference frame specified in the segment descriptor.
!
!     CLKOUT   is the encoded spacecraft clock time associated with
!              the returned C-matrix and, optionally, the returned
!              angular velocity vector.
!
!     FOUND    is .TRUE. if a C-matrix and an angular velocity vector
!              (if requested) were found to satisfy the pointing
!              request. FOUND will be false otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the data type of the segment is not one of those supported
!         by this routine, the error SPICE(CKUNKNOWNDATATYPE) is
!         signaled.
!
!     2)  If the specified handle does not belong to any file that is
!         currently known to be open, an error is signaled by a routine
!         in the call tree of this routine.
!
!     3)  If DESCR is not a valid, packed descriptor of a segment in
!         the CK file specified by HANDLE, the results of this routine
!         are unpredictable.
!
!     4)  If TOL is negative, FOUND is .FALSE.
!
!     5)  If NEEDAV is .TRUE., but the segment doesn't contain AV data,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The structure of this routine is just a big case statement. Each
!     segment data type is supported by two routines:
!
!        CKRnn   which reads a single logical pointing record from a
!                segment of type nn. (A logical record is defined as
!                a collection of numbers sufficient to determine the
!                C-matrix, and optionally the angular velocity vector,
!                at the input time.)
!
!        CKEnn   which evaluates the pointing record returned by CKRnn
!                to give the C-matrix and optionally the angular
!                velocity vector at the input time.
!
!     The data type is determined from the segment descriptor, and the
!     appropriate routines are called.
!
! $ Examples
!
!     CKPFS allows you to be more selective than CKGP or CKGPAV about
!     choosing segments to satisfy CK pointing requests.
!
!     Suppose MOC.BC is a CK file consisting of several segments
!     containing Mars Observer Camera pointing data. Each segment
!     covers the same time period, but produces different pointing
!     values (one segment may contain predict values, another may
!     contain telemetry-based values, and others may contain different
!     corrected versions).
!
!     The following code fragment shows how different the results are
!     for each segment. The program steps through the file segment by
!     segment and requests pointing for the same time from each
!     segment. The results are printed to the screen.
!
!     GETIME is an imaginary routine used to get an encoded SCLK time
!     (SCLKDP) and time tolerance from the user.
!
!           SC     = -94
!           INST   = -94001
!           NEEDAV = .TRUE.
!
!           CALL CKLPF ( 'MOC.BC', HANDLE )
!
!           CALL GETIME ( SCLKDP, TOL, QUIT )
!
!     C
!     C     For each time, begin a forward search through the file, and
!     C     for each segment found, get its descriptor, identifier, and
!     C     evaluate the pointing.
!     C
!           DO WHILE ( .NOT. QUIT )
!
!              CALL DAFBFS ( HANDLE )
!              CALL DAFFNA ( FOUND  )
!
!              DO WHILE ( FOUND )
!
!                 CALL DAFGS ( DESCR )
!                 CALL DAFGN ( IDENT )
!
!                 CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,   NEEDAV,
!          .                   CMAT,   AV,    CLKOUT, PFOUND         )
!
!                 IF ( PFOUND ) THEN
!                    WRITE (*,*) 'Segment:          ', IDENT
!                    WRITE (*,*) 'C-Matrix:         ', CMAT
!                    WRITE (*,*) 'Angular velocity: ', AV
!
!                 ELSE
!                    CALL SCDECD ( SC, SCLKDP, SCLKCH )
!                    WRITE (*,*) 'Data not found at time ', SCLKCH
!
!                 END IF
!
!                 CALL DAFFNA ( FOUND )
!
!              END DO
!
!              CALL GETIME ( SCLKDP, TOL, QUIT )
!
!           END DO
!
! $ Restrictions
!
!     1)  A C-kernel file should have been loaded by either CKLPF
!         or DAFOPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB)
!
!        Bug fix: this routine now sets the output FOUND to
!        .FALSE. if a SPICE error is detected.
!
!        The routine was updated to handle data type 6 segments.
!        Several comment typos were corrected.
!
!-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
!
!        The routine was updated to handle data type 5 segments.
!
!-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
!
!        The routine was updated to handle data type 4 segments.
!        The RECSIZ size parameter was eliminated. The dimension
!        of the RECORD buffer is now defined by the CKMRSZ parameter
!        specified in the 'ckparam.inc' include file.
!
!-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
!
!        The routine was updated to handle data type 3 segments.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
!
!         The routine was updated to handle data type 2 segments.
!
!         FOUND is now initialized to false.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!         The restriction that a C-kernel file must be loaded
!         was explicitly stated.
!
!
!-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
!
!-&


! $ Index_Entries
!
!     get pointing from CK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB)
!
!        The routine was updated to handle data type 6 segments.
!
!-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
!
!        The routine was updated to handle data type 5 segments.
!
!-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
!
!        The routine was updated to handle data type 4 segments.
!
!           a) 'ckparam.inc' include file was included.
!
!           b) RECSIZ size parameter was eliminated.
!
!           c) Size of the RECORD was reset to CKMRSZ, parameter
!              defined in the 'ckparam.inc' include file.
!
!           d) Calls to CKR04 and CKE04 were added to the case
!              statement.
!
!-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
!
!        The routine was updated to handle data type 3 segments.
!
!           a) RECSIZ was increased to 17.
!
!           b) Calls to CKR03 and CKE03 were added to the case
!              statement.
!
!-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
!
!        1) The routine was updated to handle data type 2 segments.
!
!        2) FOUND is initialized to false to guard against it being
!           left unchanged from its previous value when an error is
!           detected.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!        1) The restriction that a C-kernel file must be loaded
!           was explicitly stated.
!
!-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
!
!        The following changes were made as a result of the
!        NAIF CK Code and Documentation Review:
!
!        1) The variable SCLK was changed to SCLKDP.
!        2) The declarations for the parameters RECSIZ, NDC, and NIC
!           were moved from the "Declarations" section of the header
!           to the "Local parameters" section of the code below the
!           header. These parameters are not meant to modified by
!           users.
!        3) The header was updated.
!        4) The comments in the code were improved.
!
!-    Beta Version 1.0.0, 07-MAY-1990 (RET) (IMU)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
      INTEGER               NDC
      PARAMETER           ( NDC    = 2  )

      INTEGER               NIC
      PARAMETER           ( NIC    = 6  )

!
!     Local variables
!
      INTEGER               ICD    ( NIC    )
      INTEGER               TYPE

      DOUBLE PRECISION      DCD    ( NDC    )
      DOUBLE PRECISION      RECORD ( CKMRSZ )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKPFS' )
      END IF

!
!     Start off with FOUND set to false.
!
      FOUND  = .FALSE.

!
!     Upgrading CKPFS to accommodate new data types involves following
!     these steps:
!
!     1)  Write the two new routines CKRnn and CKEnn. (You may need to
!         add or subtract from the arguments used in the existing CKRnn
!         and CKEnn calling sequences, but should not have to change
!         the inputs or outputs to CKPFS.)
!
!     2)  Insert a new case into the code of CKPFS.
!
!     3)  Depending on the size of RECORD returned from CKRnn, modify
!         the parameter RECSIZ.  (You will only need to change it if
!         RECSIZ is not large enough for the new CKRnn's RECORD.)
!

!
!     Unpack the descriptor to see what the data type of the segment is,
!     and call the appropriate read-and-evaluate routines.
!
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

      TYPE = ICD( 3 )

      IF ( TYPE .EQ. 1 ) THEN

         CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, &
                      FOUND )

         IF ( FOUND ) THEN
            CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 2 ) THEN

         CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL, RECORD, FOUND )

         IF ( FOUND ) THEN
            CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 3 ) THEN

         CALL CKR03 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)

         IF ( FOUND ) THEN
            CALL CKE03 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 4 ) THEN

         CALL CKR04 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)

         IF ( FOUND ) THEN
            CALL CKE04 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 5 ) THEN

         CALL CKR05 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)

         IF ( FOUND ) THEN
            CALL CKE05 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE  IF ( TYPE .EQ. 6 ) THEN

         CALL CKR06 (HANDLE, DESCR, SCLKDP, TOL, NEEDAV, RECORD, FOUND)

         IF ( FOUND ) THEN
            CALL CKE06 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
         END IF

      ELSE

         CALL SETMSG ( 'The data type # is not currently supported.' )
         CALL ERRINT ( '#', TYPE                                     )
         CALL SIGERR ( 'SPICE(CKUNKNOWNDATATYPE)'                    )

      END IF

!
!     In case an evaluator signaled an error, we check the SPICE
!     error status here. If a SPICE error occurred, indicate no
!     data were found.
!
      IF ( FAILED() ) THEN
         FOUND = .FALSE.
      END IF

      CALL CHKOUT ( 'CKPFS' )
      RETURN
      END
