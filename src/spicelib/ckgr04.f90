!$Procedure CKGR04 ( C-kernel, get record, type 04 )

      SUBROUTINE CKGR04 ( HANDLE, DESCR, RECNO, RECORD )

! $ Abstract
!
!     Return a specified pointing record from a CK type 04 segment.
!     The segment is identified by a CK file handle and segment
!     descriptor.
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
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               RECNO
      DOUBLE PRECISION      RECORD  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of the file containing the segment.
!     DESCR      I   The segment descriptor.
!     RECNO      I   The number of the pointing record to be returned.
!     RECORD     O   The pointing record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              desired segment. The file should have been opened
!              for read or write access, either by CKLPF, DAFOPR,
!              or DAFOPW.
!
!     DESCR    is the packed descriptor of the data type 4 segment.
!
!     RECNO    is the number of the pointing record to be returned
!              from the data type 4 segment.
!
! $ Detailed_Output
!
!     RECORD   is the pointing record indexed by RECNO in the
!              segment. The contents of the record are as follows:
!
!              ---------------------------------------------------
!              | The midpoint of the approximation interval      |
!              ---------------------------------------------------
!              | The radius of the approximation interval        |
!              ---------------------------------------------------
!              | Number of coefficients for q0                   |
!              ---------------------------------------------------
!              | Number of coefficients for q1                   |
!              ---------------------------------------------------
!              | Number of coefficients for q2                   |
!              ---------------------------------------------------
!              | Number of coefficients for q3                   |
!              ---------------------------------------------------
!              | Number of coefficients for AV1                  |
!              ---------------------------------------------------
!              | Number of coefficients for AV2                  |
!              ---------------------------------------------------
!              | Number of coefficients for AV3                  |
!              ---------------------------------------------------
!              | q0 Cheby coefficients                           |
!              ---------------------------------------------------
!              | q1 Cheby coefficients                           |
!              ---------------------------------------------------
!              | q2 Cheby coefficients                           |
!              ---------------------------------------------------
!              | q3 Cheby coefficients                           |
!              ---------------------------------------------------
!              | AV1 Cheby coefficients (optional)               |
!              ---------------------------------------------------
!              | AV2 Cheby coefficients (optional)               |
!              ---------------------------------------------------
!              | AV3 Cheby coefficients (optional)               |
!              ---------------------------------------------------
!
! $ Parameters
!
!     See 'ckparam.inc'.
!
! $ Exceptions
!
!     1)  If the segment is not of data type 4, the error
!         SPICE(CKWRONGDATATYPE) is signaled.
!
!     2)  If RECNO is less than one or greater than the number of
!         records in the specified segment, the error
!         SPICE(CKNONEXISTREC) is signaled.
!
!     3)  If the specified handle does not belong to any DAF file that
!         is currently known to be open, an error is signaled by a
!         routine in the call tree of this routine.
!
!     4)  If DESCR is not a valid descriptor of a segment in the CK
!         file specified by HANDLE, the results of this routine are
!         unpredictable.
!
! $ Files
!
!     The file specified by HANDLE should be open for read or
!     write access.
!
! $ Particulars
!
!     For a detailed description of the structure of a type 4 segment,
!     see the CK required reading.
!
!     This is a utility routine that may be used to read the individual
!     pointing records that make up a type 4 segment. It is normally
!     used in conjunction with CKNR04, which gives the number of
!     pointing records stored in a segment.
!
! $ Examples
!
!     Suppose that DATA.BC is a CK file that contains segments of
!     data type 4. Then the following code fragment extracts the
!     data packets contained in the segment.
!
!     C
!     C     CK parameters include file.
!     C
!           INCLUDE               'ckparam.inc'
!
!     C
!     C     Local variables.
!     C
!           DOUBLE PRECISION      DCD    ( 2 )
!           DOUBLE PRECISION      DESCR  ( 5 )
!           DOUBLE PRECISION      PKTDAT ( CK4RSZ )
!
!           INTEGER               AVFLAG
!           INTEGER               HANDLE
!           INTEGER               I
!           INTEGER               ICD    ( 6 )
!           INTEGER               K
!           INTEGER               LASTAD
!           INTEGER               NCOEF  ( QAVSIZ )
!           INTEGER               NREC
!
!           LOGICAL               FOUND
!     C
!     C     First load the file. (The file may also be opened by using
!     C     CKLPF.)
!     C
!           CALL DAFOPR ( 'DATA.BC', HANDLE )
!     C
!     C     Begin forward search. Find the first array.
!     C
!           CALL DAFBFS ( HANDLE )
!           CALL DAFFNA ( FOUND  )
!     C
!     C     Get segment descriptor.
!     C
!           CALL DAFGS ( DESCR )
!     C
!     C     Unpack the segment descriptor into its double precision
!     C     and integer components.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!           IF ( ICD( 3 ) .EQ. 4 ) THEN
!     C
!     C        How many records does this segment contain?
!     C
!              CALL CKNR04 ( HANDLE, DESCR, NREC )
!
!              DO I = 1, NREC
!     C
!     C           Get the data records stored in the segment.
!     C
!                 CALL CKGR04 ( HANDLE, DESCR, I, PKTDAT )
!     C
!     C           Print data packet contents. Print coverage interval
!     C           midpoint & radii first.
!     C
!                 WRITE (2,*) PKTDAT (1)
!                 WRITE (2,*) PKTDAT (2)
!     C
!     C           Decode numbers of coefficients.
!     C
!                 CALL ZZCK4D2I ( PKTDAT(3), QAVSIZ, CK4PCD, NCOEF )
!     C
!     C           Print number of coefficients for Q0, Q1, Q2 and Q3.
!     C
!                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 1 ), NCOEF( 2 )
!                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 3 ), NCOEF( 4 )
!     C
!     C           Print number coefficients for AV1, AV2 and AV3.
!     C
!                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 5 ), NCOEF( 6 )
!                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 7 )
!     C
!     C           Print Cheby coefficients.
!     C
!                 LASTAD = 0
!
!                 DO K = 1, QAVSIZ
!                    LASTAD = LASTAD + NCOEF( K )
!                 END DO
!
!                 DO K = 4, LASTAD + 4
!                    WRITE (2,*) PKTDAT (K)
!                 END DO
!
!              END DO
!
!           END IF
!
! $ Restrictions
!
!     1)  The binary CK file containing the segment whose descriptor
!         was passed to this routine must be opened for read or write
!         access by either CKLPF, DAFOPR, or DAFOPW.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     Y.K. Zaiko         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
!
!        Minor header edits.
!
!-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
!
!-&


! $ Index_Entries
!
!     get CK type_4 record
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NDC
      PARAMETER           ( NDC    = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC    = 6 )

!
!     Length (in DPs) of non-coefficient front part of RECORD when
!     it contains decoded numbers of coefficients. It is one less
!     than the length of the same part in a record exchanged between
!     CKR04 and CKE04 because it doesn't contain time at which
!     pointing has to be evaluated.
!
      INTEGER                 SHFTAD
      PARAMETER             ( SHFTAD = CK4SFT - 1 )

!
!     Local variables
!
      DOUBLE PRECISION      DCD    ( NDC )

      INTEGER               ENDS   ( 1 )
      INTEGER               ICD    ( NIC )
      INTEGER               K
      INTEGER               NREC
      INTEGER               NUMALL
      INTEGER               NUMCFT ( QAVSIZ )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKGR04' )
      END IF

!
!     Unpack descriptor and check segment data type. Signal an error
!     if it's not 4.
!
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

      IF ( ICD( 3 ) .NE. CK4DTP ) THEN
         CALL SETMSG ( 'Data type of the segment should be 4: '      // &
                       'Passed  descriptor shows type = #.'          )
         CALL ERRINT ( '#', ICD( 3 )                                 )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKGR04'                                      )
         RETURN
      END IF

!
!     If a request was made for a data record which doesn't
!     exist, then signal an error and leave.
!
      CALL CKNR04 ( HANDLE, DESCR, NREC )

      IF ( ( RECNO .LT. 1 ) .OR. ( RECNO .GT. NREC ) ) THEN
         CALL SETMSG ( 'Requested record number (#) does not '       // &
                       'exist. There are # records in the segment.'  )
         CALL ERRINT ( '#', RECNO                                    )
         CALL ERRINT ( '#', NREC                                     )
         CALL SIGERR ( 'SPICE(CKNONEXISTREC)'                        )
         CALL CHKOUT ( 'CKGR04'                                      )
         RETURN
      END IF

!
!     Get the data record indexed by RECNO.
!
      CALL SGFPKT ( HANDLE, DESCR, RECNO, RECNO, RECORD, ENDS )

!
!     Decode 7 numbers of coefficients from double precision value.
!
      CALL ZZCK4D2I ( RECORD( 3 ), QAVSIZ, CK4PCD, NUMCFT )

!
!     Compute total number of coefficients in the fetched packet.
!
      NUMALL = 0

      DO K = 1, QAVSIZ
         NUMALL = NUMALL + NUMCFT ( K )
      END DO

!
!     Move polynomial coefficients to the right to free space for
!     decoded numbers of coefficients and insert these numbers
!     starting from the third position.
!
      DO K = NUMALL, 1, -1
         RECORD ( K + SHFTAD ) = RECORD( K + 3 )
      END DO

      DO K = 1, 7
         RECORD( K + 2 ) = NUMCFT( K )
      END DO

!
!     All done.
!
      CALL CHKOUT ( 'CKGR04' )

      RETURN

      END
