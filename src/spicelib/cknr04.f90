!$Procedure CKNR04 ( C-kernel, number of records, data type 4 )

      SUBROUTINE CKNR04 ( HANDLE, DESCR, NREC )

! $ Abstract
!
!     Return the number of pointing records in a CK type 04 segment.
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
      INTEGER               NREC

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of the file containing the segment.
!     DESCR      I   The descriptor of the type 4 segment.
!     NREC       O   The number of pointing records in the segment.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              segment. The file should have been opened for read
!              or write access, either by CKLPF, DAFOPR, or DAFOPW.
!
!     DESCR    is the packed descriptor of a data type 4 segment.
!
! $ Detailed_Output
!
!     NREC     is the number of pointing records in the type 4
!              segment.
!
! $ Parameters
!
!     See 'ckparam.inc'.
!
! $ Exceptions
!
!     1)  If the segment indicated by DESCR is not a type 4 segment,
!         the error SPICE(CKWRONGDATATYPE) is signaled.
!
!     2)  If the specified handle does not belong to any DAF file that
!         is currently known to be open, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If DESCR is not a valid descriptor of a segment in the CK
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
!     For a complete description of the internal structure of a type 4
!     segment, see the CK required reading.
!
!     This routine returns the number of pointing records contained
!     in the specified segment. It is normally used in conjunction
!     with CKGR04 which returns the Ith pointing record in the
!     segment.
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
!     C
!     C     $Declarations.
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
!     number of CK type_4 records
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
      PARAMETER           ( NDC = 2 )

      INTEGER               NIC
      PARAMETER           ( NIC = 6 )

!
!     Local variables
!
      DOUBLE PRECISION      DCD    ( NDC )

      INTEGER               ICD    ( NIC )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKNR04' )
      END IF

!
!     Check whether our segment is of the type 4 by unpacking
!     descriptor and checking value of its third integer component.
!
      CALL DAFUS ( DESCR, NDC, NIC, DCD, ICD )

      IF ( ICD( 3 ) .NE. CK4DTP ) THEN
         CALL SETMSG ( 'Data type of the segment should be 4: '      // &
                       'Passed descriptor shows type = #.'           )
         CALL ERRINT ( '#', ICD ( 3 )                                )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                      )
         CALL CHKOUT ( 'CKNR04'                                      )
         RETURN
      END IF

!
!     The number of records (packets) can be obtained by a call to
!     SGMETA. This number is a meta item 12 (see sgparam.inc for
!     details.)
!
      CALL SGMETA ( HANDLE, DESCR, 12, NREC )

!
!     All done.
!
      CALL CHKOUT ( 'CKNR04' )

      RETURN

      END
