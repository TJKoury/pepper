!$Procedure CKW04B ( CK type 04: Begin a segment )

      SUBROUTINE CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )

! $ Abstract
!
!     Begin a type CK04 segment in the DAF file associated with
!     HANDLE. See also CKW04A and CKW04E.
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

      INCLUDE               'sgparam.inc'
      INCLUDE               'ckparam.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               INST
      LOGICAL               AVFLAG
      CHARACTER*(*)         REF
      DOUBLE PRECISION      BEGTIM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of an DAF file open for writing.
!     SEGID      I   The string to use for segment identifier.
!     INST       I   The NAIF ID code for the SC or instrument.
!     AVFLAG     I   The angular rates flag.
!     REF        I   The reference frame for this segment.
!     BEGTIM     I   The segment coverage start encoded SCLK time
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a CK file that has been
!              opened for writing.
!
!     SEGID    is the segment identifier. CK segment identifier
!              may contain up to 40 printing ASCII characters.
!
!     INST     is the SPICE ID for the SC structure or instrument
!              whose orientation are to be recorded in a CK file.
!
!     AVFLAG   is a logical flag that indicates whether segment will
!              contain angular rate information.
!
!     REF      is the name of a reference frame that pointing is
!              given with respect to, for example 'J2000'.
!
!     BEGTIM   is the encoded SCLK time for the start of the segment
!              coverage.
!
! $ Detailed_Output
!
!     None.
!
!     The input data is used to create the segment summary for the
!     segment being started in the DAF file associated with HANDLE.
!
!     See the $Particulars section for details about the structure of a
!     type 4 CK segment.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the files
!     'sgparam.inc' and 'ckparam.inc'.
!
! $ Exceptions
!
!     1)  If a file access error occurs while this routine begins a new
!         type 04 segment, the error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If numeric ID for given reference frame cannot be resolved
!         from it's name, the error SPICE(INVALIDREFFRAME) is signaled.
!
!     3)  If SEGID is more than 40 characters long, the error
!         SPICE(SEGIDTOOLONG) is signaled.
!
!     4)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
! $ Files
!
!     See HANDLE in the $Detailed_Input section.
!
! $ Particulars
!
!     This routine begins writing a type 4 CK segment to the open DAF
!     file that is associated with HANDLE. The file must have been
!     opened with write access.
!
!     This routine is one of a set of three routines for creating and
!     adding data to type 4 CK segments. These routines are:
!
!        CKW04B: Begin a type 4 CK segment. This routine must be
!                called before any data may be added to a type 4
!                segment.
!
!        CKW04A: Add data to a type 4 CK segment. This routine may be
!                called any number of times after a call to CKW04B to
!                add type 4 records to the CK segment that was
!                started.
!
!        CKW04E: End a type 4 CK segment. This routine is called to
!                make the type 4 segment a permanent addition to the
!                DAF file. Once this routine is called, no further type
!                4 records may be added to the segment. A new segment
!                must be started.
!
!     A type 4 CK segment consists of coefficient sets for variable
!     order Chebyshev polynomials over consecutive time intervals of
!     a variable length. The gaps between intervals are allowed.
!     The Chebyshev polynomials represent individual quaternion
!     components q0, q1, q2 and q3 and individual angular velocities
!     AV1, AV2 and AV3 if they are included with the data.
!
!     The pointing data supplied to the type 4 CK writer (CKW04A)
!     is packed into an array as a sequence of records,
!
!        ----------------------------------------------------
!        | Record 1 | Record 2 | .. | Record N-1 | Record N |
!        ----------------------------------------------------
!
!     with each record in data packets has the following format.
!
!        ----------------------------------------------------
!        | The midpoint of the approximation interval       |
!        ----------------------------------------------------
!        | The radius of the approximation interval         |
!        ----------------------------------------------------
!        | Number of coefficients for q0                    |
!        ----------------------------------------------------
!        | Number of coefficients for q1                    |
!        ----------------------------------------------------
!        | Number of coefficients for q2                    |
!        ----------------------------------------------------
!        | Number of coefficients for q3                    |
!        ----------------------------------------------------
!        | Number of coefficients for AV1                   |
!        ----------------------------------------------------
!        | Number of coefficients for AV2                   |
!        ----------------------------------------------------
!        | Number of coefficients for AV3                   |
!        ----------------------------------------------------
!        | q0 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q1 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q2 Cheby coefficients                            |
!        ----------------------------------------------------
!        | q3 Cheby coefficients                            |
!        ----------------------------------------------------
!        | AV1 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!        | AV2 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!        | AV3 Cheby coefficients (optional)                |
!        ----------------------------------------------------
!
! $ Examples
!
!     Assume that we have:
!
!        HANDLE   is the handle of an CK file opened with write
!                 access.
!
!        SEGID    is a character string of no more than 40 characters
!                 which provides a pedigree for the data in the CK
!                 segment we will create.
!
!        INST     is the SPICE ID code for the instrument whose
!                 pointing data is to be placed into the file.
!
!        AVFLAG   angular rates flag.
!
!        REFFRM   is the name of the SPICE reference frame for the
!                 pointing data.
!
!        BEGTIM   is the starting encoded SCLK time for which the
!                 segment is valid.
!
!        ENDTIM   is the ending encoded SCLK time for which the segment
!                 is valid.
!
!        N        is the number of type 4 records that we want to
!                 put into a segment in an CK file.
!
!        NPKTS    is integer array which contains the lengths of
!                 variable size data packets
!
!        RECRDS   contains N type 4 records packaged for the CK
!                 file.
!
!        SCSTRT   contains the initial encoded SC time for each of
!                 the records contained in RECRDS, where
!
!                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1
!
!                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST
!
!     Then the following code fragment demonstrates how to create
!     a type 4 CK segment if all of the data for the segment is
!     available at one time.
!
!     C
!     C     Begin the segment.
!     C
!           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID )
!     C
!     C     Add the data to the segment all at once.
!     C
!           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT )
!     C
!     C     End the segment, making the segment a permanent
!     C     addition to the CK file.
!     C
!           CALL CKW04E ( HANDLE, ENDTIM )
!
! $ Restrictions
!
!     1)  The file containing the segment should be opened for read
!         or write access either by CKOPN or DAFOPW.
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
!-    SPICELIB Version 1.0.2, 17-AUG-2021 (JDR)
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
!     begin writing a type_4 CK segment
!
!-&


!
!     Spicelib functions
!
      INTEGER               LASTNB
      LOGICAL               RETURN

!
!     Local Parameters
!

!
!     DAF ND and NI values for CK files and length of a DAF descriptor.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + ( NI + 1 ) / 2 )

!
!     The number of generic segment constants in a type 4 CK segment.
!
      INTEGER               NCONST
      PARAMETER           ( NCONST = 0 )

!
!     The integer codes of the first and last printable ASCII
!     characters.
!
      INTEGER               FPRINT
      PARAMETER           ( FPRINT = 32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT = 126 )

!
!     The maximum number of characters allowed in a CK segment
!     identifier.
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

!
!     Local variables
!
      DOUBLE PRECISION      DCD    ( ND )
      DOUBLE PRECISION      DCOEFF
      DOUBLE PRECISION      DESCR  ( NDESCR )

      INTEGER               I
      INTEGER               ICD    ( NI )
      INTEGER               REFCOD
      INTEGER               VALUE

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW04B' )
      END IF

!
!     Create a descriptor for the segment we are about to write. First
!     assign start and stop times.
!
      DCD( 1 ) = BEGTIM
      DCD( 2 ) = 0.D0

!
!     Second, resolve reference frame ID code from its name and
!     assign it to the corresponding descriptor component. Signal
!     an error if frame is not recognized.
!
      CALL NAMFRM ( REF, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN
         CALL SETMSG ( 'The reference frame # is not supported.' )
         CALL ERRCH  ( '#', REF                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                  )
         CALL CHKOUT ( 'CKW04B'                                  )
         RETURN
      END IF

      ICD ( 2 ) = REFCOD

!
!     Third, assign values to the rest of the integer components of
!     the segment descriptor.
!
      ICD ( 1 ) = INST
      ICD ( 3 ) = CK4DTP

      IF ( AVFLAG ) THEN
         ICD ( 4 ) = 1
      ELSE
         ICD ( 4 ) = 0
      END IF

!
!     Now pack the segment descriptor.
!
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )

!
!     Check that all characters in the SEGID are printable.
!
      DO I = 1, LASTNB(SEGID)

         VALUE = ICHAR( SEGID(I:I) )

         IF ( ( VALUE .LT. FPRINT ) .OR. ( VALUE .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '      // &
                          'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW04B'                                )
            RETURN

         END IF

      END DO

!
!     Also check if the segment identifier is too long.
!
      IF ( LASTNB( SEGID ) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than '   // &
                       '40 characters.'                           )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                      )
         CALL CHKOUT ( 'CKW04B'                                   )
         RETURN

      END IF

!
!     We've got a valid descriptor and identifier and can begin
!     the segment. For this data type, we want to use an explicit
!     reference value index where the reference epochs are in
!     increasing order. We also want the index returned for a
!     particular request epoch to be the index of the greatest
!     reference epoch less than or equal to the request epoch. These
!     characteristics are prescribed by the mnemonic EXPLE. See the
!     include file 'sgparam.inc' for more details.
!
      CALL SGBWVS ( HANDLE, DESCR, SEGID, NCONST, DCOEFF, EXPLE )

!
!     No need to check FAILED() here, since all we do after this
!     point is checking out.
!
      CALL CHKOUT ( 'CKW04B' )

      RETURN

      END
