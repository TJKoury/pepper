!$Procedure PCK03B ( PCK, begin a type 3 segment )

      SUBROUTINE PCK03B ( HANDLE, &
                          SEGID,  BODY,  FRAME, &
                          FIRST,  LAST,  CHBDEG  )

! $ Abstract
!
!     Begin a type 03 PCK segment in the binary PCK file associated with
!     HANDLE. See also PCK03A and PCK03E.
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
!     PCK
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'sgparam.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         SEGID
      INTEGER               BODY
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               CHBDEG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of a DAF file open for writing.
!     SEGID      I   The string to use for segment identifier.
!     BODY       I   The NAIF ID code for the body of the segment.
!     FRAME      I   The inertial frame for this segment.
!     FIRST      I   The first epoch for which the segment is valid.
!     LAST       I   The last epoch for which the segment is valid.
!     CHBDEG     I   The degree of the Chebyshev Polynomial used.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a PCK file that has been
!              opened for writing.
!
!     SEGID    is the segment identifier. A PCK segment identifier
!              may contain up to 40 printing characters. It may also be
!              blank.
!
!     BODY     is the SPICE ID code for the body whose orientation
!              information is to be stored in the PCK segment being
!              created.
!
!     FRAME    is the inertial reference frame to which the orientation
!              data for BODY is relative.
!
!     FIRST    are the bounds on the ephemeris times, expressed as
!     LAST     seconds past J2000, for which the states can be used
!              to interpolate a state for BODY.
!
!     CHBDEG   is the degree of the Chebyshev Polynomial used for
!              each set of Chebyshev coefficients that are to be stored
!              in the segment.
!
! $ Detailed_Output
!
!     None.
!
!     The data are stored in the PCK segment in the DAF
!     attached to HANDLE.
!
!     See the $Particulars section for details about the
!     structure of a type 03 PCK segment.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If the degree of the Chebyshev Polynomial to be used for this
!         segment is negative, the error SPICE(INVALIDARGUMENT) is
!         signaled.
!
!     2)  If there is any error in the structure or content of the
!         inputs other than the degree of the Chebyshev Polynomial, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     3)  If a file access error occurs while this routine begins a new
!         type 03 segment, the error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine begins a type 03 segment in the binary PCK file that
!     is associated with HANDLE. The file must have been opened with
!     write access.
!
!     This routine is one of a set of three routines for creating and
!     adding data to type 03 PCK segments. These routines are:
!
!        PCK03B: Begin a type 03 PCK segment. This routine must be
!                called before any data may be added to a type 03
!                segment.
!
!        PCK03A: Add data to a type 03 PCK segment. This routine may be
!                called any number of times after a call to PCK03B to
!                add type 03 records to the PCK segment that was
!                started.
!
!        PCK03E: End a type 03 PCK segment. This routine is called to
!                make the type 03 segment a permanent addition to the
!                PCK file. Once this routine is called, no further type
!                03 records may be added to the segment. A new segment
!                must be started.
!
!     A type 03 PCK segment consists of coefficient sets for fixed order
!     Chebyshev polynomials over consecutive time intervals, where the
!     time intervals need not all be of the same length. The Chebyshev
!     polynomials represent the orientation of a body specified relative
!     to an inertial frame by the angles RA, DEC, W and body fixed
!     angular rates for each axis of the body fixed coordinate system
!     defined by RA, DEC, and W. All of the angles and the angular rates
!     of the axes are given in degrees.
!
!     The orientation data supplied to the type 03 PCK writer is packed
!     into an array as a sequence of logical records,
!
!        -----------------------------------------------------
!        | Record 1 | Record 2 | ... | Record N-1 | Record N |
!        -----------------------------------------------------
!
!     with each record has the following format.
!
!           ------------------------------------------------
!           |  The midpoint of the approximation interval  |
!           ------------------------------------------------
!           |  The radius of the approximation interval    |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for RA                |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for DEC               |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for W                 |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the X-axis rate   |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Y-axis rate   |
!           ------------------------------------------------
!           |  CHBDEG+1 coefficients for the Z-axis rate   |
!           ------------------------------------------------
!
! $ Examples
!
!     Assume we have the following for each of the examples that
!     follow.
!
!        HANDLE   is the handle of a PCK file opened with write
!                 access.
!
!        SEGID    is a character string of no more than 40 characters
!                 which provides a pedigree for the data in the PCK
!                 segment.
!
!        BODY     is the SPICE ID code for the body whose orientation
!                 data is to be placed into the file.
!
!        REFFRM   is the name of the SPICE inertial reference frame
!                 the orientation data is relative to.
!
!        FIRST    is the starting epoch, in seconds past J2000, for
!                 the orientation data to be placed into the segment.
!
!        LAST     is the ending epoch, in seconds past J2000, for
!                 the orientation data to be placed into the segment.
!
!     Example 1:
!
!        For this example, we also assume that:
!
!           N        is the number of type 03 records that we want to
!                    put into a segment in PCK file.
!
!           RECRDS   contains N type 03 records packaged for the PCK
!                    file.
!
!           ETSTRT   contains the initial epochs for each of the
!                    records contained in RECRDS, where
!
!                       ETSTRT(I) < ETSTRT(I+1), I = 1, N-1
!
!                       ETSTRT(1) <= FIRST, ETSTRT(N) < LAST
!
!                       ETSTRT(I+1), I = 1, N-1, is the ending epoch for
!                       record I as well as the initial epoch for record
!                       I+1.
!
!        Then the following code fragment demonstrates how to create a
!        type 03 PCK segment if all of the data for the segment is
!        available at one time.
!
!        C
!        C     Begin the segment.
!        C
!              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM,
!             .              FIRST,  LAST,  CHBDEG        )
!        C
!        C     Add the data to the segment all at once.
!        C
!              CALL PCK03A ( HANDLE, N, RECRDS, ETSTRT )
!        C
!        C     End the segment, making the segment a permanent addition
!        C     to the PCK file.
!        C
!              CALL PCK03E ( HANDLE )
!
!     Example 2:
!
!        In this example we want to add type O3 PCK records, as
!        described above in the $Particulars section, to the segment
!        being written as they are generated. The ability to write the
!        records in this way is useful if computer memory is limited. It
!        may also be convenient from a programming perspective to write
!        the records one at a time.
!
!        For this example, assume that we want to generate N type 03 PCK
!        records, one for each of N time intervals, writing them all to
!        the same segment in a PCK file. Let
!
!           N        be the number of type 03 records that we want to
!                    generate and put into a segment in an PCK file.
!
!           RECORD   be an array with enough room to hold a single type
!                    03 record, i.e. RECORD should have dimension at
!                    least 6 * (CHBDEG + 1 ) + 2.
!
!           START    be an array of N times that are the beginning
!                    epochs for each of the intervals of interest. The
!                    times should be in increasing order and the start
!                    time for the first interval should equal the
!                    starting time for the segment.
!
!                       START(I) < START(I+1), I = 1, N-1
!
!                       START(1) = FIRST
!
!           STOP     be an array of N times that are the ending epochs
!                    for each of the intervals of interest. The times
!                    should be in increasing order and the stop time for
!                    interval I should equal the start time for interval
!                    I+1, i.e., we want to have continuous coverage in
!                    time across all of the records. Also, the stop time
!                    for the last interval should equal the ending time
!                    for the segment.
!
!                       STOP(I) < STOP(I+1), I = 1, N-1
!
!                       STOP(I) = START(I+1), I = 1, N-1
!
!                       STOP(N) = LAST
!
!           GENREC( TIME1, TIME2, RECORD )
!
!                    be a subroutine that generates a type 03 PCK record
!                    for a time interval specified by TIME1 and TIME2.
!
!        Then the following code fragment demonstrates how to create a
!        type 03 PCK segment if all of the data for the segment is not
!        available at one time.
!
!        C
!        C     Begin the segment.
!        C
!              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM,
!             .              FIRST,  LAST,  CHBDEG        )
!
!        C
!        C     Generate the records and write them to the segment in the
!        C     PCK file one at at time.
!        C
!              DO I = 1, N
!
!                 CALL GENREC ( START(I), STOP(I), RECORD   )
!                 CALL PCK03A ( HANDLE, 1, RECORD, START(I) )
!
!              END DO
!
!        C
!        C     End the segment, making the segment a permanent addition
!        C     to the PCK file.
!        C
!              CALL PCK03E ( HANDLE )
!
! $ Restrictions
!
!     1)  The binary PCK file must be open with write access.
!
!     2)  Only one segment may be written to a particular PCK file at a
!         time. All of the data for the segment must be written and the
!         segment must be ended before another segment may be started in
!         the file.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (EDW) (BVS)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Corrected order of header sections to conform to NAIF
!        standard.
!
!        Removed comments from the $Declarations section.
!
!-    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     begin writing a type_03 PCK segment
!
!-&


!
!     Spicelib functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Parameters
!
!     DAF ND and NI values for PCK files.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
!
!     Length of an PCK descriptor.
!
      INTEGER               NDESCR
      PARAMETER           ( NDESCR =  ND + (NI+1)/2 )
!
!     Number of Euler angles.
!
      INTEGER               NEANGS
      PARAMETER           ( NEANGS = 6 )
!
!     The type of this segment.
!
      INTEGER               TYPE
      PARAMETER           ( TYPE = 03 )
!
!     The number of constants.
!
      INTEGER               NCONST
      PARAMETER           ( NCONST = 1 )
!
!     Local variables
!
      DOUBLE PRECISION      DESCR ( NDESCR )
      DOUBLE PRECISION      DCOEFF

      INTEGER               NCOEFF
      INTEGER               PKTSIZ
!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCK03B' )
      END IF
!
!     First, check the degree of the polynomial to be sure that it is
!     not negative.
!
      IF ( CHBDEG .LT. 0 ) THEN

         CALL SETMSG ( 'The degree of the Chebyshev Polynomial'  // &
                       ' was negative, #. The degree of the'     // &
                       ' polynomial must be greater than or'     // &
                       ' equal to zero.'                          )
         CALL ERRINT ( '#', CHBDEG                                )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'PCK03B'                                   )
         RETURN

      END IF
!
!     Create a descriptor for the segment we are about to write.
!
      CALL PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'PCK03B' )
         RETURN

      END IF

!
!     We've got a valid descriptor, so compute a few things and begin
!     the segment.
!
      NCOEFF = CHBDEG + 1
      PKTSIZ = NEANGS * NCOEFF + 2
      DCOEFF = DBLE ( NCOEFF )
!
!     For this data type, we want to use an explicit reference value
!     index where the reference epochs are in increasing order. We also
!     want to have as the index for a particular request epoch the index
!     of the greatest reference epoch less than or equal to the request
!     epoch. These characteristics are prescribed by the mnemonic EXPLE.
!     See the include file 'sgparam.inc' for more details.
!
      CALL SGBWFS ( HANDLE, DESCR,  SEGID,  NCONST, &
                            DCOEFF, PKTSIZ, EXPLE   )

      CALL CHKOUT ( 'PCK03B' )
      RETURN

      END
