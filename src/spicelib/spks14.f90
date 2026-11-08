!$Procedure SPKS14 ( S/P Kernel, subset, type 14 )

      SUBROUTINE SPKS14 ( SRCHAN, SRCDSC, DSTHAN, DSTDSC, DSTSID )

! $ Abstract
!
!     Extract a subset of the data in a type 14 SPK segment into a new
!     type 14 segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               SRCHAN
      DOUBLE PRECISION      SRCDSC(*)
      INTEGER               DSTHAN
      DOUBLE PRECISION      DSTDSC(*)
      CHARACTER*(*)         DSTSID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SRCHAN     I   Handle of the SPK file with the source segment.
!     SRCDSC     I   Descriptor for the source segment.
!     DSTHAN     I   Handle of the SPK file for the destination segment.
!     DSTDSC     I   Descriptor for the destination segment.
!     DSTSID     I   Segment identifier for the new segment.
!
! $ Detailed_Input
!
!     SRCHAN   is the handle of the SPK file containing the source
!              segment.
!
!     SRCDSC   is the SPK descriptor for the source segment.
!
!     DSTHAN   is the handle of the SPK file containing the new segment.
!
!     DSTDSC   is the SPK descriptor for the destination segment. It
!              contains the desired start and stop times for the
!              requested subset.
!
!     DSTSID   is the segment identifier for the destination segment.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the length of the SPK record that is to be moved is larger
!         than MAXREC, the error SPICE(SPKRECTOOLARGE) is signaled.
!
! $ Files
!
!     See arguments SRCHAN, DSTHAN.
!
! $ Particulars
!
!     This subroutine copies a subset of the data form one SPK segment
!     to another.
!
!     The exact structure of a segment of SPK type 14 is detailed in
!     the SPK Required Reading. Please see this document for details.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  We assume that the source descriptor actually describes a
!         segment in the source SPK file containing the time coverage
!         that is desired for the subsetting operation.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 08-MAR-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     subset type_14 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     This is the maximum size type 14 record that we can move. This
!     allows a 20th degree Chebyshev Polynomial, which should be more
!     than sufficient. This should be the same as the value in SPKPV.
!
      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 128 )
!
!     Reference frame name size. See CHGIRF.
!
      INTEGER               FRMSIZ
      PARAMETER           ( FRMSIZ = 16 )
!
!     DAF ND and NI values for SPK files.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
!
!     Length of a state.
!
      INTEGER               NSTATE
      PARAMETER           ( NSTATE = 6 )
!
!     Local Variables
!
      CHARACTER*(FRMSIZ)    MYFRAM

      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      DTEMP  ( ND     )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      MYREF
      DOUBLE PRECISION      RECORD ( MAXREC )

      INTEGER               BEGIDX
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               CHBDEG
      INTEGER               DUMMY
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               IFRAME
      INTEGER               ITEMP  ( NI )
      INTEGER               RECSIZ

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS14' )
      END IF
!
!     First, unpack the destination segment descriptor and set some
!     local variables.
!
      CALL DAFUS( DSTDSC, ND, NI, DTEMP, ITEMP )

      BEGTIM = DTEMP(1)
      ENDTIM = DTEMP(2)
      BODY   = ITEMP(1)
      CENTER = ITEMP(2)
      IFRAME = ITEMP(3)

      CALL IRFNAM ( IFRAME, MYFRAM )
!
!     If we can't find the code, it can't be an SPK file.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
!
!     Get the constants for this segment. There is only one.
!
      CALL SGFCON ( SRCHAN, SRCDSC, 1, 1, DTEMP )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
!
!     The first element of DTEMP now contains the number of coefficients
!     used for the Chebyshev polynomials. We need the degree of the
!     polynomial which is one less than the number of coefficients.
!
      CHBDEG = INT ( DTEMP(1) ) - 1
!
!     Compute the size of the SPK record and signal an error if there is
!     not enough room in the variable RECORD to hold it.
!
      RECSIZ = (CHBDEG + 1) * NSTATE + 2

      IF ( RECSIZ .GT. MAXREC ) THEN
         CALL SETMSG ( 'Storage for # double precision numbers' &
         //            ' is needed for an SPK data record and' &
         //            ' only # locations were available. Update' &
         //            ' the parameter MAXREC in the subroutine' &
         //            ' SPKS14 and notify the NAIF group of this' &
         //            ' problem.'                                )
         CALL ERRINT ( '#', RECSIZ                                )
         CALL ERRINT ( '#', MAXREC                                )
         CALL SIGERR ( 'SPICE(SPKRECTOOLARGE)'                    )
         CALL CHKOUT ( 'SPKS14'                                   )
         RETURN
      END IF

!
!     Get the beginning and ending indices for the packets we need for
!     the destination segment.
!
      CALL SGFRVI ( SRCHAN, SRCDSC, BEGTIM, MYREF, BEGIDX, FOUND )
      CALL SGFRVI ( SRCHAN, SRCDSC, ENDTIM, MYREF, ENDIDX, FOUND )
!
!     Begin the destination segment.
!
      CALL SPK14B ( DSTHAN, DSTSID, BODY, CENTER, MYFRAM, &
                            BEGTIM, ENDTIM, CHBDEG        )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS14' )
         RETURN
      END IF
!
!     Now we get the data one record at a time from the source segment
!     and write it out to the destination segment.
!
      DO I = BEGIDX, ENDIDX

         CALL SGFPKT ( SRCHAN, SRCDSC, I, I, RECORD, DUMMY )
         CALL SGFREF ( SRCHAN, SRCDSC, I, I, MYREF         )
         CALL SPK14A ( DSTHAN, 1, RECORD, MYREF            )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPKS14' )
            RETURN
         END IF

      END DO
!
!     Now all we need to do is end the segment.
!
      CALL SPK14E ( DSTHAN )

      CALL CHKOUT ( 'SPKS14' )
      RETURN

      END
