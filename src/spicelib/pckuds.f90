!$Procedure PCKUDS (PCK, unpack segment descriptor )

      SUBROUTINE PCKUDS ( DESCR, BODY, FRAME, TYPE, &
                          FIRST, LAST, BEGIN, END   )

! $ Abstract
!
!     Unpack the contents of a PCK segment descriptor
!
! $ Required_Reading
!
!     PCK.
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      DESCR ( * )
      INTEGER               BODY
      INTEGER               FRAME
      INTEGER               TYPE
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               BEGIN
      INTEGER               END

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DESCR      I   A PCK segment descriptor.
!     BODY       O   The NAIF ID code for the body of the segment.
!     FRAME      O   The code for the inertial frame of this segment.
!     TYPE       O   The type of PCK segment.
!     FIRST      O   The first epoch for which the segment is valid.
!     LAST       O   The last  epoch for which the segment is valid.
!     BEGIN      O   Beginning DAF address of the segment.
!     END        O   Ending DAF address of the segment.
!
! $ Detailed_Input
!
!     DESCR    is a PCK segment descriptor.
!
! $ Detailed_Output
!
!     BODY     is the NAIF ID code for the body of the segment.
!
!     FRAME    is the SPICE ID code for the inertial frame to which
!              the body fixed orientation is referenced.
!
!     TYPE     is the type of PCK segment.
!
!     FIRST    is the first epoch for which the segment has
!              orientation data.
!
!     LAST     is the last epoch for which the segment has
!              orientation data.
!
!     BEGIN    is the starting address of the data associated
!              with this descriptor.
!
!     END      is the last address of the data associated with
!              this descriptor.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine extracts the contents of a PCK segment
!     descriptor into the components needed for reading and
!     evaluating the data stored in the segment. It serves
!     as a macro for expanding the PCK segment descriptor.
!
! $ Examples
!
!     Suppose you wished to summarize a particular PCK segment
!     and that you have the descriptor for that segment in hand.
!     The following code fragment shows how you might use this
!     routine to create a summary message concerning the segment.
!
!     CALL PCKUDS ( DESCR, BODY, FRAME, TYPE, FIRST, LAST )
!
!     Convert the start and stop times to ephemeris calendar strings
!
!     CALL ETCAL ( FIRST, FSTCAL )
!     CALL ETCAL ( LAST,  LSTCAL )
!
!     WRITE (*,*)
!     WRITE (*,*) 'Body     : ', BODY
!     WRITE (*,*) 'Frame ID : ', FRAME
!     WRITE (*,*) 'Data Type: ', TYPE
!     WRITE (*,*)
!     WRITE (*,*) 'Segment Start : ', FSTCAL
!     WRITE (*,*) 'Segment Stop  : ', LSTCAL
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 02-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Corrected order of header sections to conform to NAIF
!        standard.
!
!-    SPICELIB Version 1.0.0, 1994-JAN-4 (WLT)
!
!-&


! $ Index_Entries
!
!     Unpack and PCK segment descriptor
!
!-&


!
!     SPICELIB Functions
!
      LOGICAL              FAILED
      LOGICAL              RETURN

!
!     Local Parameters
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )

!
!     Local Variables
!
      DOUBLE PRECISION      DPPART ( ND )

      INTEGER               IPART  ( NI )

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKUDS' )
      END IF
!
!     No judgments are made about the descriptor when we
!     unpack it.  If things were done right when the descriptor
!     was created, it should be fine now.
!
      CALL DAFUS ( DESCR, ND, NI, DPPART, IPART )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKUDS' )
         RETURN
      END IF

      BODY   = IPART(1)
      FRAME  = IPART(2)
      TYPE   = IPART(3)
      BEGIN  = IPART(4)
      END    = IPART(5)

      FIRST  = DPPART(1)
      LAST   = DPPART(2)

      CALL CHKOUT ( 'PCKUDS' )
      RETURN

      END
