!$Procedure SPKR10 ( SPK, read record from SPK type 10 segment )

      SUBROUTINE SPKR10 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 10
!     (NORAD two line element sets).
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
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'sgparam.inc'
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 10.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is the record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  It is assumed that the descriptor and handle supplied are
!         for a properly constructed type 10 segment. No checks are
!         performed to ensure this.
!
!     2)  If an error occurs while looking up SPK data, the error is
!         signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 10 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
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
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 1 ) THEN
!              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
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
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated entry
!        #2 in $Exceptions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.1.0, 09-MAR-2009 (EDW)
!
!        Removed declaration of unused variable DOINT.
!
!-    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     read record from type_10 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               NCONST
      PARAMETER           ( NCONST = 8  )

      INTEGER               NELEMS
      PARAMETER           ( NELEMS = 10 )
!
!     We have 2 nutation/obliquity terms and their rates giving us
!     four angle components for each packet.
!
      INTEGER               NANGS
      PARAMETER           ( NANGS =   4 )
!
!     BEGEL1 is the location in the record where the first
!     two-line element set will begin.
!
      INTEGER               BEGEL1
      PARAMETER           ( BEGEL1 = NCONST + 1      )
!
!     BEGEL2 is the location in the record where the second
!     two-line element set will begin.
!
      INTEGER               BEGEL2
      PARAMETER           ( BEGEL2 = BEGEL1 + NELEMS + NANGS )
!
!     ENSET1 and ENSET2 are the locations in the record where the
!     last element of set 1 and set 2 will be located.
!
      INTEGER               ENSET1
      PARAMETER           ( ENSET1 = BEGEL1 + NELEMS - 1 )

      INTEGER               ENSET2
      PARAMETER           ( ENSET2 = BEGEL2 + NELEMS - 1 )


      DOUBLE PRECISION      VALUE


      INTEGER               ENDS    ( 2 )
      INTEGER               FROM
      INTEGER               INDX
      INTEGER               NEPOCH
      INTEGER               TO
      INTEGER               PUTELM
      INTEGER               GETELM
      INTEGER               I
      INTEGER               SET1
      INTEGER               SET2

      LOGICAL               FOUND

      SAVE


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR10' )

!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      CALL SGFCON ( HANDLE, DESCR, 1,  NCONST, RECORD(1) )

!
!     Locate the time in the file closest to the input ET.
!
      CALL SGFRVI ( HANDLE, DESCR, ET, VALUE, INDX, FOUND )

!
!     Determine which pair of element sets to choose so that
!     they will bracket ET.
!
      IF ( ET .LE. VALUE ) THEN

         FROM = MAX( INDX-1, 1 )
         TO   = INDX

      ELSE

         CALL SGMETA ( HANDLE, DESCR, NREF, NEPOCH )
         FROM = INDX
         TO   = MIN( INDX+1, NEPOCH  )

      END IF

!
!     Fetch the element sets
!
      CALL SGFPKT ( HANDLE, DESCR, FROM, TO, RECORD(BEGEL1), ENDS )
!
!     If the size of the packets is not 14, this is an old style
!     two-line element set without nutation information.  We simply
!     set all of the angles to zero.
!
      IF ( ENDS(1) .EQ. NELEMS ) THEN
!
!        First shift the elements to their proper locations in RECORD
!        so there will be room to fill in the zeros.
!
         PUTELM = ENSET2
         GETELM = ENSET1 + NELEMS

         DO WHILE ( GETELM .GT. ENSET1 )

            RECORD(PUTELM) = RECORD(GETELM)
            PUTELM         = PUTELM - 1
            GETELM         = GETELM - 1

         END DO

         SET1 = ENSET1 + 1
         SET2 = ENSET2 + 1

         DO I = 1, NANGS
            RECORD(SET1) = 0.0D0
            RECORD(SET2) = 0.0D0
            SET1         = SET1 + 1
            SET2         = SET2 + 1
         END DO

      END IF

!
!     If we only got one element set, ET  was either before the
!     first one in the segment or after the last one in the
!     segment.  We simply copy the one fetched a second time so
!     that the record is properly constructed.
!
      IF ( FROM .EQ. TO ) THEN
         CALL MOVED ( RECORD(BEGEL1), NELEMS + NANGS, RECORD(BEGEL2) )
      END IF

      CALL CHKOUT ( 'SPKR10' )
      RETURN
      END
