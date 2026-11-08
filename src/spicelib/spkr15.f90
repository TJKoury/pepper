!$Procedure SPKR15 ( Read SPK record from segment, type 15 )

      SUBROUTINE SPKR15 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 15
!     (Precessing Conic Propagation).
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

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR (5)
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD(*)

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
!              a SPK segment of type 15.
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
!     1)  If the segment specified by DESCR is not a type 15 segment,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     2)  A type 15 segment should have exactly 16 values. If this
!         is not the case, the error SPICE(MALFORMEDSEGMENT) is
!         signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine reads all of the data from a type 15 SPK segment.
!
!     The structure of the data retrieved in RECORD is:
!
!         RECORD(1)             epoch of the orbit elements at periapse
!                               in ephemeris seconds past J2000.
!         RECORD(2)-RECORD(4)   unit trajectory pole vector
!         RECORD(5)-RECORD(7)   unit periapsis vector
!         RECORD(8)             semi-latus rectum---p in the
!                               equation:
!
!                               r = p/(1 + ECC*COS(Nu))
!
!         RECORD(9)             eccentricity
!         RECORD(10)            J2 processing flag describing
!                               what J2 corrections are to be
!                               applied when the orbit is
!                               propagated.
!
!                                Value       Meaning
!                                -----  -----------------------------
!                                1      Regress line of nodes only.
!                                2      Precess line of apsides only.
!                                3      Don't use J2 corrections.
!                                Other  Regress line of nodes
!                                       and precess line of apsides.
!
!         RECORD(11)-RECORD(13) unit central body pole vector
!         RECORD(14)            central body GM
!         RECORD(15)            central body J2
!         RECORD(16)            central body radius
!
!     Except for J2, units are radians, km, seconds.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
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
!           IF ( TYPE .EQ. 15 ) THEN
!              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
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
!     S. Schlaifer       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
!
!-&


! $ Index_Entries
!
!     read record from type_15 SPK segment
!
!-&


!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

!
!     The difference between the first and last address of a type 15
!     segment should be 15.
!
      INTEGER               DIFF
      PARAMETER           ( DIFF = 15 )

      DOUBLE PRECISION      DC  ( ND )
      INTEGER               IC  ( NI )

      INTEGER               BEGIN
      INTEGER               END
      INTEGER               TYPE

!
!     Standard Spice Error Handling.
!
      IF (RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN('SPKR15')
!
!     Unpack the segment descriptor.
!
      CALL DAFUS( DESCR, ND, NI, DC, IC )

      TYPE  = IC(4)
      BEGIN = IC(5)
      END   = IC(6)
!
!     Make sure that this really is a type 15 data segment.
!
      IF (TYPE .NE. 15) THEN

         CALL SETMSG ('You are attempting to locate type 15 ' // &
                      'data in a type # data segment.')
         CALL ERRINT ('#', TYPE                       )
         CALL SIGERR ('SPICE(WRONGSPKTYPE)'           )
         CALL CHKOUT ('SPKR15'                        )
         RETURN

      END IF

!
!     Since it doesn't cost much we make sure that the segment has
!     the correct amount of data.
!
      IF ( END - BEGIN .NE. DIFF ) THEN

         CALL SETMSG ( 'A type 15 segment should contain exactly ' &
         //            '16 double precision values.  The segment ' &
         //            'supplied had #.  The segment is badly ' &
         //            'formed. ' )

         CALL ERRINT ( '#',   END-BEGIN+1         )
         CALL SIGERR ( 'SPICE(MALFORMEDSEGMENT)'  )
         CALL CHKOUT ( 'SPKR15'                   )
         RETURN

      END IF

!
!     Read the data for the record.
!
      CALL DAFGDA ( HANDLE, BEGIN, END, RECORD )
      CALL CHKOUT ('SPKR15')
      RETURN
      END
