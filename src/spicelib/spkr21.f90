!$Procedure SPKR21 ( Read SPK record from segment, type 21 )

      SUBROUTINE SPKR21 ( HANDLE, DESCR, ET, RECORD )

! $ Abstract
!
!     Read a single SPK data record from a segment of type 21
!     (Extended Difference Lines).
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
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'spk21.inc'

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
!     ET         I   Evaluation epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 21.
!
!     ET       is an epoch for which a data record from a specific
!              segment is required. The epoch is represented as
!              seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of an
!              ephemeris object, relative to its center of motion,
!              in an inertial reference frame.
!
!              The contents of RECORD are as follows:
!
!                 RECORD(1):         The difference table size per
!                                    Cartesian component. Call this
!                                    size MAXDIM; then the difference
!                                    line (MDA) size DLSIZE is
!
!                                      ( 4 * MAXDIM ) + 11
!
!                 RECORD(2)
!                    ...
!                 RECORD(1+DLSIZE):  An extended difference line.
!                                    The contents are:
!
!                    Dimension  Description
!                    ---------  ----------------------------------
!                    1          Reference epoch of difference line
!                    MAXDIM     Stepsize function vector
!                    1          Reference position vector,  x
!                    1          Reference velocity vector,  x
!                    1          Reference position vector,  y
!                    1          Reference velocity vector,  y
!                    1          Reference position vector,  z
!                    1          Reference velocity vector,  z
!                    MAXDIM,3   Modified divided difference
!                               arrays (MDAs)
!                    1          Maximum integration order plus 1
!                    3          Integration order array
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the maximum table size of the input record exceeds
!         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
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
!     structure of a data type 21 segment.
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
!              CALL SPKR21 ( HANDLE, DESCR, ET, RECORD )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     F.T. Krogh         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LSTLTD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

!
!     Local variables
!
      DOUBLE PRECISION      DATA   ( BUFSIZ )
      DOUBLE PRECISION      DC     ( 2 )

      INTEGER               BEGIN
      INTEGER               DFLSIZ
      INTEGER               END
      INTEGER               I
      INTEGER               IC     ( 6 )
      INTEGER               MAXDIM
      INTEGER               NDIR
      INTEGER               NREC
      INTEGER               OFF
      INTEGER               OFFD
      INTEGER               OFFE
      INTEGER               OFFR
      INTEGER               RECNO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR21' )

!
!     Unpack the segment descriptor.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      BEGIN = IC(5)
      END   = IC(6)

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFD      The offset of the first directory epoch.
!
!        OFFE      The offset of the first epoch.
!
!
!     the number of directory epochs.
!
!     We'll fetch the difference table dimension as well.
!
      CALL DAFGDA ( HANDLE, END-1, END, DATA )

      NREC   = NINT ( DATA(2) )
      NDIR   = NREC / BUFSIZ
      OFFD   = END  - NDIR - 2
      OFFE   = OFFD - NREC

      MAXDIM = NINT ( DATA(1) )

      IF ( MAXDIM .GT. MAXTRM ) THEN

         CALL SETMSG ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         CALL ERRINT ( '#',  MAXDIM                            )
         CALL ERRINT ( '#',  MAXTRM                            )
         CALL SIGERR ( 'SPICE(DIFFLINETOOLARGE)'               )
         CALL CHKOUT ( 'SPKR21'                                )
         RETURN

      END IF

!
!     The difference line dimension per component is the
!     first element of the output record.
!
      RECORD( 1 ) = MAXDIM

!
!     Set the difference line size.
!
      DFLSIZ = ( 4 * MAXDIM ) + 11

!
!     What we want is the record number: once we have that, we can
!     compute the offset of the record from the beginning of the
!     segment, grab it, and go. But how to find it?
!
!     Ultimately, we want the first record whose epoch is greater
!     than or equal to ET. If there are BUFSIZ or fewer records, all
!     the record epochs can be examined in a single group.
!
      IF ( NREC .LE. BUFSIZ ) THEN

         CALL DAFGDA ( HANDLE, OFFE+1, OFFE+NREC, DATA )
         RECNO = LSTLTD ( ET, NREC, DATA ) + 1

         OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ
         CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )

         CALL CHKOUT ( 'SPKR21' )
         RETURN

      END IF

!
!     Searching directories is a little more difficult.
!
!     The directory contains epochs BUFSIZ, 2*BUFSIZ, and so on. Once
!     we find the first directory epoch greater than or equal to ET, we
!     can grab the corresponding set of BUFSIZ record epochs, and
!     search them.
!
      DO I = 1, NDIR

         CALL DAFGDA ( HANDLE, OFFD+I, OFFD+I, DATA )

         IF ( DATA(1) .GE. ET ) THEN

            OFF = OFFE + (I - 1) * BUFSIZ
            CALL DAFGDA ( HANDLE, OFF+1, OFF+BUFSIZ, DATA )

            RECNO = ((I - 1) * BUFSIZ) + LSTLTD ( ET, BUFSIZ, DATA ) + 1

            OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ
            CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )

            CALL CHKOUT ( 'SPKR21' )
            RETURN

         END IF

      END DO

!
!     If ET is greater than the final directory epoch, we want one
!     of the final records.
!
      I = MOD ( NREC, BUFSIZ )

      CALL DAFGDA ( HANDLE, END-NDIR-I-1, END-NDIR-2, DATA )

      RECNO = (NDIR * BUFSIZ) + LSTLTD ( ET, I, DATA ) + 1

      OFFR = (BEGIN - 1) + (RECNO - 1) * DFLSIZ

      CALL DAFGDA ( HANDLE, OFFR+1, OFFR+DFLSIZ, RECORD(2) )


      CALL CHKOUT ( 'SPKR21' )
      RETURN
      END
