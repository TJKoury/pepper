!$Procedure ZZPSWRIT ( Plate set, write to file )

      SUBROUTINE ZZPSWRIT ( UNIT, V1, P1 )

! $ Abstract
!
!     Write a plate set to a DSK "plate-vertex" format text file (code
!     1 in the MKDSK schema) specified by a Fortran logical unit.
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
!     CELLS
!     DSK
!
! $ Keywords
!
!     DSK
!     PLATE
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               UNIT
      DOUBLE PRECISION      V1   ( LBCELL : * )
      INTEGER               P1   ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit of output file.
!     V1         I   Vertex cell of a plate set.
!     P1         I   Plate cell of a plate set.
!
! $ Detailed_Input
!
!     UNIT           is a Fortran logical unit of a text for open
!                    for write access. The input plate set will be
!                    written to the file designated by this unit.
!
!     V1             is a SPICELIB double precision cell containing the
!                    vertices of a plate set.
!
!     P1             is a SPICELIB integer cell containing the
!                    plates of a plate set.
!
! $ Detailed_Output
!
!     None. See the Files section below.
!
! $ Parameters
!
!     LBCELL is the lower bound of the SPICELIB cell data structure.
!
! $ Exceptions
!
!     1) If the cardinality of V1 is not a multiple of 3, the error
!        SPICE(BADVERTEXARRAY) is signaled.
!
!     2) If the cardinality of P1 is not a multiple of 3, the error
!        SPICE(BADPLATEARRAY) is signaled.
!
!     3) If an I/O error occurs during a Fortran write operation,
!        the error SPICE(FILEWRITEFAILED) will be signaled.
!
! $ Files
!
!     The output file written by this routine
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    DSKLIB Version 1.0.0, 03-SEP-2014 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               CARDI

!
!     Local parameters
!

!
!     Local variables
!
      INTEGER               CP1
      INTEGER               CV1
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               K
      INTEGER               NP
      INTEGER               NV

!
!     Saved variables
!
      CALL CHKIN ( 'ZZPSWRIT' )

      NV = CARDD( V1 ) / 3
      NP = CARDI( P1 ) / 3

!
!     Make sure the cardinality of the input vertex set is a
!     multiple of 3.
!
      CV1 = CARDD( V1 )
      NV  = CV1 / 3

      IF ( ( NV * 3 ) .NE. CV1 ) THEN

         CALL SETMSG ( 'Input vertex set cardinality # is not ' &
         //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CV1                                )
         CALL SIGERR ( 'SPICE(BADVERTEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSWRIT'                              )
         RETURN

      END IF

!
!     Make sure the cardinality of the input plate set is a
!     multiple of 3.
!
      CP1 = CARDI( P1 )
      NP  = CP1 / 3

      IF ( ( NP * 3 ) .NE. CP1 ) THEN

         CALL SETMSG ( 'Input plate set cardinality # is not ' &
         //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CP1                                )
         CALL SIGERR ( 'SPICE(BADPLATEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSWRIT'                              )
         RETURN

      END IF

      WRITE( UNIT, FMT = '(1X,I10)', IOSTAT = IOSTAT )  NV

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Write of NV failed; IOSTAT = #.' )
         CALL ERRINT ( '#', IOSTAT                       )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'          )
         CALL CHKOUT ( 'ZZPSWRIT'                        )
         RETURN

      END IF

      DO I = 1, NV

         J = 3*(I-1)

         WRITE( UNIT, &
                FMT    = '(1X,I11,3E26.17)', &
                IOSTAT = IOSTAT )             I, ( V1(J+K),  K = 1, 3 )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Write of vertex # failed; IOSTAT = #.' )
            CALL ERRINT ( '#', I                                  )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'ZZPSWRIT'                              )
            RETURN

         END IF

      END DO

      WRITE( UNIT, FMT = '(1X,I10)', IOSTAT = IOSTAT )  NP

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Write of NP failed; IOSTAT = #.' )
         CALL ERRINT ( '#', IOSTAT                       )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'          )
         CALL CHKOUT ( 'ZZPSWRIT'                        )
         RETURN

      END IF


      DO I = 1, NP

         J = 3*(I-1)

         WRITE( UNIT, &
                FMT    = '(1X,4I10)', &
                IOSTAT = IOSTAT )       I, ( P1(J+K),  K = 1, 3 )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Write of plate # failed; IOSTAT = #.' )
            CALL ERRINT ( '#', I                                 )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
            CALL CHKOUT ( 'ZZPSWRIT'                             )
            RETURN

         END IF

      END DO

      CALL CHKOUT ( 'ZZPSWRIT' )
      END
