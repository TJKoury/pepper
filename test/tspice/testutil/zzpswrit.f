C$Procedure ZZPSWRIT ( Plate set, write to file )
 
      SUBROUTINE ZZPSWRIT ( UNIT, V1, P1 )
 
C$ Abstract
C
C     Write a plate set to a DSK "plate-vertex" format text file (code
C     1 in the MKDSK schema) specified by a Fortran logical unit.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     CELLS
C     DSK
C
C$ Keywords
C
C     DSK
C     PLATE
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               UNIT
      DOUBLE PRECISION      V1   ( LBCELL : * )
      INTEGER               P1   ( LBCELL : * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     UNIT       I   Logical unit of output file.
C     V1         I   Vertex cell of a plate set.
C     P1         I   Plate cell of a plate set.
C
C$ Detailed_Input
C
C     UNIT           is a Fortran logical unit of a text for open
C                    for write access. The input plate set will be
C                    written to the file designated by this unit.
C
C     V1             is a SPICELIB double precision cell containing the
C                    vertices of a plate set.
C
C     P1             is a SPICELIB integer cell containing the
C                    plates of a plate set.
C
C$ Detailed_Output
C
C     None. See the Files section below.
C     
C$ Parameters
C
C     LBCELL is the lower bound of the SPICELIB cell data structure.
C
C$ Exceptions
C
C     1) If the cardinality of V1 is not a multiple of 3, the error 
C        SPICE(BADVERTEXARRAY) is signaled.
C
C     2) If the cardinality of P1 is not a multiple of 3, the error 
C        SPICE(BADPLATEARRAY) is signaled.
C
C     3) If an I/O error occurs during a Fortran write operation,
C        the error SPICE(FILEWRITEFAILED) will be signaled.
C
C$ Files
C
C     The output file written by this routine 
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
C     
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    DSKLIB Version 1.0.0, 03-SEP-2014 (NJB)
C
C-&
 

C
C     SPICELIB functions
C     
      INTEGER               CARDD
      INTEGER               CARDI

C
C     Local parameters
C 

C
C     Local variables
C 
      INTEGER               CP1
      INTEGER               CV1
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               K
      INTEGER               NP
      INTEGER               NV

C
C     Saved variables
C
      CALL CHKIN ( 'ZZPSWRIT' )

      NV = CARDD( V1 ) / 3
      NP = CARDI( P1 ) / 3 

C
C     Make sure the cardinality of the input vertex set is a 
C     multiple of 3.
C
      CV1 = CARDD( V1 )
      NV  = CV1 / 3

      IF ( ( NV * 3 ) .NE. CV1 ) THEN

         CALL SETMSG ( 'Input vertex set cardinality # is not '
     .   //            'a multiple of 3.'                      )
         CALL ERRINT ( '#', CV1                                )
         CALL SIGERR ( 'SPICE(BADVERTEXARRAY)'                 )
         CALL CHKOUT ( 'ZZPSWRIT'                              )
         RETURN

      END IF

C
C     Make sure the cardinality of the input plate set is a 
C     multiple of 3.
C
      CP1 = CARDI( P1 )
      NP  = CP1 / 3

      IF ( ( NP * 3 ) .NE. CP1 ) THEN

         CALL SETMSG ( 'Input plate set cardinality # is not '
     .   //            'a multiple of 3.'                      )
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

         WRITE( UNIT, 
     .          FMT    = '(1X,I11,3E26.17)',
     .          IOSTAT = IOSTAT )             I, ( V1(J+K),  K = 1, 3 )

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

         WRITE( UNIT, 
     .          FMT    = '(1X,4I10)', 
     .          IOSTAT = IOSTAT )       I, ( P1(J+K),  K = 1, 3 )

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
