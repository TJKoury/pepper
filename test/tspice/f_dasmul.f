C$Procedure F_DASMUL ( DAS test, multiple DAS files )
 
      SUBROUTINE F_DASMUL ( OK )
 
C$ Abstract
C
C     Exercise DAS routines using multiple DAS files.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
  

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the ability of the DAS system to read from
C     FTSIZE open DAS files, where FTSIZE is the limit imposed by
C     the handle manager subsystem.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This file currently open DAS files using DASOPR. 
C     When the required supporting code is available, the
C     DASOPR calls can be replaced by calls to FURNSH and
C     KINFO.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 11-FEB-2017 (NJB)
C
C        Removed unneeded declarations.
C
C        26-FEB-2015 (NJB)
C
C          Original version.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C

      INTEGER               SCHCOD
      PARAMETER           ( SCHCOD = 2 )
  

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
 
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

      INTEGER               MAXCLU
      PARAMETER           ( MAXCLU = 1000 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 12 )

      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2 )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )


      INTEGER               FILEX
      PARAMETER           ( FILEX  = 100000 )

      INTEGER               RECX
      PARAMETER           ( RECX   =  1000 )


C
C     Local Variables
C
      CHARACTER*(MAXVAL)    CDATA
      CHARACTER*(FILSIZ)    DAS    ( FTSIZE )
      CHARACTER*(MAXVAL)    XCDATA

      DOUBLE PRECISION      DDATA  ( MAXVAL )
      DOUBLE PRECISION      XDDATA ( MAXVAL )

      INTEGER               CLNWDS ( MAXCLU )
      INTEGER               CLTYPS ( MAXCLU )
      INTEGER               HANDLE ( FTSIZE )
      INTEGER               I
      INTEGER               IDATA  ( MAXVAL )
      INTEGER               J
      INTEGER               K
      INTEGER               NCLUST
      INTEGER               NCOMRC
      INTEGER               NFILES
      INTEGER               NSCHEM
      INTEGER               RECNO
      INTEGER               XIDATA ( MAXVAL )

      LOGICAL               FCLOSE
      LOGICAL               SGRGAT
 

C
C     Saved all.
C     
      SAVE 

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DASMUL' )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create DAS files.' )

      NFILES = FTSIZE 

      NFILES = 5000

      NCOMRC = 2

      NCLUST = 3
      
      CLTYPS(1) = DP
      CLTYPS(2) = CHR
      CLTYPS(3) = INT

      CLNWDS(1) = 5
      CLNWDS(2) = 7
      CLNWDS(3) = 12

      SGRGAT    = .FALSE.
      FCLOSE    = .TRUE.
      NSCHEM    = SCHCOD
      
      DO I = 1, NFILES

         DAS(I) = 'mult_test_#.das'
         CALL REPMI ( DAS(I), '#', I, DAS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF (  EXISTS( DAS(I) )  ) THEN
            CALL DELFIL ( DAS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         CALL TSTDAS ( DAS(I),   'DSK',  NCOMRC, I,      NCLUST,
     .                 CLTYPS,   CLNWDS, SGRGAT, FCLOSE, NSCHEM,
     .                 HANDLE(I)                                 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
      

      DO I = 1, NFILES

         CALL DASOPR( DAS(I), HANDLE(I) )

C         CALL FURNSH ( DAS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

C         CALL KINFO ( DAS(I), FILTYP, SOURCE, HANDLE(I), FOUND )
C         CALL CHCKXC ( .FALSE., ' ', OK )
C         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      END DO         

C
C     Read data from open files.
C
      DO I = 1, NFILES

         DO J = 1, NCLUST

            IF ( CLTYPS(J) .EQ. INT ) THEN

               CALL DASRDI ( HANDLE(I), 1, CLNWDS(J), IDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 3

               DO K = 1, CLNWDS(J)
                  XIDATA(K) = ( I * FILEX ) + RECX*RECNO + K
               END DO               

               CALL CHCKAI ( 'IDATA', IDATA,  '=', 
     .                                XIDATA, CLNWDS(J), OK )
               

            ELSE IF ( CLTYPS(J) .EQ. DP ) THEN

               CALL DASRDD ( HANDLE(I), 1, CLNWDS(J), DDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 1

               DO K = 1, CLNWDS(J)
                  XDDATA(K) = ( I * FILEX ) + RECX*RECNO + DBLE(K)
               END DO

               CALL CHCKAD ( 'DDATA', DDATA,  '=', 
     .                                XDDATA, CLNWDS(J), 0.D0, OK )

            ELSE

               CALL DASRDC ( HANDLE(I), 1, CLNWDS(J), 1, MAXVAL, CDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 2

               DO K = 1, CLNWDS(J)

                  XIDATA(K)   = ( I * FILEX ) + RECX*RECNO + K

                  XCDATA(K:K) = CHAR( MOD( XIDATA(K), 128 ) )

               END DO

               CALL CHCKSC ( 'CDATA', CDATA, '=', XCDATA, OK )

            END IF

         END DO

      END DO

      DO I = 1, NFILES

         CALL DASCLS ( HANDLE(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete DAS files.' )


      DO I = 1, NFILES

         CALL DELFIL ( DAS(I)    )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END
