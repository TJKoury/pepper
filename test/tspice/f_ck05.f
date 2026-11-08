C$Procedure F_CK05 ( CK data type 05 tests )
 
      SUBROUTINE F_CK05 ( OK )
 
C$ Abstract
C
C     This routine tests the SPICELIB CK type 5 routines.
C     This is rudimentary set of tests to make sure the routines
C     cycle in every environment; thorough tests are performed
C     in the tspice_c test package.
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

      INCLUDE 'ck05.inc'

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
C     This routine tests routines that write and read type 05 CK 
C     data.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 27-JAN-2014 (NJB)
C
C        Added header.
C
C        Removed unused declaration of parameter MAXDEG.
C
C-    SPICELIB Version 1.0.0, 06-SEP-2002 (NJB)
C
C-&
 
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     
      LOGICAL               EXISTS
 
C
C     Local Parameters
C
      CHARACTER*(*)         CK05
      PARAMETER           ( CK05   = 'ck05_test.bc' )

      CHARACTER*(*)         REF
      PARAMETER           ( REF    = 'J2000' )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 200 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )
      
      
C
C     Local Variables
C      
      CHARACTER*(SIDLEN)    SEGID
      
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      EPOCHS ( 0 : BIGN-1 )
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      T1PACK ( C05PS1, 0 : BIGN-1 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               DEGREE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INST

      LOGICAL               AVFLAG
      LOGICAL               FOUND

C
C     Saved variables
C     
      SAVE                  Z

C
C     Initial values
C
      DATA                  Z / 0.D0, 0.D0, 1.D0 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CK05' )

C
C     Create and load  leapseconds kernel.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Define some CK data sets for use in testing.  First set:  a 
C     sequence of rotations about the z-axis.  The C-matrix starts
C     out as the identity.  The ith attitude is obtained by 
C     rotating about the z-axis by i microradians relative to the 
C     (i-1)st attitude.
C
C     The ith epoch is simply i.
C

      ANGLE = 0.D0
      SCALE = 1.D-9

      DO I = 0, BIGN-1

         ANGLE = ANGLE - I * SCALE

         CALL AXISAR ( Z, ANGLE, CMAT )

         CALL M2Q ( CMAT, T1PACK(1,I) )

         EPOCHS(I) =  DBLE(I)

      END DO

      CALL TCASE (  'CKW05 test:  create a new CK containing '//
     .              'segment of subtype 1.'                    )
 

      AVFLAG = .TRUE.
      RATE   = 1000.D0
      SEGID  = 'CK type 05 test segment.'
      INST   = 1
      DEGREE = 11

C
C     Open a new CK file.
C
      IF ( EXISTS(CK05) ) THEN
         CALL DELFIL ( CK05 )
      END IF

      CALL CKOPN ( CK05, ' ', 0, HANDLE )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKW05 ( HANDLE, C05TP1, DEGREE, EPOCHS(0), EPOCHS(BIGN-1),
     .             INST,   REF,    AVFLAG, SEGID,     BIGN,
     .             EPOCHS, T1PACK, RATE,   1,         EPOCHS        )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCLS ( HANDLE )



      CALL TCASE ( 'Recover pointing from segment of subtype 1 in '//
     .             'CK file CK05'                                  )
  
C
C     Now we'll use the CK user-level readers to look up pointing. 
C   
      CALL FURNSH ( CK05 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = 0.D0

      DO I = 0, BIGN - 1

         CALL CKGPAV ( INST,   EPOCHS(I), TOL, REF, CMAT, AV, 
     .                 CLKOUT, FOUND                          )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            CALL CHCKSD ( 'CLKOUT',   CLKOUT, '~', 
     ,                     EPOCHS(I), TIGHT,   OK )

            CALL M2Q ( CMAT, Q )
            CALL CHCKXC ( .FALSE., ' ', OK )
      
            CALL CHCKAD ( 'Q', Q, '~', T1PACK(1,I), 4, TIGHT, OK )

         END IF


      END DO

      CALL UNLOAD ( CK05 )

      IF ( EXISTS(CK05) ) THEN
         CALL DELFIL ( CK05 )
      END IF


      CALL T_SUCCESS ( OK )
      RETURN
      END
