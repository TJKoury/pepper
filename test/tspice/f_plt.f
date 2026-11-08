C$Procedure F_PLT ( Plate routine tests )
 
      SUBROUTINE F_PLT ( OK )
 
C$ Abstract
C
C     Exercise plate routines.
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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB plate routines that 
C     are not covered by their own test families. These are:
C
C        PLTAR
C        PLTNRM
C        PLTVOL
C        PLTEXP
C
C     The plate routine
C
C        PLTNP
C
C     has its own, separate test family.
C
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
C-    TSPICE Version 1.0.0, 24-OCT-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PLTAR
      DOUBLE PRECISION      PLTVOL

      INTEGER               CARDD
      INTEGER               CARDI

C
C     Local Parameters 
C
C      DOUBLE PRECISION      TIGHT 
C      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT 
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 1000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      AREA
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      EDGE1  ( 3 )
      DOUBLE PRECISION      EDGE2  ( 3 )
      DOUBLE PRECISION      IVERTS ( 3, 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      OVERTS ( 3, 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )
      DOUBLE PRECISION      VOL
      DOUBLE PRECISION      VOUT   ( LBCELL : 3*MAXV )
      DOUBLE PRECISION      VOUT1  ( LBCELL : 3*MAXV )
      DOUBLE PRECISION      VRTCES ( 3, MAXV )
      DOUBLE PRECISION      XAREA
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      XVERTS ( 3, 3 )
      DOUBLE PRECISION      XVOL

      INTEGER               I
      INTEGER               J
      INTEGER               NP
      INTEGER               NV
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               PLATS2 ( 3, MAXP )
      INTEGER               POUT   ( LBCELL : 3*MAXP )
      INTEGER               POUT1  ( LBCELL : 3*MAXP )

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_PLT' )



C**********************************************************************
C
C     PLTAR tests
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTAR Error: too few plates' 

      CALL TCASE ( TITLE )
 
      CALL CLEARD ( 3*MAXV, VRTCES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( 3*MAXP, PLATES )

      NV = 3
      NP = -1

      AREA = PLTAR ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTAR Error: too few vertices' 

      CALL TCASE ( TITLE )

      NV = 2
      NP = 1

      AREA = PLTAR ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWVERTICES)', OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTAR Error: bad vertex index in plate' 

      CALL TCASE ( TITLE )
C
C     Initialize cells.
C
      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a plate set for a box.
C
      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NP = CARDI(POUT) / 3
      NV = CARDD(VOUT) / 3

C
C     Create a bad plate: first vertex index is 0.
C
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 1, 1 ) = 0
      
      AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

C
C     Restore the original plate. It's simplest to copy the
C     plate set. Set another invalid vertex index.
C
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 2, 1 ) = 0

      DO I = 1, NP

         DO J = 1, 3

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = 0

            AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = NV+1

            AREA = PLTAR ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

         END DO

      END DO
 

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTAR: Find area of a box centered at the origin.' 

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NV = CARDD( VOUT ) / 3
      NP = CARDI( POUT ) / 3

      AREA = PLTAR ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XAREA = 2 * ( ( A * B ) + ( B * C ) + ( A * C ) )

      CALL CHCKSD ( 'AREA', AREA, '~/', XAREA, VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTAR: Find area of a box that excludes the origin.' 

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C     
C     Create a vector by which to translate the vertices of the box.
C
      CALL VPACK ( 100.D0, 200.D0, -400.D0, OFFSET )

C
C     Create a translated box. The plate set doesn't change.
C
      CALL ZZPSXLAT ( VOUT, OFFSET, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compute the area of the translated box.
C     
      AREA = PLTAR ( NV, VOUT1(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AREA', AREA, '~/', XAREA, VTIGHT, OK )



C**********************************************************************
C
C     PLTVOL tests
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTVOL Error: too few plates' 

      CALL TCASE ( TITLE )
 
      CALL CLEARD ( 3*MAXV, VRTCES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARI ( 3*MAXP, PLATES )

      NV = 4
      NP = 3

      AREA = PLTVOL ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPLATES)', OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTVOL Error: too few vertices' 

      CALL TCASE ( TITLE )

      NV = 3
      NP = 4

      AREA = PLTVOL ( NV, VRTCES, NP, PLATES )
      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWVERTICES)', OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLVOL Error: bad vertex index in plate' 

      CALL TCASE ( TITLE )
C
C     Initialize cells.
C
      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a plate set for a box.
C
      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NP = CARDI(POUT) / 3
      NV = CARDD(VOUT) / 3

C
C     Create a bad plate: first vertex index is 0.
C
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 1, 1 ) = 0
      
      VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

C
C     Restore the original plate. It's simplest to copy the
C     plate set. Set another invalid vertex index.
C
      CALL MOVEI ( POUT(1), 3*NP, PLATS2 )

      PLATS2( 2, 1 ) = 0

      DO I = 1, NP

         DO J = 1, 3

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = 0

            VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

            CALL MOVEI ( POUT(1), 3*NP, PLATS2 )
            PLATS2( J, I ) = NV+1

            VOL = PLTVOL ( NV, VRTCES, NP, PLATS2 )
            CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

         END DO

      END DO
 


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTVOL: Find volume of a box centered at the origin.' 

      CALL TCASE ( TITLE )

      CALL SSIZED ( 3*MAXV, VOUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = 10.D0
      B = 20.D0
      C = 30.D0

      CALL ZZPSBOX ( A, B, C, VOUT, POUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NV = CARDD( VOUT ) / 3
      NP = CARDI( POUT ) / 3

      VOL = PLTVOL ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XVOL = A * B * C

      CALL CHCKSD ( 'VOL', VOL, '~/', XVOL, VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTVOL: Find volume of a box that excludes the origin.' 

      CALL SSIZED ( 3*MAXV, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( 3*MAXP, POUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C     
C     Create a vector by which to translate the vertices of the box.
C
      CALL VPACK ( 100.D0, 200.D0, -400.D0, OFFSET )

C
C     Create a translated box. The plate set doesn't change.
C
      CALL ZZPSXLAT ( VOUT, OFFSET, VOUT1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compute the volume of the translated box.
C     
      VOL = PLTVOL ( NV, VOUT(1), NP, POUT(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XVOL = A * B * C

      CALL CHCKSD ( 'VOL', VOL, '~/', XVOL, VTIGHT, OK )



C**********************************************************************
C
C     PLTNRM tests
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'PLTNRM: Compute an upward normal of an equilateral '
     .//       'triangle lying in the X-Y plane and centered '
     .//       'at the origin.' 

      CALL TCASE( TITLE )

      
      S = SQRT(3.D0)/2

      CALL VPACK (    S,  -0.5D0,  0.D0, V1 )
      CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 )
      CALL VPACK (   -S,  -0.5D0,  0.D0, V3 )


      CALL PLTNRM ( V1, V2, V3, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compute an expected normal vector.
C
      CALL VSUB ( V2, V1, EDGE1 )
      CALL VSUB ( V3, V2, EDGE2 )

      CALL VCRSS ( EDGE1, EDGE2, XNORML )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'NORMAL', NORMAL, '~~/', XNORML, 3, VTIGHT, OK )

C
C     Convert normal to unit length and compare to a known vector.
C
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL VHATIP ( NORMAL )

      CALL CHCKAD ( 'NORMAL (unit)', NORMAL, '~~/', 
     .              XNORML, 3, VTIGHT, OK )




C**********************************************************************
C
C     PLTEXP tests
C
C**********************************************************************



      TITLE = 'PLTEXP: expand a plate that is parallel to, but not '
     .//      'contained in, the X-Y plane.'

      CALL TCASE ( TITLE )

      CALL VPACK (  -1.D0,  -1.D0,   3.D0,  IVERTS(1,1) )
      CALL VPACK (   2.D0,  -1.D0,   3.D0,  IVERTS(1,2) )
      CALL VPACK (   2.D0,   4.D0,   3.D0,  IVERTS(1,3) )

      DELTA = 1.D0

      CALL PLTEXP ( IVERTS, DELTA, OVERTS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Prepare the expected output.
C
      S = 1.D0 / 3.D0

      CALL VLCOM3 ( S, IVERTS(1,1), S, IVERTS(1,2), 
     .              S, IVERTS(1,3),    CENTER      )

      DO I = 1, 3

         CALL VSUB  ( IVERTS(1,I), CENTER, OFFSET )

         CALL VLCOM ( 1.D0, CENTER, 2.D0, OFFSET, XVERTS(1,I) )

      END DO
     

      CALL CHCKAD ( 'OVERTS', OVERTS, '~~/', XVERTS, 9, VTIGHT, OK )
      
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


