
C$Procedure      F_BODVAR ( BODVAR family tests )

      SUBROUTINE F_BODVAR ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routines
C     
C        BODVAR
C        BODVRD
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
C$ Version
C
C-    TSPICE Version 1.0.0, 11-FEB-2004 (NJB)
C
C-&
 
C
C     SPICELIB functions
C

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT 
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    CVALS   ( 3 )
      CHARACTER*(FILSIZ)    PCK

      DOUBLE PRECISION      RADII   ( 3 )
      DOUBLE PRECISION      XRADII  ( 3 )

      INTEGER               N

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_BODVAR' )

      CALL TCASE ( 'Setup:  create full text PCK file.' )


      PCK = 'test_0008.tpc'

C
C     Create the PCK file, load it, and delete it.
C      
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the radii of the earth from the kernel pool using
C     BODVRD.
C
      CALL TCASE ( 'Look up earth radii using BODVRD' )

      CALL VPACK  ( 6378.14D0, 6378.14D0, 6356.75D0, XRADII )

      CALL BODVRD ( 'EARTH', 'RADII', 3,  N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'radii count', N,  '=', 3, 0, OK )
      
      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )



      CALL TCASE ( 'Look up earth radii using BODVRD.  Name = 399' )

      CALL VPACK  ( 6378.14D0, 6378.14D0, 6356.75D0, XRADII )

      CALL BODVRD ( '399', 'RADII', 3,  N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'radii count', N,  '=', 3, 0, OK )
      
      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )


C
C     Error cases for BODVRD:
C
      CALL TCASE ( 'Output array too small' )

      CALL BODVRD ( 'EARTH', 'RADII', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


      CALL TCASE ( 'Data type mismatch' )

      CVALS(1) = 'A'
      CVALS(2) = 'B'
      CVALS(3) = 'C'

      CALL PCPOOL ( 'BODY399_SYMBOLIC_RADII', 3, CVALS )

      CALL BODVRD ( 'EARTH', 'SYMBOLIC_RADII', 3,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(TYPEMISMATCH)', OK )


      CALL TCASE ( 'Variable not present' )

      CALL BODVRD ( 'EARTH', 'radii', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


      CALL TCASE ( 'Body name not associated with code' )

      CALL BODVRD ( 'XYZ', 'RADII', 2,  N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )


C
C     Try again with BODVAR.
C
      CALL TCASE ( 'Look up earth radii using BODVAR' )
      CALL CLEARD ( 3, RADII )

      CALL BODVAR ( 399, 'RADII', N, RADII )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Earth radii', RADII,  '~', XRADII,  3, TIGHT , OK )




      CALL TCASE ( 'Variable not present' )

      CALL BODVAR ( 399, 'radii', N, RADII )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL T_SUCCESS ( OK )

      END
