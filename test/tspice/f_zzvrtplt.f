C$Procedure      F_ZZVRTPLT ( Test ZZVRTPLT )
 
      SUBROUTINE F_ZZVRTPLT ( OK )

C$ Abstract
C
C     Test the private SPICELIB routine ZZVRTPLT.
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
C     DSK
C
C$ Keywords
C
C     TEST 
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE 
      LOGICAL               OK

      INCLUDE 'dsk02.inc'
 
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
C     This routine tests the private routine ZZVRTPLT.
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
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 20-MAY-2016  (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C


C
C     Local parameters
C
      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

      INTEGER               MXCEL
      PARAMETER           ( MXCEL  = 3*MAXP  )

      INTEGER               MXLIST
      PARAMETER           ( MXLIST = 3*MAXP + MAXV )

C
C     Local Variables
C 
      CHARACTER*(LBLSIZ)    LABEL
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VERTS  ( 3, MAXV  )

      INTEGER               CELLS  ( 2, MXCEL )
      INTEGER               CELSIZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               PLATES ( 3, MAXP  )
      INTEGER               MAXLST
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLIST
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               P
      INTEGER               PLT
      INTEGER               PLTLST ( MXCEL )
      INTEGER               VRTPTR ( MAXV  )
      INTEGER               VTX

      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZVRTPLT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Set-up: create plates for a tessellated '
     .//           'ellipsoid. NLON = 100; NLAT = 50; NV = '
     .//           '4902; NP = 9800.'                       )
  
      A = 3000.D0
      B = 2000.D0
      C = 1000.D0

      NLON = 100
      NLAT = 50

      CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV,
     .                MAXP, NV, VERTS, NP,   PLATES      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Create vertex-plate mapping for tessellated ' 
     .//           'ellipsoid.'                                  )

      MAXLST = MXLIST

      CALL ZZVRTPLT ( NV,    NP,     PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST        ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The output list size should be exactly 
C
C        NV  +  3*NP
C
C     since each plate is on exactly three vertex lists, and
C     since there is one plate count per vertex.
C      
      CALL CHCKSI ( 'NLIST', NLIST, '=', NV+3*NP, 0, OK )

C
C     Each vertex pointer should be in the range 1:NLIST-1.
C
      DO I = 1, NV

         LABEL = 'VRTPTR(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSI ( LABEL, VRTPTR, '>=', 1,       0, OK )
         CALL CHCKSI ( LABEL, VRTPTR, '<=', NLIST-1, 0, OK )

      END DO

C
C     Given the tiling pattern, except for the polar vertices,
C     each vertex's plate count should be in the range 1:6.
C
      DO I = 1, NV-2

         N = PLTLST( VRTPTR(I) )
         LABEL = 'Plate count(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSI ( LABEL, N, '>=', 1, 0, OK )
         CALL CHCKSI ( LABEL, N, '<=', 6, 0, OK )

      END DO

C
C     The polar vertices are shared by NLON plates each.
C
      I = NV-1

      N = PLTLST( VRTPTR(I) )
      LABEL = 'Plate count(#)'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( LABEL, N, '=', NLON, 0, OK )

      I = NV

      N = PLTLST( VRTPTR(I) )
      LABEL = 'Plate count(#)'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( LABEL, N, '=', NLON, 0, OK )

C
C     Make sure each plate is on the plate lists of
C     its three vertices.
C
      DO I = 1, NP

         DO J = 1, 3
C
C           P is the pointer from vertex J of plate I
C           into the plate list. N is the plate count
C           for that vertex.
C
            VTX = PLATES(J,I)

            P   = VRTPTR( VTX )

            N   = PLTLST( P )
            
            FOUND = .FALSE.
            K     = 0

            DO WHILE ( ( K .LT. N ) .AND. ( .NOT. FOUND ) ) 
C
C              Examine the next plate in the list of the
C              current vertex.
C
               K     = K + 1

               PLT   = PLTLST( P + K )
               
               FOUND = ( PLT .EQ. I ) 

            END DO
            

            LABEL = 'Entry for plate(#), vertex(#)'

            CALL REPMI ( LABEL, '#', I,   LABEL )
            CALL REPMI ( LABEL, '#', VTX, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         END DO

      END DO

C
C     If the last test passed, each plate is on at least the three
C     plate lists of its own vertices. The plate is not on any other
C     vertex's list, since total entry count is correct. The plate list
C     is valid.
C

C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad plate count.'  )

      MAXLST = MXCEL

      CALL ZZVRTPLT ( NV,    0,      PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST        ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )


      CALL ZZVRTPLT ( NV,    -1,     PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST        ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad vertex count.'  )

      MAXLST = MXCEL

      CALL ZZVRTPLT ( 0,     NP,     PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST        ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )


      CALL ZZVRTPLT ( -1,    NP,     PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST        ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Cell array too small.'  )

      MAXLST = MXCEL

      CELSIZ = 3*NP - 1

      CALL ZZVRTPLT ( NV,    NP,     PLATES, CELSIZ, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST         ) 
      CALL CHCKXC ( .TRUE., 'SPICE(CELLARRAYTOOSMALL)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Plate list too small.'  )


      MAXLST = 3*NP + NV - 1

      CALL ZZVRTPLT ( NV,    NP,     PLATES, MXCEL, MAXLST, 
     .                CELLS, VRTPTR, NLIST,  PLTLST         ) 
      CALL CHCKXC ( .TRUE., 'SPICE(PLATELISTTOOSMALL)', OK )
      

      CALL T_SUCCESS ( OK )
      RETURN
      END
